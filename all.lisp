(defpackage #:bosom-serpent/all
  (:use #:cl #:alexandria #:serapeum)
  (:nicknames #:bosom-serpent)
  (:import-from #:overlord
    #:define-loader-language #:module-exports #:module-ref)
  (:import-from #:burgled-batteries
    #:run #:run* #:startup-python #:defpyfun)
  (:import-from #:python.cffi
    #:callable.check
    #:.dec-ref
    #:object.call-object
    #:.is-initialized
    #:name-error #:attribute-error)
  (:import-from #:uiop
    #:native-namestring #:absolute-pathname-p)
  (:import-from #:trivial-garbage
    #:finalize)
  (:import-from #:bordeaux-threads
    #:make-lock #:with-lock-held)
  (:shadowing-import-from #:cl-ppcre
    #:scan)
  (:import-from #:trivia #:match)
  (:import-from #:trivia.ppcre #:ppcre)
  (:shadowing-import-from #:burgled-batteries
    #:import))
(in-package #:bosom-serpent/all)

;;; NB If you initialize Python in a Lisp image, save the Lisp image,
;;; and reload it, then Python will not be initialized. Thus the
;;; defensive attitude of the code below.

;;; Uniquifying module names.

(defvar *module-counter* 0
  "Counter to ensure that Python module names are globally unique.")
(declaim (type (integer 0 *) *module-counter*))

(defun uniquify-module (source)
  "Generate a globally unique module name for SOURCE.

Using unique names allows us to reliably reload SOURCE when it
changes, and ensures that we can recognize when SOURCE has not been
loaded (by listening for the correct NameError)."
  (fmt "~a_~a"
       (pathname-name source)
       (incf *module-counter*)))

;;; Locking.

(def py-lock (make-lock)
  "Lock for all access to Python.")

(defmacro with-py-lock (&body body)
  `(with-lock-held (py-lock)
     ,@body))

;;; Executing Python strings.

(defun py (control-string &rest args)
  "Format a Python string and execute it."
  (run (fmt "~?" control-string args)))

(define-compiler-macro py (&whole call control-string &rest args)
  "Wrap a literal control-string with `formatter'."
  (if (stringp control-string)
      `(py (formatter ,control-string) ,@args)
      call))

;;; The language.

(define-loader-language bosom-serpent/python2 (source)
  (with-py-lock
    (ensure-python)
    (make 'python-module :source source))
  :extension "py")

;;; Ensuring things are loaded.

(defun ensure-python ()
  "Ensure that Python is running.
If Python is not running, after starting it, load the modules we will
need."
  (unless (.is-initialized)
    (startup-python)
    (assert (.is-initialized))
    (py "import sys, imp, code")))

(defun ensure-module (name source)
  (ensure-python)
  (unless (py "~s in sys.modules" name)
    (py "~a = imp.load_source(~s, ~s)"
        name name (native-namestring source))))

;;; Lazy-load modules.

;;; XXX There doesn't actually seem to be a better way to do this.
(defun name-error-name (name-error)
  (match (princ-to-string name-error)
    ((ppcre "^name '(\\w+)' is not defined$" name) name)))

(defun call/module (module thunk)
  "Call THUNK, ensuring that MODULE is loaded by handling NameError
exceptions."
  (ensure-python)
  (tagbody :retry
     (handler-bind ((name-error
                      (lambda (e)
                        (with-slots ((module-name name) source) module
                          (when (equal (name-error-name e) module-name)
                            (progn
                              (ensure-module module-name source)
                              (go :retry)))))))
       (return-from call/module
         (funcall thunk)))))

(defmacro with-module ((module) &body body)
  "Run BODY, ensuring that MODULE is loaded."
  (with-thunk (body)
    `(call/module ,module ,body)))

;;; Identifiers.

(eval-and-compile
  (defun pythonic (id)
    "Make a Lisp identifier pythonic."
    (assure string
      (etypecase-of (or string symbol) id
        (string id)
        (symbol
         (~>> id
              string-invert-case
              (substitute #\_ #\-)))))))

(defun lispy (id)
  "Make a Python identifier lispy."
  (assure keyword
    (etypecase-of (or string symbol) id
      (keyword id)
      (symbol (make-keyword id))
      (string
       (~>> id
            string-invert-case
            (substitute #\- #\_)
            make-keyword)))))

;;; Calling Python functions.

(defun call/ptr (fn ptr)
  "Call FN on PTR, decrementing the ref count of PTR once done."
  (unwind-protect
       (funcall fn ptr)
    (.dec-ref ptr)))

(defmacro w/ptr ((name expr) &body body)
  "Run BODY, binding NAME the pointer returned by EXPR, and
decrementing the pointer's ref count once done."
  (with-thunk (body name)
    `(call/ptr ,body ,expr)))

(defun pycall (fn &rest args)
  "Call FN, a Python function, with ARGS."
  (let ((fn (pythonic fn)))
    (with-py-lock
      (w/ptr (p (run* fn))
        (object.call-object p args)))))

(define-compiler-macro pycall (&whole call fn &rest args)
  "Try to convert function names from Lispy to Pythonic form at
compile time."
  (cond ((stringp fn) call)
        ((constantp fn)
         `(pycall ,(pythonic (eval fn)) ,@args))
        (t call)))

(defclass python-module ()
  ((name :type string
         :documentation "The unique name for the Python module.")
   (source :initarg :source :type pathname
           :documentation "The pathname to load the module from.")
   (cache :initform (make-hash-table)
          :documentation "A cache for closed-over Python methods."))
  (:documentation "Wrapper for a Python module."))

(defmethods python-module (self name source cache)
  (:method initialize-instance :after (self &key)
    (setf name (uniquify-module source))

    (let ((name name))
      (finalize self
                (lambda ()
                  (ignore-errors
                   (py "del sys.modules[~:*~s]; del ~a" name))))))

  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~a" name)))

  (:method module-ref (self (key symbol))
    (check-type key keyword)
    (ensure-python)
    ;; The cache is to ensure that closures are not needlessly
    ;; allocated when importing bindings instead of values.
    (synchronized (cache)
      (ensure2 (gethash key cache)
        (let* ((py-key (pythonic key))
               (py-name (concat name "." py-key)))
          (with-py-lock
            (with-module (self)
              (w/ptr (p (run* py-name))
                (if (callable.check p)
                    (lambda (&rest args)
                      (with-module (self)
                        (with-py-lock
                          (apply #'pycall py-name args))))
                    (cffi:convert-from-foreign p 'cpython::object!)))))))))
  
  (:method module-exports (self)
    ;; "The public names defined by a module are determined by checking
    ;; the module’s namespace for a variable named __all__; if defined,
    ;; it must be a sequence of strings which are names defined or
    ;; imported by that module. The names given in __all__ are all
    ;; considered public and are required to exist. If __all__ is not
    ;; defined, the set of public names includes all names found in the
    ;; module’s namespace which do not begin with an underscore
    ;; character ('_')."
    (with-module (self)
      (with-py-lock
        (handler-case
            ;; Check for __all__.
            (map 'list #'lispy (py "~a.__all__" name))
          (attribute-error ()
            ;; Any member name that don't start with _.
            (loop for name across (py "dir(~a)" name)
                  unless (string^= "_" name)
                    collect (lispy name))))))))
