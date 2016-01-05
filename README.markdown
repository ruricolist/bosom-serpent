> “I had heard of this; but my conception came far short of the truth.
> What has befallen you? Why do I find you thus?”

> “Oh, ‘tis a mere nothing! A snake! A snake! The commonest thing in
> the world. A snake in the bosom – that’s all,” answered Roderick
> Elliston.

— Nathaniel Hawthorne, [The Bosom Serpent][hawthorne].

Bosom Serpent is a library for using Python modules from Lisp. It is a
thin layer over [burgled-batteries][] which allows Python modules to
be imported as Loom modules. It is primarily intended for
demonstration purposes, as a reference for how to wrap an embedded
language for Loom, but may be found useful in itself.

Bosom Serpent does not allow you to import arbitrary Python modules
directly. Instead, the trick is to add a Python file as part of your
Lisp system that can be loaded as a module and re-exports the
functionality you're interested in. It is assumed that this file will
also do the work of wrapping the output of Python functions in a form
that Lisp will understand.

Say you want to use the Python module `antigravity` from Lisp. You
create a Python file in your project named `antigravity_stub.py`:

     import antigravity

     def fly():
         antigravity.fly()

     def levitate(object):
         antigravity.levitate(object)

You can then import this Python module into Lisp:

     (loom:import antigravity
       :as bosom-serpent:python2
       :from \"antigravity_stub\"
       :binding (#'fly #'levitate))

     (fly) => \"I think this is the Python.\"

Limitations: there is only one running Python interpreter at a time,
and access to the interpreter is strictly single-threaded.

hawthorne: http://www.online-literature.com/hawthorne/132/
burgled-batteries: https://github.com/pinterface/burgled-batteries
loom: https://github.com/TBRSS/loom
