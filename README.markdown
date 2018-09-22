> “Oh, ’tis a mere nothing! A snake! A snake! The commonest thing in
> the world. A snake in the bosom – that’s all,” answered Roderick
> Elliston.

— Nathaniel Hawthorne, [The Bosom Serpent][TBS].

Bosom Serpent is a library for using Python modules from Lisp. It is a
thin layer over [burgled-batteries][] and allows Python modules to
be imported as [Vernacular][] modules.

Bosom Serpent does not import Python modules directly into Lisp. The
trick is to add a Python file as part of your Lisp system and load it
as a module which re-exports the functionality you're interested in.
This file will also do the work of wrapping the output of Python
functions in a form that Lisp will understand.

Say you want to use the Python module `shlex` from Lisp. You create a
Python file in your Lisp system. The name of the file doesn’t matter.

    # shlex_stub.py
    import shlex

    def lex(s):
        return shlex.split(s)

You can then import this Python module into Lisp from your system:

     (vernacular:import shlex
       :as :bosom-serpent/python2
       :from "shlex_stub.py"
       :binding (#'lex))

     (lex "how now 'brown cow'") -> #("how" "now" "brown cow")

Limitations: same as any Python embedding. There is only one running
Python interpreter at a time, and access to the interpreter is
strictly single-threaded.

# Postscript

I wrote Bosom Serpent as a demonstration of how to wrap an embedded
runtime for use with Vernacular, but it has proven unexpectedly
useful. The fact that a shim is required to use a Python module, which
might seem onerous at first, is what makes it work so well.

The stumbling block for embedding Python into a host language is
translating Python data types into something the host language can
understand. Solving this problem in a general way is much harder than
it looks. What usually happens? The programmer gives up on the
embedding and writes a Python script that returns JSON.

Having a shim gives you a place to translate Python classes into
simpler, portable data structures, without losing the efficiency of
embedding over calling out to a separate Python process.

[TBS]: http://www.online-literature.com/hawthorne/132/
[burgled-batteries]: https://github.com/pinterface/burgled-batteries
[Overlord]: https://github.com/ruricolist/overlord
[Vernacular]: https://github.com/ruricolist/vernacular
