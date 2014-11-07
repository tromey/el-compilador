Welcome to El Compilador, a compiler for Emacs Lisp.

## Dreams

I've long wanted to write a compiler for Emacs Lisp.  Here it is.  In
the long term I have a few goals for Emacs and Emacs Lisp that are
served by this project:

I think Emacs should move more strongly toward self-hosting.  I think
too much of Emacs is written in C, and in the long term this should be
migrated to lisp.  Beyond just being more fun to hack, having Emacs
written in Emacs Lisp would make it simpler to upgrade the language
implementation.

There are plenty of functions currently written in C which were either
translated for performance (`widget-apply`) or just because some other
part of the core needed to call it.  These would stop being acceptable
reasons to write in C.

The C core is also badly behaved about making direct calls.  This is
ok for primitives like `cons`, but not ok for functions that one might
reasonably want to advise or rewrite, like `read`.  Normally this lack
of indirection is just because it is a pain to write -- automatic
translation could eliminate this problem.

I'm also interested in using the compiler to either write a JIT or a
new register-based bytecode interpreter.  These could be done without
modifying Emacs once the new FFI code lands.

Finally, I think it is bad and wrong that Emacs has three bytecode
interpreters (the Emacs Lisp one, the regexp engine, and CCL).  There
can be only one, and we can use this work to push Emacs toward that
goal.

## Use

Currently the compiler can't generate useful code.  Alpha software
hurray!

Meanwhile, you can use the function in `loadup.el` to load the
compiler and then use the two handy entry points:

* `elcomp--do`.  The debugging entry point.  This takes a form,
  compiles it, and then dumps the resulting IR into a buffer.  For
  example, you can try this on a reasonably direct translation of
  `nthcdr` from `fns.c`:

```elisp
(elcomp--do '(defun defun (num list)
	       (cl-check-type num integer)
	       (let ((i 0))
		 (while (and (< i num) list)
		   (setq list (cdr list))
		   (setq i (1+ i)))
		 list)))
```

* `elcomp--c-do`.  This is like `elcomp--do` but uses the "C" back
  end.  You will note that while the output is vaguely C-like, it will
  not compile.

## Implementation

El Compilador is an
[SSA-based](http://en.wikipedia.org/wiki/Static_single_assignment_form)
compiler.  The objects in the IR are described in `elcomp.el`.  EIEIO
or `cl-defstruct` are used for most things.

The compiler provides a number of optimization passes:

* Jump threading, `elcomp/jump-thread.el`.  This also does some simple
  optimizations on predicates, like `not` removal.

* Exception handling cleanup, `elcomp/eh-cleanup.el`.  This removes
  useless exception edges.

* Block coalescing, `elcomp/coalesce.el`.  This merges basic blocks
  when possible.

* Constant and copy propagation, `elcomp/cprop.el`.  This also
  evaluates pure functions.

* Dead code elimination, `elcomp/dce.el`.

* Type inference, `elcomp/typeinf.el`.  This is a flow-sensitive type
  inferencer.


## To-Do

There are any number of bugs.  There are some notes about them in
various files.

The C back end is incomplete.  There's no out-of-ssa pass and the
results of type inferencing aren't fully used.

The into-SSA pass is written in the stupidest possible way.  Making
this smarter would be nice.
