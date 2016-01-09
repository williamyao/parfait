# Parfait

Parallelism is hard. Parfait provides some primitives to make it
easier. Particularly, Parfait focuses on providing primitives for
working with parallelism in a functional way, using threads to return
values instead of perform side effects.

## Usage

Everything in Parfait is a shameless port of Clojure's basic
parallelism primitives into Common Lisp. For a more in-depth
explanation, see
[Clojure for the Brave and True](http://www.braveclojure.com/concurrency),
or consult the Clojure documentation.

In addition to the concurrency primitives, Parfait also provides a
readtable for easier usage of `DEREF`, called `PARFAIT-DEREF`. This
makes `&X` the equivalent of `(DEREF X)`; the readtable should be merged
into another before being used. Make sure that the readtables do not
conflict in their usage of the macro character `&`.

+ **DEREF** OBJ

  Return the value contained by OBJ, blocking until the value is available.

+ **FUTURE** &BODY BODY

  Starts a new thread, which runs BODY. Returns a promise; dereferencing
  the promise will return the value of BODY. BODY will only run once,
  atomically, and DEREFs after the first will not block.

+ **DELAY** &BODY BODY

  Returns a delay; dereferencing the delay will return the value of
  BODY. Unlike a function object, BODY will only run once, atomically,
  and DEREFs after the first will not block.

+ **PROMISE**

  Returns a promise. Attempting to dereference this promise will block
  until a value is DELIVERed. Allows for decoupling of variable
  declaration and binding until necessary.

+ **DELIVER** PROMISE VALUE

  Set the value of a promise and unblock any threads trying to
  dereference it. Does nothing, if the promise has already been
  delivered.
