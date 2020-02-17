# term-rewriter
_Mini project one CS467_

The project uses minimal features of stack, namely building and testing. I've
set everything up, so all you need to do to get started:

``` shell
stack setup

# to run the "main" of our program (see `app/Main.hs`),
stack run

# to run all tests defined in our suite (see `test/Spec.hs`),
stack test
```

If you run `stack test` you'll see our `Match` works, but `Unify` fails. Take a
look at the test and make it pass! Classic TDD.

> For reference material, look at the equations that describe `unify` and
> `narrow` on the [web page
> (lec-06)](https://www.cse.iitb.ac.in/~ananya/cs467/lec-06.html).

## `MATCH`
We've already implemented it for you (`src/Match.hs`).

> Start with `src/Language.hs`, it describes the data types we operate on and
> some utilities.

## `UNIFY`
This is your first task. Complete the function bodies. You are free to ignore
completing some suggested bodies, **but you must define the functions exported by
the module.**

### What's exported by a module? What is a module?
Each haskell file that starts with a `module` declaration is a module. By
default all top-level definitions are "exported" by the module (ie, modules that
import them get everything).

Modules can selectively export definitions (like describing public ones) by,

``` haskell
module Foo (publicOne, publicTwo) where
...
```

Importing modules can do selective imports using similar syntax, and can also do
qualified imports (much like the `from ___ import ___ as ___` idiom from
python).

### Issue board is locked. Use Moodle
### Do not fork this repo, clone it.
### Do not cheat, [collaborate](https://www.cse.iitb.ac.in/~supratik/courses/copying-and-discussion.html).
