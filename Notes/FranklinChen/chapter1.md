# My notes: Chapter 1: Building abstractions with procedures

## My remark on "procedure"

Interesting that the word "procedure" is used, since at some point in
the 1980s, thanks to the influence of C, which did not distinguish
between "procedure" and "function", people started calling everything
"function" instead.

> computation process

> Computational processes are abstract beings that inhabit computers.

## Lisp

> Lisp descriptions of processes, called procedures, can themselves be
> represented and manipulated as Lisp data.

My comment: in retrospect, this is slightly overstated and misleading:

- In 1984, Standard ML and Haskell, providing very concise ways of
  writing ASTs using algebraic data types, were not to be unleashed on
  the world until 1990.
- It will always still be true, however, that working on S-expressions
  as ASTs is extremely convenient, and I sure miss that when not
  working in Lisp!

## 1.1: Elements of programming

- primitive expressions
- means of combination
- means of abstraction

> examine some typical interactions with an interpreter

My comment: strictly speaking, it may not be an interpreter. It's a
REPL, which may be connected to a compiler or an
interpreter. Unfortunately, for decades now, many newcomers to
programming are confused about the difference between an interpreter
and a compiler, and associate the existence of a REPL with being an
"interpreter". This may change, now that probably most newer languages
with compiled implementations also have a REPL.

(TODO mention REPLs for C++, Swift, upcoming Java 9.)

### Naming

- variable
- value

> name things with `define`

This was uncommon back in 1984, where if you were using Pascal or C,
you don't just name things. In those languages, you write declarations
that "allocate storage" for things that are not yet constructed, and
then you fill the storage (possibly through immediate initialization,
but sometimes with delayed construction). It's a different kind of
model of computation.

```scheme
(define circumference (* 2 pi radius))
```

### Procedure definitions

```scheme
(define (square x) (* x x))
```

I don't like this pedagogy, because it makes functions seem special
when they are not. I would teach functions with a lambda up front, so
that

```scheme
(define square
  (lambda (x) (* x x)))
```

makes it clear that we are defining `square` to be a value that
happens to be `(lambda (x) (* x x))`, just as `circumference` above
was a definition of something that happens not to be a lambda.

I have seen many students get confused about the status of functions
when they learn this shortcut syntax first instead of the core syntax.

### Substitution model for procedure application

In retrospect, having studied evaluation models formally, the
presentation here is on the mysterious side, I think.

(TODO Link to formal operational semantics presentations.)

### Applicative versus normal order

These correspond to "call by value" and "call by name".

### Conditional expressions

#### `cond`

I don't like `cond` at all.

I distinctly remember first learning `cond`. I found it difficult to
read because of all the nesting, and also difficult to understand in
full generality (some of which is mentioned in
[this footnote](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#call_footnote_Temp_26).

Clojure's [`cond`](https://clojuredocs.org/clojure.core/cond)
is arguably friendlier than Scheme's.

As of October 2015, Elm 1.6 is about to be released, and word is that
its `cond`-equivalent is going to be removed from the language because
of [usability issues](https://github.com/elm-lang/elm-plans/issues/6).

#### My comment on Scheme's commitment to truthiness

Note the footnote about conditionals in Scheme. Today we call Scheme
"truthy" (because of many languages that were inspired to do similar
things rather than have a strict two-value boolean notion of
conditional).

> "Interpreted as either true or false" means this: In Scheme, there
> are two distinguished values that are denoted by the constants `#t`
> and `#f`. When the interpreter checks a predicate's value, it
> interprets `#f` as false. Any other value is treated as true.

There are reasons Scheme chose truthiness (at least in slightly better
form than Lisp's). (TODO Explain this later.)

But I think "truthiness" is simply a mistake in programming languages,
and that the past thirty years have shown this (see how it's affected
readability and correctness in C, C++, Perl, Ruby, JavaScript for
example). (TODO gather links on the gotchas from allowing truthiness.)

#### My comment on case analysis

A great advantage of the ML family of languages from 1990 on is that
case analysis can be done in a more formal way, and checked for
exhaustiveness by the compiler, when done as "pattern matching" on a
type.

For example,

```scheme
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
```

in Haskell would be defined as

```haskell
abs :: (Num a, Ord a) => a -> a
abs x =
  case compare x 0 of
    GT -> x
    EQ -> 0
    LT -> -x
```

An even more sophisticated Rust version (see [playground](http://is.gd/dK0D41)):

```rust
use std::cmp::Ordering;
use std::num::Zero;
use std::ops::Neg;

fn abs<T: Ord + Zero + Neg<Output=T>>(x: T) -> T {
    match x.cmp(&T::zero()) {
        Ordering::Greater => x,
        Ordering::Equal => T::zero(),
        Ordering::Less => -x,
    }
}
```

It is outside the scope of this chapter to explain the Haskell and
Rust implementations, but this comparison with Scheme brings up a
pedagogical matter: it does seem that starting out programming with an
dynamically typed language like Scheme is easier, because you don't
have to deal up front with the details of expressive type systems.

#### `and`, `or`

These are "truthy" operators.

### Newton's method

I think this math/engineering-centric example is a distraction from
teaching general programming because not everyone is an MIT EECS
major!

### Black box abstractions

Local names, internal definitions, block structure.

My comment: I know it's too early, but internal definitions of
functions is really sugar for `letrec` underneath.

#### My comment on modules

I think this 1984 presentation of the virtues of lexical scoping
for hiding information is problematic. Recall that Pascal at the time
had internal definitions also, so this was part of the culture before
**modules** came into the picture.

Modules are a far better way to hide information *selectively*, rather
than permanently. For example, when unit testing "helper functions",
one wants them to be visible, not hidden as an internal definition
inside another function.

Module systems were in the air in the 1970s and 1980s (see Clu, Ada,
Modula-2, Standard ML) but Scheme decided not to standardize on them
because of a lot of different competing proposals and implementations.
Note that Common Lisp in 1984 did introduce a package system.

On the other hand, it is pedagogically useful to show how a module
system can be implemented if one doesn't already have one. This is
done later in SICP. Amusingly, thirty years after SICP, JavaScript is
still finalizing a standard module system, illustrating how
contentious the subject is if a module system is not provided built
into a language from the start!

##### Current state of modules for Scheme

I looked at the current state of
[Scheme standardization](http://trac.sacrideo.us/wg/wiki) and it looks
like
[modules are still being discussed](http://trac.sacrideo.us/wg/wiki/ModuleSystems)
after R7RS,
decades later. [R6RS](http://www.r6rs.org/) in 2007 did apparently
[define a module system](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_chap_7).

I'm going to make a confession here. I gave up on Scheme after 1997 (I
last used the R4RS standard) in part because of no standardization of
a module system. This was a huge problem. If you wanted to use
someone's Scheme code, it either didn't use modules at all or used the
module system of one of dozens of competing implementations of Scheme,
and if you were in luck, you might be able to do some copy/pasting to
get it working with yours.

## 1.2: Procedures and the processes they generate

I think this discussion of recursion is problematic. I was a TA for a
computer science class at CMU that used this kind of example (with
Standard ML as language), and I heard the comments by many
students. Many came away thinking "recursion is pointless and
inefficient" and "tail recursion is a cute trick". I haven't yet
decided how to explain this stuff better. I do know that I'm tired of
factorial.

### Tail recursion

Also, the fact that tail recursion is not available in many languages
gives students the impression that languages like Scheme are
pointless!

[ECMAScript6](http://www.ecma-international.org/ecma-262/6.0/) has
[tail call elimination](http://www.ecma-international.org/ecma-262/6.0/#sec-tail-position-calls),
however. This is hugely important for useful functional idioms, but
also
object-oriented idioms, as
[passionately argued by Guy Steele in 2009](http://www.eighty-twenty.org/2011/10/01/oo-tail-calls.html). Note
he was the very inventor of Scheme in 1975, the first language to
mandate tail call elimination, so he's been waiting forty years for
language implementations to get this right!

### Tree recursion

I hate Fibonacci.

> One should not conclude from this that tree-recursive processes are
> useless. When we consider processes that operate on hierarchically
> structured data rather than numbers, we will find that tree
> recursion is a natural and powerful tool.

So why present an example that is useless and misleading? This was
terrible pedagogy. Recursion is super-important, and yet the first two
examples presented are completely pointless (factorial and Fibonacci).

#### Counting change

This is a tricky problem, and I don't like the solution, for a number
of reasons.

(TODO Show a better solution and explain why.)

### Orders of growth

This is an abbreviated discussion of a much larger topic of
computational complexity. The discussion here is much too vague and
brief, completely horrible.

Also, today, one could at least show live animations and graphs of all
this stuff.

(TODO Link to good resources.)

### Exponentiation

More math!

## 1.3: Formulating abstractions with higher-order procedures

Good to bring in higher-order right away.

More math.

### `lambda`

Finally introduced here. I would have done it on day one when defining
functions.

`let` for local variables.

I like that it discusses how `let` is just the same as applying a
`lambda` to stuff.

The syntax for `let` is somewhat verbose because of nesting (again,
something that Clojure changed).

There's a subtlety here in the difference with `let*` is not
discussed.
