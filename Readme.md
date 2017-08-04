# Quick summary of the PCP

The [Post correspondence problem](https://en.wikipedia.org/wiki/Post_correspondence_problem) is an [undecidable](https://en.wikipedia.org/wiki/Undecidable_problem) [decision problem](https://en.wikipedia.org/wiki/Decision_problem).
In June 2017 a friend invited me to have a look at it together and maybe tinker with it a little.
This document summarizes part of our session.

## A summary of the problem

The PCP can be described like this:
* We are given a set of tuples of strings.
* We can combine two tuples to form a new one
by concatenating the first and second strings each.
* We may use every tuple as often as we'd like to.
* For the given set we want to answer the question:
Is there a sequence that we can combine the given tuples in
so that the first and second string of the resulting tuple are the same?

### Example problem:
Given the tuples `x = ("aa", "a")` and `y = ("a", "aa")`,
we can form a sequence combining `x` and `y`,
which yields the tuple `("aaa", "aaa")` as one solution.

* If we have one solution we can see that we can produce several more.
Not only `x,y` is a solution, but also `x,y,x,y,x,y`.
More generally: We'll always have no or infinitely many solutions.
* If we only have tuples like `("a", "aa"), ("c", "bdb")`,
we can easily spot that no solutions can be found.

## Let's write some Haskell!

While it is impossible to write a solver for the general PCP,
the bounded PCP can be solved.
A bound could be a limitation in steps to check for a solution,
or (a bit more loosely) a constraint in time allowed to solve a problem.
Using this definition we decided to write a PCP solver in Haskell
where the bound would be us getting bored with waiting and terminating the program.

We start our Haskell code with a module declaration and some imports:
```haskell
module Main where

import Control.Monad (guard)
import Data.Monoid ((<>))
import Data.List (partition)
```

### Next we define some type aliases:
When reducing a sequence of tuples we call the resulting `Tuple` our `Backlog`.
For example the sequence `("ab", "a"), ("b", "aa")` would be reduced to a tuple `("", "aa")`.

In general reducing a `Tuple` may yield three outcomes:
1. A `Backlog` with the `Tuple` `("", "")` - when we found a solution.
2. A `Backlog` with a `Tuple` where the first string is empty but the second isn't.
2. A `Backlog` with a `Tuple` where the first string isn't empty but the second is.

```haskell
type Tuple = (String, String)
type Backlog = Tuple
```

When combining several tuples in search of a solution, we call these `Steps`.
```haskell
type Steps = [Tuple]
```

We speak of solution attempts as `Solutions`.
These have a somewhat more complex structure,
and it would be tempting to refactor them into a cleaner structure.
The reasoning behind this structure is as follows:
* Our solver can traverse the space of reachable `Backlog`s, which form a [DAG](https://en.wikipedia.org/wiki/Directed_acyclic_graph) using [BFS](https://en.wikipedia.org/wiki/Breadth-first_search).
* After combining a new `Tuple` with our current `Backlog` we obtain an updated `Backlog` and a series of `Steps` that led us to this `Backlog`.
The type of such a result is `(Backlog, Steps)`.
* When scanning a list of possible solutions obtained
by combining a `Tuple` with a `Backlog`,
we need to partition this list into actual solutions
where the `Backlog` is `("", "")`,
and the intermediate steps that we need to continue searching on.

```haskell
type Solutions = ([(Backlog, Steps)], [(Backlog, Steps)])
```

### Data to process:
Because we don't want to bother with input/output,
we just define our current problem in a list named `tuples`:

```haskell
tuples :: [Tuple]
tuples = [
--  ("00",  "1"),
--  ("1",   "0"),
--  ("0",  "01"),
--  ("110", "1"),
--  ("0", "010"),
--  ("01", "00"),
--  ("01", "00")

--  ("001",  "0"),
--  ("01", "011"),
--  ("01", "101"),
--  ("10", "001")

--  ("0",   "010"),
--  ("1",   "101"),
--  ("0101", "01")

  ("10",  "0"),
  ("0", "100"),
  ("001", "0"),
  ("0",  "01")
  ]
```

### Building the solver from functions:

The first thing we want to have is a way of combining tuples.
Given two tuples we want to concatenate both first and second elements
to form a new `Tuple`.
We can write this in a general fashion that allows us
to reuse it for a similar purpose later:

```haskell
combine :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
combine (a, b) (c,d) = (a <> c, b <> d)
```

After combining tuples we need to check if they are still valid.
A `Tuple` is valid, iff:
1. At least one of it's strings is `""`.
2. The first two characters of both strings are the same,
and the rest of both strings forms a valid `Tuple`.

```haskell
isValid :: Tuple -> Bool
isValid = uncurry go
  where
    go "" _ = True
    go _ "" = True
    go (x:xs) (y:ys) = x==y && go xs ys
```

When combining two tuples we also need to reduce
the resulting `Tuple` to a `Backlog`.
To do this we:
* Recursively remove all leading characters that are equal
in the first and second fields of the `Tuple`.
* Stop if either string of the `Tuple` is empty
or the strings start with a different character.

Notice that reducing a `Tuple` doesn't change it's validity
and that an invalid `Tuple` leads to a `Backlog`
that doesn't have an empty string in any field.

```haskell
reduceTuple :: Tuple -> Backlog
reduceTuple t@("", _) = t
reduceTuple t@(_, "") = t
reduceTuple t@((x:xs), (y:ys))
  | x==y = reduceTuple (xs, ys)
  | otherwise = t
```

Using our `tuples` together with the `reduceTuple` and `isValid` functions
we can now compute all `next` states reachable from a `Backlog`.
To do this, we:
1. Consider the given `backlog` together with each `Tuple` `t`
from our list of `tuples`.
2. Obtain a `backlog'` by computing the reduced `Tuple` of the combination
of `backlog` and `t`
3. Picking only the cases where `backlog'` is a valid `Tuple`,
whilst also returning `t`, so that we can later build our `Steps` from this.

```haskell
next :: Backlog -> [(Backlog, Tuple)]
next backlog = do
  t <- tuples
  let backlog' = reduceTuple (combine backlog t)
  guard (isValid backlog')
  return (backlog', t)
```

After computing the `next` reachable states we use `addSteps`
to build a log of `Steps` taken from previous `Steps`
and the `Tuple` returned by `next`.
Since the new `Steps` corresponds with a specific `Backlog`,
we produce a list of `(Backlog, Steps)`:

```haskell
addSteps :: Steps -> [(Backlog, Tuple)] -> [(Backlog, Steps)]
addSteps steps = fmap (\(b, t) -> (b, t:steps))
```

Given that we're building a solver it makes sense to know when to stop.
We introduce the predicate `isEmpty`:

```haskell
isEmpty :: Tuple -> Bool
isEmpty = (==) ("", "")
```

Using `isEmpty` we can `partition` the structure obtained from `addSteps`
into found solutions and states that need to be investigated further:

```haskell
findSolutions :: [(Backlog, Steps)] -> Solutions
findSolutions = partition (isEmpty. fst)
```

We combine `findSolutions`, `addSteps` and `next` into the `nextSolutions` function,
which computes the `Solutions` for a given `Backlog` and `Steps`:

```haskell
nextSolutions :: Backlog -> Steps -> Solutions
nextSolutions backlog steps = findSolutions (addSteps steps (next backlog))
```

Going one step further than `nextSolutions`,
we want to compute the `Solutions` for several states in `continueSolving`.
The implementation of BFS is hidden in these steps of code:
1. We partition our upcomming states using `findSolutions` in `nextSolutions`.
2. We use `foldl combine ([], [])` in `continueSolving`
to gather the results of `nextSolutions` for the individual states.
3. In the `solvePrefixes` function we use recursion to repeat these steps.

```haskell
continueSolving :: [(Backlog, Steps)] -> Solutions
continueSolving bs =
  let bs' = fmap (uncurry nextSolutions) bs
  in foldl combine ([], []) bs'
```

`solvePrefixes` has two main Jobs:
1. To gather the `Steps` from the first parts of a `Solutions`
obtained by calling `continueSolving`.
2. To concatenate the so obtained `[Steps]` with future `[Steps]`
for other potential solutions by calling itself
using the second part of the `Solutions` from `continueSolving`.

```haskell
solvePrefixes :: [(Backlog, Steps)] -> [Steps]
solvePrefixes bs =
  let (solutions, continuations) = continueSolving bs
      solutionSteps = fmap (reverse . snd) solutions
  in  solutionSteps <> solvePrefixes continuations
```

The function `solvePrefixes` can be specialized
to take only one element instead of a list.
Since `solvePrefixes` takes a list of tuples,
we can instead build `solvePrefix` to take both parts of the tuple separately:

```haskell
solvePrefix :: Backlog -> Steps -> [Steps]
solvePrefix backlog steps = solvePrefixes [(backlog, steps)]
```

All that's left to do now is:
* to initialize `solvePrefix` with the right parameters.
Since we compute `Steps` before checking for a solution,
we can pass the tuple of empty strings as a valid starting point
along with an empty history of `Steps`.
* We use `head` and `print` to search for only the first solution
and print that to the console.

```haskell
main :: IO ()
main = print (head (solvePrefix ("", "") []))
```

## Running the code

1. Install [markdown-unlit](https://github.com/sol/markdown-unlit)
using cabal:
```sh
cabal update && cabal install markdown-unlit
```
2. Use `ghci Main.lhs` or `ghc --make -O2 -pgmL markdown-unlit Main.lhs`.
