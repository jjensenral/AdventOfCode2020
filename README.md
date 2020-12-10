# AdventOfCode2020
Doing Advent of Code 2020 in Emacs Lisp!?

The basic idea is to use Emacs Lisp (and not the CL extensions,
ideally), and use the power of Emacs (buffers, regexps) as a part of
the solution, rather than just using Emacs as a lisp-script engine.

It is mostly intended as a learning resource, focusing on getting the
job done first and exposing interesting techniques (maybe) second.

## Possible points of interest

These are the potentially least uninteresting parts of each day's
code.  The numbers below reference the days in question.

01. Use of `do*` to loop across lists, à la `maplist`
02. Use of `re-search-forward` to loop through buffer collecting matches
03. `solve2` is functional.  `incf` simple macro (which is also in `'cl`)
04. This is a slightly trickier puzzle or at least seems to require more code.  Maybe the test dispatch is the thing to check out.
05. Using the same list twice to inspect adjacent elements
06. How often do you use `mapcan` to solve a problem?
07. The counting algorithm for part 2 is pretty neat if I may say so myself, recursing through the graph
08. Going backwards through the program to fix it?
09. Using an efficient queue implemented as LISt Processing
