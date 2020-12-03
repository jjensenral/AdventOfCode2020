# AdventOfCode2020
Doing Advent of Code 2020 in Emacs Lisp!?

The basic idea is to use Emacs Lisp (and not the CL extensions,
ideally), and use the power of Emacs (buffers, regexps) as a part of
the solution, rather than just using Emacs as a lisp-script engine.

It is mostly intended as a learning resource, focusing on getting the
job done first and exposing interesting techniques (maybe) second.

## Possible points of interest

01. Use of `do*` to loop across lists, à la `maplist`
02. Use of `re-search-forward` to loop through buffer collecting matches
03. `solve2` is functional.  `incf` simple macro (which is also in `'cl`)


