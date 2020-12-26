# AdventOfCode2020
Doing Advent of Code 2020 in Emacs Lisp!?

The basic idea is to use Emacs Lisp (and not the CL extensions,
ideally), and use the power of Emacs (buffers, regexps) as a part of
the solution, rather than just using Emacs as a lisp-script engine.

It is mostly intended as a learning resource, focusing on getting the
job done first and exposing interesting techniques (maybe) second.

As this was done in real time, eventually a few pieces of code became
repetitive, so some common resources have been factored out into
util.el.  There is also a queue in queue.el.


## Note

Most of the code use `defvar` to define parameters (in the absence of
`defparameter`).  It is one of the peculiarities of `defvar` that if a
variable is already set with it, another `defvar` will not override it
but leave the original value unchanged.  So if you load more than one
of the programs, take care that they do update their parameter values.

Although the author has mostly tried to stay with core ELisp, in some
cases, using a CL function has significantly shortened or improved
readability of the code.  You may need to `(require 'cl)`.

### Emacs Compatibility Notes

Some differences between the ELisps have been found during development
and testing.


Older Emacsen in particular may have compatibility issues.  For
example, in Emacs 24, `#'=` accepts only two arguments, whereas later
Emacsen (26, 27) (and the Common Lisp standard) accept multiple
arguments.

Another sticking point seems to be the existence of the function
`string-to-int`.  If it is not present in your Emacs, you can
substitute `string-to-number` in the code, or alias them:

```
(fboundp 'string-to-int)
nil
(setf (symbol-function 'string-to-int) (symbol-function 'string-to-number))
#<subr string-to-number>
(string-to-int "123465")
123465
```

Occasionally you might see strange behaviour in the code, such as
strings being inexplicably interpreted as symbols.  If this happens,
sadly, there is no other option than to restart Emacs.  Yes, sometimes
it is necessary.

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
10. More LISt Processing, using generic `split-sequence` (in utils) to reduce the problem
11. Sadly, my ELisp code has a bug; it works on the test but not on the full input.  Until I fix the bug, I solved the second half it in C++ instead (not included here)
12. Why would you sometimes use `member` to check membership if it's guaranteed to be true?
13. Lots of integery and listy calculations...
14. Bit manipulations.  Backtick outside of macros.
15. Go-faster-stripes (well, a hash table) makes code run faster!  A queue of length two...
16. More uses of `mapcan` than you can shake a can at.
17. Finally caved in and used `loop` - well, it makes sense to use it where it makes sense.
18. Data is code.  Sometimes.
19. Took a long time to get this right, possibly having taken the "significantly more difficult" route.  This code works, but exceeds Emacs built-in limits on the second part (tested by translating to Common Lisp)
