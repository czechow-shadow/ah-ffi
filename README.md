# ah-ffi

## Profiling with GHC 8.4.3
Based on https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/profiling.html

Enabling profiling with stack (this will rebuild all necessary modules):

`stack build --profile`

or `ghc --prof`

use `stack exec ah-ffi -- +RTS -i0.0001 -hy -p` for more frequent sampling
(note, that timescale of the samples dumped is 10x lower than it should be).


`stack exec ah-ffi -P` will give more info in the \*.prof file
`stack exec ah-ffi -pa` will give even more detailed info in the \*.prof file (all CAFs)

CAF = Constant Applicative Form

There is some unclear relationship between specifying -V (interval of RTS
ticks) -> and sample times. Also -V0 is interestring for deterministic context
switching


`-xc` - enable trace (aka, print a full stack trace when an exception happens);
does it print rethrown exceptions? -> I do not think so


`-l` - log events to event log


+RTS -s -hc -xt -L256 -p -RTS

+RTS -s -hm -xt -L256 -p -RTS
