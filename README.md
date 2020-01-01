# kademlia

WIP

[Kademlia](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)
implemented in Haskell.

Implementation is mostly as described in the paper, but uses 256-bit
identifiers instead of the original 160-bit identifiers.  This alteration was
made to facilitate the use of SHA256 hashes instead of the 160-bit SHA-1 hashes
described in the paper.

## Developing

This project is built using [stack](https://www.haskellstack.org).

Development requires the `stack` CLI to be installed locally.

`stack ghci` will compile all modules and load them into a `ghci` interpreter
session.

### Installing

Invoking `stack install` from the root of this repository will build the
application and place the executable in `$HOME/.local/bin/kademlia`.
