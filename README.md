Code-Tools
====

These are tools to help you code. Well, one of them, anyway.

Installation
----

* Ensure your computer can compile and run Haskell. On Ubuntu 10.04 do this:

    sudo apt-get install ghc cabal-install
    cabal update
    cabal install cabal-install

* Clone this repo with git, cd into it, and do this:

    cabal configure --user
    cabal build
    cabal install

* Tool scripts will install into $HOME/.cabal/bin. Add that to your path.
