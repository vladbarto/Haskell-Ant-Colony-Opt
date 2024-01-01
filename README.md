# Haskell Implementation of Ant Colony Optimization

# Author: Ing. Bartolomei Vlad
[TOC]


## Purpose
This project is designed to test the Ant Colony Optimization (will be often referred in this document as ACO) in Haskell, a Functional Programming language, on the Travelling Salesman Person problem (which will be referred as TSP).

## Contents of the project
### Doku File
The project contains a documentation in Doku folder which addresses the results of ACO + TSP on different benchmarks and also TSP + 3 different algorithms.

## Setup
You will need to have two windows opened:
- `emacs` -- a text editor
- `system terminal` + `GHCi`  
In addition to that, working with Haskell implies importing some packages from some libraries. For that, you need:
- `ghcup` - an universal installer for Haskell
- `cabal` or `stack`. We'll use `cabal`  
__What to install__/__Steps__:
```commandline
sudo snap install emacs --classic
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
> The last line is for `ghcup`  

> If you choose to work in Vim, you can forget about Emacs
```commandline
ghcup install ghc 8.10.7
ghcup set ghc 8.10.7 (in case you have multiple ghc versions installed)
```

For Cabal:
```commandline
ghcup install cabal
```
## Some libraries you are going to need:
- `Control.Lens` ==> 
```commandline 
cabal install --lib lens
```

## Working with Haskell code
Usually, when you write Haskell code, you want to store it in a `.hs` file.  
In the same folder as the file you are currently writing:
- type `ghci` in terminal
- to load (once, at the beginning) the file, type `:l file.hs`, where file.hs is your code
- to __re__load the file (a.k.a. recompile) type `:r` (it already knows it's the same file)

## 1. File: rickdzekman-ant-colony-optimization-algorithm-for-tsp-optimize-in-haskell-ee5a30ec533d
### Contents we are going to modify at least once:
- `Main.hs`: to be loaded and reloaded in ghci
- `AntColony.cabal`: a `.cabal` file, important for build-dependencies
> Here we modify the following:
> ```commandline
> build-depends:
>    base >= 4.14 && < 5,
>    mtl >= 2.2 && < 3,
>    -- other dependencies...
> ```

### Monad.Random missing
- at first
> ```commandline
> cabal update
> cabal install --lib random
> cabal install --lib MonadRandom
> ```
- The problem persisted, so we have rebuilt the `.cabal` file
  - open a Terminal and make sure you are in the project directory (i.e. where the `.cabal` file is)
  - Sometimes, build artifacts can cause issues. Clean your project by typing and running:
  ```commandline
    cabal clean
    cabal build
    ```


