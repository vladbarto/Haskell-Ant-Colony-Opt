# Haskell Implementation of Ant Colony Optimization

# Author: ing. Bartolomei Vlad
[TOC]


## Purpose
This project is designed to test the Ant Colony Optimization (will be often referred in this document as ACO) in Haskell, a Functional Programming language, on the Travelling Salesman Person problem (which will be referred as TSP).

## Contents of the project
### Doku File
The project contains a documentation in Doku folder which addresses the results of ACO + TSP on different benchmarks and also TSP + 3 different algorithms.

## Setup
You will need to have two windows opened:
- a text editor, whatever suits you best (Visual Studio, Notepad, Vim, Emacs and so on)
- `system terminal` + `GHCi`  
In addition to that, working with Haskell implies importing some packages from some libraries. For that, you need:
- `ghcup` - an universal installer for Haskell
- `cabal` or `stack`. We'll use `cabal`  

__What to install__ / __Steps__:
-
First we take care of `ghcup`:  
```commandline
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Then we install `ghc`, which will have `ghci` included
```commandline
ghcup install ghc 8.10.7
ghcup set ghc 8.10.7 (in case you have multiple ghc versions installed)
```

For Cabal:
```commandline
ghcup install cabal
```
> Important note: Cabal and Ghc must have compatible versions if you want to avoid library-installing problems

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
 
### Some libraries you are going to need:
- `Control.Lens` ==> 
```commandline 
cabal install --lib lens
```

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
  
## 2. File: control-search-local
### Setting up the project
- cabal init
- import lines with control search
- hoogle website
- add to .cabal the build-dependencies `local-search`
- run project with cabal run `project-folder-name`
- download combinatorial-problems package
- modify cabal from it
- cabal update && cabal run
- in .cabal of your project add path to the other cabal as extra-path-..something

- cabal install cabal-install
- 
### Contents:
- `Main.hs`

## 3. File: moo
### Setting up the project
- ```commandline
  cabal build
  ```

### Contents:
- `Main.hs`