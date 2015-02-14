[![Build Status](https://travis-ci.org/gbataille/halray.svg)](https://travis-ci.org/gbataille/halray)

Our first haskell project, trying to build a raytracer

# Developper setup

Clone the repository
```
git clone https://github.com/gbataille/halray
```
`cd` into it
Create a cabal sandbox
```
cabal sandbox init
```
Install your project, its dependencies and the dev dependencies in the sandbox
```
cabal install --enable-tests
```
Configure the project
```
cabal configure --enable-tests
```
Build the project
```
cabal build
```
Run the tests
```
cabal test
```
Run the halray program
```
cabal run ARGUMENTS
```
