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

# Adding test modules
To add a new test module, you need to:
* Create the proper module folder hierarchy under the __test/__ folder
* Add the module qualified name to the __other-modules__ list of the test-suite
  definition in the _.cabal_
* Add `htf_MODULE_NAME_WITH_DOT_AS_UNDERSCORE_thisModulesTests`

# DUmmy tests
