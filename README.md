# Buffon-Needle-Haskell
## Build / Run
```sh
stack build --ghc-options -O2
stack exec buffon-exe -- --number 1000000 --method A
```
## Usage
```plain
Usage: buffon-exe (-n|--number INT) (-M|--method METHOD)

  Estimate Pi using Buffon's needle problem
```
# hit-n-miss
## Build / RUn
```sh
stack build --ghc-options -O2
stack exec hit-n-miss -- -n10000
```
