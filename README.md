
# Building dang

## Required build tools

 - alex
 - happy

## Required libraries

 - GraphSCC
 - base
 - bytestring
 - cereal
 - containers
 - directory
 - filepath
 - monadLib
 - pretty
 - process
 - syb
 - template-haskell
 - text
 - https://github.com/elliottt/llvm-pretty

### Test Suite

 - test-framework
 - test-framework-quickcheck2
 - QuickCheck (>= 2)

## Building the source

Dang builds using gnu make, and as such, you can build the binary by typing:

```shell
$ make
```

Once the build system finishes, you should be left with an executable called
`dang` in the `build/bin` directory.

Optionally, you can build and run the test suite by making the `test` target:

```shell
$ make test
```

# Using dang

Dang doesn't really do anything except type-check at the moment.  Given that,
you'll want to run it with the -c option, to avoid having it try to link
anything.
