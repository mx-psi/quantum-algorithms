This project was developed for my undergraduate dissertation. 
You may read the complete text [on this separate repository](https://github.com/mx-psi/tfg).

-----------

A copy of this file in spanish is available on file `LEEME.md`.

Dependencies
============

All dependencies are managed by `stack`. For installing `stack` on Unix
OSs `make install-deps` can be run. On Windows, a binary is provided on
<https://docs.haskellstack.org/en/stable/README/>.

The first time there is an attempt to build the binary all dependencies
will be installed by `stack`, including GHC Haskell compiler, so this
may take some time.

Building
========

**Important note:** The building process has only been tested on Linux
distributions. In order to run the program on Windows, the `stack.yaml`
file MUST be edited so that the preprocessor works. In particular,
`quipper/convert_template.sh` should be replaced by
`quipper/convert_template.bat`.

------------------------------------------------------------------------

`make build` will build the associated binaries and copy them to the
`bin` folder.

The first time there is an attempt to build the binary all dependencies
will be installed by `stack`, including GHC Haskell compiler, so this
may take some time.

Binaries created on my computer (running Linux Mint 18.1) are attached.

Running the binaries
====================

The previous process creates two binaries, Both binaries include help
instructions by running them with the option `--help`.

The first one, `quantum`, implements a command-line interface for
running several quantum algorithms. Each algorithm is associated with a
subcommand.

Some algorithms take an oracle as input, defined by its truth table.
This can be passed as an argument or, if called without this argument,
the oracle will be read from standard input.

The syntax for oracles is a subset of csv files. Each line is expected
to have an input value, represented by a string of `0` and `1`, and
separated by a comma, an output value, represented by a single bit.

For example, the NOT logical operation can be given as

    0,1
    1,0

Some examples of oracles are included in the `oracles` folder.

The second binary, `diagrams`, generates the diagrams used in the document.
The option `--gen-all` generates a selection of the diagrams used,
while providing a circuit in the form of a truth table with the option `--circuit` generates a circuit diagram for the reversible version of
that circuit.

Documentation
=============

The documentation, generated with Haddock, is available in the `docs`
folder. It can be rebuilt by using `make docs`.

Note that the process for building the documentation requires the manual
preprocessing of the code files and thus can not directly be done using
`stack`.

To generate the documentation on Windows the `Makefile` MUST be edited
and the preprocessing script be changed as it was the case in the
building process.

Testing
=======

The code includes a test suite with both unit tests and property-based
testing. It can be run using `make tests`.

Note that due to the nature of the implemented algorithms, it is
possible (yet unlikely) that some of these tests fail (this is indicated
in the output log).

Code organization
=================

The code is organized as follows

1.  the `oracles` folder includes examples of oracles written in the
    syntax accepted by several algorithms.
2.  the `quipper` folder includes preprocessing scripts that were taken
    from the Quipper project to allow for the compilation.
3.  the `src` folder contains the code. It has several subfolders

    -   the `apps` subfolder has a folder per binary and includes the
        main program,
    -   the `lib` subfolder contains files common to both binaries,
        defining the algorithms (in the `Algorithms` subfolder), as well
        as several auxiliary files.
    -   the `test` subfolder includes the testing suite code.

4.  the `docs` folder includes the generated documentation.
5.  the `package.yaml` and `stack.yaml` files define the dependencies
    and compilation steps.
