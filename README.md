# abide

A small Haskell library for working with calling conventions across multiple
ABIs.  The library is composed of two parts:

### Generation

A small DSL for describing simple finite state transducers in a way that lends
itself to generating files in AT&T FSM format, which is what is used by
OpenFST.

### Computation

After the FSM files are generated, we read them back in and generate code to
execute them in Haskell.

We could just write a DSL for defining and executing FSTs, but this separation
allows us to verify the FST separately and and thereby to simplify bug-fixing
and potential extensions.
