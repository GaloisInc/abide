# Overview

The abide library provides a machine-readable encoding Application Binary Interface (ABI) information for a number of machine architectures.  The ABI for a platform governs how data is represented and shared between functions (e.g., as function arguments and in the layout of structures).  To a first approximation, each operating system and architecture pair have their own ABI (though some platforms may support multiple different ABIs).  The abide library provides two components:

1. A DSL for specifying ABIs and persisting them in a programming language neutral format, and
2. A Haskell library for interpreting the ABI information

## Generation

A small DSL for describing simple finite state transducers in a way that lends itself to generating files in AT&T FSM format, which is what is used by OpenFST.

## Computation

After the FSM files are generated, we read them back in and generate code to execute them in Haskell.

We could just write a DSL for defining and executing FSTs, but this separation allows us to verify the FST separately and and thereby to simplify bug-fixing and potential extensions.

# Acknowledgements

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. HR001118C0029.

# Distribution

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
