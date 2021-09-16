# Welcome to System-O

System-O is a type checker for distributed systems using Open-Api v3.

The idea is that we can give it two reifed sets of open api specs, and it can tell us if one is a sub-type of the other.

Steps are as follows:
    1) Read a manifest, enumerating the specs that are currently in use, and any exclusions that may be necessary
    2) Flatten those specs, resolving all transitive dependencies, and create a snapshot of the concrete specs in use.
    3) Assuming we want to validate a change, create a flatten schema for the service being changed.
    4) load up these flatten specs, and see if one is a subtype of the other.

WIP:
    moving toward part 4 - defining the subtype relation

Next steps:
    Define a manifest schema
    Define a reification strategy

Notes:
    This is written in haskell - it's pretty strong at recursion, pattern matching, etc.  There is a handy open-api parser/domain model.  I think it'll be fun,.