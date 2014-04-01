# sqlite-to-pgsql

## Purpose

Some scripts to convert sqlite databases into SQL which
can be loaded into PostgreSQL.  Especially developed and
tested for the sqlite databases produced by Firefox.

## Contents

: Makefile		- tells make how to run tests
: README			- this file
: sqlite-to-pgsql		- master shell script
: sqlite-to-pgsql.awk	- awk script that does the real work
: tests.debug		- debug output of running tests
: tests.good.pgsql	- what the tests are supposed to look like
: tests.pgsql		- sql output of running tests
: tests.sqlite		- input for running tests

## Some ways to improve it

### Automatically adding missing columns and types

Some of the fields in Mozilla's places.sqlite database are incompletely
defined, lacking types.  I've put in some fallback types in the awk
script but this is very much a kludge.

A better solution would be to make a pre-pass over the sqlite data to
collect the field names used and analyze the values being given which
could then be used in the main pass to declare missing types and fix
wonky types.
