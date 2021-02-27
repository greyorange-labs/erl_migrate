# mnesia_migrate
A tool to upgrade/downgrade schema and migrate data of erlang app's database

[![Build Status](https://travis-ci.org/greyorange/go_migrate.svg?branch=main)](https://travis-ci.org/greyorange/mnesia_migrate)

# Installation

* run `make deps` if you are using erlang.mk

# Usage

* apply upgrade
* apply downgrade
* get current head
* get current applied head
* Pass argument `Options` type = maps:map() 
* To enable print statements of library, add {debug, true} in sys.config
* Run `detect_revision_sequence_conflicts(Options)` to get a list of revision id where there is a fork in the revision tree.

# License

MIT License
