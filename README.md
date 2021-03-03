# go_migrate
A tool to upgrade/downgrade schema and migrate data of erlang app's database

[![Build Status](https://travis-ci.org/greyorange/go_migrate.svg?branch=master)](https://travis-ci.org/greyorange/go_migrate)

# Installation

* run `make deps` to install depedencies  
* run `make` to compile code


# Usage
* Data structure of Argument `Options` is map
```
Options =
  #{
      app_db_name => amar_db,
      app_name => mhs,
      migration_beam_files_dir_path => "/Users/amar.c/workspace/rc105/butler_server/apps/mhs/ebin/",
      migration_src_files_dir_path => "apps/mhs/src/migrations/"
   }
```
* ceate migation src file
```
go_migrate:create_migration_file(Options).
```

* apply upgrade
```
go_migrate:apply_upgrades(Options).

```
* apply downgrade
```
go_migrate:apply_downgrades(Options, Num).

```
* To enable print statements of library, add {debug, true} in sys.config under go_migrate app config section
* Run `go_migrate:detect_revision_sequence_conflicts(Options)` to get a list of revision id where there is a fork in the revision tree.

# License

MIT License
