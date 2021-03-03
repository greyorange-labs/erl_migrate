# erl_migrate
A tool to upgrade/downgrade schema and migrate data of erlang app's database

[![Build Status](https://travis-ci.org/greyorange/erl_migrate.svg?branch=master)](https://travis-ci.org/greyorange/erl_migrate)

# Installation

* run `make deps` to install depedencies  
* run `make` to compile code


# Usage
* Data structure of Argument `Options` is map

* create migation src file
```
Args = 
   #{
      schema_name => mhs, 
      migration_src_files_path => "apps/mhs/src/migrations",
      migration_beam_files_path => "/opt/mhs/ebin"
   }
erl_migrate:create_migration_file(Args).
```

* apply upgrade
```
Args =
   #{schema_name => mhs},
erl_migrate:apply_upgrades(Args).

```
* apply downgrade
```
Args =
   #{schema_name => mhs},
erl_migrate:apply_downgrades(Args, Num).

```

* detect revesion seq conflicts
```
Args =
   #{schema_name => mhs},
erl_migrate:detect_revision_sequence_conflicts(Args, Num).
```

* To enable print statements of library, add {debug, true} in sys.config under erl_migrate app config section

# License

MIT License
