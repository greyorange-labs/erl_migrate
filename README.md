# erl_migrate
A tool to upgrade/downgrade schema and migrate data of an erlang app's database(s)

[![Build Status](https://travis-ci.org/greyorange/erl_migrate.svg?branch=master)](https://travis-ci.org/greyorange/erl_migrate)

# Installation

* run `make deps` to install depedencies  
* run `make` to compile code
* run `make eunit` to test UT's


# Usage
* Data structure of Argument `Args` is a map
```
   #{
      schema_name => NameOfSchema,
      migration_src_files_path => SrcFilePath,
      migration_beam_files_path => BeamFilePath,
      schema_instance => SchemaInstanceName,
      down_base_migration => boolean()
   }
```

* create migration file
```
Args = 
   #{
      schema_name => schema_name_1, 
      migration_src_files_path => "apps/app_name/src/migrations",
      migration_beam_files_path => "/opt/app_name/ebin"
   }
erl_migrate:create_migration_file(Args).
```

* apply upgrade
```
Args =
   #{schema_name => schema_name_1, schema_instance => schema_instance_1},
erl_migrate:apply_upgrades(Args).

```
* apply downgrade
```
Args =
   #{schema_name => schema_name_1, schema_instance_1},
erl_migrate:apply_downgrades(Args, Num).

```

* detect revision seq conflicts
```
Args =
   #{schema_name => schema_name_1},
erl_migrate:detect_revision_sequence_conflicts(Args, Num).
```

* To enable print statements of library, add {debug, true} in sys.config under erl_migrate app config section

# License

MIT License
