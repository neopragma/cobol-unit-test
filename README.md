# Cobol unit testing framework for mainframe programs

The goal of the project is to enable isolated unit testing of individual paragraphs in Cobol programs in a standalone environment with no connection to a zOS system. 

Please see [the wiki](https://github.com/neopragma/cobol-unit-test/wiki) for more information.

## Description

The unit test precompiler copies the program under test and inserts test code into the WORKING-STORAGE SECTION and PROCEDURE DIVISION. You can then run the copy of the program, which executes the isolated unit test cases without running the program from start to finish.

### Directory structure

```
README.md                     This file.
envvars                       Environment variable settings. Sourced by bash scripts.
compile                       Bash script to compile a program.
run-ut                        Run the unit tests for a program.
run-examples                  Run all the unit test examples provided with the project.
target/                       Destination directory for compiled programs (.so and executable).
src/
  |
  +-- main/
  |     |
  |     +-- cobol/            "Production" source code.
  |     |     |
  |     |     +-- copy/       "Production" copy library.
  |     |
  |     +-- resources/        "Production" resource files.
  |
  +-- test/  
        |
        +-- cobol/            Test source code.
        |     |
        |     +-- unit-tests/ Unit test copy files.
        |
        +-- resources/        Test resource files, including unit test configuraiton files.
  
```

### Examples

Compile the program ```SAMPLE```, located at ```src/main/cobol/SAMPLE.CBL```

```sh
./compile SAMPLE
```

Run the unit tests for ```SAMPLE```. Configuration file is ```src/test/resources/SAMPLEC``` and unit tests are in ```src/test/cobol/unit-tests/SAMPLET```.

```sh
./run-ut SAMPLEC SAMPLET
```

Run all the provided examples.

```sh
./run-examples
```

The sample programs are:

* SAMPLE - minimal unit test code to demonstrate basic usage of the framework.
* CONVERT - convert a CSV file into a fixed-format file.
* FIZZBUZZ - the classic FizzBuzz exercise.
* INVDATE - calculate the date of the last day of the month.

The unit test framework comprises:

* ZUTZCPC - the precompiler that inserts test code into a copy of the program under test.
* ZUTZCPD - PROCEDURE DIVISION copybook containing common test code.
* ZUTZCWS - WORKING-STORAGE SECTION copybook containing common test code.

### Why the funky names?

We want the unit test framework to be usable on zOS, *nix, and Windows systems. Most objects on zOS systems are limited to 8-character names, and IBM product names follow a certain pattern. We followed the same pattern in an informal way. Referring to [this document](http://www-01.ibm.com/support/knowledgecenter/SSLTBW_1.13.0/com.ibm.zos.r13.bpxa800/bpxza8c008.htm), we did not find an existing product with the prefix ZUT. So, the names of the components in the unit testing framework mean:

* ZUT - zOS Unit Test
* ZC - zOS Cobol
* PC - precompiler
* PD - PROCEDURE DIVISION
* WS - WORKING-STORAGE
