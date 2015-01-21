# Cobol unit testing framework for mainframe programs

The goal of the project is to enable isolated unit testing of individual paragraphs in Cobol programs in a standalone environment with no connection to a zOS system. 

Please see [the wiki](https://github.com/neopragma/cobol-unit-test/wiki) for more information.

## Description

The unit test precompiler copies the program under test and inserts test code into the WORKING-STORAGE SECTION and PROCEDURE DIVISION. You can then run the copy of the program, which executes the isolated unit test cases without running the program from start to finish.

## A brief sample

```
           TESTSUITE 'CONVERT COMMA-DELIMITED FILE TO FIXED FORMAT' 

      *****************************************************************
      * COBOL COMMENTS ARE IGNORED SO YOU CAN DO THIS SORT OF THING IF
      * YOU PLEASE.
      *****************************************************************  

           BEFORE-EACH
               MOVE FOO TO BAR
               MOVE ZERO TO WS-COUNT
           END-BEFORE

           AFTER-EACH
               INITIALIZE WS-RESULTS-TABLE
           END-AFTER

           TESTCASE 'IT CONVERTS TEXT FIELD 1 TO UPPER CASE' 
               MOVE 'something' TO TEXT-VALUE-1
               PERFORM 2100-CONVERT-TEXT-FIELD-1
               EXPECT TEXT-OUT-1 TO BE 'SOMETHING'

           TESTCASE 'IT HANDLES FILE NOT FOUND GRACEFULLY'
               MOCK
                   FILE INPUT-FILE 
                   ON OPEN STATUS FILE-NOT-FOUND
               END-MOCK    
               PERFORM 0100-OPEN-INPUT
               EXPECT WS-INPUT-FILE-STATUS TO BE '35'
               EXPECT WS-FRIENDLY-MESSAGE TO BE 'SORRY, COULDN''T FIND INPUT-FILE'
```

### Notes

The precompiler recognizes certain keywords and substitutes test code, so that you need not code a lot of boilerplate code manually.

TESTSUITE - Provides a description for a series of test cases. The description is echoed in the output from the test run.

MOCK - declares a mock. Current version has very basic support for mocking batch file I/O. This feature is currently under active development.

BEFORE-EACH, AFTER-EACH - the precompiler copies these statements into paragraphs that are performed at the start and end of each test case.

TESTCASE - identifies a test case. The description is echoed in the output of the test run.

EXPECT - asserts an expected result. Current version only supports an equality check for PIC X items. 

The precompiler ignores Cobol-style comment lines.


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
