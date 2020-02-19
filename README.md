# Cobol unit testing framework for mainframe programs

The goal of the project is to enable isolated unit testing of individual paragraphs in Cobol programs in a standalone environment with no connection to a zOS system.

Please see [the wiki](https://github.com/neopragma/cobol-unit-test/wiki/) for more information.

## Notice

This is the current version used in Bankdata, Denmark. 

Atm. we are not making more pull requests to the original source, as the ones we allready have made, are not processed.

## Changes from master
### Features:
* Added section mocking.
* Validating more numbers, pic s9(8) is no longer the limit.
* Removed the z/OS version, as it is a very minor fix to use the pc version. Added description of how to fix it.
* All mocks can now contain more mocked lines.
* Added eyecatchers surrounding inserted code, for use with gathering code coverage info.

### Bugfixes:
* Avoid infinite loops when missing . at end of testsuite file.
* Comments in area A of the COBOL program no longer breaks the unit test.
* Resets mock counters between testcases.
* Handling of IN structures in expect statements.
* Handling of expect statements that span multiple lines.
* Handling of arrays in expect statements.
* Using max value as end point, when traversing occurs structures containing lines of code, instead of relying on spaces as end marker.
* Multi mock issue from sfauvel.
* fix parsing of multiline SELECT statements without FILE STATUS from mmitch.
* Fixed error caused by END-EXEC not being on position 7, in tested program source.
* Fixed errors in verify mock call.
* Fixed call handling, so a call will terminate at other COBOL reserved words, and not just . or END-CALL.
* Fixed token gathering, so it will correctly pick up tokens like: "Hello 'World'".

### Other fixes:
* Fixed a lot of warnings and performance issues with the z/OS COBOL compiler.
