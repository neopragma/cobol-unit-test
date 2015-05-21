@echo off
REM =====================================================================
REM Environment variable settings for cobol unit test (Windows).
REM
REM Example:
REM
REM call setenv
REM
REM VERSION      The version string for the entire project
REM              Returned by various scripts with -v | --version option
REM PROJECT      Path to the project root directory
REM MAINSRC      Path to the application Cobol source
REM TESTSRC      Path to the test Cobol source
REM MAINCPY      Path to the application Cobol copy library
REM TESTCPY      Path to the unit test copy library
REM MAINRSC      Path to the application resources directory
REM TESTRSC      Path to the test resources directory
REM TARGET       Path to the target binary directory (.so & executables)
REM COBCPY       Used by GNU Cobol to locate copybooks
REM COB_LIBRARY_PATH Used by GNU Cobol to locate .dll files
REM =====================================================================

set "VERSION=0.2.2 May 26 2015"
set "COBOL=true"

REM Settings for GnuCOBOL

set "COMPAT=ibm"
set "CONFIGPATH=c:\GnuCOBOL\config"
set "COBCONFIG=%CONFIGPATH%\%COMPAT%.conf"

REM Settings for the project

set "PROJECT=c:\projects\cobol-unit-test"
set "MAINSRC=%PROJECT%\src\main\cobol"
set "TESTSRC=%PROJECT%\src\test\cobol"
set "MAINCPY=%MAINSRC%\copy"
set "TESTCPY=%TESTSRC%\unit-tests"
set "MAINRSC=%PROJECT%\src\main\resources"
set "TESTRSC=%PROJECT%\src\test\resources"
set "TARGET=%PROJECT%\target"
set "COBCPY=%MAINCPY%;%TESTCPY%"
set "COB_LIBRARY_PATH=%TARGET%"

REM Add project directories to PATH

set "PATH=%PROJECT%;%TARGET%;%PATH%"

