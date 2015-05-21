@echo off
rem ================================================================================
rem Run unit tests
rem
rem %1 Test configuration file name
rem %2 Program to be tested (without suffix)
rem %3 Unit test cases copy file
rem %4 Test driver program name
rem ================================================================================

setlocal enabledelayedexpansion
setlocal enableextensions
if errorlevel 1 (
  echo Unable to enable extensions
  echo Bailing out 
  goto eof
)

rem ================================================================================
rem Set environment variables if necessary
rem ================================================================================

if not defined COBOL (
  call setenv
)

rem ================================================================================
rem Process command-line arguments
rem ================================================================================

set SUBPROGRAM=false
set VERBOSE=false
set CONFIGFILE=
set PROGRAMNAME=
set UNITTESTFILE=
set DRIVERNAME=

if (%1)==() goto showhelp
:cmdline

rem If the next item does not begin with a dash, assume there are no more
rem command line options. Remaining items are arguments.

echo.%~1 | findstr /r "^-" 1>nul
if errorlevel 1 goto saveargs

if (%1)==() goto cmdlinex
if /I "%~1"=="-h" goto showhelp
if /I "%~1"=="--help" goto showhelp
if /I "%~1"=="-s" set SUBPROGRAM=true & shift
if /I "%~1"=="--subprogram" set SUBPROGRAM=true & shift
if /I "%~1"=="-o" set "COBCONFIG=c:\GnuCOBOL\config\%~2.conf" & shift
if /I "%~1"=="--compat" set COMPAT=%~2 & set "COBCONFIG=%CONFIGPATH%\%COMPAT%.conf" & shift
if /I "%~1"=="-v" set VERBOSE=true & shift
if /I "%~1"=="--verbose" set VERBOSE=true & shift

if not (%1)==() goto cmdline
:cmdlinex

:saveargs
set CONFIGFILE=%TESTRSC%\%~1
shift
set PROGRAMNAME=%~1
shift
set UNITTESTFILE=%TESTCPY%\%~1
shift
set DRIVERNAME=%~1


rem Ready to run the unit tests

set "SRCPRG=%MAINSRC%\%PROGRAMNAME%.CBL"
set "SOURCE=%MAINSRC%"
set "TESTPRG=%TESTSRC%\TESTPRG.CBL"
set "TESTNAME=TESTPRG"
set "UTSTCFG=%CONFIGFILE%"
set "UTESTS=%UNITTESTFILE%"

if /I %VERBOSE% equ true (
  echo INFO: COMPAT=%COMPAT%
  echo INFO: COBCONFIG=%COBCONFIG%
  echo INFO: VERBOSE=%VERBOSE%
  echo INFO: SUBPROGRAM=%SUBPROGRAM%
  echo INFO: CONFIGFILE=%CONFIGFILE%
  echo INFO: PROGRAMNAME=%PROGRAMNAME%
  echo INFO: UNITTESTFILE=%UNITTESTFILE%
  echo INFO: DRIVERNAME=%DRIVERNAME%
  echo INFO: SRCPRG=%SRCPRG%
  echo INFO: TESTPRG=%TESTPRG%
  echo INFO: TESTNAME=%TESTNAME%
  echo INFO: UTSTCFG=%UTSTCFG%
  echo INFO: Config file contains:
  echo ----------
  type %UTSTCFG%
  echo ----------
  echo INFO: UTESTS=%UTESTS%
  echo INFO: COBCPY=%COBCPY%
  echo INFO: COBOPTS=%COBOPTS%
  echo INFO: COB_LIBRARY_PATH=%COB_LIBRARY_PATH%
  echo INFO: SOURCE=%SOURCE%
  echo INFO: TARGET=%TARGET%
)

if exist %TARGET%\%TESTNAME% (
  del %TARGET%\%TESTNAME%
)

rem ================================================================================
rem Merge test cases with the code under test.
rem ================================================================================

if /I %VERBOSE% equ true (
  echo INFO: Running precompiler to merge test cases with the code under test
)

%TARGET%\ZUTZCPC 

if /I %VERBOSE% equ true (
  set "VOPT=--verbose"
) else (
  set "VOPT="
)

if errorlevel 0 (
  if /I %SUBPROGRAM% equ true (
    goto testsubprogram
  ) else (
    goto testmainprogram  
  )  
)
goto eof

rem ================================================================================
rem Run unit tests for a subprogram:
rem 1. Compile the program under test.
rem 2. Rename the .dll file to the correct name.
rem 3. Execute the test drive program named on the command line.
rem ================================================================================

:testsubprogram

if /I %VERBOSE% equ true (
  echo INFO: Compiling subprogram containing unit test code
)

call compile --test --subprogram %VOPT% %TESTNAME%
move %TARGET%\%TESTNAME%.dll %TARGET%\%PROGRAMNAME%.dll
%TARGET%\%DRIVERNAME%
goto eof

rem ================================================================================
rem Run unit tests for a main program:
rem 1. Compile the program under test.
rem 2. Move the executable file to the target directory.
rem 3. Execute the test program.
rem ================================================================================

:testmainprogram

if /I %VERBOSE% equ true (
  echo INFO: Compiling test program containing unit test code
)

set COBOPTS=-x
call compile --test %VOPT% %TESTNAME%
move %MAINSRC%\%PROGRAMNAME%.exe %TARGET%\%TESTNAME%.exe
%TARGET%\%TESTNAME%
goto eof

rem ================================================================================
rem Display usage help for this script.
rem ================================================================================

:showhelp
echo Run unit tests for a Cobol program
echo Version %VERSION%
echo Usage: run-ut [options] config-file-name program-name-without-suffix unit-test-file-name [driver-program-name]
echo     ^-h ^| --help     Display usage help (this text) and exit
echo     ^-o ^| --compat   Cobol compatibility (bs2000, cobol85, cobol2002, ibm, mf, mvs, default)
echo     ^-s ^| --subprogram Generate a .dll (not an .exe)
echo     ^-v ^| --verbose  Display progress information

:eof