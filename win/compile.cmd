@echo off
rem ================================================================================
rem Compile script for Windows.
rem Version 0.0.1 20 May 2015
rem ================================================================================

setlocal enabledelayedexpansion
setlocal enableextensions
if errorlevel 1 (
  echo ERROR: Unable to enable extensions
  echo ERROR: Bailing out 
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

set CLEAN=false
set SOURCE=
set SUBPROGRAM=false
set TEST=false
set VERBOSE=false
set PROGRAMNAME=

if (%1)==() goto showhelp
:cmdline

rem If the next item does not begin with a dash, assume it''s the program name

echo.%~1 | findstr /r "^-" 1>nul
if errorlevel 1 (
  set PROGRAMNAME=%~1
  shift
)

if (%1)==() goto cmdlinex
if /I "%~1"=="-h" goto showhelp
if /I "%~1"=="--help" goto showhelp
if /I "%~1"=="-c" set CLEAN=true & shift
if /I "%~1"=="--clean" set CLEAN=true & shift
if /I "%~1"=="-s" set SUBPROGRAM=true & shift
if /I "%~1"=="--subprogram" set SUBPROGRAM=true & shift
if /I "%~1"=="-t" set TEST=true & shift
if /I "%~1"=="--test" set TEST=true & shift
if /I "%~1"=="-o" set "COBCONFIG=c:\GnuCOBOL\config\%~2.conf" & shift
if /I "%~1"=="--compat" set "COBCONFIG=c:\GnuCOBOL\config\%~2.conf" & shift
if /I "%~1"=="-v" set VERBOSE=true & shift
if /I "%~1"=="--verbose" set VERBOSE=true & shift

if not (%1)==() goto cmdline
:cmdlinex

rem ================================================================================
rem Determine whether source file is in the test or main subdirectory.
rem ================================================================================

if /I %TEST% equ true (set SOURCE=%TESTSRC%) else (set SOURCE=%MAINSRC%)  

if not exist %TARGET% md %TARGET%

if /I %VERBOSE% equ true (
  echo INFO: SOURCE=%SOURCE%
  echo INFO: TARGET=%TARGET%
)

rem ================================================================================
rem Set GNU COBOL compiler options for main programs vs. subprograms
rem ================================================================================

if /I %SUBPROGRAM% equ true (
  set SUFFIX=.dll
  set COBOPTS=^-m
) else (
  set SUFFIX=.exe
  set COBOPTS=^-x
)

if /I %VERBOSE% equ true (echo INFO: COBOPTS=%COBOPTS%)

rem ================================================================================
rem Delete existing object file from target directory if --clean specified.
rem ================================================================================

if /I %VERBOSE% equ true (echo INFO: CLEAN=true: Deleting %TARGET%\%PROGRAMNAME%%SUFFIX%)

if /I %CLEAN% equ true (
  if exist %TARGET%\%PROGRAMNAME%%SUFFIX% (
    del %TARGET%\%PROGRAMNAME%%SUFFIX%
  )
)

rem ================================================================================
rem Compile and move the object file to the target directory.
rem ================================================================================

if /I %VERBOSE% equ true (
  echo INFO: Config file is %COBCONFIG%
  echo INFO: COBCPY is %COBCPY%
)

echo ====== COMPILE IS NEXT ======

cobc -I %MAINCPY% -I %TESTCPY% -conf=%COBCONFIG% %COBOPTS% %SOURCE%\%PROGRAMNAME%.CBL

if errorlevel 0 (
  if /I %VERBOSE% equ true (
    echo INFO: Moving %PROGRAMNAME%%SUFFIX% to target dir %TARGET%)
  move "%SOURCE%\%PROGRAMNAME%%SUFFIX%" "%TARGET%\%PROGRAMNAME%%SUFFIX%"
)

goto eof

rem ================================================================================
rem Display usage help for this script.
rem ================================================================================

:showhelp
echo GNU COBOL compile script
echo Version %VERSION%
echo Usage: compile [options] program-name-without-suffix [subprogram-names]
echo     ^-c ^| --clean    Delete the existing executable before compiling
echo     ^-h ^| --help     Display usage help (this text) and exit
echo     ^-o ^| --compat   Cobol compatibility (bs2000, cobol85, cobol2002, ibm, mf, mvs, default)
echo     ^-t ^| --test     Source is in the project test directory (not main)
echo     ^-s ^| --subprogram Generate a .dll (not an .exe)
echo     ^-v ^| --verbose  Display progress information

:eof
exit /B 0