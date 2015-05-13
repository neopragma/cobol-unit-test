rem ================================================================================
rem Compile script for Windows.
rem ================================================================================

@echo off
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

set CLEAN=
set SOURCE=
set SUBPROGRAM=
set TEST=
if (%1)==() goto showhelp
:getopts
if (%1)==() goto getoptsx
if /I "%~1"=="-h" goto showhelp
if /I "%~1"=="--help" goto showhelp
if /I "%~1"=="-c" set CLEAN=true & shift
if /I "%~1"=="--clean" set CLEAN=true & shift
if /I "%~1"=="-s" set SUBPROGRAM=true & shift
if /I "%~1"=="--subprogram" set SUBPROGRAM=true & shift
if /I "%~1"=="-t" set TEST=true & shift
if /I "%~1"=="--test" set TEST=true & shift
if not (%1)==() goto getopts
:getoptsx


if /I %TEST% equ true (set SOURCE=%TESTSRC%) else (set SOURCE=%MAINSRC%)  

echo SOURCE is %SOURCE%
echo SUBPROGRAM is %SUBPROGRAM%

if /I %SUBPROGRAM% equ true (

  echo in if block
  set SUFFIX=.dll
  set COBOPTS=^-m
) else (
  echo in else block
  set SUFFIX=
  set COBOPTS=^-x
)


echo SUFFIX is %SUFFIX%
echo COBOPTS is %COBOPTS%

goto eof




if [ $SUBPROGRAM == true ]; then        # make a dynamic link module
  SUFFIX='.so'
  COBOPTS='-m'
else                                    # make an executable
  SUFFIX=
  COBOPTS='-x'
fi

                                        # remove existing output file, if any
if [ $CLEAN == true ] && [ -e "$TARGET/${1}${SUFFIX}" ]; then
    rm "$TARGET/${1}${SUFFIX}"
fi  

cobc "$COBOPTS" -std=ibm "$SOURCE/$1.CBL"        # compile, assemble, link w/ selected options

if [ $? -eq 0 ]                         # copy output file to target directory
  then
    mv "${1}${SUFFIX}" "$TARGET/."
    exit 0
  else
    exit 1  
fi


:showhelp
echo GNU COBOL compile script
echo Version %VERSION%
echo Usage: compile [options] program-name-without-suffix [subprogram-names]
echo     ^-c ^| --clean  Delete the existing executable before compiling
echo     ^-h ^| --help     Display usage help (this text) and exit
echo     ^-t ^| --test     Source is in the project test directory (not main)
echo     ^-s ^| --subprogram Generate a callable subprogram (not an executable)

:eof
