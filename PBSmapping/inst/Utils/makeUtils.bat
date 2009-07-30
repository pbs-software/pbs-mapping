@echo off
SETLOCAL

rem ***** Edit the MinGW PATH variables listed below *****

set MINGW_PATH=C:\MinGW\bin

rem *********** No need to edit anything below ***********

set Path=%MINGW_PATH%;

if not exist "%MINGW_PATH%\g++.exe" (
  ECHO Can't find MinGW c++ compiler - check MINGW_PATH "%MINGW_PATH%"
  ECHO contains the program "g++.exe"
  set PBSERROR=1 )

if not exist "%MINGW_PATH%\mingw32-make.exe" (
  ECHO Can't find MinGW Make utility - check MINGW_PATH "%MINGW_PATH%"
  ECHO contains the program "mingw32-make.exe"
  set PBSERROR=1 )

if not defined PBSERROR (
  mingw32-make.exe
)

pause
