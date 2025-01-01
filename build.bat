@ECHO OFF
rem Call this batch file with an argument of tcc3 or tcc2 to select compiler
IF NOT [%2]==[] SET BASEPATH=%2
IF [%2]==[] SET BASEPATH=
IF NOT [%3]==[] SET MAKE=%3
IF [%3]==[] SET MAKE=make
IF [%1]==[] goto help
SET OLD_PATH=%PATH%
goto %1

:help
ECHO build tcc# [tcpath] [make]
ECHO e.g.
ECHO build tcc2 D:\tc201 d:\dgjpp\bin\make
ECHO or
ECHO build tcc3 D:\tc30
ECHO.
ECHO Note: ensure UPX is in your path, optionally make in C:\bin
goto end

:tcc2
rem ############# Turbo C 2.01 ########################
IF [%BASEPATH%]==[] set BASEPATH=C:\tc201
set PATH=C:\bin;%BASEPATH%;%PATH%
goto tcshared

:tcc3
rem ############# TURBO_C 3 ########################
IF [%BASEPATH%]==[] set BASEPATH=C:\tc
set PATH=C:\bin;%BASEPATH%\bin;%PATH%
rem TC3 may create relocation records, if so use Small memory model
goto tcshared

:tcshared
rem ############# TURBO_C shared ########################
set LIBS=%BASEPATH%\lib
set INCLUDE=%BASEPATH%/include
set CC=tcc
set LD=tlink
set LDFLAGS=/m /s /c /t $(LIBS)\c0t.obj $(EXTRA_OBJS) share.obj,share.com,,$(LIBS)\cs.lib
set CFLAGS=-I../kitten -I../tnyprntf -I%INCLUDE% -mt -1 -c -o
goto doit

:doit
set EXTRA_OBJS=

set EXTRA_OBJS=%EXTRA_OBJS% kitten.obj
rem # if you want to build without kitten comment the above and uncomment
rem the following
rem set CFLAGS=-DNOCATS %CFLAGS%

set EXTRA_OBJS=%EXTRA_OBJS% tnyprntf.obj
rem # if you want to build without tnyprntf comment the above and uncomment
rem the following
rem set CFLAGS=-DNOPRNTF %CFLAGS%

set UPXARGS=upx --8086 --best
rem if you don't want to use UPX set
rem     UPXARGS=-rem
rem if you use UPX: then options are
rem     --8086 for 8086 compatibility
rem   or
rem     --best for smallest

rem We use GNU make for all targets
%MAKE% -C src

rem cleanup
:end
set PATH=%OLD_PATH%
set MAKE=
set BASEPATH=
set INCLUDE=
set LIBS=
set CC=
set CFLAGS=
set LD=
set LDFLAGS=
set EXTRA_OBJS=
set UPXARGS=
