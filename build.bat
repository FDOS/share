rem Call this batch file with an argument of tcc3 or tcc2 to select compiler
goto %1

:tcc2
rem ############# Turbo C 2.01 ########################
set PATH=C:\bin;C:\tc201;%PATH%
set LIBS=C:\tc201\lib
set CC=tcc
set LD=tlink
rem Small
rem set COPT=-c -ms -1 share.c
rem set LOPT=/m /s /c $(LIBS)\c0s.obj share.obj,share.com,,$(LIBS)\cs.lib
rem Tiny
set CFLAGS=-mt -1 -c -o
set LDFLAGS=/m /s /c /t $(LIBS)\c0t.obj $(EXTRA_OBJS) share.obj,share.com,,$(LIBS)\cs.lib
rem tcc looks for includes from the current directory, not the location of the
rem file that's trying to include them, so add kitten's location
set CFLAGS=-I../kitten -I../tnyprntf %CFLAGS%
goto doit

:tcc3
rem ############# TURBO_C 3 ########################
set PATH=C:\bin;C:\tc\bin;%PATH%
set LIBS=C:\tc\lib
set CC=tcc
set LD=tlink
rem Tiny
set COPT=-c -mt -1 share.c
set LOPT=/m /s /c /t -L$(LIBS) c0t.obj share.obj,share.com,,cs.lib
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
make -C src
