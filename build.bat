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
set COPT=-c -mt -1 share.c
set LOPT=/m /s /c /t $(LIBS)\c0t.obj share.obj,share.com,,$(LIBS)\cs.lib
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
rem We use GNU make for all targets
make -C src
