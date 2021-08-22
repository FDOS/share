
# nmake makefile
# share must be linked as COM file
# best is to use TC 2.01 which is freely available

USETC2=1
COM=1

!if $(USETC2)
CCBASE=c:\tc201
BINBASE=
!else
CCBASE=c:\tc30
BINBASE=\bin
!endif

!if $(COM)
COPT=-c -mt -1 -I$(INCLUDE)
LOPT=/m /s /c /t
!else
COPT=-c -ms -1 -I$(INCLUDE)
LOPT=/m /s /c
!endif

CC=$(CCBASE)$(BINBASE)\tcc
LD=$(CCBASE)$(BINBASE)\tlink
LIBS=$(CCBASE)\lib
INCLUDE=$(CCBASE)\include

share.com: share.obj
	$(TLINK) $(LOPT) $(LIBS)\c0t.obj share.obj,share.com,,$(LIBS)\cs.lib

share.obj: share.c
	$(TCC) $(COPT) share.c

clean:
	del *.obj
	
clobber: clean
	del *.com
	del *.exe
	del *.map
