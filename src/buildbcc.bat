@REM builds updated share 
::set LDFLAGS=/m /s /c /t %BASEPATH%\lib\c0t.obj $(EXTRA_OBJS) share.obj,share.com,,%BASEPATH%\lib\cs.lib
::set CFLAGS=-I../kitten -I../tnyprntf -I%BASEPATH%/include -mt -1 -c -o
@:: -q quiet, -1- 8086 / -1 80186 instructions -mt tiny memmodel CS=DS=SS and DOS exe, -Fs force SS=DS -N- no stack checks -v debug info -c  compile only -o= name of obj file -I include dir -zl no default lib
@:: -S list file with asm (-s not supported with BC5), -Os optmize for size (default), -u- disable underscores, -X no default libs?
@:: -zPDGROUP code (_TEXT) is part of DGROUP (cs=ds)

nasm -f obj -g -D__TURBOC__ startup.asm -o startup.obj 
nasm -f obj -g -I ../lmacros/ -D__TURBOC__ amisstub.asm -o amisstub.obj 
bcc -1- -mt -Os -Fs -N- -v -zPDGROUP -c -osharetsr.obj sharetsr.c 
nasm -f obj -g -D__TURBOC__ dossup.asm -o dossup.obj 
bcc -1- -mt -Os -Fs -N- -v -zPDGROUP -c -oclib.obj clib.c 
nasm -f obj -g -I ../lmacros/ -D__TURBOC__ amishelp.asm -o amishelp.obj 
bcc -1- -mt -Os -Fs -N- -v -zPDGROUP -c -oshareprg.obj shareprg.c 
bcc -1- -mt -Os -Fs -N- -v -zPDGROUP -c -otnyprntf.obj -I..\tnyprntf ..\tnyprntf\tnyprntf.c 
bcc -1- -mt -Os -Fs -N- -v -zPDGROUP -c -okitten.obj -I..\kitten ..\kitten\kitten.c 
bcc -1- -mt -Os -Fs -N- -v -zPDGROUP -I..\kitten -I..\tnyprntf -c -osharemsg.obj sharemsg.c 
@rem nasm -f obj -g -D__TURBOC__ messages.asm -o messages.obj 
::#LINK obj[,out[,map[,lib[,def[,res]]]]]
tlink @share.cfg
