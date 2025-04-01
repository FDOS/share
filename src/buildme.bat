@REM builds updated share 
@:: -q quiet, -0 8086 instructions -ms small memmodel -btdos DOS exe, -s no stack checks -os optimize smaller code, -d3 max debug info -fo= name of obj file -I include dir -zl no default lib
nasm -f obj -g -D__WATCOM__ startup.asm -o startup.obj 
nasm -f obj -g -l amisstub.lst -Lf -I ../lmacros/ -D__WATCOM__ amisstub.asm -o amisstub.obj 
wcc -q -0 -ms -btdos -s -os -d3 -fo=sharetsr.obj sharetsr.c 
nasm -f obj -g -l dossup.lst -D__WATCOM__ dossup.asm -o dossup.obj 
wcc -q -0 -ms -btdos -s -os -d3 -fo=clib.obj clib.c 
nasm -f obj -g -I ../lmacros/ -D__WATCOM__ amishelp.asm -o amishelp.obj 
wcc -q -0 -ms -btdos -s -os -d3 -fo=shareprg.obj shareprg.c 
wcc -q -0 -ms -btdos -s -os -d3 -fo=tnyprntf.obj ..\tnyprntf\tnyprntf.c 
wcc -q -0 -ms -btdos -s -os -d3  -fo=kitten.obj ..\kitten\kitten.c 
wcc -q -0 -ms -btdos -s -os -d3 -I..\kitten -I..\tnyprntf -fo=sharemsg.obj sharemsg.c 
@rem nasm -f obj -g -D__WATCOM__ messages.asm -o messages.obj 
wlink @share.rsp 
