@REM builds updated share 
nasm -f obj -g -D__WATCOM__ startup.asm -o startup.obj 
nasm -f obj -g -I ../lmacros/ -D__WATCOM__ amisstub.asm -o amisstub.obj 
nasm -f obj -g -D__WATCOM__ messages.asm -o messages.obj 
wcc -q -0 -ms -btdos -s -os -d3 -fo=sharetsr.obj sharetsr.c 
wcc -q -0 -ms -btdos -s -os -d3 -fo=shareprg.obj shareprg.c 
wcc -q -0 -ms -btdos -s -os -d3 -I..\kitten -I..\tnyprntf -fo=sharemsg.obj sharemsg.c 
wcc -q -0 -ms -btdos -s -os -d3 -fo=clib.obj clib.c 
wcc -q -0 -ms -btdos -s -os -d3 -fo=tnyprntf.obj ..\tnyprntf\tnyprntf.c 
wcc -q -0 -ms -btdos -s -os -d3  -fo=kitten.obj ..\kitten\kitten.c 
wlink @share.rsp 
