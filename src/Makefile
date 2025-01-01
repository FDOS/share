VPATH=../kitten ../tnyprntf

all: share.com


ifeq "$(COMPILER)" "gcc"
%.obj: %.ob1
	ia16-elf-objcopy \
		--rename-section .text=.text.startup \
		--rename-section .data=.data.startup \
		--rename-section .rodata=.rodata.startup \
		--rename-section .bss=.bss.startup \
		$^ $@

%.ob1: %.c
else
%.obj: %.c
endif
	$(CC) $(CFLAGS)$@ $^

share.obj: share.c
	$(CC) $(CFLAGS)$@ $^

ifeq "$(COMPILER)" "gcc"
amishelp.obj: amishelp.asm
	nasm -f elf -I ../lmacros/ -I lmacros/ $< -o $@
else
amishelp.obj: amishelp.asm
	nasm -f obj -I ../lmacros/ -I lmacros/ $< -o $@

kitten.obj: ../kitten/kitten.c
	$(CC) $(CFLAGS)$@ $^

tnyprntf.obj: ../tnyprntf/tnyprntf.c
	$(CC) $(CFLAGS)$@ $^
endif

share.com: share.obj $(EXTRA_OBJS)
	$(LD) $(LDFLAGS)
	$(UPXARGS) $@

clean:
	$(RM) *.obj

clobber: clean
	$(RM) *.com
	$(RM) *.exe
	$(RM) *.map
