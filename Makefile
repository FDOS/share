share.com: share.obj $(XOBJS)
	$(LD) $(LOPT)

share.obj: share.c
	$(CC) $(COPT)

gcc_help.obj: gcc_help.asm
	nasm -f elf -I ../lmacros/ $< -o $@

clean:
	$(RM) *.obj

clobber: clean
	$(RM) *.com
	$(RM) *.exe
	$(RM) *.map
