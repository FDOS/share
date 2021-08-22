share.com: share.obj
	$(LD) $(LOPT)

share.obj: share.c
	$(CC) $(COPT)

clean:
	$(RM) *.obj
	
clobber: clean
	$(RM) *.com
	$(RM) *.exe
	$(RM) *.map
