/*
	FreeDOS SHARE - non-resident program (install/uninstall/status)
	Copyright (c) 2000, 2021 Ronald B. Cemer 
	Dual licensed under the GNU GPL v2 and MIT license (you choose)
*/

#include "share.h"


void far * getvect(unsigned char intno);
void setvect(unsigned char intno, void far * vector);
unsigned freemem(unsigned seg);
void close(unsigned fileno);
void keep(unsigned retcode, unsigned paras);

extern  unsigned      _Cdecl _psp;

#if defined(__GNUC__)
#define setvect(x, y) setvect(x, (__libi86_isr_t)y)
#endif

#if !(defined(__TURBOC__) || defined(__WATCOMC__) || defined(__GNUC__))
#error "This software must be compiled with Open Watcom, Borland/Turbo C/C++ or Gcc-ia16"
#endif

/* <stdlib.h> _psp, NULL, malloc, sbrk, free, atol, MK_FP, FP_SEG, FP_OFF */
long atol(const char *s) NON_RES_TEXT;
void *malloc(size_t size);
#define sbrk malloc
#define free(p)

/* string.h */
char * strchr(const char *str, int c);
void * memset(void *p, int val, size_t size);
//void * memclr(void *p, size_t size);  /* #define memclr(p, size) memset(p, 0, size) */


		/* ------------- PROTOTYPES ------------- */
/* sharemsg.c */
void usage(void) NON_RES_TEXT;
void bad_params(void) NON_RES_TEXT;
void print_program_message(unsigned msgno) NON_RES_TEXT;
void print_status_message(unsigned msgno, ...) NON_RES_TEXT;
void init_messages(char *argv0) NON_RES_TEXT;
void cleanup_messages(void) NON_RES_TEXT;

/* shareprg.c */
static unsigned short init_tables(void) NON_RES_TEXT;
int main(int argc, char **argv) NON_RES_TEXT;


	/* Multiplex interrupt handler */

#if defined(__WATCOMC__)
#pragma aux (__gcc16) i2D_next
#pragma aux (__gcc16) i2D_handler

#pragma aux (__gcc16) old_handler2f
#pragma aux (__gcc16) handler2f

#pragma aux amisnum "*"
#pragma aux (__gcc16) asm_find_resident
#pragma aux (__gcc16) asm_uninstall
#pragma aux (__gcc16) asm_enable
#pragma aux (__gcc16) asm_disable
#pragma aux (__gcc16) asm_init

#pragma aux (__gcc16) asm_get_status

#pragma aux (__gcc16) impx_install_check

#pragma aux top_of_stack "*"
#endif

/* Within IBM Interrupt Sharing Protocol header */
extern void __far __interrupt (*i2D_next)(void);
extern void __far __interrupt (*old_handler2f)(void);

/* Prototype for NASM interrupt handler function */
extern void __far __interrupt __attribute__((near_section)) i2D_handler(void);
extern void __far __interrupt __attribute__((near_section)) handler2f(void);

extern uint8_t amisnum;
extern uint16_t top_of_stack;

extern uint16_t asm_find_resident(void) NON_RES_TEXT;
extern uint16_t asm_uninstall(uint16_t mpx) NON_RES_TEXT;
extern uint16_t asm_enable(uint16_t mpx) NON_RES_TEXT;
extern uint16_t asm_disable(uint16_t mpx) NON_RES_TEXT;
extern uint16_t asm_init(void) NON_RES_TEXT;

/* int 2F and 2D multiplex and AMIS install check, sets AH=id and AL=0 */
extern uint16_t impx_install_check(uint16_t int_no, uint16_t multiplex_id) NON_RES_TEXT;


		/* ------------- INIT ------------- */

	/* Allocate tables.
	 * If we run out of memory return zero, otherwise return size
	 * in paragraphs from PSP to end of last table */

static unsigned short init_tables(void) {
	unsigned short paras;
	char *p;

	file_table_size = file_table_size_bytes / sizeof(file_t);
	lock_table_size_bytes = lock_table_size * sizeof(lock_t);
	file_table_free = file_table_size;
	lock_table_free = lock_table_size;

	/* file_table is initialized to end of resident code, no need to allocate memory for it.
	   WARNING: the memory is cleared just before exiting resident as it will clear any
	   non-resident code as well, crashing share if done too early!!!
	*/
#if 0
	if ((p = malloc(file_table_size_bytes + lock_table_size_bytes)) == NULL)
		return 0;
	memset(p, 0, file_table_size_bytes + lock_table_size_bytes);

	file_table = (void *)p;
#else
	p = (char *)file_table;
#endif
	lock_table = (void *)(p + file_table_size_bytes);

	p += file_table_size_bytes + lock_table_size_bytes;

	/* resident paras, counting from PSP:0, offset of end of our tables in paragraphs (rounded up) + sizeof(psp) */
	paras = (unsigned short)p;
	paras = (paras + 15)/16 + 0x10;
	return paras;
}




typedef struct __attribute__((packed, aligned(2))) {
	uint16_t patchoffset;
	uint16_t filesize;
	uint16_t filefree;
	uint16_t locksize;
	uint16_t lockfree;
	uint8_t patchstatus;
	uint8_t enable;
	uint8_t reserved[4]; /* pad structure to be 16 bytes, must be 0 */
} status_struct;

extern void asm_get_status(uint16_t mpx, status_struct * s) NON_RES_TEXT;

static int displaystatus(uint16_t mpx) NON_RES_TEXT;
static int displaystatus(uint16_t mpx) {
	status_struct s;
	if (mpx == 0xFFFF) {
		mpx = asm_find_resident();
	}
	if (mpx & 0xFF00) {
		print_status_message(MSG_notresident);
		return 1;
	}
	asm_get_status(mpx, &s);
	switch (s.patchstatus) {
	  case 0:
		print_status_message(MSG_patchstatus_notsupported);
		break;
	  case 1:
		print_status_message(MSG_patchstatus_indeterminate);
		break;
	  case 2:
		print_status_message(MSG_patchstatus_needed, FP_SEG(getvect(0x31)), s.patchoffset);
		break;
	  case 3:
		print_status_message(MSG_patchstatus_notneeded);
		break;
	  default:
		print_status_message(MSG_patchstatus_unknown);
		break;
	}

	if (s.enable & 1)
		print_status_message(MSG_filetable, s.filefree, s.filesize, s.lockfree, s.locksize);
	else
		print_status_message(MSG_filetable_disabled, s.filesize, s.locksize);
	return 0;
}


/* using pointer in psp, free environment block to avoid leaking DOS memory */
static void free_environment(void) NON_RES_TEXT;
static void free_environment(void)
{
	unsigned short far *p_env_seg;
	unsigned short env_seg;
	/* get a pointer to psp at offset 0x2c, value is segment of environment block */
	p_env_seg = (unsigned short far *)MK_FP(_psp, 0x2c);
	env_seg = *p_env_seg;
	/* validate an environment block is allocated, if so attempt to free it */
	if (env_seg) {
		if (freemem(env_seg) == 0) { /* attempt to deallocate MCB of ENV segment */
			/* if successful, clear field in PSP */
			*p_env_seg = 0;
		} else {
			print_program_message(MSG_failedfreeenv);
		}
	}
}

int Tprintf(const char * fmt, ...);


		/* ------------- MAIN ------------- */
int main(int argc, char **argv) {
	status_struct s;
	uint8_t far * share_installed = NULL;
	int installed = 0;
	int uninstallrequested = 0, statusrequested = 0, onlyoptions = 0;
	int enablerequested = 0, disablerequested = 0;
	unsigned i;
	unsigned short top_of_tsr;
	uint16_t rc;
	uint16_t mpx;
	uint8_t priorflag = 0;

	file_table_size_bytes = 2048;

	init_messages(argv[0]);
	
		/* See if the TSR is already installed. */
	/* disable(); */	/* no multitasking, so don't worry */
	Tprintf(" --- \n");
	Tprintf("Int 0x2f at %p\n", getvect(MUX_INT_NO));
	if ( (FP_SEG(getvect(MUX_INT_NO)) != 0) && (FP_OFF(getvect(MUX_INT_NO)) != 0xFFFF) ) {
		installed = (impx_install_check(MUX_INT_NO, MULTIPLEX_ID) & 0xff) == 0xff;
		Tprintf("Share installed status = %i\n", installed);
	} /* else { enable(); } */

		/* Process command line arguments.  Bail if errors. */
	for (i = 1; i < argc; i++) {
		char *arg = argv[i];
		if (arg == NULL) continue;
		if (arg[0] != '/') {
			usage();
			return 3;
		}
		arg++;
		switch(*arg) {
		case '?':
			usage();
			return 3;
		case 'u':
		case 'U':
		case 'r':
		case 'R':
			uninstallrequested = 1;
			break;
		case 's':
		case 'S':
			statusrequested = 1;
			break;
		case 'o':
		case 'O':
			onlyoptions = 1;
			break;
		case 'd':
		case 'D':
			disablerequested = 1;
			break;
		case 'e':
		case 'E':
			enablerequested = 1;
			break;
		case 'f':
		case 'F':
			arg++;
			if (*arg != ':') {
				usage();
				return 3;
			}
			arg++;

			file_table_size_bytes = (unsigned int)atol(arg);
			if (   (file_table_size_bytes < FILE_TABLE_MIN)
				|| (file_table_size_bytes > FILE_TABLE_MAX)   ) {
				bad_params();
				return 3;
			}
			break;
		case 'l':
		case 'L':
			arg++;
			if (*arg != ':') {
				usage();
				return 3;
			}
			arg++;

			lock_table_size = (unsigned int)atol(arg);
			if (   (lock_table_size < LOCK_TABLE_MIN)
				|| (lock_table_size > LOCK_TABLE_MAX)   ) {
				bad_params();
				return 3;
			}
			break;
		}
	}

	(void)asm_init();
	mpx = asm_find_resident();
	asm_get_status(mpx, &s);

	if (uninstallrequested) {
		if (2 == s.patchstatus) {
			share_installed = MK_FP(FP_SEG(getvect(0x31)), s.patchoffset);
			priorflag = *share_installed;
		}
		rc = asm_uninstall(mpx);
		if (rc == 0) {
			print_program_message(MSG_removed);
			if (2 == s.patchstatus) {
				if (*share_installed) {
					*share_installed = 0;
					print_status_message(MSG_patched);
				} else if (priorflag) {
					print_status_message(MSG_patched_itself);
				}
			}
			return 0;
		}
		switch (rc) {
		  case 1:
			print_program_message(MSG_notinstalled);
			return 6;
		  case 2:
			print_program_message(MSG_somefailure);
			return 7;
		  case 3:
			print_program_message(MSG_unhookerror);
			return 8;
		  case 4:
			print_program_message(MSG_unhookerrorcritical);
			return 9;
		  default:
			print_program_message(MSG_unknownerror);
			return 10;
		}
	}

	if (installed || (mpx & 0xFF00) == 0) {
		if ((mpx & 0xFF00) == 0 && (s.enable & 1) == 0) {
			/* Found and is disabled. */
			if (statusrequested || disablerequested) {
				print_program_message(MSG_isinstalleddisabled);
			} else {
				if (! asm_enable(mpx)) {
					print_program_message(MSG_enabled);
				} else {
					print_program_message(MSG_cannotenable);
				}
				asm_get_status(mpx, &s);
			}
		} else if ((mpx & 0xFF00) == 0) {
			/* Found and is enabled. */
			if (statusrequested || enablerequested) {
				print_program_message(MSG_isinstalled);
			} else if (disablerequested) {
				if (2 == s.patchstatus) {
					share_installed = MK_FP(FP_SEG(getvect(0x31)), s.patchoffset);
					priorflag = *share_installed;
				}
				if (! asm_disable(mpx)) {
					print_program_message(MSG_disabled);
					if (2 == s.patchstatus) {
						if (*share_installed) {
							*share_installed = 0;
						}
						if (priorflag) {
							print_status_message(MSG_patched);
						}
					}
				} else {
					print_program_message(MSG_cannotdisable);
				}
				asm_get_status(mpx, &s);
			} else {
				if (onlyoptions) {
					print_program_message(MSG_isinstalled);
				} else {
					print_program_message(MSG_alreadyinstalled);
				}
			}
		} else {
			print_program_message(MSG_alreadyinstalled_no_amis);
		}
		if (statusrequested) {
			(void)displaystatus(mpx);
		}
		return 1;
	}

		/* Now try to install. */

	if (onlyoptions || disablerequested || enablerequested) {
		print_program_message(MSG_prefixed_notresident);
		return 11;
	}

	top_of_tsr = init_tables();
	if (top_of_tsr == 0) {
		print_program_message(MSG_outofmemory);
		return 2;
	}

	if (FP_SEG(getvect(MUX_INT_NO)) == 0
		|| FP_OFF(getvect(MUX_INT_NO)) == 0xFFFF) {
		print_program_message(MSG_invalidhandler2f);
		return 5;
	}

	top_of_tsr += 4; // Add 64 bytes for stack
	top_of_stack = (top_of_tsr << 4);

	if (FP_SEG(getvect(0x2D)) == 0
		|| FP_OFF(getvect(0x2D)) == 0xFFFF) {
		print_program_message(MSG_invalidhandler2d);
		return 5;
	}

	/* debug message */
	Tprintf("Keeping %u paragraphs (%u bytes) at %p\n", top_of_tsr, top_of_tsr*16, MK_FP(_psp, 0));
	Tprintf("Int 0x2f at %p\n", MK_FP(_psp+0x10, FP_OFF((void near *)handler2f)));

	/* following code adapted from Ralf Brown's FINDTSR.C
	 * of 1992-09-12, Public Domain */
	{
	  int mpx;
	  /* loop through all 256 multiplex numbers */
	  for (mpx = 0 ; mpx <= 255 ; mpx++)
	  {
	    if (impx_install_check(AMIS_INT_NO, mpx) == 0) /* free ? */
	    {
	      amisnum = mpx;
	      break;
	    }
	  }
	  if (mpx == 256) {
		print_program_message(MSG_nofreeamisnum);
		return 4;
	  }
	}


		/* Hook the interrupt for the handler routine. */
	/* disable(); */
	i2D_next = getvect(0x2D);
#if defined(__WATCOMC__)
//	PRINTF("cs=ds=_psp=%04x : i2D_handler=%04x\n", _psp, FP_OFF((void near *)i2D_handler));
	/* Note: in tiny memory model, _psp == cs == ds, but in exe it's _psp+0x10 *** */
	setvect(0x2D, MK_FP(_psp+0x10, FP_OFF((void near *)i2D_handler)));
#else
	setvect(0x2D, i2D_handler);  /* this causes relocation on TC & OW */
#endif
	old_handler2f = getvect(MUX_INT_NO);
#if (defined(__TURBOC__) && (__TURBOC__ >= 0x0300))
	{
		void (near *isr)() = FP_OFF(handler2f);
		setvect(MUX_INT_NO,(void (interrupt far *)())MK_FP(_DS,isr));
	}
#elif defined(__WATCOMC__)
	setvect(MUX_INT_NO,MK_FP(_psp+0x10, FP_OFF((void near *)handler2f))); 
#else /* causes relocations when built with Turbo C/C++ 3 and OW */
	setvect(MUX_INT_NO,handler2f);
#endif
	/* enable(); */

		/* Let them know we're installed. */
	print_program_message(MSG_installed);
	if (statusrequested) {
		(void)displaystatus(amisnum);
	}

	{ // debug
		installed = (impx_install_check(MUX_INT_NO, MULTIPLEX_ID) & 0xff) == 0xff;
		Tprintf("Share installed status = %i\n", installed);
	}


		/* Any access to environment variables must */
		/* be done prior to this point.  Here we    */
		/* free the environment table to prevent    */
		/* wasting that memory.  In fact, if the    */
		/* TSR were removed from memory and we did  */
		/* not do this, we would not be able to     */
		/* recover this memory.                     */
	free_environment();

	cleanup_messages();

		/* If you run the program with output redirection
		   such as > NUL then the SFT entries are leaked by
		   int 21.31 by default. Like most TSRs we never
		   use the resident's PSP for file operations again
		   so better close all handles so as not to leak. */
	for (i = 0; i < 20; ++i) {
		close(i);	/* clean up file handles */
	}
	
	/* clears file_table and lock_table, then exits leaving resident portion */
	cleartable_stayresident(top_of_tsr); /* size is set to top_of_tsr paragraphs */
	return 0;
}
