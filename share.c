/*
	FreeDOS SHARE
	Copyright (c) 2000 Ronald B. Cemer under the GNU GPL
	You know the drill.
	If not, see www.gnu.org for details.  Read it, learn it, BE IT.  :-)
*/

/* #include <stdio.h> */ /* (fprintf removed...) */
/* #include <fcntl.h> */ /* Not used, using defines below... */
#include <dos.h>	/* MK_FP, FP_OFF, FP_SEG, int86, intdosx, */
			/* freemem, keep */
#include <string.h>	/* strchr, strlen, memset */

#ifdef __TURBOC__
#include <io.h>		/* write (what else?) */
#include <stdlib.h>	/* _psp, NULL, malloc, free, atol */
#define NON_RES_TEXT
#define NON_RES_DATA
#define NON_RES_RODATA
#define NON_RES_BSS
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;

#elif defined(__GNUC__)
#include <libi86/stdlib.h>
#include <unistd.h>
#define NON_RES_TEXT __attribute__((section(".text.startup")))
#define NON_RES_DATA __attribute__((section(".data.startup")))
#define NON_RES_RODATA __attribute__((section(".rodata.startup")))
#define NON_RES_BSS __attribute__((section(".bss.startup")))
#define far __far
#define getvect(x) _dos_getvect(x)
#define setvect(x, y) _dos_setvect(x, (__libi86_isr_t)y)
#define freemem(x) _dos_freemem(x)
#define keep(x, y) _dos_keep(x, y)

#define atol(s) minimal_atol(s)

#else
#error "This software must be compiled with TurboC, TurboC++ or Gcc-ia16"
#endif

/* Changed by Eric Auer 5/2004: Squeezing executable size a bit -> */
/* Replaced fprint(stderr or stdout,...) by write(hand, buf, size) */
/* Keeps stream stuff and printf stuff outside the file and TSR... */

		/* ------------- DEFINES ------------- */
#define MUX_INT_NO 0x2f
#define MULTIPLEX_ID 0x10

#define FILE_TABLE_MIN 128
#define FILE_TABLE_MAX 62000U

#define LOCK_TABLE_MIN 1
#define LOCK_TABLE_MAX 3800

	/* Valid values for openmode: */
#define OPEN_READ_ONLY   0
#define OPEN_WRITE_ONLY  1
#define OPEN_READ_WRITE  2

	/* Valid values for sharemode: */
#define SHARE_COMPAT     0
#define SHARE_DENY_ALL   1
#define SHARE_DENY_WRITE 2
#define SHARE_DENY_READ  3
#define SHARE_DENY_NONE  4

		/* ------------- TYPEDEFS ------------- */
	/* Register structure for an interrupt function. */
typedef struct {
	unsigned bp;
	unsigned di;
	unsigned si;
	unsigned ds;
	unsigned es;
	unsigned dx;
	unsigned cx;
	unsigned bx;
	unsigned ax;
	unsigned ip;
	unsigned cs;
	unsigned flags;
} intregs_t;

	/* This table determines the action to take when attempting to open
	   a file.  The first array index is the sharing mode of a previous
	   open on the same file.  The second array index is the sharing mode
	   of the current open attempt on the same file.  Action codes are
	   defined as follows:
		   0 = open may proceed
		   1 = open fails with error code 05h
		   2 = open fails and an INT 24h is generated
		   3 = open proceeds if the file is read-only; otherwise fails
			   with error code (used only in exception table below)
		   4 = open proceeds if the file is read-only; otherwise fails
			   with INT 24H (used only in exception table below)
	   Exceptions to the rules are handled in the table
	   below, so this table only covers the general rules.
	*/
static unsigned char open_actions[5][5] = {
	{ 0, 1, 1, 1, 1 },
	{ 2, 1, 1, 1, 1 },
	{ 2, 1, 1, 1, 1 },
	{ 2, 1, 1, 1, 1 },
	{ 2, 1, 1, 1, 0 },
};

typedef struct {
	unsigned char first_sharemode;
	unsigned char first_openmode;
	unsigned char current_sharemode;
	unsigned char current_openmode;
	unsigned char action;
} open_action_exception_t;

static open_action_exception_t open_exceptions[] = {
	{ 0, 0, 2, 0, 3 },
	{ 0, 0, 4, 0, 3 },	/* compatibility-read/deny none-read, MED 08/2004 */
	{ 2, 0, 0, 0, 4 },
	{ 2, 0, 2, 0, 0 },
	{ 2, 0, 4, 0, 0 },
	{ 3, 0, 2, 1, 0 },
	{ 3, 0, 4, 1, 0 },
	{ 3, 1, 4, 1, 0 },
	{ 3, 2, 4, 1, 0 },
	{ 4, 0, 0, 0, 4 },
	{ 4, 0, 0, 1, 0 },	/* deny none-read/compatibility-write */
	{ 4, 0, 0, 2, 0 },	/* deny none-read/compatibility-read+write */
	{ 4, 0, 2, 0, 0 },
	{ 4, 0, 2, 1, 0 },
	{ 4, 0, 2, 2, 0 },
	{ 4, 1, 3, 0, 0 },
	{ 4, 1, 3, 1, 0 },
	{ 4, 1, 3, 2, 0 },
};

	/* One of these exists for each instance of an open file. */
typedef struct {
	char filename[128];		/* fully-qualified filename; "\0" if unused */
	unsigned short psp;		/* PSP of process which opened this file */
	unsigned char openmode;	/* 0=read-only, 1=write-only, 2=read-write */
	unsigned char sharemode;/* SHARE_COMPAT, etc... */
	unsigned char first_openmode;	/* openmode of first open */
	unsigned char first_sharemode;	/* sharemode of first open */
} file_t;

	/* One of these exists for each active lock region. */
typedef struct {
	unsigned char used;		/* Non-zero if this entry is used. */
	unsigned long start;	/* Beginning offset of locked region */
	unsigned long end;		/* Ending offset of locked region */
	unsigned short fileno;	/* file_table entry number */
	unsigned short psp;		/* PSP of process which owns the lock */
} lock_t;


		/* ------------- GLOBALS ------------- */
static char progname[9] NON_RES_BSS;
#if defined(__GNUC__)
extern uint16_t file_table_size;	/* # of file_t we can have */
extern uint16_t file_table_size_bytes;	/* amount bytes */
extern uint16_t file_table_free;
extern uint16_t file_table_offset;
extern uint16_t lock_table_size;	/* # of lock_t we can have */
extern uint16_t lock_table_size_bytes;	/* amount bytes */
extern uint16_t lock_table_free;
extern uint16_t lock_table_offset;
#else
static unsigned int file_table_size_bytes NON_RES_DATA = 2048;
uint16_t file_table_size = 0;		/* # of file_t we can have */
uint16_t lock_table_size = 20;		/* # of lock_t we can have */
#endif
static file_t *file_table = NULL;
static lock_t *lock_table = NULL;


		/* ------------- PROTOTYPES ------------- */
static void usage(void) NON_RES_TEXT;
static void bad_params(void) NON_RES_TEXT;
unsigned short init_tables(void) NON_RES_TEXT;
int main(int argc, char **argv) NON_RES_TEXT;

/* PRINT added by Eric */
#define ERR 2	/* handle of stderr */
#define OUT 1	/* handle of stdout */
static void PRINT(int handle, const char * text) NON_RES_TEXT;
static void PRINT(int handle, const char * text) {
	(void)write (handle, text, strlen(text));
	/* return value is -1 error or N bytes_written. Ignored. */
}


	/* DOS calls this to see if it's okay to open the file.
	   Returns a file_table entry number to use (>= 0) if okay
	   to open.  Otherwise returns < 0 and may generate a critical
	   error.  If < 0 is returned, it is the negated error return
	   code, so DOS simply negates this value and returns it in
	   AX. */
static int open_check
	(char far *filename,/* far pointer to fully qualified filename */
	 unsigned short psp,/* psp segment address of owner process */
	 int openmode,		/* 0=read-only, 1=write-only, 2=read-write */
	 int sharemode);	/* SHARE_COMPAT, etc... */

	/* DOS calls this to record the fact that it has successfully
	   closed a file, or the fact that the open for this file failed. */
static void close_file
	(int fileno);		/* file_table entry number */

	/* DOS calls this to determine whether it can access (read or
	   write) a specific section of a file.  We call it internally
	   from lock_unlock (only when locking) to see if any portion
	   of the requested region is already locked.  If psp is zero,
	   then it matches any psp in the lock table.  Otherwise, only
	   locks which DO NOT belong to psp will be considered.
	   Returns zero if okay to access or lock (no portion of the
	   region is already locked).  Otherwise returns non-zero and
	   generates a critical error (if allowcriter is non-zero).
	   If non-zero is returned, it is the negated return value for
	   the DOS call. */
static int access_check
	(unsigned short psp,/* psp segment address of owner process */
	 int fileno,		/* file_table entry number */
	 unsigned long ofs,	/* offset into file */
	 unsigned long len,	/* length (in bytes) of region to access */
	 int allowcriter);	/* allow a critical error to be generated */

	/* DOS calls this to lock or unlock a specific section of a file.
	   Returns zero if successfully locked or unlocked.  Otherwise
	   returns non-zero.
	   If the return value is non-zero, it is the negated error
	   return code for the DOS 0x5c call. */
static int lock_unlock
	(unsigned short psp,/* psp segment address of owner process */
	 int fileno,		/* file_table entry number */
	 unsigned long ofs,	/* offset into file */
	 unsigned long len,	/* length (in bytes) of region to lock or unlock */
	 int unlock);		/* non-zero to unlock; zero to lock */

static int is_file_open(char far *filename);

	/* Multiplex interrupt handler */

#if defined(__TURBOC__)
static void interrupt far (*old_handler2f)() = NULL;


		/* ------------- HOOK ------------- */
static void interrupt far handler2f(intregs_t iregs) {

#define chain_old_handler2f { \
	_BX = iregs.bx;			/* Restore BX */ \
	_CX = iregs.ax;			/* Save original AX contents into CX */ \
	iregs.ax = FP_SEG((void far *)old_handler2f);	/* Set chain segment */ \
	iregs.bx = FP_OFF((void far *)old_handler2f);	/* Set chain offset */ \
	_AX = _CX;				/* Restore AX */ \
	__emit__(0x5D);			/* POP BP */ \
	__emit__(0x5F);			/* POP DI */ \
	__emit__(0x5E);			/* POP SI */ \
	__emit__(0x1F);			/* POP DS */ \
	__emit__(0x07);			/* POP ES */ \
	__emit__(0x5A);			/* POP DX */ \
	__emit__(0x59);			/* POP CX */ \
	__emit__(0xCB);			/* RETF */ \
}	/* This evil trick probably only works with Turbo C!?! */
	/* would have been better to link a NASM handler core: */
	/* nasm -fobj -o foo.obj foo.asm ... */

#elif defined(__GNUC__)
/* Within IBM Interrupt Sharing Protocol header */
extern void __far __interrupt (*i2D_next)(void);
/* Prototype for NASM interrupt handler function */
extern void __far __interrupt __attribute__((near_section)) i2D_handler(void);
extern uint8_t amisnum;
extern uint16_t asm_find_resident(void) NON_RES_TEXT;
extern uint16_t asm_uninstall(uint16_t mpx) NON_RES_TEXT;
extern uint16_t asm_enable(uint16_t mpx) NON_RES_TEXT;
extern uint16_t asm_disable(uint16_t mpx) NON_RES_TEXT;
extern uint16_t asm_init(void) NON_RES_TEXT;

/* Within IBM Interrupt Sharing Protocol header */
extern void __far __interrupt (*old_handler2f)(void);

/* Prototype for NASM wrapper function */
extern void __far __interrupt __attribute__((near_section)) handler2f(void);

extern int need_to_chain;
extern uint16_t top_of_stack;
extern intregs_t iregs;
void inner_handler(void) {
	need_to_chain = 0;

#endif

	if (((iregs.ax >> 8) & 0xff) == MULTIPLEX_ID) {
		if ((iregs.ax & 0xff) == 0) {
				/* Installation check.  Return 0xff in AL. */
			iregs.ax |= 0xff;
			return;
		}
			/* These subfuctions are nonstandard, but are highly
			   unlikely to be used by another multiplex TSR, since
			   our multiplex Id (0x10) is basically reserved for
			   SHARE.  So we should be able to get away with using
			   these for our own purposes. */
			/* open_check */
		if ((iregs.ax & 0xff) == 0xa0) {
			iregs.ax = open_check
				(MK_FP(iregs.ds, iregs.si),
				 iregs.bx,
				 iregs.cx,
				 iregs.dx);
			return;
		}
			/* close_file */
		if ((iregs.ax & 0xff) == 0xa1) {
			close_file(iregs.bx);
			return;
		}
			/* access_check (0xa2) */
			/* access_check with critical error (0xa3) */
		if ((iregs.ax & 0xfe) == 0xa2) {
			iregs.ax = access_check
				(iregs.bx,
                 iregs.cx,
#if 0
				 (   ((((unsigned long)iregs.si)<<16) & 0xffff0000L) |
					 (((unsigned long)iregs.di) & 0xffffL)   ),
				 (   ((((unsigned long)iregs.es)<<16) & 0xffff0000L) |
                     (((unsigned long)iregs.dx) & 0xffffL)   ),
#else
				 (   (((unsigned long)iregs.si)<<16) + iregs.di   ),
				 (   (((unsigned long)iregs.es)<<16) + iregs.dx   ),
#endif
				 (iregs.ax & 0x01));
			return;
		}
			/* lock_unlock lock (0xa4)*/
			/* lock_unlock unlock (0xa5) */
		if ((iregs.ax & 0xfe) == 0xa4) {
			iregs.ax = lock_unlock
				(iregs.bx,
                 iregs.cx,
#if 0
				 (   ((((unsigned long)iregs.si)<<16) & 0xffff0000L) |
  				 (((unsigned long)iregs.di) & 0xffffL)   ),
				 (   ((((unsigned long)iregs.es)<<16) & 0xffff0000L) |
                     (((unsigned long)iregs.dx) & 0xffffL)   ),
#else
				 (   (((unsigned long)iregs.si)<<16) | ((unsigned long)iregs.di)   ),
				 (   (((unsigned long)iregs.es)<<16) | ((unsigned long)iregs.dx)   ),
#endif
				 (iregs.ax & 0x01));
			return;
		}

			/* is_file_open (0xa6)*/
		if ((iregs.ax & 0xff) == 0xa6) {
			iregs.ax = is_file_open(MK_FP(iregs.ds, iregs.si));
			return;
		}
	}
		/* Chain to the next handler. */
#if defined(__TURBOC__)
	chain_old_handler2f;
#elif defined(__GNUC__)
	need_to_chain = 1;
#endif
}

static void remove_all_locks(int fileno) {
	int i;
	lock_t *lptr;

	for (i = 0; i < lock_table_size; i++) {
		lptr = &lock_table[i];
		if (lptr->used && lptr->fileno == fileno) {
			lptr->used = 0;
#if defined(__GNUC__)
			++ lock_table_free;
#endif
		}
	}
}

static void free_file_table_entry(int fileno) {
	file_table[fileno].filename[0] = '\0';
#if defined(__GNUC__)
	++ file_table_free;
#endif
}

/* DOS 7 does not have read-only restrictions, MED 08/2004 */
/*
static int file_is_read_only(char far *filename) {
	union REGS regs;
	struct SREGS sregs;

	regs.x.ax = 0x4300;
	sregs.ds = FP_SEG(filename);
	regs.x.dx = FP_OFF(filename);
	intdosx(&regs, &regs, &sregs);
	if (regs.x.cflag) return 0;
	return ((regs.h.cl & 0x19) == 0x01);
}
*/

static int fnmatches(char far *fn1, char far *fn2) {
	while (*fn1) {
		if (*fn1 != *fn2) return 0;
		fn1++;
		fn2++;
	}
	return (*fn1 == *fn2);
}

static int do_open_check
	(int fileno) {		/* file_table entry number */
	file_t *p, *fptr = &file_table[fileno];
	int i, j, action = 0, foundexc;
	unsigned char current_sharemode = fptr->sharemode;
	unsigned char current_openmode = fptr->openmode;
	open_action_exception_t *excptr;

	fptr->first_sharemode = fptr->sharemode;
	fptr->first_openmode = fptr->openmode;
	for (i = 0; i < file_table_size; i++) {
		if (i == fileno) continue;
		p = &file_table[i];
		if (p->filename[0] == '\0') continue;
		if (!fnmatches(p->filename, fptr->filename)) continue;
		fptr->first_sharemode = p->first_sharemode;
		fptr->first_openmode = p->first_openmode;
			/* Look for exceptions to the general rules first. */
		foundexc = 0;
		for (j = 0;
			 j < (sizeof(open_exceptions)/sizeof(open_action_exception_t));
			 j++) {
			excptr = &open_exceptions[j];
			if (   (excptr->first_sharemode == fptr->first_sharemode)
				&& (excptr->current_sharemode == current_sharemode)
				&& (excptr->first_openmode == fptr->first_openmode)
				&& (excptr->current_openmode == current_openmode)  ) {
				foundexc = 1;
				action = excptr->action;
				break;
			}
		}
			/* If no exception to rules, use normal rules. */
		if (!foundexc)
			action = open_actions[fptr->first_sharemode][current_sharemode];
			/* Fail appropriately based on action. */
		switch (action) {

/* DOS 7 does not have read-only restrictions, fall through to proceed, MED 08/2004 */
		case 3:
		case 4:

		case 0:		/* proceed with open */
			break;
/*		case 3:	*/		/* succeed if file read-only, else fail with error 05h */
/*			if (file_is_read_only(fptr->filename)) break;	*/
		case 1:		/* fail with error code 05h */
			free_file_table_entry(fileno);
			return -5;
/*		case 4:	*/		/* succeed if file read-only, else fail with int 24h */
/*			if (file_is_read_only(fptr->filename)) break;	*/
		case 2:		/* fail with int 24h */
			{
				union REGS regs;

				regs.h.ah = 0x0e;	/* disk I/O; fail allowed; data area */
				regs.h.al = 0;
				regs.x.di = 0x0d;	/* sharing violation */
				if ( (fptr->filename[0]!='\0') && (fptr->filename[1]==':') )
					regs.h.al = fptr->filename[0]-'A';
				free_file_table_entry(fileno);
				int86(0x24, &regs, &regs);
			}
			return -0x20;			/* sharing violation */
		}
		break;
	}
	return fileno;
}

	/* DOS calls this to see if it's okay to open the file.
	   Returns a file_table entry number to use (>= 0) if okay
	   to open.  Otherwise returns < 0 and may generate a critical
	   error.  If < 0 is returned, it is the negated error return
	   code, so DOS simply negates this value and returns it in
	   AX. */
static int open_check
	(char far *filename,/* far pointer to fully qualified filename */
	 unsigned short psp,/* psp segment address of owner process */
	 int openmode,		/* 0=read-only, 1=write-only, 2=read-write */
	 int sharemode) {	/* SHARE_COMPAT, etc... */

	int i, fileno = -1;
	file_t *fptr;

		/* Whack off unused bits in the share mode
		   in case we were careless elsewhere. */
	sharemode &= 0x07;

		/* Assume compatibility mode if invalid share mode. */
/* ??? IS THIS CORRECT ??? */
	if ( (sharemode < SHARE_COMPAT) || (sharemode > SHARE_DENY_NONE) )
		sharemode = SHARE_COMPAT;

		/* Whack off unused bits in the open mode
		   in case we were careless elsewhere. */
	openmode &= 0x03;

		/* Assume read-only mode if invalid open mode. */
/* ??? IS THIS CORRECT ??? */
	if ( (openmode < OPEN_READ_ONLY) || (openmode > OPEN_READ_WRITE) )
		openmode = OPEN_READ_ONLY;

	for (i = 0; i < file_table_size; i++) {
		if (file_table[i].filename[0] == '\0') {
			fileno = i;
			break;
		}
	}
	if (fileno == -1) return -1;
	fptr = &file_table[fileno];

		/* Copy the filename into ftpr->filename. */
	for (i = 0; i < sizeof(fptr->filename); i++) {
		if ((fptr->filename[i] = filename[i]) == '\0') break;
	}
#if defined(__GNUC__)
	-- file_table_free;
#endif
	fptr->psp = psp;
	fptr->openmode = (unsigned char)openmode;
	fptr->sharemode = (unsigned char)sharemode;
		/* Do the sharing check and return fileno if
		   okay, or < 0 (and free the entry) if error. */
	return do_open_check(fileno);
}

	/* DOS calls this to record the fact that it has successfully
	   closed a file, or the fact that the open for this file failed. */
static void close_file
	(int fileno) {		/* file_table entry number */

	remove_all_locks(fileno);
	free_file_table_entry(fileno);
}

	/* DOS calls this to determine whether it can access (read or
	   write) a specific section of a file.  We call it internally
	   from lock_unlock (only when locking) to see if any portion
	   of the requested region is already locked.  If psp is zero,
	   then it matches any psp in the lock table.  Otherwise, only
	   locks which DO NOT belong to psp will be considered.
	   Returns zero if okay to access or lock (no portion of the
	   region is already locked).  Otherwise returns non-zero and
	   generates a critical error (if allowcriter is non-zero).
	   If non-zero is returned, it is the negated return value for
	   the DOS call. */
static int access_check
	(unsigned short psp,/* psp segment address of owner process */
	 int fileno,		/* file_table entry number */
	 unsigned long ofs,	/* offset into file */
	 unsigned long len,	/* length (in bytes) of region to access */
	 int allowcriter) {	/* allow a critical error to be generated */
	int i;
	file_t *fptr = &file_table[fileno];
	char far *filename = fptr->filename;
	lock_t *lptr;
	unsigned long endofs = ofs + len;

	if (endofs < ofs) {
		endofs = 0xffffffffL;
		len = endofs-ofs;
	}

	if (len < 1L) return 0;

	for (i = 0; i < lock_table_size; i++) {
		lptr = &lock_table[i];
		if (   (lptr->used)
			&& ( (psp == 0) || (lptr->psp != psp) )
			&& (fnmatches(filename, file_table[lptr->fileno].filename))
			&& (   ( (ofs>=lptr->start) && (ofs<lptr->end) )
				|| ( (endofs>lptr->start) && (endofs<=lptr->end) )   )   ) {
			if (allowcriter) {
				union REGS regs;

				regs.h.ah = 0x0e;	/* disk I/O; fail allowed; data area */
				regs.h.al = 0;
				regs.x.di = 0x0e;	/* lock violation */
				if ( (fptr->filename[0]!='\0') && (fptr->filename[1]==':') )
					regs.h.al = fptr->filename[0]-'A';
				free_file_table_entry(fileno);
				int86(0x24, &regs, &regs);
			}
			return -0x21;			/* lock violation */
		}
	}
	return 0;
}

	/* DOS calls this to lock or unlock a specific section of a file.
	   Returns zero if successfully locked or unlocked.  Otherwise
	   returns non-zero.
	   If the return value is non-zero, it is the negated error
	   return code for the DOS 0x5c call. */
static int lock_unlock
	(unsigned short psp,/* psp segment address of owner process */
	 int fileno,		/* file_table entry number */
	 unsigned long ofs,	/* offset into file */
	 unsigned long len,	/* length (in bytes) of region to lock or unlock */
	 int unlock) {		/* non-zero to unlock; zero to lock */

	int i;
	lock_t *lptr;
	unsigned long endofs = ofs + len;

    if (endofs < ofs) {
		endofs = 0xffffffffL;
		len = endofs-ofs;
	}

	if (len < 1L) return 0;

    /* there was a error in the code below preventing any other
     than the first locked region to be unlocked (japheth, 09/2005) */

	if (unlock) {
		for (i = 0; i < lock_table_size; i++) {
			lptr = &lock_table[i];
			if (   (lptr->used)
				&& (lptr->psp == psp)
				&& (lptr->fileno == fileno)
				&& (lptr->start == ofs)
				&& (lptr->end == endofs)   ) {
				lptr->used = 0;
#if defined(__GNUC__)
				++ lock_table_free;
#endif
				return 0;
			}
		}
			/* Not already locked by us; can't unlock. */
		return -(0x21);		/* lock violation */
	} else {
		if (access_check(0, fileno, ofs, len, 0)) {
				/* Already locked; can't lock. */
			return -(0x21);		/* lock violation */
		}
		for (i = 0; i < lock_table_size; i++) {
			lptr = &lock_table[i];
			if (!lptr->used) {
				lptr->used = 1;
				lptr->start = ofs;
				lptr->end = ofs+(unsigned long)len;
				lptr->fileno = fileno;
				lptr->psp = psp;
#if defined(__GNUC__)
				-- lock_table_free;
#endif
				return 0;
			}
		}
		return -(0x24);		/* sharing buffer overflow */
	}
}

static int is_file_open(char far *filename)
{
	int i;

	for (i = 0; i < file_table_size; i++) {
		if (fnmatches(filename, file_table[i].filename))
			return 1;
	}
	return 0;
}

		/* ------------- INIT ------------- */
	/* Allocate tables.
	 * If we run out of memory return zero, otherwise return size
	 * in paragraphs from PSP to end of last table */

unsigned short init_tables(void) {
	unsigned short paras;
	char far *fptr;
#if defined(__TURBOC__)
	char *onebyte;

	file_table_size = file_table_size_bytes / sizeof(file_t);
	if ((file_table = malloc(file_table_size_bytes)) == NULL)
		return 0;
	memset(file_table, 0, file_table_size_bytes);

	if ((lock_table = malloc(lock_table_size * sizeof(lock_t))) == NULL) {
		free(file_table);
		file_table = NULL;
		return 0;
	}
	memset(lock_table, 0, lock_table_size * sizeof(lock_t));

	/* Allocate a single byte.  This tells us the size of the TSR.
	   Free the byte when we know the address. */
	onebyte = malloc(1);
	if (onebyte == NULL) {
		free(file_table);
		file_table = NULL;
		free(lock_table);
		lock_table = NULL;
		return 0;
	}
	fptr = (char far *)onebyte;
	free(onebyte);

#else /* GNUC */
	char *p;

	file_table_size = file_table_size_bytes / sizeof(file_t);
	lock_table_size_bytes = lock_table_size * sizeof(lock_t);
	file_table_free = file_table_size;
	lock_table_free = lock_table_size;

	p = sbrk(file_table_size_bytes + lock_table_size_bytes);
	if (p == (void *)-1)
		return 0;

	// No need to memset() as sbrk() does it for us

	file_table = (void *)p;
	lock_table = (void *)(p + file_table_size_bytes);
	file_table_offset = (uint16_t)file_table;
	lock_table_offset = (uint16_t)lock_table;

	fptr = (char far *)sbrk(0);
#endif

	paras = (FP_SEG(fptr)+((FP_OFF(fptr)+15) >> 4)) - _psp;
				/* resident paras, counting from PSP:0 */
	return paras;
}

static const char msg_usage1[] NON_RES_RODATA = "Installs file-sharing and locking "
			"capabilities on your hard disk.\r\n\r\n";
static const char msg_usage2[] NON_RES_RODATA = " [/F:space] [/L:locks]"
#if defined(__GNUC__)
		 " [/U] [/S] [/O] [/D] [/E]"
#endif
		 "\r\n\r\n"
		 "  /F:space   Allocates file space (in bytes) "
			"for file-sharing information.\r\n"
		 "  /L:locks   Sets the number of files that can "
			"be locked at one time.\r\n"
#if defined(__GNUC__)
		 "  /U         Uninstall a resident instance.\r\n"
		 "  /S         Show patch status and table sizes.\r\n"
		 "  /O         Only operate if already resident, do not install.\r\n"
		 "  /D         Disable a resident instance.\r\n"
		 "  /E         Enable a resident instance. (Default.)\r\n"
#endif
		 ;

static const char msg_badparams[] NON_RES_RODATA = ": parameter out of range!\r\n";
static const char msg_alreadyinstalled[] NON_RES_RODATA = " is already installed!\r\n";
static const char msg_outofmemory[] NON_RES_RODATA = ": out of memory!\r\n";
static const char msg_invalidhandler2f[] NON_RES_RODATA = ": invalid interrupt 2Fh handler!\r\n";
static const char msg_installed[] NON_RES_RODATA = " installed.\r\n";

#if defined(__GNUC__)
static const char msg_alreadyinstalled_no_amis[] NON_RES_RODATA = " is already installed, but not found on AMIS interrupt!\r\n";
static const char msg_enabled[] NON_RES_RODATA = " enabled.\r\n";
static const char msg_disabled[] NON_RES_RODATA = " disabled.\r\n";
static const char msg_cannotenable[] NON_RES_RODATA = " cannot be enabled, check TSR version.\r\n";
static const char msg_cannotdisable[] NON_RES_RODATA = " cannot be disabled, check TSR version.\r\n";
static const char msg_isinstalled[] NON_RES_RODATA = " is installed resident.\r\n";
static const char msg_isinstalleddisabled[] NON_RES_RODATA = " is installed resident, but currently disabled.\r\n";
static const char msg_nofreeamisnum[] NON_RES_RODATA = ": no free AMIS multiplex number!\r\n";
static const char msg_invalidhandler2d[] NON_RES_RODATA = ": invalid interrupt 2Dh handler!\r\n";
static const char msg_removed[] NON_RES_RODATA = " removed.\r\n";
static const char msg_cannotremove[] NON_RES_RODATA = ": cannot remove, ";
static const char msg_notinstalled[] NON_RES_RODATA = "not yet installed.\r\n";
static const char msg_somefailure[] NON_RES_RODATA = "some failure.\r\n";
static const char msg_unhookerror[] NON_RES_RODATA = "handlers hooked AMIS-incompatible.\r\n";
static const char msg_unhookerrorcritical[] NON_RES_RODATA = "internal unhook error.\r\n";
static const char msg_unknownerror[] NON_RES_RODATA = "unknown error.\r\n";
static const char msg_notresident[] NON_RES_RODATA = "Program is not resident!\r\n";
static const char msg_patchstatus[] NON_RES_RODATA = "Patch status: ";
static const char msg_patchstatus_notsupported[] NON_RES_RODATA = "not supported by TSR.\r\n";
static const char msg_patchstatus_indeterminate[] NON_RES_RODATA = "indeterminate.\r\n";
static const char msg_patchstatus_needed[] NON_RES_RODATA = "needed, flag at ";
static const char msg_patchstatus_needed_2[] NON_RES_RODATA = "h:";
static const char msg_patchstatus_needed_3[] NON_RES_RODATA = "h.\r\n";
static const char msg_patchstatus_notneeded[] NON_RES_RODATA = "not needed.\r\n";
static const char msg_patchstatus_unknown[] NON_RES_RODATA = "unknown.\r\n";
static const char msg_patched[] NON_RES_RODATA = "Patched the share_installed byte of old FreeDOS kernel to zero.\r\n";
static const char msg_patched_itself[] NON_RES_RODATA = "Resident patched the share_installed byte of old FreeDOS kernel to zero.\r\n";
static const char msg_prefixed_notresident[] NON_RES_RODATA = ": Program is not resident!\r\n";
static const char msg_filetable[] NON_RES_RODATA = "File table: ";
static const char msg_free[] NON_RES_RODATA = " free / ";
static const char msg_total_locktable[] NON_RES_RODATA = " total, lock table: ";
static const char msg_total_eol[] NON_RES_RODATA = " total\r\n";
#endif

static void usage(void) {
	PRINT(ERR, msg_usage1);
	PRINT(ERR, progname);
	PRINT(ERR, msg_usage2);
}

static void bad_params(void) {
	PRINT(ERR, progname);
	PRINT(ERR, msg_badparams);
}

#if defined(__GNUC__)
/* Naive implementation of atol(), only decimal digits allowed, no signs */
static long minimal_atol(const char *s) NON_RES_TEXT;
static long minimal_atol(const char *s) {
	long val;
	const char *p;

	for (val = 0, p = s; *p; p++) {
		if (*p == ' ')
			continue;
		if (*p < '0' || *p > '9')
			break;
		val *= 10;
		val += *p - '0';
	}

	return val;
}

void displaynumber(int handle, uint16_t number) NON_RES_TEXT;
void displaynumber(int handle, uint16_t number) {
	char buffer[6];
	int index = 5;
	buffer[index--] = 0;
	do {
		buffer[index--] = (number % 10) + '0';
		number /= 10;
	} while (number);
	PRINT(handle, &buffer[index + 1]);
}

void displayhexnumber(int handle, uint16_t number, uint8_t mindigits) NON_RES_TEXT;
void displayhexnumber(int handle, uint16_t number, uint8_t mindigits) {
	char buffer[6];
	char hexit;
	int index = 5;
	buffer[index--] = 0;
	if (mindigits > 4)
		mindigits = 4;
	do {
		hexit = (number % 16);
		if (hexit < 10)
			buffer[index--] = hexit + '0';
		else
			buffer[index--] = hexit - 10 + 'A';
		number /= 16;
		if (mindigits)
			-- mindigits;
	} while (number || mindigits);
	PRINT(handle, &buffer[index + 1]);
}

typedef struct {
	uint16_t patchoffset;
	uint16_t filesize;
	uint16_t filefree;
	uint16_t locksize;
	uint16_t lockfree;
	uint8_t patchstatus;
	uint8_t enable;
} status_struct;

extern void asm_get_status(uint16_t mpx, status_struct * s) NON_RES_TEXT;

int displaystatus(uint16_t mpx) NON_RES_TEXT;
int displaystatus(uint16_t mpx) {
	status_struct s;
	if (mpx == 0xFFFF) {
		mpx = asm_find_resident();
	}
	if (mpx & 0xFF00) {
		PRINT(OUT, msg_notresident);
		return 1;
	}
	asm_get_status(mpx, &s);
	PRINT(OUT, msg_patchstatus);
	switch (s.patchstatus) {
	  case 0:
		PRINT(OUT, msg_patchstatus_notsupported);
		break;
	  case 1:
		PRINT(OUT, msg_patchstatus_indeterminate);
		break;
	  case 2:
		PRINT(OUT, msg_patchstatus_needed);
		displayhexnumber(OUT, FP_SEG(getvect(0x31)), 4);
		PRINT(OUT, msg_patchstatus_needed_2);
		displayhexnumber(OUT, s.patchoffset, 4);
		PRINT(OUT, msg_patchstatus_needed_3);
		break;
	  case 3:
		PRINT(OUT, msg_patchstatus_notneeded);
		break;
	  default:
		PRINT(OUT, msg_patchstatus_unknown);
		break;
	}
	PRINT(OUT, msg_filetable);
	if (s.enable & 1) {
		displaynumber(OUT, s.filefree);
		PRINT(OUT, msg_free);
	}
	displaynumber(OUT, s.filesize);
	PRINT(OUT, msg_total_locktable);
	if (s.enable & 1) {
		displaynumber(OUT, s.lockfree);
		PRINT(OUT, msg_free);
	}
	displaynumber(OUT, s.locksize);
	PRINT(OUT, msg_total_eol);
	return 0;
}
#endif

		/* ------------- MAIN ------------- */
int main(int argc, char **argv) {
	unsigned short far *usfptr;
	unsigned short top_of_tsr;
	int installed = 0;
#if defined(__GNUC__)
	status_struct s;
	uint8_t far * share_installed = NULL;
	uint8_t priorflag = 0;
	uint16_t rc;
	uint16_t mpx;
	int uninstallrequested = 0, statusrequested = 0, onlyoptions = 0;
	int enablerequested = 0, disablerequested = 0;
#endif
	int i;
	uint8_t ii;

#if defined(__GNUC__)
	file_table_size_bytes = 2048;
#endif

		/* Extract program name from argv[0] into progname. */
	if (argv[0] != NULL) {
		char *p = argv[0], *p2, c;
		int i;
		if ( (p[0] != '\0') && (p[1] == ':') )
			p += 2;
		while ((p2 = strchr(p, '\\')) != NULL)
			p = p2+1;
		p2 = progname;
		for (i = 0; i < 8; i++) {
			c = p[i];
			if ( (c == '.') || (c == '\0') )
				break;
			*(p2++) = c;
		}
		*p2 = '\0';
	}

		/* See if the TSR is already installed. */
	/* disable(); */	/* no multitasking, so don't worry */
	if (FP_SEG(getvect(MUX_INT_NO)) != 0
		&& FP_OFF(getvect(MUX_INT_NO)) != 0xFFFF) {
		union REGS regs;
		/* enable(); */
		regs.h.ah = MULTIPLEX_ID;
		regs.h.al = 0;
		int86(MUX_INT_NO,&regs,&regs);
		installed = ((regs.x.ax & 0xff) == 0xff);

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
#if defined(__GNUC__)
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
#endif
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

#if defined(__GNUC__)
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
			PRINT(OUT, progname);
			PRINT(OUT, msg_removed);
			if (2 == s.patchstatus) {
				if (*share_installed) {
					*share_installed = 0;
					PRINT(OUT, msg_patched);
				} else if (priorflag) {
					PRINT(OUT, msg_patched_itself);
				}
			}
			return 0;
		}
		PRINT(ERR, progname);
		PRINT(ERR, msg_cannotremove);
		switch (rc) {
		  case 1:
			PRINT(ERR, msg_notinstalled);
			return 6;
		  case 2:
			PRINT(ERR, msg_somefailure);
			return 7;
		  case 3:
			PRINT(ERR, msg_unhookerror);
			return 8;
		  case 4:
			PRINT(ERR, msg_unhookerrorcritical);
			return 9;
		  default:
			PRINT(ERR, msg_unknownerror);
			return 10;
		}
	}

	if (installed || (mpx & 0xFF00) == 0) {
		if ((mpx & 0xFF00) == 0 && (s.enable & 1) == 0) {
			/* Found and is disabled. */
			if (statusrequested || disablerequested) {
				PRINT(OUT, progname);
				PRINT(OUT, msg_isinstalleddisabled);
			} else {
				if (! asm_enable(mpx)) {
					PRINT(OUT, progname);
					PRINT(OUT, msg_enabled);
				} else {
					PRINT(ERR, progname);
					PRINT(ERR, msg_cannotenable);
				}
				asm_get_status(mpx, &s);
			}
		} else if ((mpx & 0xFF00) == 0) {
			/* Found and is enabled. */
			if (statusrequested || enablerequested) {
				PRINT(OUT, progname);
				PRINT(OUT, msg_isinstalled);
			} else if (disablerequested) {
				if (2 == s.patchstatus) {
					share_installed = MK_FP(FP_SEG(getvect(0x31)), s.patchoffset);
					priorflag = *share_installed;
				}
				if (! asm_disable(mpx)) {
					PRINT(OUT, progname);
					PRINT(OUT, msg_disabled);
					if (2 == s.patchstatus) {
						if (*share_installed) {
							*share_installed = 0;
						}
						if (priorflag) {
							PRINT(OUT, msg_patched);
						}
					}
				} else {
					PRINT(ERR, progname);
					PRINT(ERR, msg_cannotdisable);
				}
				asm_get_status(mpx, &s);
			} else {
				if (onlyoptions) {
					PRINT(OUT, progname);
					PRINT(OUT, msg_isinstalled);
				} else {
					PRINT(ERR, progname);
					PRINT(ERR, msg_alreadyinstalled);
				}
			}
		} else {
			PRINT(ERR, progname);
			PRINT(ERR, msg_alreadyinstalled_no_amis);
		}
		if (statusrequested) {
			(void)displaystatus(mpx);
		}
		return 1;
	}
#else
	if (installed) {
		PRINT(ERR, progname);
		PRINT(ERR, msg_alreadyinstalled);
		return 1;
	}

		/* Now try to install. */
#endif

#if defined(__GNUC__)
	if (onlyoptions || disablerequested || enablerequested) {
		PRINT(ERR, progname);
		PRINT(ERR, msg_prefixed_notresident);
		return 11;
	}
#endif

	top_of_tsr = init_tables();
	if (top_of_tsr == 0) {
		PRINT(ERR, progname);
		PRINT(ERR, msg_outofmemory);
		return 2;
	}

	if (FP_SEG(getvect(MUX_INT_NO)) == 0
		|| FP_OFF(getvect(MUX_INT_NO)) == 0xFFFF) {
		PRINT(ERR, progname);
		PRINT(ERR, msg_invalidhandler2f);
		return 5;
	}

#if defined(__GNUC__)
	top_of_tsr += 4; // Add 64 bytes for stack
	top_of_stack = (top_of_tsr << 4);

	if (FP_SEG(getvect(0x2D)) == 0
		|| FP_OFF(getvect(0x2D)) == 0xFFFF) {
		PRINT(ERR, progname);
		PRINT(ERR, msg_invalidhandler2d);
		return 5;
	}

	/* following code adapted from Ralf Brown's FINDTSR.C
	 * of 1992-09-12, Public Domain */
	{
	  int mpx;
	  union REGS regs;
	  /* loop through all 256 multiplex numbers */
	  for (mpx = 0 ; mpx <= 255 ; mpx++)
	  {
	    regs.h.ah = mpx ;
	    regs.h.al = 0 ;  /* installation check */
	    int86(0x2D,&regs,&regs) ;
	    if (regs.h.al == 0) /* free ? */
	    {
	      amisnum = mpx;
	      break;
	    }
	  }
	  if (mpx == 256) {
		PRINT(ERR, progname);
		PRINT(ERR, msg_nofreeamisnum);
		return 4;
	  }
	}


		/* Hook the interrupt for the handler routine. */
	/* disable(); */
	i2D_next = getvect(0x2D);
	setvect(0x2D, i2D_handler);
#endif
	old_handler2f = getvect(MUX_INT_NO);
	setvect(MUX_INT_NO,handler2f);
	/* enable(); */

		/* Let them know we're installed. */
	PRINT(OUT, progname);
	PRINT(OUT, msg_installed);
#if defined(__GNUC__)
	if (statusrequested) {
		(void)displaystatus(amisnum);
	}
#endif

		/* Any access to environment variables must */
		/* be done prior to this point.  Here we    */
		/* free the environment table to prevent    */
		/* wasting that memory.  In fact, if the    */
		/* TSR were removed from memory and we did  */
		/* not do this, we would not be able to     */
		/* recover this memory.                     */

	usfptr = MK_FP(_psp, 0x2c);	/* MK_FP is the counterpart */
					/* of FP_OFF and FP_SEG ... */
	freemem(*usfptr);	/* deallocate MCB of ENV segment */

		/* If you run the program with output redirection
		   such as > NUL then the SFT entries are leaked by
		   int 21.31 by default. Like most TSRs we never
		   use the resident's PSP for file operations again
		   so better close all handles so as not to leak. */
	for (ii = 0; ii < 20; ++ii) {
		close(ii);	/* clean up file handles */
	}

		/* Terminate and stay resident. */
	keep(0,top_of_tsr);	/* size is set to top_of_tsr paragraphs */
	return 0;
}
