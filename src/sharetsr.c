/*
	FreeDOS SHARE - resident portion
	Copyright (c) 2000, 2021 Ronald B. Cemer 
	Dual licensed under the GNU GPL v2 and MIT license (you choose)
	see https://github.com/roncemer/freedos-share
*/


#include "share.h"

typedef unsigned size_t;
void * memclr(void *p, size_t size);  /* #define memclr(p, size) memset(p, 0, size) */
void keep(unsigned retcode, unsigned paras);


/*
#if (__WATCOMC__)
#pragma data_seg ( _TEXT, CODE )
#endif
*/


		/* ------------- TYPEDEFS ------------- */
	/* Register structure for an interrupt function. */
typedef struct  __attribute__((packed, aligned(2))) {
	uint16_t bp;
	uint16_t di;
	uint16_t si;
	uint16_t ds;
	uint16_t es;
	uint16_t dx;
	uint16_t cx;
	uint16_t bx;
	uint16_t ax;
	uint16_t ip;
	uint16_t cs;
	uint16_t flags;
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
#if 1
extern unsigned char open_actions[5][5];
#else
static unsigned char open_actions[5][5] = {
	{ 0, 1, 1, 1, 1 },
	{ 2, 1, 1, 1, 1 },
	{ 2, 1, 1, 1, 1 },
	{ 2, 1, 1, 1, 1 },
	{ 2, 1, 1, 1, 0 },
};
#endif

typedef struct {
	unsigned char first_sharemode;
	unsigned char first_openmode;
	unsigned char current_sharemode;
	unsigned char current_openmode;
	unsigned char action;
} open_action_exception_t;

#if 1
extern open_action_exception_t open_exceptions[18];
#else
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
#endif

//file_t *file_table = NULL;
//lock_t *lock_table = NULL;
extern file_t *file_table;
extern lock_t *lock_table;


#if (__WATCOMC__)
#pragma data_seg ( )
#endif


		/* ------------- PROTOTYPES ------------- */

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

#if defined(__WATCOMC__)
#pragma aux iregs "*"
#pragma aux (__gcc16) inner_handler
#pragma aux (__gcc16) critical_error
#pragma aux need_to_chain "*"
#endif


/* invoke DOS critical error handler */
extern void critical_error(uint16_t error_code, uint16_t drive);


extern int need_to_chain;
extern intregs_t iregs;

void inner_handler(void) {
	need_to_chain = 0; /* by default assume we process request and no need to chain */
	
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
				 to_ulong(iregs.si, iregs.di),
				 to_ulong(iregs.es, iregs.dx),
				 (iregs.ax & 0x01));
			return;
		}
			/* lock_unlock lock (0xa4)*/
			/* lock_unlock unlock (0xa5) */
		if ((iregs.ax & 0xfe) == 0xa4) {
			iregs.ax = lock_unlock
				(iregs.bx,
                 iregs.cx,
				 to_ulong(iregs.si, iregs.di),
				 to_ulong(iregs.es, iregs.dx),
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
	need_to_chain = 1;
}

static void remove_all_locks(int fileno) {
	int i;
	lock_t *lptr;

	for (i = 0; i < lock_table_size; i++) {
		lptr = &lock_table[i];
		if (lptr->used && lptr->fileno == fileno) {
			lptr->used = 0;
			++ lock_table_free;
		}
	}
}

static void free_file_table_entry(int fileno) {
	if ((fileno >= 0) && (fileno < file_table_size)) {
		file_table[fileno].filename[0] = '\0';
		++ file_table_free;
	}
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
	open_action_exception_t *excptr;
	unsigned char current_sharemode = fptr->sharemode;
	unsigned char current_openmode = fptr->openmode;

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
				/* fptr no longer valid after we free fileno entry, so get drive first */
				uint16_t drive = ( (fptr->filename[0]!='\0') && (fptr->filename[1]==':') ) ? fptr->filename[0]-'A' : 0;
				free_file_table_entry(fileno);
				critical_error(
					0x0d,	/* sharing violation */
					drive
				);
				return -0x20;			/* sharing violation */
			}
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
	-- file_table_free;
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

	/* check that fileno is within valid range */
	if (fileno < 0) /* fileno == -1 */
		return 0; /* assume no locks, so ok */
	if (fileno > lock_table_size)
		return -0x6; /* invalid handle */

	/* check for overflow, if so validate only to max len possible */
	if (endofs < ofs) {
		endofs = 0xffffffffL;
		len = endofs-ofs;
	}

	if (len < 1L) return 0;
	
	/* for each entry in lock table, see if matching entry in range
	   requested and return lock violation if match found
	 */
	for (i = 0; i < lock_table_size; i++) {
		lptr = &lock_table[i];
		if (   (lptr->used)
			&& ( (psp == 0) || (lptr->psp != psp) )
			&& (fnmatches(filename, file_table[lptr->fileno].filename))
			&& (   ( (ofs>=lptr->start) && (ofs<lptr->end) )
				|| ( (endofs>lptr->start) && (endofs<=lptr->end) )   )   ) {
			if (allowcriter) {
				free_file_table_entry(fileno);
				critical_error(
					0x0e,	/* lock violation */
					( (fptr->filename[0]!='\0') && (fptr->filename[1]==':') ) ? fptr->filename[0]-'A' : 0
				);
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
				++ lock_table_free;
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
				-- lock_table_free;
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

/* the file and lock table overwrite the transient code, 
   so the last thing we do when loading share is clear the table and
   exit to DOS resident, this chunk of code is not used by 
   resident portion, but placed below table to ensure not
   overwritten by clearing code - other option is to ensure
   placed high enough that max size table won't overwrite.

   paras is # of paragraphs to leave resident
*/
void cleartable_stayresident(unsigned short paras)
{
	/* initial installation, clear memory, but done last so can overwrite all code up to this point */
	memclr(file_table, file_table_size_bytes + lock_table_size_bytes);
	
	/* Terminate and stay resident. */
	keep(0, paras);
}
/* sets memory to 0 */
void * memclr(void *src, size_t size) {
	char *p, *p_end;
	p = src;
	for (p_end=p+size; p < p_end; p++)
		*p = 0;
	return src;
}

