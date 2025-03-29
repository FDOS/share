/*
	FreeDOS SHARE
	Copyright (c) 2000, 2021 Ronald B. Cemer 
	Dual licensed under the GNU GPL v2 and MIT license (you choose)
*/
#ifndef __SHARE_H__
#define __SHARE_H__

#pragma pack (1)  /* structures are packed and default aligned to 1 byte */


#if defined(__TURBOC__) || (__WATCOMC__)
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned size_t;

#define NON_RES_TEXT
#define NON_RES_DATA
#define NON_RES_RODATA
#define NON_RES_BSS

#define __attribute__(x)

#elif defined(__GNUC__)
#define NON_RES_TEXT __attribute__((section(".text.startup")))
#define NON_RES_DATA __attribute__((section(".data.startup")))
#define NON_RES_RODATA __attribute__((section(".rodata.startup")))
#define NON_RES_BSS __attribute__((section(".bss.startup")))

#define far __far

#else
#error "This software must be compiled with TurboC, TurboC++, Open Watcom, or Gcc-ia16"
#endif


#define NULL 0


/* merge an unsigned 32 bit value from its two unsigned 16 bit halves */
#if 0
#define to_ulong(hiword,loword) \
				 (   ((((unsigned long)(hiword))<<16) & 0xffff0000L) |
					 (((unsigned long)(loword)) & 0xffffL)   )
#else
#define to_ulong(hiword,loword) \
				 (   (((unsigned long)(hiword))<<16) + (unsigned short)(loword)   )
#endif

/* convert separate segment:offset 16 bit values into a far pointer (single 32 bit value) */
#define MK_FP(seg,off) ((void far *)to_ulong(seg,off))

#define FP_SEG(p)((unsigned)(((unsigned long)(p)>>16u) & 0xffffuL))
#define FP_OFF(p)((unsigned)(p))


		/* ------------- DEFINES ------------- */
#define AMIS_INT_NO 0x2d
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

#if defined (__WATCOMC__)
/* specify calling convention and extern naming with no underscore */
#pragma aux file_table_size "*"
#pragma aux file_table_size_bytes "*"
#pragma aux file_table_free "*"
//#pragma aux file_table_offset "*"
#pragma aux file_table "*"
#pragma aux lock_table_size "*"
#pragma aux lock_table_size_bytes "*"
#pragma aux lock_table_free "*"
//#pragma aux lock_table_offset "*"
#pragma aux lock_table "*"


/* gcc-ia16 version of __cdecl, similar to OW except don't preceed with _ underscore & doesn't modify __es 
   __caller [] -- all arguments are passed on the stack, caller pops off on return
   __value ... -- return result in __ax
   __modify [__ax __bx __cx __dx] -- may be modified, all other registers unchanged
*/
#pragma aux __gcc16 "*" \
__parm __caller [] \
__value __struct __float __struct __routine [__ax] \
__modify [__ax __bx __cx __dx]

#endif

extern uint16_t file_table_size;	/* # of file_t we can have */
extern uint16_t file_table_size_bytes;	/* amount bytes */
extern uint16_t file_table_free;
extern uint16_t file_table_offset;
extern uint16_t lock_table_size;	/* # of lock_t we can have */
extern uint16_t lock_table_size_bytes;	/* amount bytes */
extern uint16_t lock_table_free;
extern uint16_t lock_table_offset;

extern file_t *file_table;
extern lock_t *lock_table;

void cleartable_stayresident(unsigned short paras);


#define MSG_USAGE_START 0  /* set 0 */

#define MSG_PROGRAM_START 10  /* set 1 */
#define MSG_badparams 0
#define MSG_alreadyinstalled 1
#define MSG_outofmemory 2
#define MSG_invalidhandler2f 3
#define MSG_installed 4
#define MSG_alreadyinstalled_no_amis 5
#define MSG_enabled 6
#define MSG_disabled 7
#define MSG_cannotenable 8
#define MSG_cannotdisable 9
#define MSG_isinstalled 10
#define MSG_isinstalleddisabled 11
#define MSG_nofreeamisnum 12
#define MSG_invalidhandler2d 13
#define MSG_removed 14
#define MSG_notinstalled 15
#define MSG_somefailure 16
#define MSG_unhookerror 17
#define MSG_unhookerrorcritical 18
#define MSG_unknownerror 19
#define MSG_prefixed_notresident 20
#define MSG_failedfreeenv 21

#define MSG_STATUS_START 32  /* set 2 */
#define MSG_notresident 0
#define MSG_patchstatus_notsupported 1
#define MSG_patchstatus_indeterminate 2
#define MSG_patchstatus_needed 3
#define MSG_patchstatus_notneeded 4
#define MSG_patchstatus_unknown 5
#define MSG_patched 6
#define MSG_patched_itself 7
#define MSG_filetable 8
#define MSG_filetable_disabled 9


#endif /* __SHARE_H__ */
