/*
	FreeDOS SHARE - non-resident messages (output)
	Copyright (c) 2000, 2021 Ronald B. Cemer 
	Dual licensed under the GNU GPL v2 and MIT license (you choose)
*/

#include "share.h"

#if defined(__WATCOMC__)
//#pragma data_seg ( _DATA_startup, TDATA )
//#pragma code_seg ( _TEXT_startup, TCODE )
#endif

#include <stdarg.h>
#include "tnyprntf.h"
//extern int PRINTF(char *s, ...);

#if 0
#include "kitten.h"
#else
	typedef int nl_catd;
	#define catopen(name, x) 0
	#define catgets(cat, set, msg, str) str
	#define catclose(cat)
#endif

char * strchr(const char *str, int c);


		/* ------------- PROTOTYPES ------------- */
void usage(void) NON_RES_TEXT;
void bad_params(void) NON_RES_TEXT;
void print_program_message(unsigned msgno) NON_RES_TEXT;
void print_status_message(unsigned msgno, ...) NON_RES_TEXT;
void init_messages(char *argv0) NON_RES_TEXT;
void cleanup_messages(void) NON_RES_TEXT;

		/* ------------- GLOBALS ------------- */

static char progname[9] NON_RES_BSS;

static nl_catd cat NON_RES_BSS;    /* handle for localized messages (catalog) */


/* help, status, and error messages, see cats for translations */
/* NOTE: order here matters, update defines in share.h if changes!!! */
#if 1 //defined(__WATCOMC__)
#pragma aux messages "*"
extern const char *messages[] NON_RES_RODATA;
#else
static const char *messages[] NON_RES_RODATA = {
/* usage messages, set 0 */
	"%s [/F:space] [/L:locks]\n",
	"%s [/F:space] [/L:locks] [/U] [/S] [/O] [/D] [/E]\n",
	"  /F:space   Allocates file space (in bytes) for file-sharing information.\n",
	"  /L:locks   Sets the number of files that can be locked at one time.\n",
	"  /U         Uninstall a resident instance.\n",
	"  /S         Show patch status and table sizes.\n",
	"  /O         Only operate if already resident, do not install.\n",
	"  /D         Disable a resident instance.\n",
	"  /E         Enable a resident instance. (Default.)\n",
	"Installs file-sharing and locking capabilities on your hard disk.\n",

/* program/error messages, set 1*/
	/* MSG_badparams */ "%s: parameter out of range!\n",
	/* MSG_alreadyinstalled */ "%s: is already installed!\n",
	/* MSG_outofmemory */ "%s: out of memory!\n",
	/* MSG_invalidhandler2f */ "%s: invalid interrupt 2Fh handler!\n",
	/* MSG_installed */ "%s: installed.\n",
	/* MSG_alreadyinstalled_no_amis */ "%s: is already installed, but not found on AMIS interrupt!\n",
	/* MSG_enabled */ "%s: enabled.\n",
	/* MSG_disabled */ "%s: disabled.\n",
	/* MSG_cannotenable */ "%s: cannot be enabled, check TSR version.\n",
	/* MSG_cannotdisable */ "%s: cannot be disabled, check TSR version.\n",
	/* MSG_isinstalled */ "%s: is installed resident.\n",
	/* MSG_isinstalleddisabled */ "%s: is installed resident, but currently disabled.\n",
	/* MSG_nofreeamisnum */ "%s: no free AMIS multiplex number!\n",
	/* MSG_invalidhandler2d */ "%s: invalid interrupt 2Dh handler!\n",
	/* MSG_removed */ "%s: removed.\n",
	/* MSG_notinstalled */ "%s: cannot remove, not yet installed.\n",
	/* MSG_somefailure */ "%s: cannot remove, some failure.\n",
	/* MSG_unhookerror */ "%s: cannot remove, handlers hooked AMIS-incompatible.\n",
	/* MSG_unhookerrorcritical */ "%s: cannot remove, internal unhook error.\n",
	/* MSG_unknownerror */ "%s: cannot remove, unknown error.\n",
	/* MSG_prefixed_notresident */ "%s: program is not resident!\n",
	/* MSG_failedfreeenv */ "%s: failed to free environment block.\n",

/* status messages, set 2 */
	/* MSG_notresident */ "Program is not resident!\n",
	/* MSG_patchstatus_notsupported */ "Patch status: not supported by TSR.\n",
	/* MSG_patchstatus_indeterminate */ "Patch status: indeterminate.\n",
	/* MSG_patchstatus_needed */ "Patch status: needed, flag at %04Xh:%04Xh.\n",
	/* MSG_patchstatus_notneeded */ "Patch status: not needed.\n",
	/* MSG_patchstatus_unknown */ "Patch status: unknown.\n",
	/* MSG_patched */ "Patched the share_installed byte of old FreeDOS kernel to zero.\n",
	/* MSG_patched_itself */ "Resident patched the share_installed byte of old FreeDOS kernel to zero.\n",
	/* MSG_filetable */ "File table: %u free / %u total, lock table: %u free / %u total\n",
	/* MSG_filetable_disabled */ "File table: %u total, lock table: %u total\n",
};
#endif


void init_messages(char *argv0) {
	cat = catopen("share", 0);

		/* Extract program name from argv[0] into progname. */
	if (argv0 != NULL) {
		int i;
		char *p = argv0;
		char *p2;
		char c;

		if ( (p[0] != '\0') && (p[1] == ':') )
			p += 2;
		while ( ((p2 = strchr(p, '\\')) != NULL) ||
				((p2 = strchr(p, '/')) != NULL) )
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
}

void cleanup_messages(void) {
	catclose(cat);
}


void usage(void) {
	int i;
	PRINTF(catgets(cat, 0, 1, messages[MSG_USAGE_START+1]), progname);
	for (i = MSG_USAGE_START+2; i < MSG_PROGRAM_START; i++)
		PRINTF(catgets(cat, 0, i, messages[MSG_USAGE_START+i]));
}

void bad_params(void) {
	print_program_message(MSG_badparams);
}

void print_program_message(unsigned msgno) {
	PRINTF(catgets(cat, 1, msgno, messages[MSG_PROGRAM_START+msgno]), progname);
}

void print_status_message(unsigned msgno, ...) {	
	va_list arg;

	va_start(arg, msgno);
	PRINTF(catgets(cat, 2, msgno, messages[MSG_STATUS_START+msgno]), arg);
}

