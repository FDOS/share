; holds string constants used in messages

%include "segs.inc"

s.const


global messages
messages dw .s0_0,.s0_1,.s0_2,.s0_3,.s0_4,.s0_5,.s0_6,.s0_7,.s0_8,.s0_9, \
            .s1_0,.s1_1,.s1_2,.s1_3,.s1_4,.s1_5,.s1_6,.s1_7,.s1_8,.s1_9,.s1_10,.s1_11,.s1_12,.s1_13,.s1_14,.s1_15,.s1_16,.s1_17,.s1_18,.s1_19,.s1_20,.s1_21, \
			.s2_0,.s2_1,.s2_2,.s2_3,.s2_4,.s2_5,.s2_6,.s2_7,.s2_8,.s2_9

; /* usage messages, set 0 */
.s0_0 db	`%s [/F:space] [/L:locks]\n`, 0
.s0_1 db	`%s [/F:space] [/L:locks] [/U] [/S] [/O] [/D] [/E]\n`, 0
.s0_2 db	`  /F:space   Allocates file space (in bytes) for file-sharing information.\n`, 0
.s0_3 db	`  /L:locks   Sets the number of files that can be locked at one time.\n`, 0
.s0_4 db	`  /U         Uninstall a resident instance.\n`, 0
.s0_5 db	`  /S         Show patch status and table sizes.\n`, 0
.s0_6 db	`  /O         Only operate if already resident, do not install.\n`, 0
.s0_7 db	`  /D         Disable a resident instance.\n`, 0
.s0_8 db	`  /E         Enable a resident instance. (Default.)\n`, 0
.s0_9 db	`Installs file-sharing and locking capabilities on your hard disk.\n`, 0

; /* program/error messages, set 1*/
.s1_0 db    `%s: parameter out of range!\n`, 0 			; MSG_badparams
.s1_1 db    `%s: is already installed!\n`, 0 			; MSG_alreadyinstalled
.s1_2 db    `%s: out of memory!\n`, 0 					; MSG_outofmemory
.s1_3 db    `%s: invalid interrupt 2Fh handler!\n`, 0	; MSG_invalidhandler2f
.s1_4 db    `%s: installed.\n`, 0						; MSG_installed
.s1_5 db    `%s: is already installed, but not found on AMIS interrupt!\n`, 0	; MSG_alreadyinstalled_no_amis
.s1_6 db    `%s: enabled.\n`, 0							; MSG_enabled
.s1_7 db    `%s: disabled.\n`, 0						; MSG_disabled
.s1_8 db    `%s: cannot be enabled, check TSR version.\n`, 0					; MSG_cannotenable
.s1_9 db    `%s: cannot be disabled, check TSR version.\n`, 0					; MSG_cannotdisable
.s1_10 db   `%s: is installed resident.\n`, 0									; MSG_isinstalled
.s1_11 db   `%s: is installed resident, but currently disabled.\n`, 0			; MSG_isinstalleddisabled
.s1_12 db   `%s: no free AMIS multiplex number!\n`, 0							; MSG_nofreeamisnum
.s1_13 db   `%s: invalid interrupt 2Dh handler!\n`, 0							; MSG_invalidhandler2d
.s1_14 db   `%s: removed.\n`, 0													; MSG_removed
.s1_15 db   `%s: cannot remove, not yet installed.\n`, 0						; MSG_notinstalled
.s1_16 db   `%s: cannot remove, some failure.\n`, 0								; MSG_somefailure
.s1_17 db   `%s: cannot remove, handlers hooked AMIS-incompatible.\n`, 0		; MSG_unhookerror
.s1_18 db   `%s: cannot remove, internal unhook error.\n`, 0					; MSG_unhookerrorcritical
.s1_19 db   `%s: cannot remove, unknown error.\n`, 0							; MSG_unknownerror
.s1_20 db   `%s: program is not resident!\n`, 0									; MSG_prefixed_notresident
.s1_21 db   `%s: failed to free environment block.\n`, 0						; MSG_failedfreeenv

; /* status messages, set 2 */
.s2_0 db    `Program is not resident!\n`, 0										; MSG_notresident
.s2_1 db    `Patch status: not supported by TSR.\n`, 0							; MSG_patchstatus_notsupported
.s2_2 db    `Patch status: indeterminate.\n`, 0									; MSG_patchstatus_indeterminate
.s2_3 db    `Patch status: needed, flag at %04Xh:%04Xh.\n`, 0					; MSG_patchstatus_needed
.s2_4 db    `Patch status: not needed.\n`, 0									; MSG_patchstatus_notneeded
.s2_5 db    `Patch status: unknown.\n`, 0										; MSG_patchstatus_unknown
.s2_6 db    `Patched the share_installed byte of old FreeDOS kernel to zero.\n`, 0				; MSG_patched
.s2_7 db    `Resident patched the share_installed byte of old FreeDOS kernel to zero.\n`, 0		; MSG_patched_itself
.s2_8 db	`File table: %u free / %u total, lock table: %u free / %u total\n`, 0				; MSG_filetable
.s2_9 db    `File table: %u total, lock table: %u total\n`, 0					; MSG_filetable_disabled

