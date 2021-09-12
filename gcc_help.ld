/*
 * Linker script for DOS TSR executables with combined data and text segment.
 */

OUTPUT_FORMAT(binary)
ENTRY(_start)
INPUT(-l:crtbegin.o -l:crtend.o -l:dos-t-c0.o)
GROUP(-lc -lgcc -ldos-t -lm)

SEARCH_DIR("/usr/ia16-elf/lib");
SEARCH_DIR("/usr/lib/x86_64-linux-gnu/gcc/ia16-elf/6.3.0");

/* Currently shows a problem with _exit() ending up in non resident text
   NOCROSSREFS_TO(".non_resident_text", ".text")
 */

SECTIONS
  {
    . = SEGMENT_START ("text-segment", 0x100);
    /* Target text sections.  */
    .text : {
		__stext = .;
	/* copy __lnon_resident_text bytes from __edata to __snon_resident_text */
		*(.tsr_text) *(.tsr_text.*)

	/* program resident text */
		*(.text) *(.text.*)
		__etext = .;
	}
	__ltext = __etext - __stext;

    /* Target data sections.  */
    .data : {
		__sdata = .;

		/* Build lists of constructors and destructors.  */
		KEEP (*crtbegin*.o(.ctors))
		KEEP (*(EXCLUDE_FILE (*crtend*.o ) .ctors))
		KEEP (*(SORT(.ctors.*)))
		KEEP (*(.ctors))

		KEEP (*crtbegin*.o(.dtors))
		KEEP (*(EXCLUDE_FILE (*crtend*.o ) .dtors))
		KEEP (*(SORT(.dtors.*)))
		KEEP (*(.dtors))

		*(.rodata) *(.rodata.*)
		*(.data) *(.data.*)
		*(.gcc_except_table)
		__edata = .;
	}

    .bss (NOLOAD) : {
		 __sbss = .;
                *(.bss) *(.bss.*)
                *(COMMON)
                __ebss = .;

                /* Minimum address allowed for sbrk() to use.  */
                __heap_end_minimum = ALIGN(8);
	}

	__ldata = __edata - __sdata;
	__lbss0 = __ebss - __sbss;
	__lbss1 = __lbss0 + 1;
	__lbss = __lbss1 / 2;

    .non_resident_text 0xf000 - __lnon_resident_text : AT (__edata) {
		__snon_resident_text = .;
		*(.startupA)
		*(.msdos_init) *(.msdos_init.*)
		*(.startupB)
		*(.init)
		*(.startupC)
		*(.fini)
		*(.startupD)
		*(.non_resident_text) *(.non_resident_text.*)
		__enon_resident_text = .;
	}
	__lnon_resident_text = __enon_resident_text - __snon_resident_text;

    ASSERT(. <= 0xfd00, "Error: too large for a .com file.")

    /* Used by the memory resizing and DPMI initialization code in
       dos-models-crt0.S.  */
    __msdos_initial_alloc_paras = 0x1000;

    /DISCARD/ : { *(.*) }
  }
