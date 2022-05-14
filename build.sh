#!/bin/sh

if [ x"${COMPILER}" = "xgcc" ] ; then
  export CC="ia16-elf-gcc"
  export COPT="-mtsr -Wall -fpack-struct -mcmodel=tiny -c share.c -o share.obj -Os"
  export XOBJS="gcc_help.obj"
  export LD="ia16-elf-gcc"
  export LOPT="-mtsr share.obj ${XOBJS} -o share.com -li86 -Wl,-Map=share.map"
  make -C src

elif [ x"${COMPILER}" = "xtcc2-emu" ] ; then
  if ! $(file "share.c" | grep -q CRLF) ; then
    echo "Warning: Turbo C 2.01 doesn't process files with Unix line endings"
    echo "         Converting ..."
    unix2dos "src/share.c"
    UNDO=1
  fi
  dosemu -q -td -K . -E "build.bat tcc2"
  [ "$UNDO" = "1" ] && dos2unix "src/share.c"
  exit $?

elif [ x"${COMPILER}" = "xtcc3-emu" ] ; then
  dosemu -q -td -K . -E "build.bat tcc3"
  exit $?

else
  echo "Please set the COMPILER env var to one of"
  echo "Cross compile           : 'gcc'"
  echo "Native compile (Dosemu) : 'tcc2-emu' or 'tcc3-emu'"
  exit 1
fi
