#!/bin/sh

if [ x"${COMPILER}" = "xgcc" ] ; then
  export CC="ia16-elf-gcc"
  export COPT="-Wall -fpack-struct -mcmodel=tiny -o "
  export LOPT="-li86"

elif [ x"${COMPILER}" = "xtcc2-emu" ] ; then
  dosemu -q -td -K . -E "build.bat tcc2"
  exit $?

elif [ x"${COMPILER}" = "xtcc3-emu" ] ; then
  dosemu -q -td -K . -E "build.bat tcc3"
  exit $?

else
  echo "Please set the COMPILER env var to one of"
  echo "Cross compile           : 'gcc' (not working yet)"
  echo "Native compile (Dosemu) : 'tcc2-emu' or 'tcc3-emu'"
  exit 1
fi

make
