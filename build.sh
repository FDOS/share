#!/bin/sh

if [ x"${COMPILER}" = "xgcc" ] ; then
  export CC="ia16-elf-gcc"
  export CFLAGS="-mtsr -Wall -fpack-struct -mcmodel=tiny -c -Os -o"
  export EXTRA_OBJS="amishelp.obj"
  export LD="ia16-elf-gcc"
  export LDFLAGS="-mtsr share.obj \${EXTRA_OBJS} -o share.com -li86 -Wl,-Map=share.map"

elif [ x"${COMPILER}" = "xtcc2-emu" ] ; then
  if ! $(file "share.c" | grep -q CRLF) ; then
    echo "Warning: Turbo C 2.01 doesn't process files with Unix line endings"
    echo "         Converting ..."
    unix2dos src/share.c kitten/kitten.c kitten/kitten.h tnyprntf/tnyprntf.c tnyprntf/tnyprntf.h
    UNDO=1
  fi
  dosemu -q -td -K . -E "build.bat tcc2"
  [ "$UNDO" = "1" ] && dos2unix src/share.c kitten/kitten.c kitten/kitten.h tnyprntf/tnyprntf.c tnyprntf/tnyprntf.h
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

export EXTRA_OBJS="${EXTRA_OBJS} tnyprntf.obj"
# if you want to build without tnyprntf comment the above and uncomment
# the following
# export CFLAGS="-DNOPRNTF ${CFLAGS}"

export EXTRA_OBJS="${EXTRA_OBJS} kitten.obj"
# if you want to build without kitten comment the above and uncomment
# the following
# export CFLAGS="-DNOCATS ${CFLAGS}"

export UPXARGS="upx --8086 --best"
# if you don't want to use UPX set
#     UPXARGS=true
# if you use UPX: then options are
#     --8086 for 8086 compatibility
#   or
#     --best for smallest

make -C src
