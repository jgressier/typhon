#!/bin/sh -u

usage() {
  echo "usage: `basename $0` -o[1|2]"
}

typhdir="$HOME/CODES/TYPHON-DEV/GGNEW/TYPHON/SOURCE"
typhon="$typhdir/Typhon-seq"
export LD_LIBRARY_PATH="$typhdir/Lib:$LD_LIBRARY_PATH"

[ $# -ne 1 ] && usage && exit 1

case $1 in
  -o[12]) o=$1;;
  *)   usage ; exit 1;;
esac

frpm="main.rpm"
fref="main$o.rpm"

echo $fref

exit

save=0
if [ -f "$frpm" ]; then
  fsav="$frpm.sav.$$"
  while [ -e $fsav ]; do
    fsav="$fsav."
  done
  save=1
  mv $frpm $fsav
fi

ln -s $fref $frpm

$typhon

rm $frpm

if [ $save -eq 1 ]; then
  mv $fsav $frpm
fi

exit 0
