#!/bin/sh
#
# Check non-regression of all NRG cases containing nrgconf.sh
#
echo ------------------------------------------------------------------
echo TYPHON non regression verification
echo ------------------------------------------------------------------

# --- initialization ---
#
export  HOMEDIR=$PWD
export   EXEDIR=$PWD/SOURCE
export   BINDIR=$PWD/bin
export   NRGDIR=$PWD/NRG
export  MESHDIR=$PWD/VALIDATION/mesh
export   TMPDIR=/tmp/typhon.$$
export CONFFILE=nrgconf.sh
#
col=60
mkdir $TMPDIR
trap "rm -Rf $TMPDIR ; exit 1" 0 2

cd $NRGDIR
if [ -z "$1" ] ; then
  LISTCASES=$(find . -name $CONFFILE)
else
  LISTCASES=$(find . -name $CONFFILE | grep $1)
fi

for FILE in $LISTCASES ; do
  # init
  CASE=${FILE%/$CONFFILE}
  DIR=$NRGDIR/$CASE
  #
  echo -n checking $CASE...
  rm -f $TMPDIR/* 2> /dev/null
  # configure
  . $FILE
  cd $MESHDIR
  cp $MESHFILE $TMPDIR
  cd $DIR
  cp $INPUTFILE $TMPDIR
  cd $TMPDIR
  echo $CASE >> $HOMEDIR/check.log
  $EXEDIR/Typhon-$TYPE_EXE >> $HOMEDIR/check.log 2>&1
  if [ $? ] ; then
    for fic in $TO_CHECK ; do
      diff -bB $fic $DIR/$fic #>> diff.log
      case $? in
        0) echo -e \\033[${col}G$fic identical ;;
        1) echo -e \\033[${col}G$fic changed ;;
        *) echo -e \\033[${col}G$fic comparison failed ;;
      esac
    done
  else
    echo -e \\033[${col}Gcomputation failed
  fi
done
