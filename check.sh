#!/bin/sh
#
# Check non-regression of all NRG cases containing nrgconf.sh
#
echo ------------------------------------------------------------------
echo TYPHON non regression verification
echo ------------------------------------------------------------------

# --- initialization ---
#
. bin/shconf.sh
#
export  HOMEDIR=$PWD
export   EXEDIR=$PWD/SOURCE
export   BINDIR=$PWD/bin
export   NRGDIR=$PWD/NRG
export  MESHDIR=$PWD/NRG/COMMON
export   TMPDIR=/tmp/typhon.$$
export CONFFILE=nrgconf.sh
export  DIFFCOM=$DIFF
#
col=60
mkdir $TMPDIR
trap "rm -Rf $TMPDIR ; exit 1" 0 2

rm diff.log check.log 2> /dev/null

# --- get list of cases ---
#
cd $NRGDIR
if [ -z "$1" ] ; then
  LISTCASES=$(find . -name $CONFFILE)
else
  LISTCASES=$(find . -name $CONFFILE | grep $1)
fi

# --- tests ---
#
echo diffing with $DIFF

for FILE in $LISTCASES ; do
  # init
  CASE=${FILE%/$CONFFILE}
  DIR=$NRGDIR/$CASE
  #
  echo -n checking $CASE...
  rm -f $TMPDIR/* 2> /dev/null
  # configure
  cd $NRGDIR
  . $FILE
  cd $MESHDIR
  cp $MESHFILE $TMPDIR
  cd $DIR
  cp $INPUTFILE $TMPDIR
  cd $TMPDIR
  echo $CASE >> $HOMEDIR/check.log
  echo $CASE >> $HOMEDIR/diff.log
  $EXEDIR/Typhon-$TYPE_EXE >> $HOMEDIR/check.log 2>&1
  if [ $? ] ; then
    for fic in $TO_CHECK ; do
      $DIFFCOM $fic $DIR/$fic >> $HOMEDIR/diff.log
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
