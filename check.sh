#!/bin/sh
#
# Check non-regression of all NRG cases containing nrgconf.sh
#
bar=------------------------------------------------------------------

echo $bar
echo TYPHON non regression verification
echo $bar

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
  echo $bar  >> $HOMEDIR/check.log
  echo $CASE >> $HOMEDIR/check.log
  echo $var  >> $HOMEDIR/diff.log
  echo $CASE >> $HOMEDIR/diff.log
  case $TYPE_EXE in
    seq) $EXEDIR/Typhon-$TYPE_EXE >> $HOMEDIR/check.log 2>&1 ;;
    mpi) mpirun -np ${MPIPROCS:-2} $EXEDIR/Typhon-$TYPE_EXE >> $HOMEDIR/check.log 2>&1 ;;
    *)   echo -e \\033[${col}G$fic unknown typhon executable ;;
  esac
  if [ $? -eq 0 ] ; then
    for fic in $TO_CHECK ; do
      if [ -f "$fic" ] ; then
        $DIFFCOM $fic $DIR/$fic >> $HOMEDIR/diff.log 2>&1 
        case $? in
          0) echo -e \\033[${col}G$fic identical ;;
          1) echo -e \\033[${col}G$fic changed ;;
          *) echo -e \\033[${col}G$fic comparison failed ;;
        esac
      else
        echo -e \\033[${col}G$fic missing
      fi
    done
  else
    echo -e \\033[${col}Gcomputation failed
  fi
done
