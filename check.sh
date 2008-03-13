#!/bin/sh -u
#
# Check non-regression of all NRG cases containing nrgconf.sh
#
bar=------------------------------------------------------------------

echo $bar
echo TYPHON non regression check
echo $bar

# --- directory initialization ---
#
export HOMEDIR=$PWD
export  EXEDIR=$PWD/SOURCE
export  BINDIR=$PWD/bin
export  NRGDIR=$PWD/NRG
export MESHDIR=$PWD/NRG/COMMON
export  TMPDIR=/tmp/typhon.$$

# --- directory check ---
#
if [ ! -d $MESHDIR ] ; then
  echo directory $MESHDIR not found
  echo you must cd into a typhon directory
  exit 1
fi

# --- initialization ---
#
. bin/shconf.sh
#
export REFCONF=nrgconf.sh
export DIFFCOM=$DIFF
export LD_LIBRARY_PATH=$EXEDIR/Lib:$LD_LIBRARY_PATH

# --- print parameter ---
#
ncol=60 ; col="\\033[${ncol}G"
mkdir $TMPDIR
trap "rm -Rf $TMPDIR" 0 2

rm diff.log check.log 2> /dev/null

# --- get list of cases ---
#
cd $NRGDIR
if [ ${#} -eq 0 ] ; then
  LISTCONFS=$(find . -name $REFCONF)
else
  grepargs=$(for i in $@ ; do echo "-e $i" ; done)
  LISTCONFS=$(find . -name $REFCONF | grep $grepargs)
fi

# --- tests ---
#
echo diffing with $DIFF

for CONF in $LISTCONFS ; do
  # init
  CASE=${CONF%/$REFCONF}
  DIR=$NRGDIR/$CASE
  #
  echo -n checking $CASE...
  rm -f $TMPDIR/* 2> /dev/null
  # configure
  cd $NRGDIR
  . $CONF
  cd $MESHDIR
  cp $MESHFILE $TMPDIR
  cd $DIR
  cp $INPUTFILE $TMPDIR
  cd $TMPDIR
  echo $bar  >> $HOMEDIR/check.log
  echo $CASE >> $HOMEDIR/check.log
  echo $bar  >> $HOMEDIR/diff.log
  echo $CASE >> $HOMEDIR/diff.log
  case $TYPE_EXE in
    seq) $EXEDIR/Typhon-$TYPE_EXE >> $HOMEDIR/check.log 2>&1 ;;
    mpi) mpirun -np ${MPIPROCS:-2} \
         $EXEDIR/Typhon-$TYPE_EXE >> $HOMEDIR/check.log 2>&1 ;;
    *)   echo -e ${col}$fic unknown typhon executable ;;
  esac
  if [ $? -eq 0 ] ; then
    for fic in $TO_CHECK ; do
      if [ -f "$fic" ] ; then
        $DIFFCOM $fic $DIR/$fic >> $HOMEDIR/diff.log 2>&1
        case $? in
          0) echo -e ${col}$fic identical ;;
          1) echo -e ${col}$fic changed ;;
          *) echo -e ${col}$fic comparison failed ;;
        esac
      else
        echo -e ${col}$fic missing
      fi
    done
  else
    echo -e ${col}computation failed
  fi
done
