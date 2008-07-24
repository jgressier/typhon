#!/bin/sh -u
#
# Check non-regression of all NRG cases containing nrgconf.sh
#
bar=----------------------------------------------------------------

echo $bar
echo TYPHON non regression check
echo $bar

# --- directory initialization ---
#
ORIGDIR=$PWD
HOMEDIR=$(dirname $0)
cd $HOMEDIR
export HOMEDIR=$PWD
export  EXEDIR=$HOMEDIR/SOURCE
export  BINDIR=$HOMEDIR/bin
export  NRGDIR=$HOMEDIR/NRG
export MESHDIR=$HOMEDIR/NRG/COMMON
export  TMPDIR=/tmp/typhon.$$

# --- directory check ---
#
if [ ! -d $MESHDIR ] ; then
  echo directory $MESHDIR not found
  echo $HOMEDIR/$(basename $0) is not in a valid typhon directory
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
ncol1=50 ; col1="\\033[${ncol1}G"
ncol2=75 ; col2="\\033[${ncol2}G"
mkdir $TMPDIR
trap "rm -Rf $TMPDIR" 0 2

rm diff.log check.log 2> /dev/null

# --- check options ---
#
if [ ${#} -eq 1 ] && [ $1 = "-h" ] ; then
  echo "Usage: $(basename $0) [-h] [-l] [<pattern>]"
  echo
  echo "       -h: prints this help"
  echo "       -l: prints list of cases"
  echo "       <pattern>: selects cases with name matching <pattern>"
  echo "       default: runs cases"
  echo
  exit 0
fi
list=0
if [ ${#} -ge 1 ] && [ $1 = "-l" ] ; then
  list=1
  shift
fi

# --- get list of cases ---
#
cd $NRGDIR
grepargs=.
if [ ${#} -gt 0 ] ; then
  grepargs=$(for i in $@ ; do echo "-e $i" ; done)
fi
LISTCONFS=$(find * -name $REFCONF | grep $grepargs)

# --- print list of cases ---
#
if [ $list -eq 1 ] ; then
  echo LISTCONFS =
  for CONF in ${LISTCONFS[*]} ; do
    echo "    ${CONF%/$REFCONF}/"
  done
  exit 0
fi

# --- tests ---
#
echo diffing with : $DIFF
echo $bar

for CONF in $LISTCONFS ; do
  # init
  CASE=${CONF%/$REFCONF}
  DIR=$NRGDIR/$CASE
  #
  echo -n checking $CASE...
  rm -f $TMPDIR/* 2> /dev/null
  # configure
  . $NRGDIR/$CONF
  cp $MESHDIR/$MESHFILE $TMPDIR
  cp $DIR/$INPUTFILE $TMPDIR
  cd $TMPDIR
  echo $bar  >> $HOMEDIR/check.log
  echo $CASE >> $HOMEDIR/check.log
  echo $bar  >> $HOMEDIR/diff.log
  echo $CASE >> $HOMEDIR/diff.log
  case $TYPE_EXE in
    seq) $EXEDIR/Typhon-$TYPE_EXE >> $HOMEDIR/check.log 2>&1 ;;
    mpi) mpirun -np ${MPIPROCS:-2} \
         $EXEDIR/Typhon-$TYPE_EXE >> $HOMEDIR/check.log 2>&1 ;;
    *)   echo -e ${col1}$fic unknown typhon executable ;;
  esac
  if [ $? -eq 0 ] ; then
    for fic in $TO_CHECK ; do
      if [ -f "$fic" ] ; then
        $DIFFCOM $fic $DIR/$fic >> $HOMEDIR/diff.log 2>&1
        case $? in
          0) echo -e ${col1}$fic ${col2}identical ;;
          1) echo -e ${col1}$fic ${col2}changed ;;
          *) echo -e ${col1}$fic ${col2}comparison failed ;;
        esac
      else
        echo -e ${col1}$fic ${col2}missing
      fi
    done
  else
    echo -e ${col2}computation failed
  fi
done

echo $bar
if [ $HOMEDIR = $ORIGDIR ] ; then
  echo check.log diff.log
else
  echo $HOMEDIR/check.log
  echo $HOMEDIR/diff.log
fi

cd $ORIGDIR
