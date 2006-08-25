#!/bin/sh

echo ---------------------------------------------------------------------
echo TYPHON installation
echo

# -- default initialization --

if [ $USER == root ] ; then
  echo . superuser installation
  DEFAULT_ROOTPATH=/opt
  DEFAULT_PROFPATH=/etc/profile.d
else
  echo . regular user installation
  DEFAULT_ROOTPATH=$HOME/local
  DEFAULT_PROFPATH=$HOME/local/profile.d
fi

# -- check installation path --

echo
unset dir
until [[ -d $dir ]] && [[ -w $dir ]] ; do
  echo Please choose and check a \(writable\) path of TYPHON installation:
  echo -n "[$DEFAULT_ROOTPATH] ? "
  read dir
  dir=${dir:-$DEFAULT_ROOTPATH}
done

TYPHON_PATH=$dir/typhon
TYPHON_INC=$TYPHON_PATH/include
TYPHON_LIB=$TYPHON_PATH/lib
TYPHON_UDF=$TYPHON_PATH/udf
echo 
echo . Installation paths
echo "scripts and binaries    :" $TYPHON_PATH
echo "include files           :" $TYPHON_INC
echo "libraries               :" $TYPHON_LIB
echo "user defined files (UDF):" $TYPHON_UDF

# -- Installation --

echo
echo . Installation...
if [ -d $TYPHON_PATH ] ; then
  echo $TYPHON_PATH already exists
else
  echo creating $TYPHON_PATH
  mkdir $TYPHON_PATH || echo unexpected error in TYPHON installation
fi

mkdir $TYPHON_INC 2> /dev/null
mkdir $TYPHON_LIB 2> /dev/null
mkdir $TYPHON_UDF 2> /dev/null

cp    SOURCE/Typhon-seq      $TYPHON_PATH
cp    SOURCE/Typhon-mpi      $TYPHON_PATH
cp    SOURCE/UDF/udf*.f90    $TYPHON_UDF
cp    SOURCE/Include/*       $TYPHON_INC
cp    SOURCE/Lib/libt_udf.so $TYPHON_LIB
cp -r GUI/TYMON              $TYPHON_PATH
cp    bin/tymon              $TYPHON_PATH
cp    bin/*.sh               $TYPHON_PATH

# -- check configuration scripts --

echo
unset dir
until [[ -d $dir ]] && [[ -w $dir ]] ; do
  echo Please choose and check a \(writable\) path for configuration files:
  echo -n "[$DEFAULT_PROFPATH] ? "
  read dir
  dir=${dir:-$DEFAULT_PROFPATH}
done
echo creating TYPHON configuration file...
PROF_FILE=$dir/typhonconf.sh
sedpath=$(echo $TYPHON_PATH | sed 's/\//\\\//g')
cat bin/typhonconf.sh | sed "s/SEDTYPHONPATH/$sedpath/" > $PROF_FILE

echo
echo Please check that $PROF_FILE will be read in your profile files...
echo \(or run \" . $PROF_FILE \" to immediately use TYPHON\)


