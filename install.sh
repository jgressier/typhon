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
TYPHON_INC=$dir/include
TYPHON_LIB=$dir/lib
echo 
echo . Installation paths
echo "scripts and binaries:" $TYPHON_PATH
echo "include files       :" $TYPHON_INC
echo "libraries           :" $TYPHON_LIB

# -- Installation --

echo
echo . Installation...
if [ -d $TYPHON_PATH ] ; then
  echo $TYPHON_PATH already exists
else
  echo creating $TYPHON_PATH
  mkdir $TYPHON_PATH || echo unexpected error in TYPHON installation
fi
cp    SOURCE/Typhon-seq $TYPHON_PATH
cp    SOURCE/Typhon-mpi $TYPHON_PATH
cp -r GUI/TYMON         $TYPHON_PATH
cp    bin/tymon         $TYPHON_PATH

if [ -d $TYPHON_INC  ]  ; then
  echo $TYPHON_INC  already exists 
else
  echo creating $TYPHON_INC
  mkdir $TYPHON_INC || echo unexpected error in TYPHON installation
fi

if [ -d $TYPHON_LIB  ]  ; then
  echo $TYPHON_LIB  already exists 
else
  echo creating $TYPHON_LIB
  mkdir $TYPHON_LIB || echo unexpected error in TYPHON installation
fi

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
echo \(or run \". $PROF_FILE\" to immediately use TYPHON\)


