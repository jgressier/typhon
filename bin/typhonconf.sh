#! /bin/sh
 
export TYPHON_PATH=SEDTYPHONPATH

export TYPHON_INCLUDE=$TYPHON_PATH/include
export TYPHON_LIBRARY=$TYPHON_PATH/lib

if [ -z "${PATH}" ] ; then
  export PATH=$TYPHON_PATH
else
  export PATH=$TYPHON_PATH:$PATH
fi
