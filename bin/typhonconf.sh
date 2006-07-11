#! /bin/sh
 
export TYPHON_PATH=SEDTYPHONPATH

if [ -z "${PATH}" ] ; then
  export PATH=$TYPHON_PATH
else
  export PATH=$TYPHON_PATH:$PATH
fi
