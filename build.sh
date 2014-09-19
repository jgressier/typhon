#!/bin/bash -u

SCRIPTDIR=$(cd $(dirname $0) ; pwd)
SCRIPTNAME=$(basename $0)
SCRIPTSPCE=${SCRIPTNAME//?/ }

DEFAULT_SOURCE_DIR=../$(basename $(pwd))

DEFAULT_FC=$(which ifort)

DEFAULT_CGNS__HOME=/volume/v1/local/Typhon_deps
DEFAULT_METIS_HOME=/volume/v1/local/CharlesX_dep

DEFAULT_USE__MPI=No
DEFAULT_USE_CGNS=Yes

# --- print usage ---
#
function writebar() {
  printf "%79s\n" | tr ' ' '='
}

function usage() {
  echo "Usage:"
  echo "  $SCRIPTNAME [options] [builddir]"
  echo
  echo "Options:"
  echo "  -h            print this help"
  echo "  -j n          parallel make with n processes"
  echo "  -Debug, -Release, -RelWithDebInfo"
  echo "                options to cmake"
  echo "                (default is -RelWithDebInfo)"
  echo
  echo "  builddir      build directory is ../builddir"
  echo "                (default is build)"
}

function error() {
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  echo -e "!!!  ERROR  !!! $@  !!!"
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  usage
  exit 1
}

cmakeopt=
makeopt=()
nb=0
BUILD_DIR_NAME=build
while [ ${#} -gt 0 ] ; do
  case "$1" in
    -h) usage ; exit 0 ;;
    -j) test ${#} -gt 1 || error "$1 option: nproc required"
        shift
        makeopt+=( -j "$1" )
        ;;
    -Debug|-Release|-RelWithDebInfo)
        cmakeopt="-DCMAKE_BUILD_TYPE=${1#-}"
        ;;
    *)  case $nb in
          0) nb=1 ; BUILD_DIR_NAME=$1 ;;
          #1) nb=1 ; SOURCE_DIR=$1 ;;
          *) error "multiple build dirs:" "$BUILD_DIR_NAME" "$1" ;;
        esac
        ;;
  esac
  shift
done

#SOURCE_DIR=$(cd $(dirname $0) ; pwd)

SOURCE_DIR=$DEFAULT_SOURCE_DIR

FC=$DEFAULT_FC

CGNS__HOME=$DEFAULT_CGNS__HOME
METIS_HOME=$DEFAULT_METIS_HOME

USE__MPI=$DEFAULT_USE__MPI
USE_CGNS=$DEFAULT_USE_CGNS

BUILD_DIR=../$BUILD_DIR_NAME

makeopt+=( -- )

cd $BUILD_DIR && \
        FC=$FC \
  CGNSHOME=$CGNS__HOME \
 METISHOME=$METIS_HOME \
 cmake $cmakeopt \
          -DTyphon_USE_MPI=$USE__MPI \
         -DTyphon_USE_CGNS=$USE_CGNS \
         $SOURCE_DIR && \
 make "${makeopt[@]}"
