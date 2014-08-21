#!/bin/bash -u

function error() {
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  echo -e "!!!  ERROR  !!! $@  !!!"
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  build_help
  exit 1
}

function build_help() {
  echo "no help available"
}

nb=0
BUILD_DIR=build
while [ ${#} -gt 0 ] ; do
  case "$1" in
    -h) build_help ; exit 0 ;;
    -j) test ${#} -gt 1 || error "$1 option : <nbproc> required"
        shift
        makeopt="-j $1"
        ;;
    *)  case $nb in
          0) nb=1 ; BUILD_DIR=$1 ;;
          #1) nb=1 ; SOURCE_DIR=$1 ;;
          *) error "multiple build dirs:" "$BUILD_DIR" "$1"
        esac
        ;;
  esac
  shift
done

#SOURCE_DIR=$(cd $(dirname $0) ; pwd)

SOURCE_DIR=../$(basename $(pwd))

FC=$(which ifort)

CGNS_HOME=/volume/v1/local/Typhon_deps
METIS_HOME=/volume/v1/local/CharlesX_dep

Typhon_USE_MPI=No
Typhon_USE_CGNS=Yes

cd ../$BUILD_DIR && \
FC=$(which ifort) \
  CGNSHOME=$CGNS_HOME \
 METISHOME=$METIS_HOME \
 cmake -DTyphon_USE_MPI=$Typhon_USE_MPI \
      -DTyphon_USE_CGNS=$Typhon_USE_CGNS \
    $SOURCE_DIR && make $makeopt
