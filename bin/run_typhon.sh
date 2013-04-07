#!/bin/sh

version=752
nbproc=""
nbthread=""
exedir=/stck-sup/typhon
meshdir=/stck-sup/mesh_repository

# --- print usage ---
#
function usage() {
  if [ $1 = 1 ] ; then
    echo "$bar"
    echo "ERROR"
    echo "$bar"
  fi
  echo
  echo "Usage: $(basename $0) [-h] [-v xxx] [-n nb]|[-t nb] <list of main files>"
  echo
  echo "       -h                : prints this help"
  echo "       -v version-number : define TYPHON version"
  echo "       -n proc-number    : run MPI    computation with specified nb of procs"
  echo "       -t thread-number  : run OpenMP computation with specified nb of threads"
  echo "  list of main files can be file names or suffixes (main.suffix)"
  echo "  mesh files are searched in local folder and in $meshdir"
  if [ -n "$1" ] ; then
    echo "error: $1"
  fi
  exit 1
}

# --- parse options

while true ; do
  case "$1" in
    -h) usage ;;
    -v) version=$2  ; shift ; shift ;;
    -n) nbproc=$2   ; shift ; shift ;;
    -t) nbthread=$2 ; shift ; shift ;;
    *) break ;;
  esac
done

if [[ -z "$nbproc" ]] ; then
  typhonexe=$exedir/typhon$version
else
  typhonexe=$exedir/typhon${version}mpi
fi

if [[ -z "$nbthread" ]] ; then
  unset OMP_NUM_THREADS
  echo using $typhonexe 
else
  export OMP_NUM_THREADS=$nbthread
  typhonexe=${typhonexe}omp
  echo using $typhonexe with $OMP_NUM_THREADS threads
fi

if [[ -z "$*" ]] ; then
  usage "missing case argument"
fi

for maincase in $* ; do
  case=${maincase#main.}
  echo run case $case
  #
  if [[ -f "main.$case" ]] ; then
    compdir=comp.$case
    mkdir $compdir
    cd $compdir
    cp ../main.$case main.rpm
    meshfile=$(sed '/BLOCK:MESH/,/FILE/!d' main.rpm | grep FILE | sed 's/^.*= *"//' | sed 's/".*$//')
    echo mesh file: $meshfile
    if [ -f ../$meshfile ] ; then
      cp ../$meshfile .
    elif [ -f $meshdir/$meshfile ] ; then
      cp $meshdir/$meshfile .
    else
      echo unable to find $meshfile
      exit 1
    fi
    if [[ -z "$nbproc" ]] ; then
      $typhonexe > output.log
    else
      mpirun -np $nbproc $typhonexe > output.log
    fi
    export smtp=smtp
    tail -n 100 output.log | mailx ${USER}@isae.fr -s "computation $case ended on $HOSTNAME"
    cd ..
  else
    echo main.$case missing
  fi
done

