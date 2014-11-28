#!/bin/bash -u

SCRIPTDIR=$(cd $(dirname $0) ; pwd)
SCRIPTNAME=$(basename $0)
SCRIPTSPCE=${SCRIPTNAME//?/ }

# --- Script is directly in the source dir
#     To be modified if things change...
#
DEFAULT_SOURCE_DIR=$(pwd)

# --- Default variables ---
#
CMAKE=/volume/v1/local/utils/bin/cmake
CCMAKE=/volume/v1/local/utils/bin/ccmake

DEFAULT_FC=$(which ifort)
for mpifort in mpiifort mpif90 ; do
  DEFAULT_MPIFC=$(which $mpifort 2>/dev/null)
  test $? = 0 && break
done

TYDEP=/volume/v1/local/Typhon_deps
CXDEP=/volume/v1/local/CharlesX_dep

DEFAULT_CGNS__HOME=$TYDEP
DEFAULT_METIS_HOME=$CXDEP

# DEFAULT_USE_CGNS=Yes ; OTHER_USE_CGNS=nocgns
# DEFAULT_USE__MPI=No  ; OTHER_USE__MPI=mpi

N=No # Only $OPT and no$OPT, not yes$OPT
V=CGNS ; v=cgns ; DEF=Yes ; eval "DEFAULT_USE_$V"='$DEF' ; Z=${N/$DEF}$v ; eval "OTHER_USE_$V"='${Z/#No/no}'
V=_MPI ; v=mpi  ; DEF=No  ; eval "DEFAULT_USE_$V"='$DEF' ; Z=${N/$DEF}$v ; eval "OTHER_USE_$V"='${Z/#No/no}'

DEFAULT_CMAKE_BUILD_TYPE=$(grep set'('CMAKE_BUILD_TYPE CMakeLists.txt | awk '{print $2}')
UNKNOWN_CMAKE_BUILD_TYPE="defined in CMakeLists.txt"
UNKNOWN_CMAKE_BUILD_TYPE+=${DEFAULT_CMAKE_BUILD_TYPE:+": \`${DEFAULT_CMAKE_BUILD_TYPE}'"}

# --- print bar ---
#
function writebar() {
  printf "%79s\n" | tr ' ' "${1:-=}"
}

# --- print usage ---
#
function usage() {
  echo "$SCRIPTNAME"
  echo
  echo "Usage:"
  echo "  $SCRIPTNAME [options] [builddir]"
  echo
  echo "Options:"
  echo "  -h|--help     prints this help"
  echo "  -f|--force    force creation and use of build directory"
  echo "  -j n          parallel make with n processes"
  echo "  -s srcdir     source directory (default=\$(pwd))"
  echo "  -n|--dryrun   writes and checks name of build directory only"
  echo "  -d|--Debug,"
  echo "  -r|--Release,"
  echo "  -w|--RelWithDebInfo"
  echo "                options to cmake (default is ${UNKNOWN_CMAKE_BUILD_TYPE})"
  echo "  --[no]cgns    cgns compilation (default=${DEFAULT_USE_CGNS,})"
  echo "  --[no]mpi     mpi  compilation (default=${DEFAULT_USE__MPI,})"
  echo
  echo "  builddir      name of build directory, with default according to options used:"
  echo "                ../build[_opt][_nocgns][_mpi] where opt is debug, release or rdb"
}

# --- print error ---
#
function error() {
  usage
  echo
  writebar "@"
  echo ERROR${1:+:}
  test ${#} -gt 0 && printf "  %s\n" "$@"
  writebar "@"
  exit 1
}

function write() {
  test ${#} -gt 0 && printf "  %s\n" "$@" \
                  || echo
  exit 0
}

# --- Default options ---
#
force=
buildopt=()
builddef=
makeopt=()
SRC_DIR=()
BUILD_DIR=()
use_cgns=
use__mpi=
dryrun=
test ${#} -eq 0 && BUILD_DIR=../build

# --- Parse options ---
#
while [ ${#} -gt 0 ] ; do
  case "$1" in
    -h|--help)
        usage ; exit 0 ;;
    -f|--force)
        force=1 ;;
    -j) test ${#} -gt 1 || error "\`$1' option: nproc required"
        shift
        makeopt+=( "-j $1" )
        ;;
    -s) test ${#} -gt 1 || error "\`$1' option: source dir required"
        shift
        SRC_DIR+=( "$1" )
        ;;
    -n|--dryrun)
        dryrun=1 ;;
    -d|--Debug|\
    -r|--Release|\
    -w|--RelWithDebInfo)
        case $1 in
          -d) buildopt+=( --Debug          ) ;;
          -r) buildopt+=( --Release        ) ;;
          -w) buildopt+=( --RelWithDebInfo ) ;;
          *)  buildopt+=( "${1}" ) ;;
        esac
        builddef="-DCMAKE_BUILD_TYPE=${1#--}"
        ;;
    --cgns)   use_cgns=( Yes${use_cgns#Yes}  ) ;;
    --nocgns) use_cgns=(    ${use_cgns%No}No ) ;;
    --mpi)    use__mpi=( Yes${use__mpi#Yes}  ) ;;
    --nompi)  use__mpi=(    ${use__mpi%No}No ) ;;
    #
    --) shift ; break ;;
    -*) error "Unknown option: \`$1'" ;;
    *)  BUILD_DIR+=( "$1" ) ;;
  esac
  shift
done

# --- Read builddir ---
while [ ${#} -gt 0 ] ; do
  BUILD_DIR+=( "$1" )
  shift
done

# --- Multiple option processing ---
#
function multipleopt() {
  local msg tab
  msg="$1" ; shift
  for i in "$@" ; do
    tab+=( "  \`$i'" )
  done
  error "Multiple $msg:" "${tab[@]}"
}

# --- Check at most one src dir option is given ---
#
if [ "${#SRC_DIR[@]}" -gt 1 ] ; then
  multipleopt "source directories" "${SRC_DIR[@]}"
fi

# --- Define source directory ---
#
SRC_DIR=${SRC_DIR:-$DEFAULT_SOURCE_DIR}

# --- Check source directory ---
#
test -e "$SRC_DIR" || error "\`$SRC_DIR' does not exist"
test -d "$SRC_DIR" || error "\`$SRC_DIR' is not a valid directory"
f=CMakeLists.txt
if [ ! -f "$SRC_DIR/$f" ] ; then
  error "\`$SRC_DIR' is not a valid source directory" \
        "  (\`$f' is missing)"
fi

# --- Set default option ---
#
DEFAULT_CMAKE_BUILD_TYPE=$(grep set'('CMAKE_BUILD_TYPE CMakeLists.txt | awk '{print $2}')

# --- Check at most one -j option is given ---
#
if [ "${#makeopt[@]}" -gt 1 ] ; then
  multipleopt "make build options" "${makeopt[@]}"
fi

# --- Check at most one cgns option is given ---
#
if [ "$use_cgns" = YesNo ] ; then
  multipleopt "cgns options" yes no
fi
use_cgns=${use_cgns/$DEFAULT_USE_CGNS} # Remove the default option

# --- Check at most one mpi option is given ---
#
if [ "$use__mpi" = YesNo ] ; then
  multipleopt "mpi options" yes no
fi
use__mpi=${use__mpi/$DEFAULT_USE__MPI} # Remove the default option

# --- Check at most one cmake build option is given ---
#
if [ ${#buildopt[@]} -gt 1 ] ; then
  multipleopt "cmake build options" "${buildopt[@]}"
fi

# --- Default build directory is given ---
#
if [ ${#BUILD_DIR[@]} -eq 0 ] ; then
  case "${buildopt:-}" in
    --Debug)          BUILD_DIR=build_debug ;;
    --Release)        BUILD_DIR=build_release ;;
    --RelWithDebInfo) BUILD_DIR=build_rdb ;;
    '')               BUILD_DIR=build ;;
    *) error "Unknown cmake option: \`$buildopt'" ;;
  esac
  BUILD_DIR+=${use_cgns:+_$OTHER_USE_CGNS}
  BUILD_DIR+=${use__mpi:+_$OTHER_USE__MPI}
  BUILD_DIR=../$BUILD_DIR
fi

# --- Check at most one build directory is given ---
#
if [ ${#BUILD_DIR[@]} -gt 1 ] ; then
  multipleopt "build dirs" "${BUILD_DIR[@]}"
fi

# --- Disable force if dryrun ---
#
if [ -n "$dryrun" -a -n "$force" ] ; then
  echo "Option \`-f' is ignored in dry-run"
  force=
fi

# --- Print build directory ---
#
if [ -n "$dryrun" ] ; then
  echo "build directory is \`$BUILD_DIR'"
  cmd=write
else
  cmd=error
fi

# --- Check build directory exists ---
#
if [ ! -e "$BUILD_DIR" ] ; then
  if [ -n "$force" ] ; then
    mkdir -p "$BUILD_DIR" || error "\`$BUILD_DIR' could not be created"
    echo "\`$BUILD_DIR' is created..."
  else
    $cmd "\`$BUILD_DIR' does not exist" \
         "You may consider option \`-f'"
    exit 0 # only for dryrun
  fi
fi

# --- Check build directory is a directory ---
#
if [ ! -d "$BUILD_DIR" ] ; then
  $cmd "\`$BUILD_DIR' is not a valid directory"
  exit 0 # only for dryrun
fi

# --- Check build directory is a valid build directory ---
#
if [ ! -n "$force" ] ; then
  find "$BUILD_DIR" -mindepth 1 -print -quit | grep -q .
  # If some file or dir exists, CMakeCache.txt should exist
  if [ $? = 0 ] ; then
    f=CMakeCache.txt
    if [ ! -f "$BUILD_DIR/$f" ] ; then
      $cmd "\`$BUILD_DIR' is not a valid cmake directory" \
           "  (\`$f' is missing in non-empty directory)" \
           "You may consider option \`-f'"
      exit 0 # only for dryrun
    fi
  fi
  test -n "$dryrun" && exit 0
fi

# --- Check build directory is not something like "./some/path/../." ---
#
echo ./$BUILD_DIR/ | sed 's:.*/\.\./::' | grep -q '[^/\.]'
if [ $? -ne 0 ] ; then
  error "Build directory is not a specific directory (trailing ..'s):" "  \`$BUILD_DIR'"
fi

# --- Define variables from defaults ---
#

SOURCE_DIR=${SRC_DIR}

FC=$DEFAULT_FC

CGNS__HOME=$DEFAULT_CGNS__HOME
METIS_HOME=$DEFAULT_METIS_HOME

USE_CGNS=${use_cgns:-$DEFAULT_USE_CGNS}
USE__MPI=${use__mpi:-$DEFAULT_USE__MPI}

test $USE__MPI = Yes && FC=$DEFAULT_MPIFC

makeopt+=( -- )

# --- Change directory ---
#

cd $BUILD_DIR
test $? = 0 || error "Could not chdir to \`$BUILD_DIR'"

echo FC=$FC
echo SRC_DIR=$SRC_DIR
echo BUILD_DIR=$BUILD_DIR

        FC=$FC \
  CGNSHOME=$CGNS__HOME \
 METISHOME=$METIS_HOME \
 $CMAKE $builddef \
         -DTyphon_USE_CGNS=$USE_CGNS \
          -DTyphon_USE_MPI=$USE__MPI \
         $SOURCE_DIR && \
 make ${makeopt[@]}

