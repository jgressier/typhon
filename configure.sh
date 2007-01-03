##!/bin/sh

echo ------------------------------------------------------------------
echo TYPHON configuration
echo ------------------------------------------------------------------

TOOLSCONF=TOOLS/configure
MAKECONF=SOURCE/defvar.make
SHELLCONF=bin/shconf.sh

configure_help() {
  echo "TYPHON configuration help"
  echo "  set F90LIB to help finding external libraries (i.e. export F90LIB=/opt/aero/lib)"
  }

check() {
  local com
  echo -n checking $1...
  shift
  com=$1
  shift
  check_$com $*
  }

success() {
  local col
  col=60
  echo -e \\033[${col}G$1
  }

fail()    {
  local col
  col=60
  echo -e \\033[${col}G$1
  }

warning()    {
  echo -e "!!! warning !!!" "$*"
  }

error()    {
  echo -e "!!!  ERROR  !!!" "$*"
  configure_help
  exit 1
  }

check_system() {
  export SYS=$(uname -s)
  success $SYS
  }

check_proc() {
  PROC=$(uname -m) || PROC=$(uname -p)
  export PROC
  success $PROC
  }

check_echo() {
  export ECHO="echo -e"
  success $ECHO
  }

check_diff() {
  local difflist="ndiff diff"
  for diffcom in $difflist ; do
    DIFF=$(which $diffcom) 2> /dev/null
    if [ -n "$DIFF" ] ; then
      success $DIFF
      break
    fi
  done
  case $DIFF in
    */ndiff) export DIFF="$DIFF -relerr 1.E-12" ;;
    */diff)  export DIFF="$DIFF -bB" ;;
    *)     fail "not found" ;;
  esac
  }

check_f90compiler() {
  local namelist="ifort ifc pgf90 lfc pathf90 af90 f90 f95 g95 gfortran"
  for exe in $namelist ; do
    F90C=$(which $exe 2> /dev/null)
    if [ -n "$F90C" ] ; then
      success $F90C
      F90C=${F90C##*/}
      check "$F90C fortran 90 conformance" f90conformance
      break
    fi
  done
  if [ -n "$F90C" ] ; then
    export F90C
  else
    fail "not found"
  fi
  }

check_f90conformance() {
  local f90options
  f90options="$*"
  cd $TMPDIR
  rm * 2> /dev/null
  cp $LOCALDIR/$TOOLSCONF/f90conformance.f90 .
  $F90C $f90options f90conformance.f90 -o f90test > /dev/null 2>&1
  if [ $? ] ; then
    success "successfully compiled"
    ./f90test > /dev/null 2>&1
    if [ $? ] ; then
      success "checked"
    else
      fail "execution failed"
    fi
  else
    fail "compiler error"
  fi
  cd $LOCALDIR
  }

check_f90opti() {
  case $SYS-$F90C in
    *ifc|*ifort) F90_OPTI="-implicitnone -convert big_endian -O3" ;;
    *pgf90)      F90_OPTI="-fastsse -Munroll=n:4 -Mipa=fast,inline" ;;
    *pathf90)    F90_OPTI="-Ofast" ;;
    *gfortran)   F90_OPTI="-ffast-math -funroll-loops -O3" ;;
    *g95)        F90_OPTI="-ffast-math -funroll-loops -O3" ;;
    *af90)       F90_OPTI="-Ofast -fast_math" ;;
    *lfc)        F90_OPTI="--fast" ;;
    IRIX*f90)    F90_OPTI="-03" ;;
    HP-UX-f90)   F90_OPTI="-03" ;;
    *)           F90_OPTI="-03" ;;
  esac
  export F90_OPTI
  success "$F90_OPTI"
  check "$F90_OPTI options" f90conformance "$F90_OPTI"
  }

check_f90debug() {
  case $SYS-$F90C in
    *ifc|*ifort) F90_DEBUG="-implicitnone -convert big_endian -g -traceback -CB" ;;
    *)           F90_DEBUG="-g" ;;
  esac
  export F90_DEBUG
  success "$F90_DEBUG"
  check "$F90_DEBUG options" f90conformance "$F90_DEBUG"
  }

check_f90prof() {
  case $SYS-$F90C in
    *)           F90_PROF="$F90_OPTI -pg" ;;
  esac
  export F90_PROF
  success "$F90_PROF"
  check "$F90_PROF options" f90conformance "$F90_PROF"
  }

check_library() {
  local    name=$1
  local     ext=$2
  local pathlib="$F90LIB /usr/lib /usr/local/lib /opt/lib /opt/local/lib"
  local fullname
  for dir in $pathlib ; do
    if [ -r "$dir/lib$name.$ext" ] ; then
      fullname=$dir/lib$name.$ext
      break
    fi
  done
  if [ -n "$fullname" ] ; then
    success $fullname
    export LIB_$name=$fullname
  else
    fail "not found"
  fi
  }

check_f90module() {
  local module
  cd $TMPDIR
  rm * 2> /dev/null
  cp $LOCALDIR/$TOOLSCONF/module.f90 .
  $F90C -c module.f90 > /dev/null 2>&1
  if [ $? ] ; then
    module=$(ls modulename.* 2> /dev/null)
    if [ -n "$module" ] ; then
      F90modext=${module#*.}
      F90modcase=lower
    else
      module=$(ls MODULENAME.* 2> /dev/null)
      if [ -n "$module" ] ; then
        F90modext=${module#*.}
        F90modcase=upper
      fi
    fi
    if [ -n "$F90modext" ] ; then
      success "extension $F90modext"
      success "$F90modcase-case name"
      export F90modext F90modcase
    else
      fail "no module output found"
    fi
  else
    fail "error when compiling"
  fi
  cd $LOCALDIR
  }


### BASIC CHECK ###

LOCALDIR=$PWD
TMPDIR=/tmp/configure.$$
mkdir $TMPDIR
trap "cd $LOCALDIR ; rm -Rf $TMPDIR ; exit 1" 0 2

check "native system"               system
check "CPU model"                   proc
check "echo command"                echo
check "diff command"                diff
check "fortran 90 compiler"         f90compiler
#
# EXTLIBS="blas lapack cgns metis mpich mpi"
EXTLIBS="cgns metis mpich mpi"
for lib in $EXTLIBS ; do
  check "static library $lib" library $lib a
done
check "$F90C optimization options"  f90opti
check "$F90C debug        options"  f90debug
check "$F90C profiling    options"  f90prof

### DEEPER CHECK ###

if [ -n "$F90C" ] ; then
  check "fortran 90 module creation" f90module
fi



### REVIEW ###

[[ -z "$F90C" ]]         && error   "no fortran compiler found: impossible to build TYPHON"
#[[ -z "$LIB_blas"   ]]   && error   "BLAS   not available: impossible to build TYPHON"
#[[ -z "$LIB_lapack" ]]   && error   "LAPACK not available: impossible to build TYPHON"
[[ -z "$LIB_cgns"   ]]   && error   "CGNS   not available: impossible to build TYPHON"
[[ -z "$LIB_metis"  ]]   && error   "METIS  not available: TYPHON will not feature automatic distribution"
[[ -z "$LIB_mpich"  ]] && 
  [[ -z "$LIB_mpi"    ]] && warning "MPI    not available: TYPHON will not feature parallel computation"

echo Configuration ended

### SHELL CONFIGURATION ###
echo Writing Shell configuration...
rm $SHELLCONF 2> /dev/null
for VAR in SYS PROC DIFF ; do
  echo export $VAR=\"$(printenv $VAR)\" >> $SHELLCONF
done

### MAKEFILE CONFIGURATION ###
echo Writing Makefile configuration...
rm $MAKECONF 2> /dev/null
echo "SHELL       = $SHELL"                         >> $MAKECONF
echo "MAKEDEPENDS = Util/make_depends $F90modcase"  >> $MAKECONF
echo "MOD         = $F90modext"                     >> $MAKECONF
echo "CF          = $F90C"                          >> $MAKECONF
echo "FB          = -I\$(PRJINC)"                   >> $MAKECONF
echo "FO_debug    = $F90_DEBUG"                     >> $MAKECONF
echo "FO_opt      = $F90_OPTI"                      >> $MAKECONF
echo "FO_prof     = $F90_PROF"                      >> $MAKECONF
echo "FO_         = \$(FO_opt)"                     >> $MAKECONF
echo "FO          = \$(FO_\$(OPT))"                 >> $MAKECONF
echo "LINKER      = \$(CF)"                         >> $MAKECONF
echo "LINKSO      = \$(CF) -shared"                 >> $MAKECONF
echo "METISLIB    = $LIB_metis"                     >> $MAKECONF
echo "CGNSLIB     = $LIB_cgns"                      >> $MAKECONF
echo "LAPACKLIB   = $LIB_lapack $LIB_blas"          >> $MAKECONF
echo "MPILIB      = $LIB_mpich"                     >> $MAKECONF
echo Done
echo ------------------------------------------------------------------
echo
echo "to build TYPHON : cd SOURCE ; gmake clean ; gmake all OPT=opt"
echo
