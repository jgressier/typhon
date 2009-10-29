##!/bin/sh

echo ------------------------------------------------------------------
echo TYPHON configuration
echo ------------------------------------------------------------------

TOOLSCONF=TOOLS/configure

# output files
MAKECONF=config/arch.make
SHELLCONF=bin/shconf.sh

# Initialization
. $TOOLSCONF/conf_init.sh

ALLMPILIB="mpi mpich lampi mpi_f90"
ALLPATH="(echo $TYPHONPATH | sed 's/:/ /g' ) /usr /usr/local /opt /opt/local"

# CHECK system tools

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
  local namelist="$F90 ifort ifc pgf90 lfc pathf90 af90 f90 f95 sxf90 g95 gfortran"
  for exe in $namelist ; do
    F90C=$(which $exe 2> /dev/null)
    if [ -n "$F90C" ] ; then
      success $F90C
      F90C=${F90C##*/}
      case $SYS-$F90C in
	  *ifc|*ifort) F90_FB="-fPIC -implicitnone -convert big_endian" ;;
	  *gfortran)   F90_FB="-fPIC -fimplicit-none -fconvert=big-endian -ffree-line-length-none" ;;
	  *)           F90_FB="" ;;
      esac
      export F90_FB
      check "$F90C fortran 90 conformance" f90conformance "$F90_FB"
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
  f90options="$@"
  cd $TMPDIR
  rm * 2> /dev/null
  cp $LOCALDIR/$TOOLSCONF/f90conformance.f90 .
  $F90C $f90options f90conformance.f90 -o f90test >> $conflog 2>&1
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
    *ifc|*ifort) F90_OPTI="-O3" ;;
    *pgf90)      F90_OPTI="-fastsse -Munroll=n:4 -Mipa=fast,inline" ;;
    *pathf90)    F90_OPTI="-Ofast" ;;
    *gfortran)   F90_OPTI="-O3" ;;
    *g95)        F90_OPTI="-ffast-math -funroll-loops -O3" ;;
    *af90)       F90_OPTI="-Ofast -fast_math" ;;
    *lfc)        F90_OPTI="--fast" ;;
    IRIX*f90)    F90_OPTI="-03" ;;
    HP-UX-f90)   F90_OPTI="-03" ;;
    *)           F90_OPTI="-03" ;;
  esac
  export F90_OPTI
  success "$F90_OPTI"
  check "$F90C optimization" f90conformance "$F90_FB $F90_OPTI"
  }

check_f90debug() {
  case $SYS-$F90C in
    *ifc|*ifort) F90_DEBUG="-g -traceback -CB" ;;
    *)           F90_DEBUG="-g" ;;
  esac
  export F90_DEBUG
  success "$F90_DEBUG"
  check "$F90C debug" f90conformance "$F90_FB $F90_DEBUG"
  }

check_f90prof() {
  case $SYS-$F90C in
    *)           F90_PROF="$F90_OPTI -pg" ;;
  esac
  export F90_PROF
  success "$F90_PROF"
  check "$F90C profiling" f90conformance "$F90_FB $F90_PROF"
  }

check_library() {
  local    name=$1
  local     ext=$2
  local pathlib=$ALLPATH
  local fullname
  local libname=lib$name.$ext
  for dir in $pathlib ; do
    if [ -r "$dir/lib/$libname" ] ; then
      fullname=$dir/lib/$libname
      break
    fi
  done
  if [ -n "$fullname" ] ; then
    success $fullname
    export LIB_$name=$fullname
  else
    fail "not found: $libname"
  fi
  }

check_include() {
  local    name=$1
  local pathlib=$ALLPATH
  local fullname
  for dir in $pathlib ; do
    if [ -r "$dir/include/$name" ] ; then
      fullname=$dir/include/$name
      break
    fi
  done
  if [ -n "$fullname" ] ; then
    success $fullname
    rm SOURCE/Include/$name 2> /dev/null
    ln -s $fullname SOURCE/Include
  else
    fail "not found: $name"
  fi
  }

check_mpilib() {
  for name in $ALLMPILIB ; do
    if [ -n "$(printenv LIB_$name)" ] ; then
      success lib$name
      eval MPILIB=\$LIB_$name
      export MPILIB
      break
    fi
  done
  if [ -z "$MPILIB" ] ; then
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
conflog=$LOCALDIR/configure.log
rm $conflog 2> /dev/null

check "native system"               system
check "CPU model"                   proc
check "echo command"                echo
check "diff command"                diff
check "fortran 90 compiler"         f90compiler
#
# EXTLIBS="blas lapack cgns metis mpich mpi"
EXTLIBS="cgns metis $ALLMPILIB"
for lib in $EXTLIBS ; do
  check "static library $lib" library $lib a
done
EXTINC="cgnslib_f.h mpif.h"
for inc in $EXTINC ; do
  check "include file $inc" include $inc
done
check "MPI library"                 mpilib
check "$F90C optimization options"  f90opti
check "$F90C debug options"  f90debug
check "$F90C profiling options"  f90prof

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
[[ -z "$MPILIB"     ]]   && warning "MPI    not available: TYPHON will not feature parallel computation"

echo Configuration ended

### SHELL CONFIGURATION ###
echo Writing Shell configuration \($SHELLCONF\)...
rm $SHELLCONF 2> /dev/null
for VAR in SYS PROC DIFF ; do
  echo export $VAR=\"$(printenv $VAR)\" >> $SHELLCONF
done

### MAKEFILE CONFIGURATION ###
echo Writing Makefile configuration \($MAKECONF\)...
mv $MAKECONF $MAKECONF.bak 2> /dev/null  # if it exists
{
  echo "# This file was created by $(basename $0)"
  echo
  echo "SHELL       = $SHELL"
  echo "MAKEDEPENDS = \$(PRJDIR)/../TOOLS/make_depends $F90modcase"
  echo "MODEXT      = $F90modext"
  echo "FB          = $F90_FB -I\$(PRJINCDIR)"
  echo "FO_debug    = $F90_DEBUG"
  echo "FO_opt      = $F90_OPTI"
  echo "FO_prof     = $F90_PROF"
  echo "FO_         = \$(FO_opt)"
  echo "F90OPT      = \$(FO_\$(OPT))"
  echo "F90C        = $F90C \$(FB)"
  echo "LINKFB      = \$(F90OPT)"
  echo "LINKER      = \$(F90C)"
  echo "LINKSO      = \$(F90C) -shared"
  echo "METISLIB    = $LIB_metis"
  echo "CGNSLIB     = $LIB_cgns"
  echo "MPILIB      = $MPILIB"
  echo "#MPIOPT      = \$(mpif90 --showme:compile)"
  echo "#MPILIB      = \$(mpif90 --showme:link)"
} > $MAKECONF
echo Done
echo ------------------------------------------------------------------
echo
echo "to build TYPHON : cd SOURCE ; gmake clean ; gmake all OPT=opt"
echo
