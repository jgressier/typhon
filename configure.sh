##!/bin/bash -u

echo "------------------------------------------------------------------------"
echo "TYPHON configuration"
echo "------------------------------------------------------------------------"

TOOLSCONF=TOOLS/configure

# output files
MAKECONF=config/arch.make
SHELLCONF=bin/shconf.sh

# Initialization
. $TOOLSCONF/conf_init.sh

ALLMPILIB="mpi mpich lampi mpi_f90"
TYPHONPATH=${TYPHONPATH:-}
ALLPATH="$(echo $TYPHONPATH | sed 's/:/ /g' ) /usr /usr/local /opt /opt/local"
INCLUDEPATH="include"

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

check_mpif90compiler() {
  local namelist="$MPIF90 mpif90"
  for exe in $namelist ; do
    MPIF90C=$(which $exe 2> /dev/null)
    if [ -n "$MPIF90C" ] ; then
      success $MPIF90C
      MPIF90C=${MPIF90C##*/}
      MPIF90CMP=$($MPIF90C --showme:command)
      if [ $MPIF90CMP = $F90C ] ; then
        MPIF90CMP=
      fi
      export MPIF90_FC=$($MPIF90C --showme:compile)
      export MPIF90_FL=$($MPIF90C --showme:link)
      export MPIF90_INCD=$($MPIF90C --showme:incdirs)
      export MPIF90_LIBD=$($MPIF90C --showme:libdirs)
      export MPIF90_LIBS=$($MPIF90C --showme:libs)
      export MPIF90_V=$($MPIF90C --showme:version)
    fi
  done
  if [ -n "$MPIF90C" ] ; then
    export MPIF90C
    export MPIF90CMP
  else
    fail "not found"
  fi
}

check_f90compiler() {
  local namelist="${F90:-} ifort ifc pgf90 lfc pathf90 af90 sxf90 f90 f95 g90 g95 gfortran"
  for exe in $namelist ; do
    F90C=$(which $exe 2> /dev/null)
    if [ -n "$F90C" ] ; then
      success $F90C
      F90C=${F90C##*/}
      case $SYS-$F90C in
	  *ifc|*ifort)           F90_FB="-fpp -fPIC -implicitnone" ;;
	  *gfortran|*-[fg]9[05]) F90_FB="-cpp -fPIC -fimplicit-none -ffree-line-length-none" ;;
	  *)                     F90_FB="" ;;
      esac
      export F90_FB
      export F90_V=$($F90C --version)
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
  local f90options command success
  f90options="$@"
  cd $TMPDIR
  rm * 2> /dev/null
  cp $LOCALDIR/$TOOLSCONF/f90conformance.f90 .
  command="$F90C $f90options f90conformance.f90 -o f90test"
  echo $command >> $confout 2>&1
  eval $command >> $confout 2>&1
  if [ $? -eq 0 ] ; then
    success="successfully compiled"
    ./f90test > /dev/null 2>&1
    if [ $? -eq 0 ] ; then
      success "$success and checked"
    else
      fail "$success but check failed"
      FTN_ERR=1
    fi
  else
    fail "compiler error"
    FTN_ERR=1
  fi
  cd $LOCALDIR
  }

check_f90opti() {
  case $SYS-$F90C in
    *ifc|*ifort) F90_OPTIM="-O3" ; F90_OPENMP="-openmp" ;;
    *pgf90)      F90_OPTIM="-fastsse -Munroll=n:4 -Mipa=fast,inline" ;;
    *pathf90)    F90_OPTIM="-Ofast" ;;
    *gfortran)   F90_OPTIM="-O3" ;;
    *g95)        F90_OPTIM="-ffast-math -funroll-loops -O3" ;;
    *af90)       F90_OPTIM="-Ofast -fast_math" ;;
    *lfc)        F90_OPTIM="--fast" ;;
    IRIX*f90)    F90_OPTIM="-O3" ;;
    HP-UX-f90)   F90_OPTIM="-O3" ;;
    *)           F90_OPTIM="-O3" ;;
  esac
  export F90_OPTIM
  success "$F90_OPTIM"
  check "$F90C optimization" f90conformance "$F90_FB $F90_OPTIM"
  }

check_f90debug() {
  case $SYS-$F90C in
    *ifc|*ifort) F90_DEBUG="-g -traceback -CB -CU" ;;
    *)           F90_DEBUG="-g -fbacktrace -fcheck=all" ;;
  esac
  export F90_DEBUG
  success "$F90_DEBUG"
  check "$F90C debug" f90conformance "$F90_FB $F90_DEBUG"
  }

check_f90prof() {
  case $SYS-$F90C in
    *)           F90_PROFIL="$F90_OPTIM -pg" ;;
  esac
  export F90_PROFIL
  success "$F90_PROFIL"
  check "$F90C profiling" f90conformance "$F90_FB $F90_PROFIL"
  }

check_library() {
  local    name=$1
  local     ext=$2
  local pathlib="$ALLPATH"
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
  local pathlib="$ALLPATH"
  local fullname
  for dir in $pathlib ; do
    if [ -r "$dir/include/$name" ] ; then
      fullname=$dir/include/$name
      break
    fi
  done
  if [ -n "$fullname" ] ; then
    success $fullname
    for dir in $INCLUDEPATH ; do
      rm $dir/$name 2> /dev/null
      ln -s $fullname $dir
    done
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
  if [ $? -eq 0 ] ; then
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
      success "$F90modcase-case name  with  '.$F90modext' extension"
      export F90modext F90modcase
    else
      fail "no module output found"
      FTN_ERR=1
    fi
  else
    fail "error when compiling"
    FTN_ERR=1
  fi
  cd $LOCALDIR
  }


### BASIC CHECK ###

LOCALDIR=$PWD
TMPDIR=/tmp/configure.$$
mkdir $TMPDIR
trap "cd $LOCALDIR ; rm -Rf $TMPDIR ; exit 1" 0 2
conflog=$LOCALDIR/configure.log
confout=$LOCALDIR/configure.out
rm $conflog 2> /dev/null
rm $confout 2> /dev/null

{

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
check "$F90C debug options"         f90debug
check "$F90C profiling options"     f90prof
check "mpif90 compiler and options" mpif90compiler

### DEEPER CHECK ###

if [ -n "$F90C" ] ; then
  check "fortran 90 module creation" f90module
fi



### REVIEW ###

if [ -n "$TYPHONPATH" ] ; then
  echo "------------------------------------------------------------------"
  echo "run with: TYPHONPATH=$TYPHONPATH"
fi

impossible="impossible to build TYPHON"
no_feature="TYPHON will not feature"

[[ -z "$F90C" ]]       && error   "no fortran compiler found: $impossible"
[[ -n "$FTN_ERR" ]]    && error   "fortran compile/run error: $impossible"
#[[ -z "$LIB_blas"   ]] && error   "BLAS   not available: $impossible"
#[[ -z "$LIB_lapack" ]] && error   "LAPACK not available: $impossible"
[[ -z "$LIB_cgns"   ]] && error   "CGNS   not available: $impossible"
[[ -z "$LIB_metis"  ]] && error   "METIS  not available: $no_feature automatic distribution"
[[ -z "$MPILIB"     ]] && warning "MPI    not available: $no_feature parallel computation"
[[ -n "$MPIF90CMP"  ]] && warning "MPI/fortran compilers are $MPIF90CMP/$F90C"

echo "------------------------------------------------------------------------"
echo "Configuration ended"

### SHELL CONFIGURATION ###
echo "------------------------------------------------------------------------"
printf "%s" "Writing Shell configuration    ($SHELLCONF)    ..."
rm $SHELLCONF 2> /dev/null
for VAR in SYS PROC DIFF ; do
  echo "export $VAR=\"$(printenv $VAR)\"" >> $SHELLCONF
done
echo "  Done"

### MAKEFILE CONFIGURATION ###
echo "------------------------------------------------------------------------"
printf "%s" "Writing Makefile configuration ($MAKECONF) ..."
mv $MAKECONF $MAKECONF.bak 2> /dev/null  # if it exists
{
  echo "# This file was created by $(basename $0)"
  echo
  echo "SHELL       = $SHELL"
  echo "MAKEDEPENDS = \$(PRJDIR)/../TOOLS/make_depends $F90modcase"
  echo "MODEXT      = $F90modext"
  echo "FB          = $F90_FB -I\$(PRJINCDIR)"
  echo "FO_debug    = $F90_DEBUG"
  echo "FO_optim    = $F90_OPTIM"
  echo "FO_openmp   = $F90_OPTIM $F90_OPENMP"
  echo "FO_profil   = $F90_PROFIL"
  echo "FO_         = \$(FO_optim)"
  echo "F90OPT      = \$(FO_\$(opt))"
  echo "F90C        = $F90C \$(FB)"
  echo "LINKFB      = \$(F90OPT)"
  echo "LINKER      = \$(F90C)"
  echo "LINKSO      = \$(F90C) -shared"
  echo "METISLIB    = $LIB_metis"
  echo "CGNSLIB     = $LIB_cgns"
  echo "MPILIB      = $MPILIB"
  echo "MPIOPT      = $MPIF90_FC"
  echo "MPILIB      = $MPIF90_FL"
} > $MAKECONF
echo "  Done"

echo "========================================================================"
echo "to build TYPHON : make all"
echo "========================================================================"

} | tee $conflog
