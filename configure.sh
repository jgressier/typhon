##!/bin/sh

echo ------------------------------------------------------------------
echo TYPHON configuration
echo ------------------------------------------------------------------

TOOLSCONF=TOOLS/configure
MAKECONF=SOURCE/defvar.make

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

check_f90compiler() {
  local namelist="f90 ifort ifc pgf g95"
  for exe in $namelist ; do
    F90C=$(which $exe 2> /dev/null)
    if [ -n "$F90C" ] ; then
      break
    fi
  done
  if [ -n "$F90C" ] ; then
    success $F90C
  else
    fail "not found"
  fi
  }

check_library() {
  local    name=$1
  local     ext=$2
  local pathlib="/usr/lib /usr/local/lib /opt/lib /opt/local/lib"
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

check "native system"               system
check "CPU model"                   proc
check "fortran 90 compiler"         f90compiler
#
for lib in blas lapack cgns metis mpich mpi; do
  check "static library $lib" library $lib a
done

### DEEPER CHECK ###

LOCALDIR=$PWD
TMPDIR=/tmp/configure.$$
mkdir $TMPDIR
trap "cd $LOCALDIR ; rm -Rf $TMPDIR ; exit 1" 0 2

if [ -n "$F90C" ] ; then
  check "fortran 90 module creation" f90module
fi



### REVIEW ###

[[ -z "$F90C" ]]         && error   "no fortran compiler found: impossible to build TYPHON"
[[ -z "$LIB_blas"   ]]   && error   "BLAS   not available: impossible to build TYPHON"
[[ -z "$LIB_lapack" ]]   && error   "LAPACK not available: impossible to build TYPHON"
[[ -z "$LIB_cgns"   ]]   && error   "CGNS   not available: impossible to build TYPHON"
[[ -z "$LIB_metis"  ]]   && error   "METIS  not available: TYPHON will not feature automatic distribution"
[[ -z "$LIB_mpich"  ]] && 
  [[ -z "$LIB_mpi"    ]] && warning "MPI    not available: TYPHON will not feature parallel computation"

echo Configuration ended
echo Writing Makefile configuration...
rm $MAKECONF 2> /dev/null
echo "SHELL       = $SHELL"                         >> $MAKECONF
echo "MAKEDEPENDS = Util/make_depends_$F90modcase"  >> $MAKECONF
echo "MOD         = $F90modext"                     >> $MAKECONF
echo "CF          = ${F90C##*/}"                     >> $MAKECONF
echo "FB          = -I\$(PRJINC)"                   >> $MAKECONF
echo "FO_debug    = "                               >> $MAKECONF
echo "FO_opt      = -O3"                            >> $MAKECONF
echo "FO_prof     = \$(FO_opt) -pg"                 >> $MAKECONF
echo "FO_         = \$(FO_opt)"                     >> $MAKECONF
echo "FO          = \$(FO_\$(OPT))"                 >> $MAKECONF
echo "LINKER      = \$(CF)"                         >> $MAKECONF
echo "LINKSO      = \$(CF) -shared"                 >> $MAKECONF
echo "METISLIB    = $LIB_metis"                     >> $MAKECONF
echo "CGNSLIB     = $LIB_cgns"                      >> $MAKECONF
echo "LAPACKLIB   = $LIB_lapack $LIB_blas"          >> $MAKECONF
echo "MPILIB      = $LIB_mpich"                     >> $MAKECONF
echo Done
echo
echo "to build TYPHON : cd SOURCE ; gmake clean ; gmake all OPT=opt"

