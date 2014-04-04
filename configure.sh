#!/bin/bash -u

command_header() {
  echo "# This file was created automatically"
  echo "# command:"
  echo "#   $0${1:+ ${@}}"
  echo "# environment variables:"
  echo "#   TYPHONPATH=$TYPHONPATH"
  echo "#   SHELL=${SHELL:-}"
}

# Header

line() {
  printf "%$1s\n" "" | tr ' ' "$2"
}

line 72 "=" # "========================================================================"
echo "TYPHON configuration"
line 72 "=" # "========================================================================"

configure_help() {
  local dir dirlist
  echo "  OPTIONS:"
  echo "    -h:     print this help"
  echo "    -n:     * if config files do not exist:"
  echo "                 config files are created"
  echo "            * if config files exist:"
  echo "                 config files are not overwritten"
  echo "                 new files are created and diffs are shown"
  line 72 "=" # "========================================================================"
  echo
  echo "TYPHON configuration help"
  echo
  echo "  The TYPHONPATH variable should be set to find external libraries and include files:"
  echo "    * libraries     will be searched in \$TYPHONPATH/lib"
  echo "    * include files will be searched in \$TYPHONPATH/include"
  echo
  echo "  This can be done e.g. if the DAEPDIR variable is set by:"
  echo
  echo "      export TYPHONPATH=\$DAEPDIR/x86_64-Linux"
  echo "  or:"
  echo "      TYPHONPATH=\$DAEPDIR/x86_64-Linux $0"
  echo
  case "${DAEPDIR:-}" in
    "") echo "  The DAEPDIR variable is currently unset"
        dirlist=( /usr/local/aero /opt/aero )
        for dir in "${dirlist[@]}" "" ; do
          if [ -z "$dir" ] ; then
            printf "  no relevant directory found in"
            printf " '%s'" "${dirlist[@]}"
            break
          fi
          test -d "$dir" && echo "  it could be set to '$dir'" && break
          continue
        done ;;
    *)  echo "  The DAEPDIR variable is currently set to '$DAEPDIR'" ;;
  esac
  echo
  line 72 "=" # "========================================================================"
  }

TOOLSCONF=TOOLS/configure

# Initialization
# . $(dirname $0)/$TOOLSCONF/conf_init.sh
. $TOOLSCONF/conf_init.sh

new=
while [ ${#} -gt 0 ] ; do
  case "$1" in
    -h) configure_help ; exit ;;
    -n) new=1 ;;
    *)  error "unknown option: '$1'" ;;
  esac
  shift
done

# >> USE MPIF90 COMPILER
# :: USE MPIF90 COMPILER # ALLMPILIB="mpi mpich lampi mpi_f90"
# << USE MPIF90 COMPILER
IFS=":" read -a ALLPATHS <<< "${TYPHONPATH:=}"
ALLPATHS+=( "/usr" "/usr/local" "/opt" "/opt/local" )
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
  local diffcom difflist="ndiff diff"
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
  local exe exelist="${MPIF90:-} mpif90"
  for exe in $exelist ; do
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
      export MPIF90_V=$($MPIF90C --showme:version 2>&1)
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
  local exe exelist="${F90:-} ifort ifc pgf90 lfc pathf90 af90 sxf90 f90 f95 g90 g95 gfortran"
  for exe in $exelist ; do
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
  rm -f $TMPDIR/*
  cd $TMPDIR
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
    fail "compilation error"
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
    *ifc|*ifort) F90_DEBUG="-g -traceback -check -fpe0" ;;
    *)           F90_DEBUG="-g -fbacktrace -fcheck=all" ;;
  esac
  export F90_DEBUG
  success "$F90_DEBUG"
  check "$F90C debug" f90conformance "$F90_FB $F90_DEBUG"
  }

check_f90prof() {
  case $SYS-$F90C in
    *)           F90_PROFIL="$F90_OPTIM -p" ;;
  esac
  export F90_PROFIL
  success "$F90_PROFIL"
  check "$F90C profiling" f90conformance "$F90_FB $F90_PROFIL"
  }

check_library() {
  local    name=$1
  local     ext=$2
  local fullname=
  local libname=lib$name.$ext
  for fullname in "${ALLPATHS[@]/%//lib/$libname}" ; do
    if [ -r "$fullname" ] ; then
      export LIB_$name=$fullname
      export FP_$name=-D$(echo $name | tr a-z A-Z)
      success $fullname
      return
    fi
  done
  export LIB_$name=
  export FP_$name=
  fail "not found: $libname"
  }

check_include() {
  local    name=$1
  local    base=${name%.?*}
  local fullname=
  local     dir=
  for fullname in "${ALLPATHS[@]/%//include/$name}" ; do
    if [ -r "$fullname" ] ; then
      for dir in $INCLUDEPATH ; do
        rm -f $dir/$name
        ln -s $fullname $dir
      done
      export INC_$base=$fullname
      success $fullname
      return
    fi
  done
  export INC_$base=
  fail "not found: $name"
  }

# >> USE MPIF90 COMPILER
# :: USE MPIF90 COMPILER # check_mpilib() {
# :: USE MPIF90 COMPILER #   for name in $ALLMPILIB ; do
# :: USE MPIF90 COMPILER #     if [ -n "$(printenv LIB_$name)" ] ; then
# :: USE MPIF90 COMPILER #       success lib$name
# :: USE MPIF90 COMPILER #       eval MPILIB=\$LIB_$name
# :: USE MPIF90 COMPILER #       export MPILIB
# :: USE MPIF90 COMPILER #       break
# :: USE MPIF90 COMPILER #     fi
# :: USE MPIF90 COMPILER #   done
# :: USE MPIF90 COMPILER #   if [ -z "$MPILIB" ] ; then
# :: USE MPIF90 COMPILER #     fail "not found"
# :: USE MPIF90 COMPILER #   fi
# :: USE MPIF90 COMPILER #   }
# << USE MPIF90 COMPILER

check_f90module() {
  local charcase modulename module
  rm -f $TMPDIR/*
  cd $TMPDIR
  cp $LOCALDIR/$TOOLSCONF/module.f90 .
  tocase() { tr '[:lower:][:upper:]' "[:$1:][:$1:]" ;}
  $F90C -c module.f90 > /dev/null 2>&1
  if [ $? -eq 0 ] ; then
    for charcase in lower upper ; do
      modulename=$(echo modulename | tocase $charcase)
      module=$(ls $modulename.* 2> /dev/null)
      if [ -n "$module" ] ; then
        F90modext=${module#*.}
        F90modcase=$charcase
        break
      fi
    done
    if [ -n "$F90modext" ] ; then
      success "$F90modcase-case name  with  '.$F90modext' extension"
      export F90modext F90modcase
    else
      fail "no module output found"
      FTN_ERR=1
    fi
  else
    fail "compilation error"
    FTN_ERR=1
  fi
  cd $LOCALDIR
  }


### BASIC CHECK ###

LOCALDIR=$PWD
TMPDIR=/tmp/configure.$$
mkdir $TMPDIR
trap "cd $LOCALDIR ; rm -Rf $TMPDIR ; exit 1" 0 2
conflog=$LOCALDIR/configure${new:+-new}.log
confout=$LOCALDIR/configure${new:+-new}.out
if [ -n "$new" ] ; then
  conflogold=$LOCALDIR/configure.log
  confoutold=$LOCALDIR/configure.out
fi
rm -f $conflog
rm -f $confout
if [ -n "$new" ] ; then
  confdiff=$LOCALDIR/configure.diff
  rm -f $confdiff
fi

command_header > $conflog

# output files
MAKECONF=config/arch${new:+-new}.make
SHELLCONF=bin/shconf${new:+-new}.sh
if [ -n "$new" ] ; then
  MAKECONFOLD=config/arch.make
  SHELLCONFOLD=bin/shconf.sh
fi

# Perform checks

{
warning=0
# start of log (tee)redirection

check "native system"           system
check "CPU model"               proc
check "echo command"            echo
check "diff command"            diff
check "fortran 90 compiler"     f90compiler
#
# EXTLIBS="blas lapack cgns metis mpich mpi"
# >> USE MPIF90 COMPILER
EXTLIBS="cgns metis" # :: USE MPIF90 COMPILER # "cgns metis $ALLMPILIB"
# << USE MPIF90 COMPILER
for lib in $EXTLIBS ; do
  check "static library $lib" library $lib a
done
# >> USE MPIF90 COMPILER
EXTINC="cgnslib_f.h" # :: USE MPIF90 COMPILER # "cgnslib_f.h mpif.h"
# << USE MPIF90 COMPILER
for inc in $EXTINC ; do
  check "include file $inc" include $inc
done
export INC_cgns=$INC_cgnslib_f
# >> USE MPIF90 COMPILER
# :: USE MPIF90 COMPILER # check "MPI library"                 mpilib
# << USE MPIF90 COMPILER
if [ -n "$F90C" ] ; then
  check "$F90C optimization options"  f90opti
  check "$F90C debug options"         f90debug
  check "$F90C profiling options"     f90prof
  check "mpif90 compiler and options" mpif90compiler
fi

### DEEPER CHECK ###

if [ -n "$F90C" ] ; then
  check "fortran 90 module creation" f90module
fi

### REVIEW ###

if [ -n "$TYPHONPATH" ] ; then
  line 72 "-" # "------------------------------------------------------------------------"
  echo "run with: TYPHONPATH=$TYPHONPATH"
fi

impossible="impossible to build TYPHON"
no_feature="TYPHON will not feature"

[[ -z "${F90C:-}" ]]       && error   "no fortran compiler found: $impossible"
[[ -n "${FTN_ERR:-}" ]]    && error   "fortran compile/run error: $impossible"
#[[ -z "${LIB_blas:-}"   ]] && error   "BLAS   not available: $impossible"
#[[ -z "${LIB_lapack:-}" ]] && error   "LAPACK not available: $impossible"
[[ -z "${INC_cgns:-}"   ]] && export LIB_cgns= && export FP_cgns=
[[ -z "${LIB_cgns:-}"   ]] && warning "CGNS   not available: $no_feature CGNS i/o file format"
[[ -z "${LIB_metis:-}"  ]] && warning "METIS  not available: $no_feature automatic distribution"
# >> USE MPIF90 COMPILER
# [[ -z "${MPILIB:-}"     ]] && warning "MPI    not available: $no_feature parallel computation"
# << USE MPIF90 COMPILER
[[ -n "${MPIF90CMP:-}"  ]] && warning "MPI/fortran compilers are $MPIF90CMP/$F90C"

### SHELL CONFIGURATION ###
line 72 "-" # "------------------------------------------------------------------------"
printf "%s" "Writing Shell configuration    ($SHELLCONF)    ..."
rm -f $SHELLCONF
for VAR in SYS PROC DIFF ; do
  echo "export $VAR=\"$(printenv $VAR)\"" >> $SHELLCONF
done
echo "  Done"

### MAKEFILE CONFIGURATION ###
line 72 "-" # "------------------------------------------------------------------------"
printf "%s" "Writing Makefile configuration ($MAKECONF) ..."
test -z "$new" && \
mv $MAKECONF $MAKECONF.bak 2> /dev/null  # if it exists
{
  optdefault="optim"
  optnames=( )         ; optshort=( )               ; optvalue=( )
  optnames+=( debug  ) ; optshort+=( "deb dbg"    ) ; optvalue+=( "$F90_DEBUG"  )
  optnames+=( optim  ) ; optshort+=( "opt"        ) ; optvalue+=( "$F90_OPTIM"  )
  optnames+=( profil ) ; optshort+=( "prof gprof" ) ; optvalue+=( "$F90_PROFIL" )
  command_header
  echo
  # echo "${SHELL:+# }SHELL       = ${SHELL:-}"
  echo "SHELL       = /bin/sh"
  echo "MAKEDEPENDS = \$(PRJDIR)/../TOOLS/make_depends $F90modcase"
  echo "MODEXT      = $F90modext"
  echo "FPMETIS     = $FP_metis"
  echo "METISLIB    = $LIB_metis"
  echo "FPCGNS      = $FP_cgns"
  echo "CGNSLIB     = $LIB_cgns"
  echo "FOPP        = \$(FPMETIS) \$(FPCGNS)"
  echo "FB          = $F90_FB \$(FOPP) -I\$(PRJINCDIR)"
  echo "OPTIONS     = ${optnames[@]}"
  for iopt in ${!optnames[@]} ; do name=${optnames[iopt]}
    printf "%-11s = %s\n" \
       "FO_$name"    "${optvalue[iopt]}"
  done
  for iopt in ${!optnames[@]} ; do name=${optnames[iopt]}
   for short in ${optshort[iopt]} $name ; do
    printf "%-11s = %s\n" \
       "opt.$short"  "$name"
   done
  done
  echo "opt.        = \$(opt.$optdefault)"
  echo "FO_         = \$(FO_$optdefault)"
  echo "F90OPT      = \$(FO_\$(optext))"
  echo "F90CMP      = $F90C"
  echo "F90C        = \$(F90CMP) \$(FB)"
  echo "LINKFB      = \$(F90OPT)"
  echo "LINKER      = \$(F90C)"
  echo "LINKSO      = \$(F90C) -shared"
  echo "OMPOPT      = $F90_OPENMP"
# >> USE MPIF90 COMPILER
# :: USE MPIF90 COMPILER #   echo "MPILIB      = $MPILIB"
# << USE MPIF90 COMPILER
  echo "MPIF90CMP   = $MPIF90C -DMPICOMPIL"
  echo "MPIF90C     = \$(MPIF90CMP) \$(FB)"
  echo "#MPIF90_FC   = $MPIF90_FC"
  echo "#MPIF90_FL   = $MPIF90_FL"
  echo "#MPIF90_INCD = $MPIF90_INCD"
  echo "#MPIF90_LIBD = $MPIF90_LIBD"
  echo "#MPIF90_LIBS = $MPIF90_LIBS"
} > $MAKECONF
echo "  Done"

line 72 "=" # "========================================================================"
echo "to build TYPHON : make all"
line 72 "=" # "========================================================================"
if [ $warning = 1 ] ; then
  echo
  line 72 "=" # "========================================================================"
  echo "$0 did not execute correctly and warnings were printed"
  line 72 "=" # "========================================================================"
  configure_help
  echo "$0 did not execute correctly and warnings were printed"
  line 72 "=" # "========================================================================"
fi

# End of log (tee)redirection
} | tee $conflog

# PIPESTATUS because of tee
if [ ${status:=${PIPESTATUS[0]}} != 0 ] ; then
  exit $status
fi

if [ -n "$new" ] ; then
  allnoexist=1
  for fileconfname in SHELLCONF MAKECONF ; do
    fileoldname=${fileconfname}OLD
    fileconf=${!fileconfname}
    fileold=${!fileoldname}
    echo
    if [ -f $fileold ] ; then
      allnoexist=0
      com="diff $fileconf $fileold"
      r=$($com)
      if [ -n "$r" ] ; then
        echo "Differences between $fileconf and $fileold"
        { echo
          echo "$com"
          echo "$r"
        } >> $confdiff
        fdiff=1
      else
        rm -f $fileconf
        echo "No differences between $fileconf (deleted) and $fileold"
      fi
    else
      echo "$fileconf renamed to $fileold"
      mv $fileconf $fileold
    fi
  done
  if [ $allnoexist = 1 ] ; then
    mv $conflog $conflogold
    mv $confout $confoutold
  fi
  echo
  if [ -f "$confdiff" ] ; then
    echo "Differences in:"
    echo "  $confdiff"
  fi
  echo
fi

