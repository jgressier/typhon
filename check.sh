#!/bin/bash -u
#
# Check non-regression of all NRG cases containing nrgconf.sh
#

SCRIPTDIR=$(cd $(dirname $0) ; pwd)
SCRIPTNAME=$(basename $0)
SCRIPTSPCE=${SCRIPTNAME//?/ }

# --- print bar ---
#
function ebar() {
  printf "%79s\n" | tr ' ' "${1:-=}"
}
bar=$(ebar)
function earg() {
  # Print each remaining argument on a line
  test ${#} -gt 0 && printf "%s\n" "$@"
}
function espc() {
  local s="  " # default 2 spaces
  # If numeric argument : number of spaces
  if [ -z "${1//[0-9]}" ] ; then
    s=$(printf "%$1s")
    shift
  fi
  # Print each remaining argument on a line
  test ${#} -gt 0 && printf "$s%s\n" "$@"
}

# --- print usage ---
#
function usage() {
  echo "Name:"
  echo "  $SCRIPTNAME"
  echo
  echo "Usage:"
  echo "  $SCRIPTNAME [options] [--] [pattern ...]"
  echo
  echo "Options:"
  echo "  -h|--help     prints this help"
  echo "  -f|--force    force execution of $REFCONF or force cleanup"
  echo "  -d|--diff-cmd <diff-command>"
  echo "                uses specified diff command (use quotes in case of options)"
  echo "  -e|--rel-err <rel-err>"
  echo "                uses specified relative error"
  echo "  -l|--list     prints list of cases"
  echo "  -k|--keep     keep (do not delete) TMPDIR and cases"
  echo "  -b|--build <b-dir>"
  echo "                name of build directory (default is \`../build')"
  echo "                (../<b-dir> if <b-dir> is not a path)"
  echo "                (<b-dir> if <b-dir> is a path)"
  echo "  -x|--exe <typhon-exe>"
  echo "                uses specified typhon exe (default is Typhon_Solver)" # (default is given by CASE/$REFCONF)"
  echo "                (uses build directory if given or if <typhon-exe> is not a path)"
  echo "  --cleanup     cleanup temporary directories (and exits)"
  echo "  --            end of options"
  echo "  pattern       selects cases with name matching pattern"
  echo
  echo "Default:"
  echo "  runs selected cases (stop with <ctrl>-\`)"
}

# --- print error ---
#
function error() {
  usage
  echo
  ebar @
  echo     "ERROR${1:+:}"
  espc "$@"
  ebar @
  exit 1
}

ebar
echo "TYPHON non regression check"
ebar

# --- Set directories (except TMPDIR)---
#
ORIGDIR=$PWD
TMPLXXX=XXXXXXXX
TMPTMPL=/tmp/typhon.${SCRIPTNAME%.sh}.$TMPLXXX
TMPSTRG=${TMPTMPL%$TMPLXXX}*
PRJDIR=$SCRIPTDIR
EXEDIR=$PRJDIR/SOURCE
BINDIR=$PRJDIR/bin
NRGDIR=$PRJDIR/NRG
MSHDIR=$PRJDIR/NRG/COMMON
REFCONF=nrgconf.sh
PRJDIRWIPE=
test $ORIGDIR = $PRJDIR && PRJDIRWIPE=$PRJDIR/

# --- Check directory ---
#
if [ ! -d $MSHDIR ] ; then
  error "directory \`$MSHDIR' not found" \
        "  \`$PRJDIR/$SCRIPTNAME' is not in a valid typhon directory"
fi

# --- Set options ---
#
#OPTS=$(getopt -o b:hfd:e:lkx -l build:,help,force,diff-cmd:,rel-err:,list,keep,exe,cleanup -n "$SCRIPTNAME" -- "$@")
#test $? != 0 && usage 1
#eval set -- "$OPTS"

# --- Default options ---
#
build=
force=0
typhexe=
diffcmd=
list=0
keeptmp=0
patlist=()
rel_err=
cleanup=0

mustbeequal() { test "$1" != "$2" && error "$1: $3" ;}

# --- Parse options ---
#
while [ ${#} -gt 0 ] ; do
  case "$1" in
    -h|--help)
        usage ; ebar ; exit 0 ;;
    -f|--force)
        force=1 ;;
    -d|--diff-cmd)
        test ${#} -gt 1 || error "missing argument to \`$1': diff command required"
        shift
        mustbeequal "${diffcmd:=$1}" "$1" "multiple values for diff command"
        ;;
    -e|--rel-err)
        test ${#} -gt 1 || error "missing argument to \`$1': value required"
        shift
        mustbeequal "${rel_err:=$1}" "$1" "multiple values for relative error"
        ;;
    -l|--list)
        list=1 ;;
    -k|--keep)
        keeptmp=1 ;;
    -b|--build)
        test ${#} -gt 1 || error "missing argument to \`$1': build directory required"
        shift
        mustbeequal "${build:=$1}" "$1" "multiple values for build directory"
        ;;
    -x|--exe)
        test ${#} -gt 1 || error "missing argument to \`$1': typhon executable required"
        shift
        mustbeequal "${typhexe:=$1}" "$1" "multiple values for typhon executable"
        ;;
    --cleanup)
        cleanup=1 ;;
    --) shift ; break ;;
    -*) error "$1: unknown option" ;;
    *)  patlist+=("-e" "$1")
        ;;
  esac
  shift
done

# --- Get patterns ---
#
while [ ${#} -gt 0 ] ; do
  if [ -n "$1" ] ; then
    patlist+=("-e" "$1")
  fi
  shift
done

# --- Cleanup temp directories ---
#
tmpdirlist=()
# -r : Backslash does not act as an escape character
# -d $'\0' : ascii 0 is delimiter (for find -print0)
while IFS= read -r -d $'\0' f ; do
  tmpdirlist+=( "$f" )
done < <( find /tmp -maxdepth 1 -type d -path "$TMPSTRG" -user "$USER" -print0 )
case "${#tmpdirlist[@]}" in
  0) case "$cleanup" in
       0) ;;
       1) echo "No temp directory to cleanup"
          ebar
          test $list = 0 && exit 0
          ;;
     esac
     ;;
  *) ls -l -d -F --color=always "${tmpdirlist[@]}"
     ebar
     case "$cleanup" in
       0) echo "You may consider option \`--cleanup' to remove these temporary directories"
          ebar
          ;;
       1) if [ $force != 1 ] ; then
            echo "You must execute the following command:"
            ebar
            printf "rm -rf" ; printf " \\\\\n  '%s'" "${tmpdirlist[@]}" ; echo
          else
            printf "rm -rf" ; printf         " '%s'" "${tmpdirlist[@]}" ; echo
            # echo rm -rf "${tmpdirlist[@]}"
          fi
          ebar
          test $list = 0 && exit 0
          ;;
     esac
     ;;
esac

# --- Get list of cases ---
#
LISTCASES=( $(cd $NRGDIR ; find * -name $REFCONF | xargs -n 1 dirname | grep "${patlist[@]:-.}") )

if [ ${#LISTCASES[@]} = 0 ] ; then
  echo "NO CASE FOUND"
  ebar
  exit 0
fi

# --- Print list of cases (and exit) ---
#
if [ $list -eq 1 ] ; then
  echo LISTCASES =
  espc 4 "${LISTCASES[@]}"
  ebar
  exit 0
fi

# --- Check executable ---
#
# if $typhexe is a simple name or empty, $build is used
forced=
if [ "$typhexe" = "$(basename "$typhexe")" ] ; then
  # if $build empty, it defaults to 'build'
  if [ -z "$build" ] ; then
    build=build
    forced=forced
    echo "build dir name  imposed: \`$build'"
  fi
  # if $typhexe empty, it defaults to 'Typhon_Solver'
  if [ -z "$typhexe" ] ; then
    typhexe=Typhon_Solver
    forced=forced
    echo "executable name imposed: \`$typhexe'"
  fi
  if [ -e "./$typhexe" ] ; then
    echo "Did you mean:   \`./$typhexe' ?"
  fi
fi
# if $typhexe is a path, $build is used if not empty
if [ -n "$build" ] ; then
  if [ "$build" = "$(basename "$build")" ] ; then
    build="../$build"
    echo "build directory imposed: \`$build'"
    forced=forced
  fi
fi
typhexe="${build:+$build/SOURCE/}$typhexe"
echo "${forced:-execed} with executable:  \`$typhexe'"
ebar
# echo stop forced ; exit
test ! -e "$typhexe" && error "$typhexe does not exist"
test ! -f "$typhexe" && error "$typhexe is not a regular file"
test ! -x "$typhexe" && error "$typhexe is not an executable file"
savedir=$(pwd)
typhdir=$(dirname "$typhexe")
typhexe=$(basename "$typhexe")
cd "$typhdir"
typhexe="$(pwd)/${typhexe}"
cd "$savedir"

# --- Check executable type ---
#
compiler=$(cd /tmp ; mpirun -np 1 $typhexe 2>&1 | grep Compiled | tail -1)
case "${compiler##*/}" in
  ifort)  compiler=seq ;;
  mpif90) compiler=mpi ;;
  *)      error "Unknown type for \`typhexe'" "  $compiler" ;;
esac
echo "TYPE_EXE=$compiler is imposed"
ebar
# To be fixed with test on build ...

# --- Initialization ---
#
cd $PRJDIR

# --- Set default diff command ---
#
# Choose diff command
difflist="ndiff diff"
for diffcom in $difflist ; do
  DIFF=$(which $diffcom) 2>/dev/null
  test -n "$DIFF" && break
done

# Check diff-cmd xor rel-err
if [ -n "$diffcmd" -a -n "$rel_err" ] ; then
  error "\`--diff-comd' ($diffcmd) and \`--rel-err' ($rel_err) options may not both be specified" \
        "Options to the specific diff command must be appended to it"
fi

# Set options to diff command
case $DIFF in
  */ndiff) export DIFF="$DIFF -relerr ${rel_err:-1.E-12}" ;;
  */diff)  export DIFF="$DIFF -bB" ;;
  *)       echo "Not found: DIFF=$DIFF" ; export DIFF= ;;
esac

# Check user diff command execution
if [ -n "$diffcmd" ] ; then
  $diffcmd <(          cat $0 ) <(cat $0) >/dev/null 2>&1 ; r=$?
  $diffcmd <( echo a ; cat $0 ) <(cat $0) >/dev/null 2>&1 ; r=$?$r
  test $r != 10 && error "Check diff command: $diffcmd"
  export DIFF="$diffcmd"
fi
test -z "$DIFF" && error "No diff command found"

# --- Set log files ---
#
logfiles=( diff.log check.log )
error=()
for f in "${logfiles[@]}" ; do
  test -e $f -a ! -f $f && error+=( "$f is not a regular file" )
done
test ${#error[@]} != 0 && error "${error[@]}"

rm -f "${logfiles[@]}"

# --- Print parameter ---
#
scol0=
for CASE in "${LISTCASES[@]}" ; do
  scol0=$(printf "%${#scol0}s" "${CASE//?/ }")
done
scol1=$scol0$(printf "%15s")
ncol2=24

# --- Tests ---
#
echo "diffing with DIFF : $DIFF"
ebar

function iserror() {
  test $? -ne 0 # "$@"
}

function next_case() {
  test "${1:--}" = -n && echo && remain= && shift
  test ${#}     -gt 0 && printf "$remain%s\n" "$@"
  test $keeptmp -eq 0 && rm -f $TMPDIR/$CASE/*
  continue
}

function fill_line() {
  test ${#} -gt 0 && printf "$remain%-${ncol2}s %s\n" "$@"
  remain=$scol1
}

function next_fic() {
  fill_line "$@"
  continue
}

# --- Define and create TMPDIR and trap remove if not keeptmp ---
#
if ! TMPDIR=$(mktemp -d $TMPTMPL 2>&1) ; then
  T=${TMPDIR%% \`/*}
  T1=${T%% « /*}
  T2=${TMPDIR#$T1}
  error "$T1" "  ${T2# }" "Could not create temporary directory"
fi
test $keeptmp -eq 0 && trap "rm -Rf $TMPDIR" EXIT SIGINT
trap "exit 63" SIGQUIT # ctrl-\ or ctrl-` stop the script

echo diff-command : $DIFF >> $PRJDIR/diff.log

#================================================================================
# Loop on cases
#================================================================================
for CASE in "${LISTCASES[@]}" ; do

  # --- Init ---
  #
  CASEDIR=$NRGDIR/$CASE

  # --- Print header ---
  #
  string="checking $CASE ..."
  printf "$string" ; repl=${string//?/?} ; remain=${scol1/$repl/}

  # --- Configure ---
  #
  file=$CASEDIR/$REFCONF
  . $file
  iserror && next_case -n "source NRG/$CASE/$REFCONF: command failed"

  # --- Create dir ---
  #
  mkdir -p $TMPDIR/$CASE
  iserror && next_case -n "mkdir -p $TMPDIR/$CASE: command failed"
  cd $TMPDIR/$CASE

  # --- Copy files ---
  #
  cp $MSHDIR/$MESHFILE $TMPDIR/$CASE
  cp $CASEDIR/$INPUTFILE $TMPDIR/$CASE

  # --- Create hostfile ---
  #
  hostname > hostfile

  # --- Write headers ---
  #
  echo "$bar"  >> $PRJDIR/check.log
  echo "$CASE" >> $PRJDIR/check.log
  echo '#' "$bar"  >> $PRJDIR/diff.log
  echo '#' "$CASE" >> $PRJDIR/diff.log

  # --- Check executable type ---
  #
  TYPE_EXE=$compiler
  case $TYPE_EXE in
    seq) exehead="" ;;
    mpi) exehead="mpirun -np ${MPIPROCS:-2} -machinefile hostfile" ;;
    *)   next_case "\`$TYPE_EXE': unknown typhon executable type" ;;
  esac

  # --- Check executable ---
  #
  EXE=${typhexe:-$EXEDIR/typhon-$TYPE_EXE}

  # --- Execute ---
  #
  echo run: \
  $exehead $EXE >> $PRJDIR/check.log
  $exehead $EXE >> $PRJDIR/check.log 2>&1
  iserror && next_case "??   computation failed"

  # --- Get file list ---
  #
  listfic=()
  cd $CASEDIR
  for fic in $TO_CHECK ; do listfic+=( "$fic" ) ; done
  cd $TMPDIR/$CASE

  # --- Compare files ---
  #
  for fic in "${listfic[@]}" ; do
    test -f "$CASEDIR/$fic" || next_fic "$fic" "REF file missing"
    test -f          "$fic" || next_fic "$fic" "run file missing"
    fics=( "$fic" "$CASEDIR/$fic" )
    r=$($DIFF ${fics[@]} 2>&1 >> diff.log ; echo $?)
    r+=$(diff ${fics[@]} > /dev/null 2>&1 ; echo $?)
    case $r in
      00) msg="ident" ;;
      0*) msg="equival" ;;
      11) msg="different"
          echo diff ${fics[@]} >> $PRJDIR/diff.log
          cat diff.log >> $PRJDIR/diff.log ;;
      *)  msg="comparison failed" ;;
    esac
    fill_line "$fic" "$msg"
    test $keeptmp -eq 0 && rm -f diff.log
  done
  next_case

#================================================================================
# End loop on cases
#================================================================================
done
ebar

# --- Print name of log files
#
if [ $PRJDIR = $ORIGDIR ] ; then
  echo check.log diff.log
else
  echo $PRJDIR/check.log
  echo $PRJDIR/diff.log
fi
ebar

# --- Print name of keep tmp dir
#
if [ $keeptmp -eq 1 ] ; then
  echo "files kept in   : $TMPDIR"
  ebar
fi
