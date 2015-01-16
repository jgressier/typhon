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
  printf "%79s\n" | tr ' ' "${1:--}"
}
bar=$(ebar =)
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
  echo "                (${DIFFERRMSG:-"default is \`$DIFF'"})"
  echo "  -e|--rel-err <rel-err>"
  echo "                uses specified relative error"
  echo "  -l|--list     prints list of cases"
  echo "  -k|--keep     keep (do not delete) TMPDIR and cases"
  echo "  -n|--dry-run  no execution, only checks $REFCONF files"
  echo "  -b|--build <b-dir>"
  echo "                name of build directory (default is \`../build')"
  echo "                (../<b-dir> if <b-dir> is not a path)"
  echo "                (<b-dir> if <b-dir> is a path)"
  echo "  --mpi         short for \`--build $mpidefblddir'"
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

# --- print warning ---
#
function warning() {
  echo "WARNING${1:+:}"
  espc "$@"
  ebar
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

ebar =
echo "TYPHON non regression check"
ebar =

# --- Set default diff command and options ---
#
# Choose diff command
difflist="ndiff diff"
if [ -z "${DIFF:=}" ] ; then
  for diffcom in $difflist ; do
    DIFF=$(which $diffcom 2>/dev/null)
    test -n "$DIFF" && break
  done
else
  diffcom=${DIFF%% *}
  diffopt=${DIFF#$diffcom}
  DIFF=$(which ${diffcom} 2>/dev/null)
  if [ -z "$DIFF" ] ; then
    export DIFFERRMSG="Diff command from env not found: DIFF=\`${DIFF:=$diffcom$diffopt}'"
  else
    DIFF="$DIFF$diffopt"
  fi
fi
if [ -z "$DIFF" ] ; then
  export DIFFERRMSG="No default diff command found from $(printf " \`%s'" $difflist)"
fi

# Set options to diff command
case $DIFF in
  */ndiff) export DIFF="$DIFF -relerr ${rel_err:-1.E-12}" ;;
  */diff)  export DIFF="$DIFF -bB" ;;
  "")      ;;
  *)       export DIFFERRMSG=${DIFFERRMSG:-"Default diff command unknown: \`$DIFF'"} ;;
esac

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
mpidefblddir=build_mpi

# --- Check directory ---
#
if [ ! -d $MSHDIR ] ; then
  error "directory \`$MSHDIR' not found" \
        "  \`$PRJDIR/$SCRIPTNAME' is not in a valid typhon directory"
fi

# --- Set options ---
#
#OPTS=$(getopt -o b:hfd:e:lknx \
#              -l build:,mpi,help,force,diff-cmd:,rel-err:,list,keep,dry-run,exe,cleanup -n "$SCRIPTNAME" -- "$@")
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
dry_run=0
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
    -n|--dry-run)
        dry_run=1 ;;
    -b|--build)
        test ${#} -gt 1 || error "missing argument to \`$1': build directory required"
        shift
        mustbeequal "${build:=$1}" "$1" "multiple values for build directory"
        ;;
    --mpi)
        mustbeequal "${build:=$mpidefblddir}" "$build" "multiple values for build directory ($1 implies $mpidefblddir)"
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

# --- Check dry_run and keeptmp ---
#
if [ $keeptmp = 1 -a $dry_run = 1 -a $force != 1 ] ; then
  echo "\`--keep' option is ignored with \`--dry-run' when without \`--force'"
  ebar
  keeptmp=0
fi

# Set dry run command prefix
case $dry_run in
  1) dry_run=: ;;
  *) dry_run=  ;;
esac

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
  *) ls -l -d -F --color=auto "${tmpdirlist[@]}"
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
  ifort)  exetype=seq ;;
  mpif90) exetype=mpi ;;
  *)      error "Unknown type for \`typhexe'" "  $compiler" ;;
esac
if [ $exetype = mpi ] ; then
  if [ -z "${MPIPROCS:-}" ] ; then
    mpistr="(default MPIPROCS=${MPIPROCSDEF:=1}, may be overriden by env or conf)"
  else
    mpistr="(MPIPROCS=${MPIPROCSDEF:=$MPIPROCS}, may be overriden by conf)"
  fi
else
  mpistr=
fi
echo "TYPE_EXE=$exetype is imposed${mpistr:+ $mpistr}"
ebar
# To be fixed with test on build ...

# --- Initialization ---
#
cd $PRJDIR

# --- Set user-defined diff command and options ---
#
# Check diff-cmd xor rel-err
if [ -n "$diffcmd" -a -n "$rel_err" ] ; then
  error "\`--diff-comd' ($diffcmd) and \`--rel-err' ($rel_err) options may not both be specified" \
        "Options to the specific diff command must be appended to it"
fi

# Check user diff command execution
if [ -n "$diffcmd" ] ; then
  $diffcmd <(          cat $0 ) <(cat $0) >/dev/null 2>&1 ; r=$?
  $diffcmd <( echo a ; cat $0 ) <(cat $0) >/dev/null 2>&1 ; r=$?$r
  test $r != 10 && error "Check diff command: $diffcmd"
  export DIFF="$diffcmd"
fi

# Check diff is defined
if [ -z "$DIFF" ] ; then
  error "$DIFFERRMSG"
fi

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
ebar =

function iserror() {
  test $? -ne 0 # "$@"
}

function next_case() {
  test "${1:--}" = -n && remain="\n         " && shift
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

checkrefconf() {
  grep -e '^export  *[A-Za-z_][A-Za-z0-9_]*=' $1 \
    | diff -q $1 - >/dev/null 2>&1
}

catrefconf() {
    echo "file: \`${1#$PRJDIRWIPE}'" "(between '---')"
    echo "---"
    cat $1
    echo "---"
}

# --- Read "y" or exit
#
read_y_or_exit() {
  local y ; read y ; test "$y" != y && exit 1
}

# --- Check commands in $REFCONF for all cases ---
#
halt=0
forceexec=0
for CASE in "${LISTCASES[@]}" ; do
  CASEDIR=$NRGDIR/$CASE
  file=$CASEDIR/$REFCONF
  checkrefconf $file
  if iserror ; then
    catrefconf $file
    echo "${file#$PRJDIRWIPE}: unsafe commands"
    ebar
    if [ $force = 1 ] ; then
      printf "Are you sure you want to force execution [y|N] ? "
      read_y_or_exit
      echo "Execution will be forced !"
      ebar
      forceexec=1
    else
      halt=1
    fi
  fi
done

if [ $halt = 1 ] ; then
  echo "You may consider option \`--force'"
  ebar
fi

if [ $forceexec = 1 ] ; then
  ebar '!'
  printf "Are you REALLY sure you want to force execution [y|N] ? "
  read_y_or_exit
  echo "EXECUTION WILL BE FORCED !"
  ebar
fi

# --- Define and create TMPDIR and trap remove if not keeptmp ---
#
if ! TMPDIR=$(mktemp -d $TMPTMPL 2>&1) ; then
  T=${TMPDIR%% \`/*} # For C/EN locale
  T1=${T%% « /*}     # For Fr locale (with nbsp " " after "«")
  T2=${TMPDIR#$T1}
  error "$T1" "  ${T2# }" "Could not create temporary directory"
fi
test $keeptmp -eq 0 && trap "rm -Rf $TMPDIR" EXIT SIGINT
trap "exit 63" SIGQUIT           # ctrl-\ or ctrl-` stop the script
echo "  ( ctrl-c to stop one run ; ctrl-\ or ctrl-\` to stop the whole script)"

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
  ${dry_run:+ebar} >> $PRJDIR/check.log 2>&1

  MPIPROCS=${MPIPROCSDEF:-}

  # --- Configure ---
  #
  file=$CASEDIR/$REFCONF
  checkrefconf $file
  if iserror ; then
    catrefconf $file >> $PRJDIR/check.log
    if [ $force = 1 ] ; then
      $dry_run echo "Execution forced !" >> $PRJDIR/check.log
      ${dry_run:+wontdo} . $file         >> $PRJDIR/check.log 2>&1
      iserror && next_case "$REFCONF: source command failed"
    else
      # This should not be executed (already checked and exited)
      next_case "$REFCONF: unsafe commands, consider option \`--force'"
    fi
  else
  {
    # Read every line in file
    readarray -t confline < $file
    # For each line
    for line in "${confline[@]}" ; do
      line=$(echo "$line" | sed 's/^ *export  *//')
      var=${line%=*}
      value=${line#$var=}
      # Remove enclosing quotes (\x22:double, \x27:single)
      for s in "\x22" "\x27" ; do
        nvalue=$(echo "$value" | sed "s/^$s\(.*\)$s$/\1/")
        test "$nvalue" != "$value" && break
      done
      value=$nvalue
      ${dry_run:+echo} export "$var"="$value"
    done
  } >> $PRJDIR/check.log 2>&1
  fi
  ${dry_run:+next_case "$REFCONF checked"}

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
  TYPE_EXE=$exetype
  case $TYPE_EXE in
    seq) exehead="" ;;
    mpi) exehead="mpirun -np ${MPIPROCS:-1} -machinefile hostfile" ;;
    *)   next_case "\`$TYPE_EXE': unknown typhon executable type" ;;
  esac

  # --- Check executable ---
  #
  ## THIS IS NO MORE USED
  ## EXE=${typhexe:-$EXEDIR/typhon-$TYPE_EXE}
  EXE=${typhexe}

  # --- Execute ---
  #
  echo run: \
  $dry_run $exehead $EXE >> $PRJDIR/check.log
  $dry_run $exehead $EXE >> $PRJDIR/check.log 2>&1
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
  printf "%s" "files kept in   : "
  ls -d -F --color=auto "$TMPDIR"
  ebar
fi
