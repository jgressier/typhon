#!/bin/bash -u

ext=tym

varlist() {
  local varname=$1 ; shift
  local values
  printf "${1:-$varname} ="
  varname=$varname[@]
  typeset values=( "${!varname}" )
  printf " \"%s\"" "${values[@]}"
  echo
}

error=
seterror() {
  echo "ERROR:"
  error="error"
  printf "  %s\n" "$@"
  #exit 1
}

blderror() {
  seterror "$(varlist build)" "$@"
  exit 1
}

build=()
files=()
f_out=()
isbld=0
while [ $# -ne 0 ] ; do
  case "$1" in
    "")   seterror "Empty argument"
          ;;
    --mpi|-mpi|mpi)
          build+=( build_mpi )
          ;;
    *.$ext)
          files+=( "$1" )
          ;;
    -o)   test $# -eq 1 && seterror "\`$1': argument required" && shift && continue
          shift
          f_out+=( "$1" )
          ;;
    *)    if [ -f "$1.$ext" ] ; then
            files+=( "$1" )
          else
            build+=( "$1" )
          fi
          ;;
  esac
  shift
done

case ${#files[@]}:${#f_out[@]} in
  0:*) seterror "No input files" ;;
  *:0) : ;;
  1:1) : ;;
  *:1) varlist files ; echo ; seterror "Too many input files with \`-o' option" ;;
  *:*) varlist f_out outfiles ; echo ; seterror "Too many \`-o' options" ;;
esac

case ${#build[@]} in
  0) build=build ;;
  1) : ;;
  *) blderror "Too many build directories" ;;
esac

exe=../$build/CFDTOOLS/FileFormat/typhon2vtk

noexistlist() { f=$1 ; while [ ! -e "$f" ] ; do echo "\"$f\"" ; f=$(dirname "$f") ; done ;}

# Check executable
[ ! -e "$exe" ] && blderror "non-existing files/dirs:" $(noexistlist "$exe")
[ ! -f "$exe" ] && blderror "\"$exe\" is not a regular file"
[ ! -x "$exe" ] && blderror "\"$exe\" is not executable"

test "$error" = "error" && exit 1

echo $exe
for infile in "${files[@]}" ; do
  $exe $infile -o "${f_out:-${infile%.$ext}}"
done
