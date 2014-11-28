#!/bin/bash -u

ext=msh

varlist() {
  varname=$1
  prtname=${2:-$varname}
  printf "$prtname ="
  eval printf '" \`%s'"'"'"' '"${'"$varname"'[@]}"'
  echo
}

error() {
  echo "ERROR:"
  printf "  %s\n" "$@"
  exit 1
}

blderror() {
  varlist build
  error "$@"
}

build=()
files=()
f_out=()
isbld=0
while [ $# -ne 0 ] ; do
  case "$1" in
    "")   error "Empty argument"
          ;;
    mpi)  build+=( build_mpi )
          ;;
    *.$ext) files+=( "$1" )
          ;;
    -o)   test $# -eq 0 && error "\`$1': argument required"
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

case ${#build[@]} in
  0) build=build ;;
  1) : ;;
  *) blderror "Too many build directories" ;;
esac

case ${#files[@]}:${#f_out[@]} in
  0:*) error "No input files" ;;
  *:0) : ;;
  1:1) : ;;
  *:1) varlist files ; error "Too many input files with \`-o' option" ;;
  *:*) varlist f_out outfiles ; error "Too many \`-o' options" ;;
esac

exe=../$build/CFDTOOLS/FileFormat/fluent2typhon

noexistlist() { f=$1 ; while [ ! -e $f ] ; do echo "\`$f'" ; f=$(dirname $f) ; done ;}

[ ! -e "$exe" ] && blderror "$(noexistlist $exe ; echo non-existing files/dirs)"
[ ! -f "$exe" ] && blderror "$exe is not a regular file"
[ ! -x "$exe" ] && blderror "$exe is not executable"

for infile in "${files[@]}" ; do
  $exe $infile -o "${f_out:-${infile%.$ext}}"
done
