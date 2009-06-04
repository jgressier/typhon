#!/bin/sh -u

SCRIPTDIR=$(cd $(dirname $0) ; pwd)
SCRIPTNAME=$(basename $0)
SCRIPTVOID=${SCRIPTNAME//?/ }

########################################################################
# --- print usage ---
########################################################################
bar="================================================================"

function usage() {
  if [ $1 = 1 ] ; then
    echo $bar
    echo "ERROR"
    echo $bar
  fi
  echo
  echo "Usage: $SCRIPTNAME [-h] [-l <nblevl>] [-d <srcdir>] \\"
  echo "       $SCRIPTVOID [-p [-r] [-n <nbcols>]] [-o <outfile>] \\"
  echo "       $SCRIPTVOID [-x <patlist>] [-X] \\"
  echo "       $SCRIPTVOID [--] <subroutinename> [...]"
  echo
  echo "       -h: prints this help"
  echo "       -l <nblevl>:  levels of descent (default is all)"
  echo "       -d <srcdir>: typhon directory (default is $SCRIPTNAME dir)"
  echo "       -p: postscript output (default is utf-8)"
  echo "       -r: landscape (default portrait) (only if postscript output)"
  echo "       -n <nbcols>:  number of postscript columns"
  echo "       -o <outfile>: prints in <outputfile> (default is stdout)"
  echo "                     (required if postscript output)"
  echo "       -x <patlist>: excludes comma-separated pattern list"
  echo "       -X: ignores builtin exclude pattern list"
  echo "       --: end of options"
  echo
  echo "       <subroutinename>: to be processed"
  echo
  if [ $1 = 1 ] ; then
    echo $bar
    echo "ERROR"
    echo $bar
  fi
  exit $1
}

function warning() {
  echo "$SCRIPTNAME: warning"
  [[ $# -gt 0 ]] && printf "$SCRIPTVOID  %s\n" "$@"
  echo "$bar"
}

function error() {
  echo "$SCRIPTNAME: ERROR"
  [[ $# -gt 0 ]] && printf "$SCRIPTVOID  %s\n" "$@"
  usage 1
}

########################################################################
# --- list of excluded subroutine names ---
########################################################################
  #new_fct_env
excludelist="
  erreur
  print_info
  new
  delete
  new.*
  seekrpmblock
  rpmgetkeyval.*
  fct_eval_real
  fct_env_set_real
  realloc
  writestr
  writereturn
"

excludeargs=()

print0=0
print1=1
print2=2
islast0=0
islast1=1
islast2=2

header[$print0$islast0]="├── "
headnx[$print0$islast0]="│   "
header[$print0$islast1]="└── "
headnx[$print0$islast1]="    "
header[$print0$islast2]=""
headnx[$print0$islast2]=""
separ[$print0]="────"
header[$print1$islast0]="@@|="
headnx[$print1$islast0]="@@|@"
header[$print1$islast1]="@@@="
headnx[$print1$islast1]="@@@@"
header[$print1$islast2]=""
headnx[$print1$islast2]=""
separ[$print1]="@@--"
header[$print2$islast0]=nexthd
headnx[$print2$islast0]=passhd
header[$print2$islast1]=lasthd
headnx[$print2$islast1]=voidhd
separ[$print2]=linehd

typeset -i lev

########################################################################
# --- definition of arbo function ---
########################################################################
function arbo() {
  local myhead myname islast n file list tabl f i isdone ok
  myhead="$1" ; shift
  myname="$1" ; shift
  islast="$1" ; shift
#
# -- check if subroutine already done --
#
  for h in "${alreadydone[@]:-}" ; do
    if [ "$h" = $myname ] ; then
      isdone=1
      break
    fi
  done
#
# -- check calls if level not too high --
#
  if [ $lev -lt $nblevl ] ; then
    file=$(echo $SOURCEDIR/*/${myname}.f90)
    if [ -f "$file" ] ; then
      list=( $(grep '^\([^!]*[) ]\)*call ' $file 2>/dev/null | sed 's/"[^"]*"//g' | \
               sed 's/(/ (/g;s/call /\ncall /g' | grep call | awk '{print $2}' | \
               grep -v "${excludeargs[@]}") )
#
# -- add (*) if subroutine already done and wrappable --
#
      if [ ${#list[@]} -gt 0 ] ; then
        ok=${isdone:+" (*)"}
      fi
    else
#
# -- add (?) if subroutine was not found --
#
      list=()
      ok=" (?)"
    fi
  fi
#
# -- add subroutine to table of already done subroutines --
#
  if [ -z ${isdone:-} ] ; then
    alreadydone+=("$myname")
  fi
#
# -- conditional print --
#
  oldhead="$myhead"
  printf "$myhead${header[$print$islast]}"
  myhead="$myhead${headnx[$print$islast]}"
#
# -- subroutine name print --
#
  echo "$myname${ok:-}"
#
# -- check called subroutines if current subroutine not already done --
#
  if [ $lev -lt $nblevl ] && [ ${#list[@]} -gt 0 ] && [ -z ${isdone:-} ] ; then
#
# -- remove duplicate subroutine names --
#
    tabl=()
    for f in ${list[@]} ; do
      for h in "${tabl[@]:-}" ; do
        [[ "$h" = $f ]] && f="" && break
      done
      [[ ! -z "$f" ]] && tabl+=($f)
    done
#
# -- call arbo with adequate arguments --
#
    typeset -i n=${#tabl[@]}-1
    for i in ${!tabl[@]} ; do
      lev=lev+dlev
      arbo "${myhead}" "${tabl[i]}" $((i==n?1:0))
      lev=lev-dlev
    done
  fi
}

########################################################################
# --- get options ---
########################################################################
OPTS=$(getopt -o hl:d:prn:o:x:X -n "$SCRIPTNAME" -- "$@")
[[ $? != 0 ]] && usage 1
eval set -- "$OPTS"

########################################################################
# --- parse options ---
########################################################################
print=0
nbcols=1 ; nbcl=0
nblevl=1 ; dlev=0
nopexc=0
while true ; do
  case "$1" in
    -h) usage 0 ;;
    -l) shift ; nblevl=$1 ; dlev=1 ;;
    -d) shift ; srcdir=$1 ;;
    -p) print=1 ;;
    -r) psopt="-r" ;;
    -n) shift ; nbcols=$1 ; nbcl=1 ;;
    -o) shift ; output=$1 ;;
    -x) shift ; for pat in $(eval echo '{'"$1"',}') ; do
                  excludeargs+=("-e" "$pat")
                done ;;
    -X) nopexc=1 ;;
    --) shift ; break ;;
  esac
  shift
done

if [ ! -z "${srcdir:-}" ] ; then
  if [ ! -d "$srcdir" ] ; then
    error "<srcdir> does not exist :" \
          "\"$srcdir\""
  fi
  d=.
  for i in $(seq 4) ; do
    dd="$srcdir/$d/SOURCE"
    [[ -d "$dd" ]] && SOURCEDIR=$(cd $dd ; pwd) && break
    d=../$d
  done
  if [ -z "${SOURCEDIR:-}" ] ; then
    error "<srcdir> is not in a valid typhon directory" \
          "\"$srcdir\""
  fi
fi

if [ -z "${SOURCEDIR:-}" ] ; then
  SOURCEDIR=$SCRIPTDIR
fi

if [ $nopexc = 0 ] ; then
  for exclude in $excludelist ; do
    excludeargs+=("-e" "^$exclude$")
  done
fi

if [ $nbcl = 1 ] && [ $print = 0 ] ; then
  warning "<nbcols> is ignored in default output"
fi
if [ $print = 1 ] && [ -z "${output:-}" ] ; then
  error "<outfile> must be provided for postscript output"
fi
if [ ! -z "${output:-}" ] ; then
  [[ $print = 1 ]] && output="${output%.ps}.ps"
  if [ -f "$output" ] ; then
    error "<outfile> already exists :" \
          "\"$output\""
  fi
fi
tmpout="/tmp/$SCRIPTNAME.tmpout.$$"
while [ -f "$tmpout" ] ; do
  tmpout="$tmpout."
done

if [ ${#} -eq 0 ] ; then
  usage 0
fi

listsub=$(grep 'subroutine ' */*.f90 | grep '.f90: *subroutine ')

listsub=( $(echo "$listsub" | sed 's/\.f90:/.f90 /;s/ *(.*//' | awk '{print $3,$1}' | sort | awk '{print $1}') )

listcal=( $(echo "$listsub" | sed 's/\.f90:/.f90 /;s/ *(.*//' | awk '{print $3,$1}' | sort | awk '{print $2}') )

for i in $(seq 20) ; do
  sep=$(printf "%s%s" "${sep:-}" "${separ[$print]}")
done
{
  printf "$sep\n"
  echo $SOURCEDIR :
  for g in "$@" ; do
  ########################################################################
  # --- initialize table of already done subroutines ---
  ########################################################################
    alreadydone=()
    lev=0
    printf "$sep\n"
    arbo "" "$g" 2
  done
} > "$tmpout"

if [ -z "${output:-}" ] ; then
  cat "$tmpout"
  rm "$tmpout"
  exit 0
fi

if [ $print = 0 ] ; then
  mv "$tmpout" "$output"
  exit 0
fi

enscript ${psopt:-} -B --columns=$nbcols --mark-wrapped-lines=plus -o \
         "$output" "$tmpout" ; mv "$output" "$tmpout"
if [ $? != 0 ] ; then
  error "enscript error"
fi

sep=""
for i in $islast0 $islast1 ; do
  sep="$sep\|${header[$print1$i]}"
  sep="$sep\|${headnx[$print1$i]}"
done
sep=$(echo "$sep" | sed 's/^\\|//')
sed "s:^(\(\($sep\)\+\):\1(:g" "$tmpout" > "$output" ; mv "$output" "$tmpout"
sed "s:^(\(\(${separ[$print1]}\)\+\):\1(:g" "$tmpout" > "$output" ; mv "$output" "$tmpout"

# Postscript macros
sed 's:%%EndSetup$:%\n'\
'/bgr { gsave currentpoint translate 0.7 0.7 0.9 setrgbcolor\n'\
'       (M) stringwidth pop dup scale 0 -0.6 translate\n'\
'       exec grestore (MMMM) stringwidth rmoveto } def\n'\
'/slw { 0.05 setlinewidth } def\n'\
'/vertfull { slw 0.5 0 moveto 0.5 2 lineto stroke } def\n'\
'/verthlfu { slw 0.5 1 moveto 0.5 2 lineto stroke } def\n'\
'/horifull { slw 0.5 1 moveto 3   1 lineto stroke } def\n'\
'/horiline { slw 0   1 moveto 4   1 lineto stroke } def\n'\
'/lasthd { { verthlfu horifull } bgr } def\n'\
'/nexthd { { vertfull horifull } bgr } def\n'\
'/passhd { { vertfull          } bgr } def\n'\
'/voidhd { {                   } bgr } def\n'\
'/linehd { { horiline          } bgr } def\n'\
'%EndSetup:'           "$tmpout" > "$output" ; mv "$output" "$tmpout"

for i in $islast0 $islast1 ; do
  sed "s:${header[1$i]}:${header[2$i]} :g" "$tmpout" > "$output" ; mv "$output" "$tmpout"
  sed "s:${headnx[1$i]}:${headnx[2$i]} :g" "$tmpout" > "$output" ; mv "$output" "$tmpout"
done
sed "s:${separ[1]}:${separ[2]} :g" "$tmpout" > "$output" ; mv "$output" "$tmpout"

mv "$tmpout" "$output"

#'/slw { 0.10 setlinewidth } def\n'\
#'/dlw { 0.20 setlinewidth } def\n'\
#'/vertfull { dlw 0.3 0    moveto 0.3 2   lineto stroke } def\n'\
#'/verthlfu { dlw 0.3 0.85 moveto 0.3 2   lineto stroke } def\n'\
#'/horifull { slw 0.3 0.9  moveto 3.5 0.9 lineto stroke } def\n'\
#'/horiline { dlw 0   1    moveto 4   1   lineto stroke } def\n'\
