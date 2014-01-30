#!/bin/bash -u

SCRIPTDIR=$(cd $(dirname $0) ; pwd)
SCRIPTNAME=$(basename $0)
SCRIPTVOID=${SCRIPTNAME//?/ }

########################################################################
# --- print usage ---
########################################################################
function writebar() {
  echo "================================================================"
}

function usage() {
  if [ $1 = 1 ] ; then
    writebar
    echo "ERROR"
    writebar
  fi
  echo "$SCRIPTNAME"
  echo
  echo "Usage:"
  echo "  $SCRIPTNAME [-h] [-l <nblevl>] [-d <srcdir>] \\"
  echo "  $SCRIPTVOID [-p [-r] [-n <nbcols>]] [-o <outfile>] \\"
  echo "  $SCRIPTVOID [-x <patlist>] [-X] [-L] \\"
  echo "  $SCRIPTVOID [--] <subroutinename> [...]"
  echo
  echo "  -h: prints this help"
  echo "  -a: unwrap all subroutines"
  echo "  -l <nblevl>:  levels of descent (default is all)"
  echo "  -d <srcdir>: typhon directory (default is $SCRIPTNAME dir)"
  echo "  -p: postscript output (default is utf-8)"
  echo "  -r: landscape (default portrait) (only if postscript output)"
  echo "  -n <nbcols>:  number of postscript columns"
  echo "  -o <outfile>: prints in <outputfile> (default is stdout)"
  echo "                (required if postscript output)"
  echo "  -x <patlist>: excludes comma-separated pattern list"
  echo "  -X: ignores builtin exclude pattern list"
  echo "  -L: prints exclude pattern list"
  echo "  -v: verbose"
  echo "  --: end of options"
  echo
  echo "  <subroutinename>: to be processed"
  echo
  if [ $1 = 1 ] ; then
    writebar
    echo "ERROR"
    writebar
  fi
  exit $1
}

function info() {
  local head=
  if [ $# -eq 0 ] ; then
    echo
    echo "$SCRIPTNAME:"
  else
    if [ $# -ne 1 ] ; then head="$1" ; shift ; fi
    printf "$SCRIPTVOID  $head%s\n" "$@"
  fi
}

function warning() {
  echo
  echo "$SCRIPTNAME: warning"
  [[ $# -gt 0 ]] && printf "$SCRIPTVOID  %s\n" "$@"
  writebar
}

function error() {
  echo
  echo "$SCRIPTNAME: ERROR"
  [[ $# -gt 0 ]] && printf "$SCRIPTVOID  %s\n" "$@"
  usage 1
}

########################################################################
# --- list of excluded subroutine names ---
########################################################################
  #new_fct_env
excludebtin=(
  erreur
  error_stop
  print_info
  print_warning
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
)

excludebtin=( "${excludebtin[@]/#/^}" )
excludebtin=( "${excludebtin[@]/%/$}" )

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
separe[$print0        ]="────"
header[$print1$islast0]="@@|="
headnx[$print1$islast0]="@@|@"
header[$print1$islast1]="@@@="
headnx[$print1$islast1]="@@@@"
header[$print1$islast2]=""
headnx[$print1$islast2]=""
separe[$print1        ]="@@--"
header[$print2$islast0]=nexthd
headnx[$print2$islast0]=passhd
header[$print2$islast1]=lasthd
headnx[$print2$islast1]=voidhd
separe[$print2        ]=linehd

########################################################################
# --- definition of arbo function ---
########################################################################
function arbo() {
  local myhead myname islast i n file list tabl f isprocessed app levl
  typeset -i i n levl
  myhead="$1" ; shift
  myname="$1" ; shift
  islast="$1" ; shift
  levl="$1" ; shift
#
# -- check if subroutine already processed --
#
  if [ -z "$nowrap" ] ; then
    printf "%s\n" "${wasprocessed[@]:-}" \
      | grep -q "^$myname$" \
      && isprocessed=1
  fi
#
# -- check calls if level not too high --
#
  if [ $levl -lt $nblevl ] ; then
    file=$(echo $SOURCEDIR/*/${myname}.f90)
    if [ -f "$file" ] ; then
#
# -- build call list if subroutine file exists
#    ($'string' is for escaping characters \\ and \n)
#
      list=( $(grep '^\([^!]*[) ]\)*call ' $file 2>/dev/null \
               | sed 's/"[^"]*"//g'         $(: remove strings) \
               | sed 's/(/ (/g'             $(: add space before opening paren) \
               | sed $'s/call /\\\ncall /g' $(: move call on next line) \
               | grep '^call '              $(: keep leading call) \
               | awk '{print $2}'           $(: get subroutine name) \
               | grep -v "${excludeopts[@]}") )
#
# -- append (*) if subroutine already processed and wrappable --
#
      if [ ${#list[@]} -gt 0 ] && [ -n "${isprocessed:-}" ] ; then
        app=" ($wrapstr)"
        wrapstr="*"
      fi
    else
#
# -- append (?) if subroutine file not found --
#
      list=()
      app=" ($unknstr)"
      unknstr="?"
    fi
  fi
#
# -- header print --
#
  printf "$myhead${header[$print$islast]}"
  myhead="$myhead${headnx[$print$islast]}"
#
# -- subroutine name print --
#
  echo "$myname${app:-}"
#
# -- check called subroutines if current subroutine not already processed --
#
  if [ $levl -lt $nblevl ] && [ ${#list[@]} -gt 0 ] && [ -z ${isprocessed:-} ] ; then
#
# -- add subroutine to table of already processed subroutines --
#
    if [ -z ${isprocessed:-} ] ; then
      wasprocessed+=("$myname")
    fi
#
# -- remove duplicate subroutine names --
#
    tabl=()
    for f in "${list[@]}" ; do
      for h in "${tabl[@]:-}" ; do
        [[ "$h" = $f ]] && f="" && break
      done
      [[ ! -z "$f" ]] && tabl+=($f)
    done
#
# -- call arbo with adequate arguments (1 for last subroutine) --
#
    n=${#tabl[@]}-1
    for i in ${!tabl[@]} ; do
      arbo "${myhead}" "${tabl[i]}" $((i==n?1:0)) $((levl+dlevl))
    done
  fi
}

########################################################################
# --- get options ---
########################################################################
if [ -z "$(getopt -T)" ] ; then
  OPTS=$(getopt -o hal:d:prn:o:x:XLv -n "$SCRIPTNAME" -- "$@")
else
  OPTS=$(getopt    hal:d:prn:o:x:XLv                     "$@")
fi
test $? = 0 || usage 1
eval set -- "$OPTS"

########################################################################
# --- parse options ---
########################################################################
outputdef=/dev/stdout
nowrap=
nblevl=1
dlevl=0
srcdir=
print=0
psopt=
nbcols=1
isnbcl=
output=
btnexc=
lstexc=
verbose=
while true ; do
  case "$1" in
    -h) usage 0 ;;
    -a) nowrap=1 ;;
    -l) shift ; nblevl=$1 ; dlevl=1 ;;
    -d) shift ; srcdir=$1 ;;
    -p) print=1 ;;
    -r) psopt="-r" ;;
    -n) shift ; nbcols=$1 ; isnbcl=is_set ;;
    -o) shift ; output=$1 ;;
    -x) shift ; IFS=',' read -a x <<< "$1"
                # plus de pb reconnaissance syntaxe...
                # eval IFS="','" read -a x '<<<' "'$1'"
                excludeargs+=("${x[@]}") ;;
    -X) btnexc=is_set ;;
    -L) lstexc=is_set ;;
    -v) verbose=is_set ;;
    --) shift ; break ;;
  esac
  shift
done

wrapstr="*${verbose:+ : previously unwrapped}"
unknstr="?${verbose:+ : not found}"

if [ ! -z "$srcdir" ] ; then
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

if [ "$lstexc" ] ; then
  info
  info "builtin exclude patterns${btnexc:+ (ignored)}:"
  info "    " "${excludebtin[@]}"
  if [ ${#excludeargs[@]} -gt 0 ] ; then
    info "additional exclude patterns:"
    info "    " "${excludeargs[@]}"
  fi
  exit 0
fi

excludeopts=("-e" "^$")
if [ ! "$btnexc" ] ; then
  for bat in "${excludebtin[@]}" ; do
    excludeopts+=("-e" "$bat")
  done
fi
if [ ${#excludeargs[@]} -gt 0 ] ; then
  for pat in "${excludeargs[@]}" ; do
    excludeopts+=("-e" "$pat")
  done
fi

if [ "$isnbcl" ] && [ $print = 0 ] ; then
  warning "<nbcols> is ignored in default output"
fi
if [ $print = 1 ] ; then
  if [ -z "$output" ] ; then
    error "<outfile> must be provided for postscript output"
  else
    output="${output%.ps}.ps"
  fi
fi
if [ -n "$output" ] ; then
  if [ -f "$output" ] ; then
    error "<outfile> already exists :" "\"$output\""
  fi
fi
if [ -n "$output" ] ; then
  tmppre=/tmp/${SCRIPTNAME%.sh}.XXXXXXXX
  tmpfrm=$tmppre.out
  tmpout=$(mktemp $tmpfrm)
  test $? -eq 0 || error "could not create temporary file"
  # older versions of mktemp only allow trailing X's
  if [ $tmpout = $tmpfrm ] ; then
    rm $tmpout
    tmptmp=$(mktemp $tmppre)
    test $? -eq 0 || error "could not create temporary file"
    mv $tmptmp $tmpout
    test $? -eq 0 || error "could not rename temporary file \"$tmptmp\" to \"$tmpout\""
  fi
else
  tmpout=$outputdef
fi

if [ ${#} -eq 0 ] ; then
  usage 0
fi

listsub=$(grep 'subroutine ' */*.f90 | grep '.f90: *subroutine ')

listsub=( $(echo "$listsub" | sed 's/\.f90:/.f90 /;s/ *(.*//' | awk '{print $3,$1}' | sort | awk '{print $1}') )

#listcal=( $(echo "${listsub[@]:-}" | sed 's/\.f90:/.f90 /;s/ *(.*//' | awk '{print $3,$1}' | sort | awk '{print $2}') )

for i in $(seq 20) ; do
  sep=$(printf "%s%s" "${sep:-}" "${separe[$print]}")
done
{
  printf "$sep\n"
  echo $SOURCEDIR :
  for g in "$@" ; do
  ########################################################################
  # --- initialize table of already processed subroutines ---
  ########################################################################
    wasprocessed=()
    printf "$sep\n"
    arbo "" "$g" 2 0
  done
} > "$tmpout"

if [ -z "$output" ] ; then
  exit 0
fi

if [ $print = 0 ] ; then
  mv "$tmpout" "$output"
  exit 0
fi

enscript $psopt -B --columns=$nbcols --mark-wrapped-lines=plus -o \
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
sed "s:^(\(\(${separe[$print1]}\)\+\):\1(:g" "$tmpout" > "$output" ; mv "$output" "$tmpout"

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
sed "s:${separe[1]}:${separe[2]} :g" "$tmpout" > "$output" ; mv "$output" "$tmpout"

mv "$tmpout" "$output"

#'/slw { 0.10 setlinewidth } def\n'\
#'/dlw { 0.20 setlinewidth } def\n'\
#'/vertfull { dlw 0.3 0    moveto 0.3 2   lineto stroke } def\n'\
#'/verthlfu { dlw 0.3 0.85 moveto 0.3 2   lineto stroke } def\n'\
#'/horifull { slw 0.3 0.9  moveto 3.5 0.9 lineto stroke } def\n'\
#'/horiline { dlw 0   1    moveto 4   1   lineto stroke } def\n'\
