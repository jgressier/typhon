#!/bin/bash -u

SCRIPTDIR=$(cd $(dirname $0) ; pwd)
SCRIPTNAME=$(basename $0)
SCRIPTSPCE=${SCRIPTNAME//?/ }

# For multi-byte characters
#symbolkind=1
symbolkind=2
if [ $symbolkind == 1 ] ; then
  esc=''
  escseq='('
fi
# Counting multi-byte characters
function multicount() {
    if [ $symbolkind == 1 ] ; then
      n=3*$(echo "$1" | sed "s/\([^$esc]\)/\1\n/g" \
                      | grep -c "$escseq")
    else
      n=$(echo "$1"|wc -mc|xargs printf "-%s+%s")
    fi
    echo $n
    }

########################################################################
# --- print commands ---
########################################################################

# --- print bar ---
#
function writebar() {
  printf "%79s\n" | tr ' ' "${1:-=}"
}

# --- error banner ---
#
function errorbanner() {
  test "${1:-}" != a && writebar
  echo "ERROR"
  test "${1:-}" != b && writebar
}

# --- print usage ---
#
function usage() {
  writebar
  echo "Name:"
  echo "  $SCRIPTNAME"
  echo
  echo "Usage:"
  echo "  $SCRIPTNAME [-h] [-l <nblevl>] [-d <srcdir>] \\"
  echo "  $SCRIPTSPCE [-p [-r] [-n <nbcols>]] [-o <outfile>] \\"
  echo "  $SCRIPTSPCE [-x <patlist>] [-z] [-L] \\"
  echo "  $SCRIPTSPCE [--] <subroutinename> [...]"
  echo
  echo "Options:"
  echo "  -h                prints this help"
  echo "  -a                unwrap all subroutines"
  echo "  -l <nblevl>       levels of descent (default is all)"
  echo "  -d <srcdir>       typhon directory (default is $SCRIPTNAME dir)"
  echo "  -p                postscript output (default is utf-8)"
  echo "    -r                landscape (default portrait) (only if postscript output)"
  echo "    -n <nbcols>       number of postscript columns"
  echo "  -o <outfile>      prints in <outfile> (default is stdout)"
  echo "                      (required if postscript output)"
  echo "  -x <patlist>      excludes comma-separated pattern list"
  echo "  -z                ignores builtin exclude pattern list"
  echo "  -X patlist        excludes comma-separated pattern list for filenames"
  echo "  -L                prints exclude pattern list"
  echo "  -v                verbose"
  echo "  --                end of options"
  echo
  echo "  <subroutinename>  to be processed"
  echo
  writebar
}

# --- print info ---
#
function info() {
  local head=
  if [ $# -eq 0 ] ; then
    echo
    echo "$SCRIPTNAME:"
  else
    if [ $# -ne 1 ] ; then head="$1" ; shift ; fi
    printf "$SCRIPTSPCE  $head%s\n" "$@"
  fi
}

# --- print anything ---
#
function publish() {
  printf "$SCRIPTNAME:%s\n" "${1:+ $1}"
  shift
  test $# -gt 0 && \
  printf "$SCRIPTSPCE  %s\n" "$@"
}

# --- print warning ---
#
function warning() {
  echo
  writebar
  publish "Warning" "$@"
  writebar
  echo
}

# --- print error ---
#
function error() {
  usage
  errorbanner a
  publish "ERROR" "$@"
  errorbanner
  exit 1
}

# get lines between $1 and $2 from file $3
function between_tags() {
  awk "BEGIN{ok=0}
    (ok==1&&\$0~/${2//\//\\/}/){ok=0;print;next}
    (ok==1                    ){     print;next}
    (ok==0&&\$0~/${1//\//\\/}/){ok=1;print;next}
  " $3; }

########################################################################
# --- list of excluded subroutine names ---
########################################################################
  #new_fct_env
excludebtin=(
  flush
  get_command_argument
  cfd_{print,write,warning,error}
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

# --- Definitions for tree display ---
#
print0=0
print1=1
print2=2
islast0=0
islast1=1
islast2=2

# For multi-byte characters
if [ $symbolkind == 1 ] ; then
header[$print0$islast0]=${escseq}0"tqq "${escseq}B
headnx[$print0$islast0]=${escseq}0"x   "${escseq}B
header[$print0$islast1]=${escseq}0"mqq "${escseq}B
headnx[$print0$islast1]="    "
else
header[$print0$islast0]="├── "
headnx[$print0$islast0]="│   "
header[$print0$islast1]="└── "
headnx[$print0$islast1]="    "
fi
header[$print0$islast2]=""
headnx[$print0$islast2]=""
if [ $symbolkind == 1 ] ; then
separe[$print0        ]=${escseq}0"qqqq"${escseq}B
else
separe[$print0        ]="────"
fi
#
header[$print1$islast0]="@@|="
headnx[$print1$islast0]="@@|@"
header[$print1$islast1]="@@@="
headnx[$print1$islast1]="@@@@"
header[$print1$islast2]=""
headnx[$print1$islast2]=""
separe[$print1        ]="@@--"
#
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
    #file=$(echo $SOURCEDIR/*/${myname}.f90)
    # If file with subroutine name exists
    file=( $(ls $SOURCEDIR/{,../CFDTOOLS/{,*/}}*/${myname}.f90 2>/dev/null) )
    # Else if file containing subroutine definition exists
    if [ -z "${file:-}" ] ; then
      file=( $(grep -l "subroutine  *${myname}" $SOURCEDIR/{../CFDTOOLS/{,*/},}*/*.f90 2>/dev/null \
             | grep -v "${exclfileopts[@]}") )
    fi
    # If either file exists (possibly many)
    if [ -f "${file:-}" ] ; then
#
# -- build call list if subroutine file exists
#    ($'string' is for escaping characters \\ and \n)
#
      list=( $(between_tags "^ *(subroutine|program) *$myname" \
                       "^ *end *(subroutine|program) *$myname" "${file[@]}" \
               | grep '^\([^!]*[) ]\)*call ' 2>/dev/null \
               | sed 's/"[^"]*"//g'         $(: remove strings) \
               | sed 's/(/ (/g'             $(: add space before opening paren) \
               | sed $'s/ call /\\\ncall /g' $(: move call on next line) \
               | grep '^call '              $(: keep leading call) \
               | awk '{print $2}'           $(: get subroutine name) \
               | grep -v "${excludeopts[@]}") )
#
# -- append (*) if subroutine already processed and wrappable --
#
      if [ ${#list[@]} -gt 0 -a -n "${isprocessed:-}" ] ; then
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
# -- header string --
#
  s="$myhead${header[$print$islast]}"
#
# -- subroutine name print --
#
  if [ -z "${file:-}" -o -z "$verbose" ] ; then
    printf "%s%s\n" "$s" "$myname${app:-}"
  else
    # For multi-byte characters
    n=$((72+$(multicount "$s")))
    ##printf "%-${n}s file:%s\n" "$s$myname${app:-}" "${file[@]#$SOURCEDIR/}"
    t="$s$myname${app:-}"
    for f in "${file[@]}" ; do
      printf "%-${n}s file:%s\n" "$t" "${f#$SOURCEDIR/}"
      t="$myhead${headnx[$print$islast]}"
      # Empty header if more than one file
      if [ $levl -lt $nblevl -a \
           ${#list[@]} -gt 0 -a -z "${isprocessed:-}" ] ; then
      t="$t${headnx[$print$islast0]}"
      fi
      n=$((72+$(multicount "$t")))
    done
  fi
#
# -- next header --
#
  myhead="$myhead${headnx[$print$islast]}"
#
# -- check called subroutines if current subroutine not already processed --
#
  if [ $levl -lt $nblevl -a ${#list[@]} -gt 0 -a -z "${isprocessed:-}" ] ; then
#
# -- add subroutine to table of already processed subroutines --
#
    if [ -z "${isprocessed:-}" ] ; then
      wasprocessed+=("$myname")
    fi
#
# -- remove duplicate subroutine names --
#
    tabl=()
    for f in "${list[@]}" ; do
      for h in "${tabl[@]:-}" ; do
        test "$h" = "$f" && f="" && break
      done
      test ! -z "$f" && tabl+=( "$f" )
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
shortopts=hal:d:prn:o:x:zX:Lv
if [ -z "$(getopt -T)" ] ; then
  getoptopts=( -o "$shortopts" -n "$SCRIPTNAME" -- )
else
  getoptopts=(    "$shortopts"                     )
fi
OPTS=$(getopt "${getoptopts[@]}" "$@" 2>    /dev/null)
OERR=$(getopt "${getoptopts[@]}" "$@" 2>&1 >/dev/null)
if [ $? != 0 ] ; then
  error "$OERR"
fi
eval set -- "$OPTS"

function intexpectedafter() {
  opt="$1" ; shift
  if [ -n "${@//[0-9]}" ] ; then
    error "integer expected after \`$opt' (found \`$@')"
  fi
}

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
excludeargs=()
exclfileargs=()
while true ; do
  case "$1" in
    -h) usage ; exit 0 ;;
    -a) nowrap=1 ;;
    -l) intexpectedafter $1 $2 ; shift
        nblevl=$1 ; dlevl=1 ;;
    -d) shift ; srcdir=$1 ;;
    -p) print=1 ;;
    -r) psopt="--landscape" ;;
    -n) intexpectedafter $1 $2 ; shift
        nbcols=$1 ; isnbcl='is_set' ;;
    -o) shift ; output=$1 ;;
    -x) shift ; IFS=',' read -a x <<< "$1"
                # plus de pb reconnaissance syntaxe...
                # eval IFS="','" read -a x '<<<' "'$1'"
                excludeargs+=("${x[@]}") ;;
    -z) btnexc='is_set' ;;
    -X) shift ; IFS=',' read -a x <<< "$1"
                # plus de pb reconnaissance syntaxe...
                # eval IFS="','" read -a x '<<<' "'$1'"
                exclfileargs+=("${x[@]}") ;;
    -L) lstexc='is_set' ;;
    -v) verbose='is_set' ;;
    --) shift ; break ;;
  esac
  shift
done

wrapstr="*${verbose:+ : previously unwrapped}"
unknstr="?${verbose:+ : not found}"

SOURCEDIR=
if [ ! -z "$srcdir" ] ; then
  if [ ! -d "$srcdir" ] ; then
    error "<srcdir> does not exist :" \
          "\"$srcdir\""
  fi
  d=$(cd "$srcdir" ; pwd)
  home=$(cd ; pwd)
  while [ -d "$d" ] ; do
    dd=$d/SOURCE
    test -d "$dd" && SOURCEDIR=$dd && break
    nd=$(cd "$d/.." && pwd)
    # break if home or /
    test "$nd" = "$d" && break
    test "$nd" = "$home" && break
    d=$nd
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

exclfileopts=("-e" "^$")
if [ ${#exclfileargs[@]} -gt 0 ] ; then
  for pat in "${exclfileargs[@]}" ; do
    exclfileopts+=("-e" "$pat")
  done
fi

if [ "$isnbcl" -a $print = 0 ] ; then
  warning "<nbcols> is ignored in default output"
fi
if [ $print = 1 ] ; then
  if [ -z "$output" ] ; then
    error "<outfile> must be provided for postscript output"
  fi
  output="${output%.ps}.ps"
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
  usage ; exit 0
fi

#ORIGDIR=$(pwd)
#cd $SOURCEDIR

#listsub=$(grep 'subroutine ' */*.f90 | grep '.f90: *subroutine ')

#listsub=( $(echo "$listsub" | sed 's/\.f90:/.f90 /;s/ *(.*//' | awk '{print $3,$1}' | sort | awk '{print $1}') )

#listcal=( $(echo "${listsub[@]:-}" | sed 's/\.f90:/.f90 /;s/ *(.*//' | awk '{print $3,$1}' | sort | awk '{print $2}') )

sep=$(for i in $(seq 20) ; do printf "%s" "${separe[$print]}" ; done)
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

ev=
ee=$(enscript $psopt -B --columns=$nbcols --mark-wrapped-lines=plus -o \
         "$output" "$tmpout" 2>&1) || \
ev=$(vim -e "$tmpout" < <(echo "set popt+=header:0"
                          echo "hardcopy > $output") 2>&1)
if [ $? != 0 ] ; then
  error "$ee" "$ev" "enscript and vim failures"
fi
echo "${ee:+enscript failed, tried vim}"
mv "$output" "$tmpout"

sep=""
for i in $islast0 $islast1 ; do
  sep="$sep\|${header[$print1$i]}"
  sep="$sep\|${headnx[$print1$i]}"
done
sep=$(echo "$sep" | sed 's/^\\|//')
spq=${separe[$print1]}
sed "s:^(\(\($sep\)\+\)\(.* m\)s$:(\3 \1 s:g" "$tmpout" > "$output" ; mv "$output" "$tmpout"
sed "s:^(\(\($spq\)\+\)\(.* m\)s$:(\3 \1 s:g" "$tmpout" > "$output" ; mv "$output" "$tmpout"
sed "s:^(\(\($sep\)\+\):\1(:g" "$tmpout" > "$output" ; mv "$output" "$tmpout"
sed "s:^(\(\($spq\)\+\):\1(:g" "$tmpout" > "$output" ; mv "$output" "$tmpout"

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
echo "file: $output"

#'/slw { 0.10 setlinewidth } def\n'\
#'/dlw { 0.20 setlinewidth } def\n'\
#'/vertfull { dlw 0.3 0    moveto 0.3 2   lineto stroke } def\n'\
#'/verthlfu { dlw 0.3 0.85 moveto 0.3 2   lineto stroke } def\n'\
#'/horifull { slw 0.3 0.9  moveto 3.5 0.9 lineto stroke } def\n'\
#'/horiline { dlw 0   1    moveto 4   1   lineto stroke } def\n'\

#cd $ORIGDIR
