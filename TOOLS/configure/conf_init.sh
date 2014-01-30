
check_column=44
strcol=$(printf "%${check_column}s")

check() {
  local str
  str="checking $1 ..." ; shift
  echo -n "$str"
  str=${str//?/?} ; remain=${strcol%$str}
  if [ ${#str} -ge $check_column ] ; then
    echo ; remain=$strcol
  fi
  check_"$@"
  }

success() {
  echo -e "${remain}$1"
  remain=$strcol
  }

fail() {
  if [ ${#remain} -le 4 ] ; then
    echo ; remain=$strcol
  fi
  echo -e "${remain#????}!!! $1"
  remain=$strcol
  }

warning() {
  echo -e "!!! warning !!! $@ !!!" | sed 's/./!/g'
  echo -e "!!! warning !!! $@ !!!"
  echo -e "!!! warning !!! $@ !!!" | sed 's/./!/g'
  warning=1
  }

error() {
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  echo -e "!!!  ERROR  !!! $@  !!!"
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  configure_help
  exit 1
  }

