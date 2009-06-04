
configure_help() {
  echo "TYPHON configuration help"
  echo "  set TYPHONPATH to help finding external libraries and include files (i.e. export TYPHONPATH=/my_usr:/opt/aero)"
  echo "  . libraries     will be searched in \$TYPHONPATH/lib"
  echo "  . include files will be searched in \$TYPHONPATH/include"
  }

check_column=60

check() {
  local com
  local str len
  str="checking $1..." ; len=$(echo "$str" | wc -c)
  echo -n "$str"
  if [ $len -ge $check_column ] ; then echo ; fi
  shift
  com=$1
  shift
  check_$com "$@"
  }

success() {
  echo -e "\\033[${check_column}G$1"
  }

fail()    {
  echo -e "\\033[${check_column}G$1"
  }

warning()    {
  echo -e "!!! warning !!! $@"
  configure_help
  }

error()    {
  echo -e "!!!  ERROR  !!! $@"
  configure_help
  exit 1
  }

