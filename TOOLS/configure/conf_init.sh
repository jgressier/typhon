
configure_help() {
  echo "TYPHON configuration help"
  echo "  set TYPHONPATH to help finding external libraries and include files (i.e. export TYPHONPATH=/my_usr:/opt/aero)"
  echo "  . libraries     will be searched in \$TYPHONPATH/lib"
  echo "  . include files will be searched in \$TYPHONPATH/include"
  }

check() {
  local com
  echo -n checking $1...
  shift
  com=$1
  shift
  check_$com $*
  }

success() {
  local col
  col=60
  echo -e \\033[${col}G$1
  }

fail()    {
  local col
  col=60
  echo -e \\033[${col}G$1
  }

warning()    {
  echo -e "!!! warning !!!" "$*"
  }

error()    {
  echo -e "!!!  ERROR  !!!" "$*"
  configure_help
  exit 1
  }

