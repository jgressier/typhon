
configure_help() {
  echo "TYPHON configuration help"
  echo "  set F90LIB to help finding external libraries (i.e. export F90LIB=/opt/aero/lib)"
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

