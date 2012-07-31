
configure_help() {
  echo "TYPHON configuration help"
  echo "  The TYPHONPATH variable should be set to find external libraries and include files:"
  echo "    * libraries     will be searched in \$TYPHONPATH/lib"
  echo "    * include files will be searched in \$TYPHONPATH/include"
  echo "  This can be done e.g. if the DAEPDIR variable is set by:"
  echo "    export TYPHONPATH=/my_usr:\$DAEPDIR/x86_64-Linux"
  echo "  or:"
  echo "    TYPHONPATH=/my_usr:\$DAEPDIR/x86_64-Linux $0"
  case "${DAEPDIR:-}" in
    "") echo "  The DAEPDIR variable is currently unset"
        for dir in /usr/local/aero /opt/aero "" ; do
          test -z "$dir" && echo "  no relevant DAEPDIR directory found" && break
          test -d "$dir" && echo "  it could be set to '$dir'" && break
        done ;;
    *)  echo "  The DAEPDIR variable is currently set to '$DAEPDIR'" ;;
  esac
  }

check_column=48

check() {
  local com
  local str len
  str="checking $1 ..." ; len=$(echo "$str" | wc -c)
  echo -n "$str"
  if [ $len -ge $((check_column-4)) ] ; then echo ; fi
  shift
  com=$1
  shift
  check_$com "$@"
  }

success() {
  echo -e "\\033[${check_column}G$1"
  }

fail()    {
  echo -e "\\033[$((check_column-4))G@@@ $1"
  }

warning()    {
  echo -e "!!! warning !!! $@"
  configure_help
  }

error()    {
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  echo -e "!!!  ERROR  !!! $@  !!!"
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  configure_help
  exit 1
  }

