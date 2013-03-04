
configure_help() {
  echo
  echo "TYPHON configuration help"
  echo
  echo "  The TYPHONPATH variable should be set to find external libraries and include files:"
  echo "    * libraries     will be searched in \$TYPHONPATH/lib"
  echo "    * include files will be searched in \$TYPHONPATH/include"
  echo
  echo "  This can be done e.g. if the DAEPDIR variable is set by:"
  echo
  echo "      export TYPHONPATH=\$DAEPDIR/x86_64-Linux"
  echo "  or:"
  echo "      TYPHONPATH=\$DAEPDIR/x86_64-Linux $0"
  echo
  case "${DAEPDIR:-}" in
    "") echo "  The DAEPDIR variable is currently unset"
        for dir in /usr/local/aero /opt/aero "" ; do
          test -z "$dir" && echo "  no relevant DAEPDIR directory found" && break
          test -d "$dir" && echo "  it could be set to '$dir'" && break
        done ;;
    *)  echo "  The DAEPDIR variable is currently set to '$DAEPDIR'" ;;
  esac
  }

check_column=44
strcol=$(printf "%${check_column}s")

check() {
  local str
  str="checking $1 ..." ; shift
  echo -n "$str"
  str=${str//?/?} ; remain=${strcol%$str}
  if [ ${#str} -ge $check_column ] ; then echo ; remain=$strcol ; fi
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
  echo -e "!!! warning !!! $@ !!!"
  configure_help
  }

error() {
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  echo -e "!!!  ERROR  !!! $@  !!!"
  echo -e "!!!  ERROR  !!! $@  !!!" | sed 's/./@/g'
  configure_help
  exit 1
  }

