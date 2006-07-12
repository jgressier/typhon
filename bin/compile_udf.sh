#! /bin/sh

echo compile udf_*.f90
ifort -shared -I$TYPHON_INCLUDE -o libt_udf.so udf_*.f90
