############################################################
##   Compilation of SHARED library of UDF functions

LDIR := UDF

####### Files

# Library
UDF_LIB = $(PRJLIBDIR)/libt_udf.a

# Modules
UDF_MOD =

# Objects
UDF_OBJ := $(UDF_MOD:.$(MODEXT)=.o)  \
           udf_kdif_aniso.o       \
           udf_ns_init.o          \

libt_udf.objects := $(UDF_OBJ:%=$(PRJOBJDIR)/%)
libt_udf.target: $(libt_udf.objects)

D_UDF_SRC := $(UDF_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

UDF_clean:
	-rm $(UDF_LIB) $(libt_udf.objects) $(UDF_MOD)


####### Dependencies

UDF/depends.make: $(D_UDF_SRC)
	$(MAKEDEPENDS) UDF

include UDF/depends.make


