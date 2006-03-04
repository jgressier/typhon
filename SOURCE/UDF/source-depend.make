############################################################
##   Compilation of SHARED library of UDF functions

LDIR := UDF

####### Files

UDF_LIB = $(PRJLIB)/libt_udf.so

UDF_MOD = 

UDF_OBJ := $(UDF_MOD:.$(MOD)=.o)  \
           udf_kdif_aniso.o       \
           udf_ns_init.o          \

D_UDF_OBJ := $(UDF_OBJ:%=$(PRJOBJ)/%)

D_UDF_SRC := $(UDF_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(UDF_LIB)

$(UDF_LIB): $(D_UDF_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling SHARED library $(UDF_LIB)
	@touch $(UDF_LIB) ; rm $(UDF_LIB)
	@$(LINKSO) -o $(UDF_LIB) $(D_UDF_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(UDF_LIB) created
	@echo ---------------------------------------------------------------

UDF_clean:
	-rm  $(UDF_LIB) $(D_UDF_OBJ) $(UDF_MOD)


####### Dependencies


UDF/depends.make: $(D_UDF_SRC)
	(cd UDF ; ../$(MAKEDEPENDS))

include UDF/depends.make





