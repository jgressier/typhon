############################################################
##   Compilation de la librairie MODZONE

LDIR := UDF

####### Files

UDF_LIB = $(PRJLIB)/libt_udf.a

UDF_MOD = 

UDF_OBJ := $(UDF_MOD:.$(MOD)=.o)  \
           udf_kdif_aniso.o

D_UDF_OBJ := $(UDF_OBJ:%=$(PRJOBJ)/%)

D_UDF_SRC := $(UDF_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(UDF_LIB)

$(UDF_LIB): $(D_UDF_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(UDF_LIB)
	@touch $(UDF_LIB) ; rm $(UDF_LIB)
	@$(AR) ruv $(UDF_LIB) $(D_UDF_OBJ)
	@echo \* Library index updating
	@$(RAN)    $(UDF_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(UDF_LIB) created
	@echo ---------------------------------------------------------------

UDF_clean:
	-rm  $(UDF_LIB) $(D_UDF_OBJ) $(UDF_MOD)


####### Dependencies


UDF/depends.make: $(D_UDF_SRC)
	(cd UDF ; ../$(MAKEDEPENDS))

include UDF/depends.make





