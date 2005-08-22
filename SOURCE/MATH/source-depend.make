############################################################
##   Compilation de la librairie MATH

LDIR = MATH

####### Files

MATH_LIB = $(PRJLIB)/libt_math.a

MATH_MOD = INTEGRATION.$(MOD) \
           INTERPOL.$(MOD)    \
           MATH.$(MOD)        \
           MATRIX_ARRAY.$(MOD)\
           SPARSE_MAT.$(MOD)  \
           SPMAT_BDLU.$(MOD)  \
           SPMAT_DLU.$(MOD)   \
           SPMAT_CRS.$(MOD)   \
           SPMAT_SDLU.$(MOD)  \

MATH_OBJ = $(MATH_MOD:.$(MOD)=.o)  \
           bdlu_bicg.o             \
           dlu_bicg.o              \
           dlu_bicg_pjacobi.o      \
           dlu_cgs.o               \
           dlu_jacobi.o            \
           dlu_lu.o                \
           solve_bicg.o            \
           solve_bicg_pjacobi.o    \
           solve_cgs.o             \
           solve_jacobi.o          \

D_MATH_OBJ = $(MATH_OBJ:%=$(PRJOBJ)/%)

D_MATH_SRC = $(MATH_OBJ:%.o=$(LDIR)/%.f90)

####### Build rules

all: $(MATH_LIB)

$(MATH_LIB): $(D_MATH_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Creating library $(MATH_LIB)
	@touch $(MATH_LIB) ; rm $(MATH_LIB)
	@$(AR) ruv $(MATH_LIB) $(D_MATH_OBJ)
	@echo \* Creating library index
	@$(RAN)    $(MATH_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(MATH_LIB) created
	@echo ---------------------------------------------------------------

MATH_clean:
	-rm  $(MATH_LIB) $(D_MATH_OBJ) $(MATH_MOD)

####### Dependencies


MATH/depends.make: $(D_MATH_SRC)
	(cd MATH ; ../$(MAKEDEPENDS))

include MATH/depends.make



