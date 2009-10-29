############################################################
##   MATH library compilation

LDIR = MATH

####### Files

# Library
MATH_LIB = $(PRJLIBDIR)/libt_math.a

# Modules
MATH_MOD = FCT_CONTAINER.$(MODEXT) \
           FCT_DEF.$(MODEXT)       \
           FCT_ENV.$(MODEXT)       \
           FCT_EVAL.$(MODEXT)      \
           FCT_FUNC.$(MODEXT)      \
           FCT_MATH.$(MODEXT)      \
           FCT_NODE.$(MODEXT)      \
           FCT_PARSER.$(MODEXT)    \
           INTEGRATION.$(MODEXT) \
           INTERPOL.$(MODEXT)    \
           MATH.$(MODEXT)        \
           MATRIX.$(MODEXT)      \
           MATRIX_ARRAY.$(MODEXT)\
           SPARSE_MAT.$(MODEXT)  \
           SPMAT_BDLU.$(MODEXT)  \
           SPMAT_DLU.$(MODEXT)   \
           SPMAT_CRS.$(MODEXT)   \
           SPMAT_SDLU.$(MODEXT)  \

# Objects
MATH_OBJ = $(MATH_MOD:.$(MODEXT)=.o)  \
           bdlu_bicg.o             \
           bdlu_bicgstab.o         \
           bdlu_gmres.o            \
           dlu_bicg.o              \
           dlu_bicg_pjacobi.o      \
           dlu_cgs.o               \
           dlu_jacobi.o            \
           dlu_gmres.o             \
           dlu_lu.o                \
           solve_bicg.o            \
           solve_bicg_pjacobi.o    \
           solve_bicgstab.o        \
           solve_cgs.o             \
           solve_jacobi.o          \
           solve_gmres.o           \

libt_math.objects = $(MATH_OBJ:%=$(PRJOBJDIR)/%)
libt_math.target: $(libt_math.objects)

D_MATH_SRC = $(MATH_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

MATH_clean:
	-rm $(MATH_LIB) $(libt_math.objects) $(MATH_MOD) MATH/depends.make


####### Dependencies

MATH/depends.make: $(D_MATH_SRC)
	$(MAKEDEPENDS) MATH

include MATH/depends.make


