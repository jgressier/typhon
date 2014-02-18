############################################################
##   MATH library compilation
#
.SUFFIXES:

# Directory
LDIR := MATH

# Library name
LIBNAME := libt_math

####### Files

# Library
$(LDIR).libfile := $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

# List of f90 modules
$(LDIR).f90MODFILES := \
    INTEGRATION.f90 \
    INTERPOL.f90    \
    MATRIX.f90      \
    MATRIX_ARRAY.f90\
    SPARSE_MAT.f90  \
    SPMAT_BDLU.f90  \
    SPMAT_DLU.f90   \
    SPMAT_CRS.f90   \
    SPMAT_SDLU.f90  \

$(LDIR)_MOD := $($(LDIR).f90MODFILES:%.f90=%.$(MODEXT))

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    bdlu_bicg.f90             \
    bdlu_bicgstab.f90         \
    bdlu_gmres.f90            \
    dlu_bicg.f90              \
    dlu_bicg_pjacobi.f90      \
    dlu_cgs.f90               \
    dlu_jacobi.f90            \
    dlu_gmres.f90             \
    dlu_lu.f90                \
    solve_bicg.f90            \
    solve_bicg_pjacobi.f90    \
    solve_bicgstab.f90        \
    solve_cgs.f90             \
    solve_jacobi.f90          \
    solve_gmres.f90           \

$(LDIR).f90names := $(notdir $($(LDIR).f90files))

$(LDIR).objnames := $($(LDIR).f90names:%.f90=%.o)

$(LDIR).objfiles := $($(LDIR).objnames:%=$(PRJOBJDIR)/%)

$(LIBNAME).objfiles := $($(LDIR).objfiles)

##GG:>>>
##GG: dependency removed
##$(LIBNAME).target: $($(LDIR).objfiles)
##GG: and replaced
$($(LDIR).libfile): $($(LDIR).objfiles)
##GG:<<<

D_$(LDIR)_SRC := $($(LDIR).objnames:%.o=$(LDIR)/%.f90)


####### Build rules

$(LDIR)_clean: %_clean:
	-rm $($*.libfile) $($*.objfiles) $($*_MOD) $*/depends.make


####### Dependencies

$(LDIR)/depends.make: %/depends.make: $(D_%_SRC)
	$(MAKEDEPENDS) $*

include $(LDIR)/depends.make

