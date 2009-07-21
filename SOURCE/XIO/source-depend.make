############################################################
##   Compilation de la librairie XIO
 
LDIR = XIO

####### Files

XIO_LIB = $(PRJLIB)/libt_xio.a

XIO_MOD = REPRISE.$(MOD)

XIO_OBJ = $(XIO_MOD:.$(MOD)=.o)  \
          comp_flux.o            \
          output_tec_str.o       \
          output_tec_ust.o       \
          output_tec_ust_ctr.o   \
          output_tecplot.o       \
          output_vtk.o           \
          output_vtk_cell.o      \
          output_vtk_scal.o      \
          output_vtk_vect.o      \
          output_vtkbin.o        \
          output_vtkbin_cell.o   \
          output_vtkbin_scal.o   \
          output_vtkbin_vect.o   \
          output_zone.o          \
          outputzone_close.o     \
          outputzone_sol.o       \
          outputzone_open.o      \
          outputzone_ustmesh.o   \
          readtyphmsh_dom.o      \
          readtyphmshfile.o      \
          writevtk_sol.o         \
          writevtk_ustmesh.o     \
 
D_XIO_OBJ = $(XIO_OBJ:%=$(PRJOBJ)/%)


####### Build rules

all: $(XIO_LIB)

$(XIO_LIB): $(D_XIO_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Creation of library : $(XIO_LIB)
	@touch $(XIO_LIB) ; rm $(XIO_LIB)
	@$(AR) ruv $(XIO_LIB) $(D_XIO_OBJ)
	@echo \* Creation of library index
	@$(RAN)    $(XIO_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(XIO_LIB) created
	@echo ---------------------------------------------------------------

XIO_clean:
	-rm  $(XIO_LIB) $(D_XIO_OBJ) $(XIO_MOD) XIO/depends.make

####### Dependencies


XIO/depends.make: $(D_XIO_SRC)
	(cd XIO ; ../$(MAKEDEPENDS))

include XIO/depends.make



