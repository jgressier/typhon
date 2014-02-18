############################################################
##   FileFormat library compilation
#
.SUFFIXES:

#
# architecture dependent and project variables

PRJDIR := ..

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

# Directory
LDIR := FileFormat

# Library name
LIBNAME := libcfdfileformat

####### Files

# List of f90 modules
$(LDIR).f90MODFILES := \
    internal/TYFMT_MESH.f90 \
    internal/TYFMT_SOL.f90  \
    internal/TYPHON_FMT.f90 \
    internal/XBIN_DATA.f90  \
    internal/XBIN_IO.f90    \
    cgns/CGNS_STRUCT.f90    \
    tecplot/TECFMT.f90      \
    tecplot/TECMESHSOL.f90  \
    vtk/VTKFMT.f90          \
    vtk/VTKMESH.f90         \
    vtk/VTKSOL.f90          \

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    internal/importtyphon_mesh.f90   \
    internal/typhonread_ustmesh.f90  \
    cgns/importcgns_mesh.f90     \
    cgns/readcgns_sol.f90        \
    cgns/readcgns_ustboco.f90    \
    cgns/readcgns_ustconnect.f90 \
    cgns/readcgnsvtex.f90        \
    cgns/readcgnszone.f90        \
    cgns/writecgns_bocomesh.f90  \
    cgns/writecgns_ustmesh.f90   \
    cgns/writecgns_sol.f90       \

# List of needed libraries
LIBDEPS := cfdfileformat cfdmesh cfdmodels cfdmath cfdbase
LOCALLINKOPT := -L$(PRJLIBDIR) $(LIBDEPS:%=-l%)

# List of f90 program files
$(LDIR).f90exefiles := \
    xbininfo.f90    \
    ty2dmesh.f90    \
    ty3dmesh.f90    \
    tymonitor.f90   \
    tymorph.f90     \
    typhon2vtk.f90  \
    typhon2tecplot.f90 \

# Dependencies for tools
this.tools: $($(LDIR).f90exefiles:%.f90=$(PRJEXEDIR)/%)

include $(CONFIGDIR)/libconf.make

##GG:>>>
##GG: dependency modified from:
##this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)
##GG: to:
this.library: $(PRJOBJDIR) $(PRJLIBDIR) $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)
##GG: or:
##default: $(PRJOBJDIR) $(PRJLIBDIR)
##GG:<<<

include depends.make

