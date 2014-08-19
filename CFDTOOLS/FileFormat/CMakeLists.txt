# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT FileFormat)
SET(KIT_LIBS
${pName}Mesh 
${pName}${KIT}
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.

SET(Kit_SRCS
internal/XBIN_IO.f90
internal/XBIN_DATA.f90
internal/typhonread_ustmesh.f90
internal/TYPHON_FMT.f90
internal/TYFMT_SOL.f90
internal/TYFMT_PART.f90
internal/TYFMT_MESH.f90
internal/importtyphon_mesh.f90
fluent/FLUENT.f90
fluent/fluentread_ustmesh.f90
fluent/importfluent_mesh.f90
gmsh/GMSH.f90
gmsh/importgmsh_mesh.f90
gmsh/gmshread_ustmesh.f90
tecplot/TECMESHSOL.f90
tecplot/TECFMT.f90
vtk/VTKSOL.f90
vtk/VTKMESH.f90
vtk/VTKFMT.f90
)

SET(Kit_EXESRCS
xbininfo.f90
typhon2vtk.f90
typhon2tecplot.f90
fluent2typhon.f90
typart.f90
tymorph.f90
tymonitor.f90
ty3dmesh.f90
ty2dmesh.f90
)

IF(${PROJECT_NAME}_USE_CGNS)
  SET(KIT_LIBS ${KIT_LIBS} "${CGNS_LIBRARY}" )
  INCLUDE_DIRECTORIES(${CGNS_INCLUDE_DIR})
  SET(Kit_SRCS
${Kit_SRCS}
cgns/writecgns_ustmesh.f90
cgns/writecgns_sol.f90
cgns/writecgns_bocomesh.f90
cgns/readcgnszone.f90
cgns/readcgnsvtex.f90
cgns/readcgns_ustconnect.f90
cgns/readcgns_ustboco.f90
cgns/readcgns_sol.f90
cgns/importcgns_mesh.f90
cgns/CGNS_STRUCT.f90
)
ENDIF(${PROJECT_NAME}_USE_CGNS)
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
# Make sure the compiler can find modules files
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Base)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Mesh)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Models)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Math)

# Make sure the linker can find this library once it is built.
LINK_DIRECTORIES ({${PROJECT_NAME}_BINARY_DIR}/${KIT})
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
ADD_LIBRARY(${pName}${KIT} ${Kit_SRCS})
TARGET_LINK_LIBRARIES(${pName}${KIT} ${KIT_LIBS})
# ---------------------------------------------------------------------------/

#ADD_SUBDIRECTORY(cgns)
#ADD_SUBDIRECTORY(gmsh)
#ADD_SUBDIRECTORY(internal)
#ADD_SUBDIRECTORY(tecplot)
#ADD_SUBDIRECTORY(vtk)

# ---------------------------------------------------------------------------/
foreach( exesourcefile ${Kit_EXESRCS} )
    get_filename_component(exename ${exesourcefile} NAME_WE)
    #string( REPLACE ".f90" "" exename ${testsourcefile} )
    add_executable(${exename} ${exesourcefile} )
    target_link_libraries(${exename} ${KIT_LIBS} )
endforeach( exesourcefile ${Kit_EXESRCS} )
