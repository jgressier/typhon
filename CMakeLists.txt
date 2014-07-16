cmake_minimum_required (VERSION 2.8.4)

project(Typhon Fortran)

#set(CMAKE_Fortran_COMPILER mpif90)
#set(CMAKE_C_COMPILER mpicc)
#set(CMAKE_CXX_COMPILER mpiCC)

SET(pName Typhon)

SET(${PROJECT_NAME}_CMAKE_DIR "${${PROJECT_NAME}_SOURCE_DIR}/CMake")
SET(CMAKE_MODULE_PATH "${${PROJECT_NAME}_CMAKE_DIR}" ${CMAKE_MODULE_PATH})
#get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

#message(${CMAKE_Fortran_COMPILER_ID}) # to check cmake processing

# Set default cmake build type to RelWithDebInfo
# (None Debug Release RelWithDebInfo MinSizeRel)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'RelWithDebInfo' as none was specified.")
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  #set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "Profiling" "MinSizeRel" "RelWithDebInfo")
endif()

# Fortran compiler flags
#enable_language(Fortran)
INCLUDE(FortranCompilerFlags)

MESSAGE(STATUS "build type set to ${CMAKE_BUILD_TYPE}")

# ${PROJECT_NAME} version number.  An even minor number corresponds to releases.
SET(${PROJECT_NAME}_MAJOR_VERSION 0)
SET(${PROJECT_NAME}_MINOR_VERSION 5)
SET(${PROJECT_NAME}_BUILD_VERSION 0)
SET(${PROJECT_NAME}_VERSION
    "${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_BUILD_VERSION}")

# Use ParMetis
OPTION(${PROJECT_NAME}_USE_METIS
           "Use Metis library." 
           ON)
MARK_AS_ADVANCED(${PROJECT_NAME}_USE_METIS)

# Use CGNS library
OPTION(${PROJECT_NAME}_USE_CGNS
       "Use CGNS library." 
       ON)
#MARK_AS_ADVANCED(${PROJECT_NAME}_USE_CGNS)

# Use Doxygen to generate documentation
OPTION(${PROJECT_NAME}_USE_DOXYGEN
       "Use Doxygen to create documentation." 
       ON)
MARK_AS_ADVANCED(${PROJECT_NAME}_USE_DOXYGEN)

# Use MPI
OPTION(${PROJECT_NAME}_USE_MPI
       "Use mpi for parallel runs." 
       ON)
#MARK_AS_ADVANCED(${PROJECT_NAME}_USE_MPI)

IF(${PROJECT_NAME}_USE_CGNS)
  INCLUDE(FindCGNS)
  ADD_DEFINITIONS("-DCGNS")
ENDIF(${PROJECT_NAME}_USE_CGNS)

# Look for Doxygen package
IF(${PROJECT_NAME}_USE_DOXYGEN)
  INCLUDE(UseDoxygen)
ENDIF(${PROJECT_NAME}_USE_DOXYGEN)

IF(${PROJECT_NAME}_USE_MPI)
  ADD_DEFINITIONS("-DMPICOMPIL")
ENDIF(${PROJECT_NAME}_USE_MPI)

# Look for ParMetis package
IF(${PROJECT_NAME}_USE_METIS)
  INCLUDE(FindMetis)
  ADD_DEFINITIONS("-DMETIS")
ENDIF(${PROJECT_NAME}_USE_METIS)

# Look for TAU package
IF(${PROJECT_NAME}_USE_TAU)
  INCLUDE(FindTAU)
ENDIF(${PROJECT_NAME}_USE_TAU)


#-----------------------------------------------------------------------------
# Configure install locations.  This allows parent projects to modify
# the install location.  Optionally allow the project to specify a
# single ${PROJECT_NAME}_INSTALL_ROOT which basically adds to its install prefix
# for ${PROJECT_NAME} only.

# Set a INSTALL_PREFIX variable to a expected installation path
# that will superseed CMAKE_INSTALL_PREFIX (which is by default
# /usr/local/ and not very useful for local installations which are expected
# to be more common)

SET(INSTALL_PREFIX_ENV $ENV{INSTALL_PREFIX})

IF(DEFINED INSTALL_PREFIX_ENV)
SET(CMAKE_INSTALL_PREFIX "${INSTALL_PREFIX_ENV}" CACHE INTERNAL "Prefix prepended to install directories" FORCE)
#message("Installation to ${CMAKE_INSTALL_PREFIX}")
ELSE (DEFINED INSTALL_PREFIX_ENV)
SET(CMAKE_INSTALL_PREFIX "$ENV{PWD}/installationDir/" CACHE INTERNAL "Prefix prepended to install directories" FORCE)
#message("Installation to ${CMAKE_INSTALL_PREFIX}")
ENDIF(DEFINED INSTALL_PREFIX_ENV)

SET(${PROJECT_NAME}_INSTALL_ROOT ${CMAKE_INSTALL_PREFIX})

# The location in which to install ${PROJECT_NAME} executables.
IF(NOT ${PROJECT_NAME}_INSTALL_BIN_DIR)
  SET(${PROJECT_NAME}_INSTALL_BIN_DIR ${${PROJECT_NAME}_INSTALL_ROOT}/bin)
ENDIF(NOT ${PROJECT_NAME}_INSTALL_BIN_DIR)

# The location in which to install ${PROJECT_NAME} header files.
IF(NOT ${PROJECT_NAME}_INSTALL_INCLUDE_DIR)
  SET(${PROJECT_NAME}_INSTALL_INCLUDE_DIR
    ${${PROJECT_NAME}_INSTALL_ROOT}/include
#    ${${PROJECT_NAME}_INSTALL_ROOT}/include/${pName}-${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}
    )
ENDIF(NOT ${PROJECT_NAME}_INSTALL_INCLUDE_DIR)

# The location in which to install ${PROJECT_NAME} libraries.
IF(NOT ${PROJECT_NAME}_INSTALL_LIB_DIR)
  SET(${PROJECT_NAME}_INSTALL_LIB_DIR 
    ${${PROJECT_NAME}_INSTALL_ROOT}/lib
#    ${${PROJECT_NAME}_INSTALL_ROOT}/lib/${pName}-${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}
    )
ENDIF(NOT ${PROJECT_NAME}_INSTALL_LIB_DIR)

#-----------------------------------------------------------------------------\
# Include directories from the build tree (required for any header files
# installed in the build tree during CMake configuration)
#-----------------------------------------------------------------------------|
SET(${PROJECT_NAME}_INCLUDE_DIRS_BUILD_TREE
  ${${PROJECT_NAME}_BINARY_DIR}
  )

#provide information on git branch used for compilation
INCLUDE(GetGitRevisionDescription)
get_git_head_revision(${PROJECT_NAME}_GIT_REFSPEC ${PROJECT_NAME}_GIT_SHA1)
#MESSAGE(${GIT_REFSPEC})
#-----------------------------------------------------------------------------\
# Configure files with settings for use by the build.
#-----------------------------------------------------------------------------|
CONFIGURE_FILE(${${PROJECT_NAME}_SOURCE_DIR}/${PROJECT_NAME}Configure.h.in
               ${${PROJECT_NAME}_BINARY_DIR}/${PROJECT_NAME}Configure.h @ONLY IMMEDIATE)
#-----------------------------------------------------------------------------/

ADD_SUBDIRECTORY(CFDTOOLS)
ADD_SUBDIRECTORY(SOURCE)

