cmake_minimum_required (VERSION 2.8.4)

project(Typhon Fortran)

#set(CMAKE_Fortran_COMPILER mpif90)
#set(CMAKE_C_COMPILER mpicc)
#set(CMAKE_CXX_COMPILER mpiCC)

SET(pName Typhon)

SET(${PROJECT_NAME}_CMAKE_DIR "${${PROJECT_NAME}_SOURCE_DIR}/CMake")
SET(CMAKE_MODULE_PATH "${${PROJECT_NAME}_CMAKE_DIR}" ${CMAKE_MODULE_PATH})
#get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

#message(${CMAKE_Fortran_COMPILER_ID})

#SET(CMAKE_Fortran_FLAGS "  " ${CMAKE_Fortran_FLAGS})
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  SET(CMAKE_Fortran_FLAGS "-cpp -ffree-line-length-none -ffixed-line-length-none -fno-range-check" ${CMAKE_Fortran_FLAGS})
  SET (CMAKE_Fortran_FLAGS_RELEASE "-O3 ")
  set (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O3 -g")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -O0")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL  "Intel") 
  SET(CMAKE_Fortran_FLAGS "-cpp " ${CMAKE_Fortran_FLAGS})
  set (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O3 -g -ip -xSSE4.2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -CA -CB -CS -CV -traceback -debug all -ftrapuv -WB -warn unused")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL  "PGI") 
  # cray compilers nersc
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -ip -xSSE4.2")
  set (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O3 -g -ip -xSSE4.2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -CA -CB -CS -CV -traceback -debug all -ftrapuv -WB -warn unused")
else (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
  set (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O0 -g")
endif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")


# Set default cmake build type to RelWithDebInfo
# (None Debug Release RelWithDebInfo MinSizeRel)
IF( NOT CMAKE_BUILD_TYPE )
   SET( CMAKE_BUILD_TYPE "RelWithDebInfo" )
ENDIF()

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
       OFF)
MARK_AS_ADVANCED(${PROJECT_NAME}_USE_CGNS)

# Use Doxygen to generate documentation
OPTION(${PROJECT_NAME}_USE_DOXYGEN
       "Use Doxygen to create documentation." 
       ON)
MARK_AS_ADVANCED(${PROJECT_NAME}_USE_DOXYGEN)

# Use MPI
OPTION(${PROJECT_NAME}_USE_MPI
       "Use mpi for parallel runs." 
       ON)
MARK_AS_ADVANCED(${PROJECT_NAME}_USE_MPI)

IF(${PROJECT_NAME}_USE_CGNS)
  #FIND_PACKAGE(CGNS REQUIRED)
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

