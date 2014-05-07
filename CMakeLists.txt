
cmake_minimum_required (VERSION 2.8.4)

#SET(NERSC_HOST_ENV $ENV{NERSC_HOST})
#if(DEFINED NERSC_HOST_ENV)
##Nersc config
#if(${NERSC_HOST_ENV} STREQUAL "hopper")
#set(CMAKE_Fortran_COMPILER ftn)
#set(CMAKE_C_COMPILER cc)
#set(CMAKE_CXX_COMPILER CC)
#endif(${NERSC_HOST_ENV} STREQUAL "hopper")
#endif(DEFINED NERSC_HOST_ENV)
#
#SET(CTR_HOST $ENV{HOST})
#if(DEFINED CTR_HOST_ENV)
#if(CTR_HOST_ENV STREQUAL "certainty")
#set(CMAKE_Fortran_COMPILER mpif90)
#set(CMAKE_C_COMPILER mpicc)
#set(CMAKE_CXX_COMPILER mpiCC)
#endif(CTR_HOST_ENV STREQUAL "certainty")
#endif(DEFINED CTR_HOST_ENV)
#
#SET(USER_HYPRE_LIBDIR     $ENV{HYPRE_DIR}/lib )
#SET(USER_HYPRE_INCLUDEDIR $ENV{HYPRE_DIR}/include/ )
#SET(USER_FFTW_LIBDIR     $ENV{FFTW_DIR}/lib )
#SET(USER_FFTW_INCLUDEDIR $ENV{FFTW_DIR}/include/ )
#SET(USER_LAPACK_LIBDIR     $ENV{LAPACK_DIR}/lib )
#SET(USER_HYPRE_INCLUDEDIR $ENV{HYPRE_DIR}/include/ )


project(Typhon Fortran)

set(CMAKE_Fortran_COMPILER mpif90)
#set(CMAKE_C_COMPILER mpicc)
#set(CMAKE_CXX_COMPILER mpiCC)

SET(pName Typhon)

SET(${PROJECT_NAME}_CMAKE_DIR "${${PROJECT_NAME}_SOURCE_DIR}/CMake")
SET(CMAKE_MODULE_PATH "${${PROJECT_NAME}_CMAKE_DIR}" ${CMAKE_MODULE_PATH})
#get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

SET(CMAKE_Fortran_FLAGS "-cpp " ${CMAKE_Fortran_FLAGS}) #ONLY GFORTRAN
 
#message("CMAKE_Fortran_COMPILER_INIT = ${CMAKE_Fortran_COMPILER_INIT}")
#message("CMAKE_Fortran_COMPILER_FULLPATH = ${CMAKE_Fortran_COMPILER_FULLPATH}")
#message("CMAKE_Fortran_COMPILER = ${CMAKE_Fortran_COMPILER}")
#message("CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")
#message("CMAKE_Fortran_COMPILER_NAME = ${CMAKE_Fortran_COMPILER_NAME}")
#message("CMAKE_Fortran_COMPILER_ID = ${CMAKE_Fortran_COMPILER_ID}")

if (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
  # cray compilers nersc
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -fastsse")
  set (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O3 -g -fastsse")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -C -traceback -Mpgicoff -Mchkptr -O0")
  set (CMAKE_C_FLAGS_RELEASE "-O3 -fastsse") 
  set (CMAKE_CXX_FLAGS_RELEASE "-O3 -fastsse") 
  set (CMAKE_C_FLAGS_RELWITHDEBINFO "-O3 -g -fastsse") 
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL  "Intel") # certainty so far
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -ip -xSSE4.2")
  set (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O3 -g -ip -xSSE4.2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -CA -CB -CS -CV -traceback -debug all -ftrapuv -WB -warn unused")
else (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
  set (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O0 -g")
endif (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")


# Set default cmake build type to RelWithDebInfo
# (None Debug Release RelWithDebInfo MinSizeRel)
IF( NOT CMAKE_BUILD_TYPE )
   SET( CMAKE_BUILD_TYPE "RelWithDebInfo" )
ENDIF()

## Use LAPACK library
#OPTION(${PROJECT_NAME}_USE_LAPACK
#       "Use LAPACK library."
#       OFF)
##MARK_AS_ADVANCED(${PROJECT_NAME}_USE_LAPACK)
#
## Use Hypre library
#OPTION(${PROJECT_NAME}_USE_HYPRE
#       "Use Hypre library."
#       OFF)
##MARK_AS_ADVANCED(${PROJECT_NAME}_USE_HYPRE)
#
## Use Fftw library
#OPTION(${PROJECT_NAME}_USE_FFTW
#       "Use fftw library."
#       ON)

# IF USE_LAPACK is active, look for LAPACK package and initialize LAPACK
#IF(${PROJECT_NAME}_USE_LAPACK)
#  FIND_PACKAGE(LAPACK REQUIRED)
#ENDIF(${PROJECT_NAME}_USE_LAPACK)
#
## IF USE_HYPRE is active, look for HYPRE package
#IF(${PROJECT_NAME}_USE_HYPRE)
#  FIND_PACKAGE(HYPRE REQUIRED)
#ENDIF(${PROJECT_NAME}_USE_HYPRE)
#
## IF USE_FFTW is active, look for FFTW package
#IF(${PROJECT_NAME}_USE_FFTW)
#  FIND_PACKAGE(FFTW REQUIRED)
#ENDIF(${PROJECT_NAME}_USE_FFTW)

# ${PROJECT_NAME} version number.  An even minor number corresponds to releases.
SET(${PROJECT_NAME}_MAJOR_VERSION 0)
SET(${PROJECT_NAME}_MINOR_VERSION 1)
SET(${PROJECT_NAME}_BUILD_VERSION 1)
SET(${PROJECT_NAME}_VERSION
    "${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_BUILD_VERSION}")

# Use CGNS library
OPTION(${PROJECT_NAME}_USE_CGNS
       "Use CGNS library." 
       OFF)
MARK_AS_ADVANCED(${PROJECT_NAME}_USE_CGNS)

IF(${PROJECT_NAME}_USE_CGNS)
  #FIND_PACKAGE(CGNS REQUIRED)
  ADD_DEFINITIONS("-DCGNS")
ENDIF(${PROJECT_NAME}_USE_CGNS)

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
message("Installation to ${CMAKE_INSTALL_PREFIX}")
ELSE (DEFINED INSTALL_PREFIX_ENV)
SET(CMAKE_INSTALL_PREFIX "$ENV{PWD}/installationDir/" CACHE INTERNAL "Prefix prepended to install directories" FORCE)
message("Installation to ${CMAKE_INSTALL_PREFIX}")
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


ADD_SUBDIRECTORY(CFDTOOLS)
ADD_SUBDIRECTORY(SOURCE)

