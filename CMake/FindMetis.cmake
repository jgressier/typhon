  # Find the PARMETIS includes and libraries
  #
  # ParMETIS is an MPI-based parallel library that implements a variety of
  # algorithms for 
  # partitioning unstructured graphs, meshes, and for computing fill-reducing
  # orderings of 
  # sparse matrices. It can be found at:
  # http://www-users.cs.umn.edu/~karypis/metis/parmetis/index.html
  #
  # METIS_INCLUDE_DIR - where to find autopack.h
  # METIS_LIBRARIES   - List of fully qualified libraries to link against.
  # METIS_FOUND       - Do not attempt to use if "no" or undefined.
  
  FIND_PATH(PARMETIS_INCLUDE_DIR parmetis.h
    $ENV{PARMETIS_INCLUDE_DIR}
    $ENV{PARMETISHOME}
    $ENV{PARMETISHOME}/include
    /usr/local/include
    /usr/include
  )
  
  FIND_LIBRARY(PARMETIS_LIBRARY parmetis
    $ENV{PARMETIS_LIB_DIR}
    $ENV{PARMETISHOME}
    $ENV{PARMETISHOME}/lib
    /usr/local/lib
    /usr/lib
  )
 
 FIND_PATH(METIS_INCLUDE_DIR metis.h
    $ENV{METIS_INCLUDE_DIR}
    $ENV{METISHOME}
    $ENV{METISHOME}/include
    $ENV{PARMETIS_INCLUDE_DIR}
    $ENV{PARMETISHOME}
    $ENV{PARMETISHOME}/include
    /usr/local/include
    /usr/include
  )
  
  FIND_LIBRARY(METIS_LIBRARY metis
    $ENV{METIS_LIB_DIR}
    $ENV{METISHOME}
    $ENV{METISHOME}/lib
    $ENV{PARMETIS_LIB_DIR}
    $ENV{PARMETISHOME}
    $ENV{PARMETISHOME}/lib
    /usr/local/lib
    /usr/lib
  )
 #TEMPORARY 
  IF(METIS_INCLUDE_DIR)
    IF(METIS_LIBRARY)
      SET( METIS_LIBRARIES ${METIS_LIBRARY})
      SET( METIS_FOUND "YES" )
    ENDIF(METIS_LIBRARY)
  ENDIF(METIS_INCLUDE_DIR)
  IF(PARMETIS_INCLUDE_DIR)
    IF(PARMETIS_LIBRARY)
      SET( METIS_LIBRARIES ${PARMETIS_LIBRARY} ${METIS_LIBRARIES})
      SET( PARMETIS_FOUND "YES" )
    ENDIF(PARMETIS_LIBRARY)
  ENDIF(PARMETIS_INCLUDE_DIR)
