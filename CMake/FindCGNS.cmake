  # Find the CGNS includes and libraries
  #
  # The CFD General Notation System (CGNS) provides a general, portable, and extensible standard 
  # for the storage and retrieval of computational fluid dynamics (CFD) analysis data. It consists
  # of a collection of conventions, and free and open software implementing those conventions. 
  # It is self-descriptive, machine-independent, well-documented, and administered by an international
  # steering committee. It is also an American Institute of Aeronautics and Astronautics (AIAA) Recommended Practice.
  # http://cgns.sourceforge.net/WhatIsCGNS.html
  #
  # CGNS_INCLUDE_DIR - where to find autopack.h
  # CGNS_LIBRARIES   - List of fully qualified libraries to link against.
  # CGNS_FOUND       - Do not attempt to use if "no" or undefined.
  
 FIND_PATH(CGNS_INCLUDE_DIR cgnslib_f.h
    $ENV{CGNS_INCLUDE_DIR}
    $ENV{CGNSHOME}
    $ENV{CGNSHOME}/include
    $ENV{PARCGNS_INCLUDE_DIR}
    $ENV{PARCGNSHOME}
    $ENV{PARCGNSHOME}/include
    /usr/local/include
    /usr/include
  )
  
  FIND_LIBRARY(CGNS_LIBRARY cgns
    $ENV{CGNS_LIB_DIR}
    $ENV{CGNSHOME}
    $ENV{CGNSHOME}/lib
    /usr/local/lib
    /usr/lib
  )

  IF(CGNS_INCLUDE_DIR)
    IF(CGNS_LIBRARY)
      SET( CGNS_LIBRARIES ${CGNS_LIBRARY})
      SET( CGNS_FOUND "YES" )
    ENDIF(CGNS_LIBRARY)
  ENDIF(CGNS_INCLUDE_DIR)
