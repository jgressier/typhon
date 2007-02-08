include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Solver features - Input files])
define([id1], [feat])
define([id2], [input])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([Parameters file])
item([keyword based (see documentation)])

skip_line
section([File formats])
item([CGNS])

skip_line
section([Mesh description])
item([scaling function])

skip_line
section([Initialization fields])
item([symbolic functions])
item([internal format (development)])

skip_line
section([User Defined Functions])
item([Conductivity definition (KDIF solver)])
item([Flow initialization (NS solver)])

skip_line
section([Stop & resume])
item([External stop of computation])


skip_line

dnl -------------------------------------------------------------
include_footer
