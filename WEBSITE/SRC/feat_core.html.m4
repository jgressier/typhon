include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Solver features - Core organization])
define([id1], [feat])
define([id2], [core])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([Time integration])
item([explicit])
item([fully implicit (backward Euler)])
<br>

section([Parallel computation])
item([automatic mesh splitting])
item([exchange values at interfaces (no gradient communication)])
<br>

section([Common methods])
item([gradient computation: least squares or weighted least squares])
<br>

skip_line

dnl -------------------------------------------------------------
include_footer

