include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([!!TITLE!!])
define([id1], [case])
define([id2], [!!SECTION!!])
define([id3], [!!NAME!!])
define([m_]id1[_]id2[_]id3, [!!NAME!!])

include_javascript([showhide.js])
include_header

dnl -------------------------------------------------------------

!!INSERT-IMG!!

dnl -------------------------------------------------------------
include_footer
