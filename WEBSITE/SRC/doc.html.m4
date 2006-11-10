include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Documentation])
define([id1], [doc])
define([id2], [])
define([id3], [])

include_header

dnl -------------------------------------------------------------

[<p>In this section, you will find useful parts which describe how to specify
physical models, numerical parameters in TYPHON. All information are organized as follows:</p>]
item([hyperlink([Input:], [doc_input.html]) ])
item([hyperlink([NS model:], [doc_ns.html])])
item([hyperlink([Conduction model:], [doc_kdif.html])])
item([hyperlink([Coupling:], [doc_coupling.html])])
item([hyperlink([Output:], [doc_output.html])])
item([hyperlink([Numerical schemes:], [doc_scheme.html])])
<br>

dnl -------------------------------------------------------------
section([Documentation news])

dnl item([hyperlink([Input], id1[_input.html])])
<br>

dnl -------------------------------------------------------------
section([Search TYPHON website])

m4_googlesearch


skip_line


dnl -------------------------------------------------------------
include_footer
