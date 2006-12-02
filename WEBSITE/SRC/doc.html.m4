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
item([hyperlink([Input:], [doc_input.html]) describes inputs files and m4_param([main.rpm]) parameters])
item([hyperlink([NS model:], [doc_ns.html]) describes Inviscid and Navier-Stokes models])
item([hyperlink([Conduction model:], [doc_kdif.html]) describes Heat Transfer models])
item([hyperlink([Coupling:], [doc_coupling.html]) describes coupling methods])
item([hyperlink([Output:], [doc_output.html]) describes different kinds of outputs])
item([hyperlink([Numerical schemes:], [doc_scheme.html]) describes numerical parameters])
<br>

dnl -------------------------------------------------------------
section([Documentation news])

item([2006, November: numerical schemes for flow equations])
<br>

dnl -------------------------------------------------------------
section([Search TYPHON website])

m4_googlesearch


skip_line


dnl -------------------------------------------------------------
include_footer
