include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Development])
define([id1], [dev])
define([id2], [])
define([id3], [])

include_header

dnl -------------------------------------------------------------

<p>
You can find: <br>
item([a presentation of the hyperlink([development team], [dev_team.html]),]) 
item([a description of all hyperlink([previous changes], [dev_chlog.html])])  
item([and an expected hyperlink([roadmap], [dev_rmap.html]) of future developments.]) 
</p>

<p>
Sourceforge project page provides: <br>
item([hyperlink([Bug tracker],     [http://sourceforge.net/tracker/?group_id=132321&atid=723918])])
</p>

skip_line

dnl -------------------------------------------------------------
include_footer
