program test_fct

use FCT_PARSER

character(len=100) :: str

str="x+y"
print*,trim(str)
print*,"x",index_oper(str,"x")
print*,"+",index_oper(str,"+")
print*,"y",index_oper(str,"y")

str="sin(x)+cos(3*y)+x"
print*,trim(str)
print*,"x",index_oper(str,"x")
print*,"y",index_oper(str,"y")
print*,"3",index_oper(str,"3")
print*,"+",index_oper(str,"+")
print*,"cos",index_oper(str,"cos")

str=" 1.E-9 + (.9622504E-0-1.E-009)*step(-2-X)"
call test_parse(str)

str="cos(_pi)"
call test_parse(str)

contains 

subroutine test_parse(str)
implicit none
character(len=*)   :: str
character(len=128) :: str2
type(st_fct_node)  :: func
integer            :: ierr

call string_to_funct(str, func, ierr)
call fct_node_to_str(func, str2)
print*,trim(str)," -> ",trim(str2)
call delete(func)

endsubroutine

endprogram
