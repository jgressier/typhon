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

endprogram
