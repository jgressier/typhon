#----------------------------------------------------------------------
# tymon_process.tcl                                  Jérémie Gressier
#                                                       July 2004
# Reading monitor files
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# tymon:process
#----------------------------------------------------------------------
proc tymon:process {} {
  global projet guivar gplot

  set projet(save) 0

  set gplot(variables) {residual massflow flux lift}

  tymon:process_file pipo

} ;# Fin proc tymon:process


#----------------------------------------------------------------------
# tymon:process_file
#----------------------------------------------------------------------
proc tymon:process_file { fic } {
  global projet guivar



} ;# Fin proc tymon:process_file















