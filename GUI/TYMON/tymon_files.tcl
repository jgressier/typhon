#----------------------------------------------------------------------
# tymon_files.tcl                                    Jérémie Gressier
#                                                       July 2004
# Initialisation pour TYMON
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# tymon:loadproject
#----------------------------------------------------------------------
proc tymon:loadproject { fic } {
  global env projet

  # LOAD TYMON.PRJ
  #set projet() 

  # LOAD
  tymon:process

  set projet(save) 1

} ;# Fin proc tymon:loadproject


#----------------------------------------------------------------------
# tymon:saveproject
#----------------------------------------------------------------------
proc tymon:saveproject { fic } {
  global env projet

  set projet(save) 1

} ;# Fin proc tymon:saveproject










