#----------------------------------------------------------------------
# tymon_data.tcl                                    Jérémie Gressier
#                                                      August 2004
# Parse data files
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# tymon:getfiles
#----------------------------------------------------------------------
proc tymon:getfiles { } {
  global monfiles

  set monfiles(list:res) [ glob -nocomplain monres.* ]
  set monfiles(list:phy) [ glob -nocomplain monphy.* ]
  puts ">$monfiles(list:res):$monfiles(list:phy)<"
  foreach file [concat $monfiles(list:res) $monfiles(list:phy)] {
    puts "init $file"
    set monfiles($file:pos)   0
    set monfiles($file:index) -1
  }

} ;# Fin proc tymon:getfiles


#----------------------------------------------------------------------
# tymon:parseallfiles_loop
#----------------------------------------------------------------------
proc tymon:parseallfiles_loop { } {
  global monfiles

  foreach file [concat $monfiles(list:res) $monfiles(list:phy)] {
    tymon:parsefile $file
  }
  after 500 tymon:parseallfiles_loop

} ;# Fin proc tymon:parseallfiles_loop


#----------------------------------------------------------------------
# tymon:openfile
#----------------------------------------------------------------------
proc tymon:openfile { file } {
  global monfiles

  set unit [open $file r]
  fconfigure $unit -blocking no -buffering line
  seek $unit $monfiles($file:pos) start
  return $unit

} ;# Fin proc tymon:openfile


#----------------------------------------------------------------------
# tymon:newvariables
#----------------------------------------------------------------------
proc tymon:newvariables { file line} {
  global mondata

  puts $line
  set line [regsub "^ *" $line ""]  ;# suppression de blanc en début de ligne
  set line [regsub " *$" $line ""]  ;# suppression de blanc en fin   de ligne
  set line [regsub " +" $line " "]
  puts $line
  set mondata($file:cur_var) [split $line " "]
  set mondata($file:scan) [string repeat "%f " [llength $mondata($file:cur_var)]]
  guib:addvariables $mondata($file:cur_var)
  foreach var $mondata($file:cur_var) {
    blt::vector create vec.$var
  }

} ;# Fin proc tymon:newvariables


#----------------------------------------------------------------------
# tymon:parsefile
#----------------------------------------------------------------------
proc tymon:parsefile { file } {
  global monfiles mondata 

  set unit [tymon:openfile $file]
  puts "lecture $file"
  while {[eof $unit] == 0} {
    gets $unit line
    if {[string length $line] > 0} {
      if [regexp -nocase "^@variables *:" $line] {
        tymon:newvariables $file [regsub -nocase  "^@variables *:" $line ""]
      } else {
        puts "index:[incr monfiles($file:index)]"
        foreach var $mondata($file:cur_var) value [scan "$line" $mondata($file:scan)] {
          vec.$var variable vec
          set vec(++end) $value
	}
      }
    }
  }
  set monfiles($file:pos) [tell $unit]
  close $unit

} ;# Fin proc tymon:parsefile












