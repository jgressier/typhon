#----------------------------------------------------------------------
# tymon_init.tcl                                     Jérémie Gressier
#                                                       July 2004
# Initialisation pour TYMON
#----------------------------------------------------------------------
# 07/2004 : création
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# tymon:init
#----------------------------------------------------------------------
proc tymon:initproject {} {
  global env guivar projet gplot

  puts stdout "TEST TYMON:INITPROJECT"
  set gplot(variables) {}

} ;# Fin proc tymon:initproject


#----------------------------------------------------------------------
# tymon:loadconf : lecture du fichier de configuration
#----------------------------------------------------------------------
proc tymon:loadconf {} {
  global env guicolor guivar settings

  # initialisation générale
  set guivar(statut)     "graphical user interface initialisation..."
  set guivar(wlist4font) ""
  set settings(font)     "helvetica -11"
  gui:initcolor

  # définition du fichier de configuration .tymon
  if ![info exists env(HOME)] {
    set guivar(conf_file) [file join $guivar(HOME) .tymon]
  } else {
    set guivar(conf_file) [file join $env(HOME) .tymon]
  }

  # lecture du fichier 
  if [file exists $guivar(conf_file)] {
    puts stdout "reading configuration file"
    source $guivar(conf_file)
  }

  # initialisation en fonction des settings
  option add *foreground $guicolor(projetfg)
  option add *background $guicolor(projetbg)
  option add *font       $settings(font)
  set guivar(font)       $settings(font)

} ;# fin proc tymon:loadconf


#----------------------------------------------------------------------
# tymon:writeconf : sauvegarde du fichier de configuration
#----------------------------------------------------------------------
proc tymon:writeconf {} {
  global guicolor guivar settings

  set output ""

  # guicolor
  foreach {name color} [array get guicolor] {
    append output "set guicolor($name) \"$color\"\n"
  }

  # settings
  foreach {name param} [array get settings] {
    append output "set settings($name) \"$param\"\n"
  }

  # écriture
  set file [open $guivar(conf_file) w]
  puts $file $output

  # fin
  close $file

} ;# fin proc tymon:writeconf


#----------------------------------------------------------------------
# tymon:loadlib
#----------------------------------------------------------------------
proc tymon:loadlib {} {
  global guivar env auto_path

  #lappend auto_path [file join $env(HOME) LOCAL BWidget-1.4.0]
  lappend auto_path $guivar(HOME)

  # Nécessite éventuellement "pkg_mkIndex"
  foreach {package version} {BWidget 1.0 BLT 2.4} {
    gui:splash_statut "reading package $package..."
    package require $package $version
  }

  # Lecture des fichiers sources TCL
  foreach source {gui_init gui_plot_init gui_update \
                  tymon_files tymon_command tymon_process \
                  tymon_settings tymon_plot tymon_data} {
    gui:splash_statut "reading tymon source $source.tcl..."
    source [file join $guivar(HOME) $source.tcl]
  }

  # Lecture des images
  foreach {image format nom} \
    { add-tab.gif gif addtab       unknown.gif gif unknown      file.gif gif file    \
      update.gif  gif update       save.gif gif save    \
      work.gif    gif process      } {
    gui:splash_statut "reading picture $image..."
    set guivar(image:$nom) [image create photo -format $format -file \
                             [file join $guivar(HOME) images $image ] ]
  }
  set guivar(image:main) $guivar(image:file)

  gui:splash_statut "reading configuration file..."
  tymon:loadconf

  #gui:splash_statut "Lecture des polices de caractères..."
  #SelectFont::loadfont
  gui:splash_statut "graphical user interface initialisation..."

} ;# Fin proc tymon:loadlib


#----------------------------------------------------------------------
# gui:splash_statut
#----------------------------------------------------------------------
proc gui:splash_statut { message } {
  global guivar

  set guivar(splashMsg) $message
  update

} ;# fin proc gui:splash_statut


#----------------------------------------------------------------------
# gui:splash
#----------------------------------------------------------------------
proc gui:splash {} {
  global guivar

  toplevel .splash -bd 3 -relief raised
  wm withdraw .splash

  image create photo "title" \
    -file [file join $guivar(HOME) images title.pnm]
  wm overrideredirect .splash 1

  label .splash.l -image title -bd 3 -relief sunken -bg black
  pack .splash.l -side top -expand 1 -fill both

  entry .splash.status -relief flat -bd 3 -bg black -fg white \
    -textvar guivar(splashMsg) -font "Helvetica"
  pack .splash.status -side bottom -expand 1 -fill both

  set sw [winfo screenwidth .]
  set sh [winfo screenheight .]
  set x [expr {($sw - 444)/2}]
  set y [expr {($sh - 264)/2-50}]

  wm geometry  .splash +$x+$y
  wm deiconify .splash
  update idletasks

} ;# fin proc gui:splash










