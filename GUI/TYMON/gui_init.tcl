#----------------------------------------------------------------------
# gui_init.tcl                                       Jérémie Gressier
#                                                      Juillet 2004
# Initialisation de l'interface graphique pour TYMON 
#----------------------------------------------------------------------
# 07/2004 : création
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# gui:initcolor
#----------------------------------------------------------------------
proc gui:initcolor {} {
  global guicolor

  # Affichage du menu
  set guicolor(menubg)       \#E0E0E0
  set guicolor(menufg)       red

  # Affichage des boutons
  set guicolor(mbuttbg)      \#EEEEEE
  set guicolor(mbuttfg)      \#344E7C
  set guicolor(mbuttabg)     \#DDDDD0
  set guicolor(mbuttafg)     \#111100
  #set guicolor(obuttbg)      \#CC7777
  #set guicolor(obuttfg)      \#F0F0F0

  # Affichage des "entry"
  set guicolor(treelines)    \#888888
  set guicolor(treecross)    \#AAAABB

  # Affichage des "entry"
  set guicolor(entryfg)      black
  set guicolor(entrybg)      white

  # Affichage des flèches et ascensceurs
  set guicolor(scrollfg)     black
  set guicolor(scrollbg)     grey80

  # Affichage du statut
  set guicolor(statutbg)     \#F4F6FC
  set guicolor(statutfg)     \#1C4284

  # Affichage Projet
  set guicolor(projetwg)     \#F0F0F0 ;#B4C2D  ;# widgets
  set guicolor(projetbg)     \#F4F4F4 ;#B4C2D  ;# fonds de panneaux
  set guicolor(projetfg)     \#1C4284

} ;# fin proc gui:initcolor


#----------------------------------------------------------------------
# gui:init : Initialisation de l'interface graphique utilisateur
#----------------------------------------------------------------------
proc gui:init { top } {
  global guivar

  set w  800
  set h  600
  set sw [winfo screenwidth .]
  set sh [winfo screenheight .]
  set x  [expr {($sw - $w)/2}]
  set y  [expr {($sh - $h)/2}]
  wm positionfrom .$top user
  wm sizefrom     .$top user
  wm title        .$top "TYMON v$guivar(version)"
  wm minsize      .$top 400 300
  wm geometry     .$top [expr $w]x$h+$x+$y
  wm protocol     .$top WM_DELETE_WINDOW tymon:quit

  # -- initialisation du menu --
  pack configure [guib:menu $top] -side top -padx 1 -pady 1 -fill x

  # -- initialisation de la frame principale (fzone) --
  pack configure [guib:main $top] -side top -fill both -expand 1

  # -- initialisation de la frame statut --
  pack configure [guib:statut $top] -side bottom -fill x

  #gui:raiseprojet

  set guivar(top) ".$top"

} ;# fin proc gui:init


#----------------------------------------------------------------------
# guib:menu : Initialisation du menu
#----------------------------------------------------------------------
proc guib:menu { f } {
  global guicolor guivar

  set fm [frame $f.menu -bg $guicolor(menubg) -relief ridge -borderwidth 2]

  # -- Mise en place du menu boutons GAUCHE (de gauche à droite)
  foreach {mb type nom aide} [ list \
      settings text  "Settings"             "" \
      addtab   image $guivar(image:addtab)  ""              \
      compile  image $guivar(image:process) ""] {
    set b [button $fm.$mb -$type $nom -relief raised -command tymon:$mb      \
      -bg $guicolor(mbuttbg) -fg $guicolor(mbuttfg) -font $guivar(font) \
      -activebackground $guicolor(mbuttabg) -activeforeground $guicolor(mbuttafg) \
      -highlightthickness 0 \
      -padx 2 -pady 2 ]
    pack configure $b -side left -padx 3 -pady 3
    lappend guivar(wlist4font) $fm.$mb
    setbindstatut $fm.$mb $aide
  }

  # -- Mise en place du menu boutons DROITE (de droite à gauche)
  foreach {mb type nom aide} [ list \
      quit         text  "Quit"              "Quit graphical interface" \
      saveproject  image $guivar(image:save) "Save project settings"] {
    button $fm.$mb -$type $nom -relief raised -command tymon:$mb      \
      -bg $guicolor(mbuttbg) -fg $guicolor(mbuttfg) -font $guivar(font) \
      -activebackground $guicolor(mbuttabg)  -activeforeground $guicolor(mbuttafg) \
      -highlightthickness 0 \
      -padx 2 -pady 2
    pack configure $fm.$mb -side right -padx 3 -pady 3
    lappend guivar(wlist4font) $fm.$mb
    setbindstatut $fm.$mb $aide
  }

  return $f.menu

} ;# fin proc guib:menu


#----------------------------------------------------------------------
# guib:statut : Initialisation de la barre de statut
#----------------------------------------------------------------------
proc guib:statut { f } {
  global guicolor guivar

  frame $f.statut -bg $guicolor(statutbg) 
  label $f.statut.statutmess -textvariable guivar(statut) \
    -anchor center -relief sunken -font $guivar(font)      \
    -bg $guicolor(statutbg) -fg $guicolor(statutfg)
  lappend guivar(wlist4font) $f.statut.statutmess

  pack configure $f.statut.statutmess -fill x -side left -expand 1

  return $f.statut

} ;# fin proc guib:statut


#----------------------------------------------------------------------
# guib:main : Init. de la frame principale avec tabs
#----------------------------------------------------------------------
proc guib:main { f } {
  global guicolor guivar

  #set fm [frame $f.main -bg $guicolor(projetbg) ]

  #set notebook [NoteBook $fm.nb]
  set fm [guib:plot $f]

  return $fm
 
} ;# fin proc guib:main


#----------------------------------------------------------------------
# setbindstatut : Définition des contrôles souris pour les statuts
#----------------------------------------------------------------------
proc setbindstatut { widget str } {
  bind $widget <Enter> "gui:setstatut {$str}"
  bind $widget <Leave> {gui:setstatut ""}
}

