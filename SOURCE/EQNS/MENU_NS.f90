!------------------------------------------------------------------------------!
! MODULE : MENU_NS                        Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  : Novembre 2002
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les options des solveurs EULER, NS, RANS
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_NS

use TYPHMAKE   ! Definition de la precision
use EQNS      ! Définition des propriétés gaz

implicit none

! -- Variables globales du module -------------------------------------------

! -- Définition des entiers caractéristiques pour le type de solveur --
integer, parameter :: eqEULER = 10
integer, parameter :: eqNSLAM = 11 
integer, parameter :: eqRANS  = 12


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_NS : options numériques les solveurs Euler, NS et RANS
!------------------------------------------------------------------------------!
type mnu_ns
  integer         :: typ_fluid         ! type de fluide (cf definitions parameter) 
  integer         :: nb_species        ! nombre d'espèces résolues
  type(st_espece), dimension(:), pointer &
                  :: properties        ! propriétés des différentes espèces
endtype mnu_ns


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_NS




