!------------------------------------------------------------------------------!
! MODULE : MENU_MPI                     Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : 
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les paramètres de distribution entre processeurs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_MPI

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_MPI : paramètres pour la distribution entre processeurs
!------------------------------------------------------------------------------!
type mnu_mpi
  character       :: temps      ! (S)tationnaire, (I)nstationnaire, (P)ériodique
endtype mnu_mpi



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_MPI




