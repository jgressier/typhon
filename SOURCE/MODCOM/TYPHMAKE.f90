!----------------------------------------------------------------------------------------
! MODULE : TYPHMAKE                       Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : 
!   Variables globales du code TYPHON
!   Definition de la precision machine et de tailles generales
!
!----------------------------------------------------------------------------------------

module TYPHMAKE

  integer, parameter :: krp = 8  ! simple (4) or double (8) precision
  integer, parameter :: kip = 4  ! simple (2) or double (4) precision
  integer, parameter :: kpp = 4  ! taille des variables parametres (2 devrait suffire)

  integer, parameter :: shortname = 32   ! tags, quantities...
  integer, parameter :: longname  = 128  ! files

  integer :: tympi_real 

endmodule TYPHMAKE
