!----------------------------------------------------------------------------------------
! MODULE : TYPHMAKE                       Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : 
!   Variables globales du code TYPHON
!   Definition de la précision machine et de tailles générales
!
! Defauts/Limitations/Divers :
!
!----------------------------------------------------------------------------------------

module TYPHMAKE

  integer, parameter :: krp = 8  ! simple (4) ou double (8) precision
  integer, parameter :: kip = 4  ! simple (2) ou double (4) precision
  integer, parameter :: kpp = 4  ! taille des variables paramètres (2 devrait suffire)

  integer, parameter :: strlen = 20   ! longueur des noms par défaut

endmodule TYPHMAKE
