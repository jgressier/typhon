!------------------------------------------------------------------------------!
! MODULE : INFO                           Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : 
!   Définition des structures de données générales
!   Encapsulation de toutes les structures
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MODINFO

use TYPHMAKE     ! Definition de la precision


implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure ST_INFO : informations générales sur la gestion du calcul
!------------------------------------------------------------------------------!
type st_info
  logical   :: fin_integration      ! fin d'intégration
  integer   :: icycle               ! cycle courant
  real(krp) :: curtps               ! temps physique courant
endtype st_info


! -- INTERFACES -------------------------------------------------------------



! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains

!------------------------------------------------------------------------------!
! Procédure : 
!------------------------------------------------------------------------------!




endmodule MODINFO

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 (v0.0.1b): création du module
!------------------------------------------------------------------------------!
