!------------------------------------------------------------------------------!
! MODULE : INFO                           Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : cf historique
!   Définition des structures de données générales pour l'intégration (gestion)
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
  real(krp) :: first_res, cur_res   ! residu initial et courant
endtype st_info


!------------------------------------------------------------------------------!
! Définition de la structure ST_INFOZONE : informations sur la zone
!------------------------------------------------------------------------------!
type st_infozone
  logical   :: fin_cycle            ! fin d'intégration du cycle
  integer   :: nbstep               ! nombre de pas maximal du cycle
  real(krp) :: cycle_dt             ! durée du cycle
  real(krp) :: residumax            ! residu maximal admissible pour le cycle
  real(krp) :: first_res, cur_res   ! residu initial (world) et courant (cycle)
endtype st_infozone


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
! mars 2003 : création du module
! sept 2003 : informations spécifiques pour l'intégration d'un cycle
!------------------------------------------------------------------------------!
