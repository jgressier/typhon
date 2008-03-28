!------------------------------------------------------------------------------!
! MODULE : MODINFO                        Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : cf historique
!   Definition des structures de donnees generales pour l'integration (gestion)
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
! Definition de la structure ST_INFO : informations generales sur la gestion du calcul
! niveau WORLD
!------------------------------------------------------------------------------!
type st_info
  logical   :: fin_integration      ! fin d'integration
  integer   :: icycle               ! cycle courant
  real(krp) :: curtps               ! temps physique courant
  real(krp) :: residu_ref, cur_res  ! residu de reference et courant
  integer   :: nbproc               ! total number of communicating processors
  integer, pointer &
            :: headproc(:)          ! id of heading proc for each zone
endtype st_info


!------------------------------------------------------------------------------!
! Definition de la structure ST_INFOZONE : informations sur la zone
!------------------------------------------------------------------------------!
type st_infozone
  character :: typ_temps            ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  logical   :: end_cycle            ! end of cycle
  integer   :: iter_tot             ! nombre d'iteration total
  integer   :: iter_loc             ! nombre d'iteration local dans le cycle
  integer   :: nbstep               ! nombre de pas maximal du cycle
  real(krp) :: cycle_start          ! starting time of current cycle
  real(krp) :: cycle_dt             ! duration      of cycle
  real(krp) :: cycle_time           ! local time    in the cycle
  real(krp) :: residumax            ! residu maximal admissible pour le cycle
  real(krp) :: residu_ref, cur_res  ! residu de reference (world) et courant (cycle)
  real(krp) :: residu_reforigine    ! residu de reference du premier cycle
  integer   :: nbproc               ! total number of communicating processors in the zone
  integer   :: headproc
  integer, pointer &
            :: proc(:)              ! list of proc. which are computing this zone
endtype st_infozone


! -- INTERFACES -------------------------------------------------------------



! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains

!------------------------------------------------------------------------------!
! Procedure : 
!------------------------------------------------------------------------------!




endmodule MODINFO

!------------------------------------------------------------------------------!
! Change history
!
! mars 2003 : creation du module
! sept 2003 : informations specifiques pour l'integration d'un cycle
! oct 2003  : ajout de residu_ref_origine
!------------------------------------------------------------------------------!
