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
  integer   :: my_id                ! id of the current process
  integer, dimension(:), pointer &
            :: head_proc            ! id of heading proc for each zone
  logical   :: cvloc                ! satisfaction du critere de convergence 
                                    ! locale
endtype st_info


!------------------------------------------------------------------------------!
! Definition de la structure ST_INFOZONE : informations sur la zone
!------------------------------------------------------------------------------!
type st_infozone
  character :: typ_temps            ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  logical   :: fin_cycle            ! fin d'integration du cycle
  integer   :: iter_tot             ! nombre d'iteration total
  integer   :: iter_loc             ! nombre d'iteration local dans le cycle
  integer   :: nbstep               ! nombre de pas maximal du cycle
  real(krp) :: cycle_dt             ! duree du cycle
  real(krp) :: residumax            ! residu maximal admissible pour le cycle
  real(krp) :: residu_ref, cur_res  ! residu de reference (world) et courant (cycle)
  real(krp) :: residu_reforigine    ! residu de reference du premier cycle
  integer   :: nbproc               ! total number of communicating processors in the zone
  integer   :: my_id                ! id of the current process
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
! Historique des modifications
!
! mars 2003 : creation du module
! sept 2003 : informations specifiques pour l'integration d'un cycle
! oct  2003 : ajout de residu_ref_origine
! oct  2004 : local convergence parameters 
!------------------------------------------------------------------------------!
