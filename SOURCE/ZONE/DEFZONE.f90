!------------------------------------------------------------------------------!
! MODULE : DEFZONE                        Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : (cf historique)
!   Définition des structures de données des zones (contient
!   maillage, type de solveur et info)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module DEFZONE

use TYPHMAKE      ! Definition de la precision/données informatiques
use MODINFO       ! Information pour la gestion de l'intégration
use MENU_SOLVER   ! Définition des solveurs
use MENU_NUM      ! Définition des paramètres numériques d'intégration
use MENU_MESH     ! Définition du maillage
use STRMESH       ! Définition des maillages structurés
use USTMESH       ! Définition des maillages non structurés
!use BOUND        ! Librairie de définition des conditions aux limites
use MENU_ZONECOUPLING ! Définition des structures d'échange entre zones
use DEFFIELD      ! Définition des champs physiques

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure ST_ZONE : zone maillage général et champ
!------------------------------------------------------------------------------!
type st_zone
  integer               :: id         ! numéro de zone
  character(len=strlen) :: nom        ! nom de la zone
  integer               :: ndom       ! nombre de domaine total (cas hybride)
  integer               :: nmesh_str  ! nombre de domaines     structurés
  integer               :: nmesh_ust  ! nombre de domaines non structurés
  type(st_infozone)     :: info       ! information sur l'intégration
  type(mnu_solver)      :: defsolver  ! type de solveur à utiliser 
  type(mnu_time)        :: deftime    ! paramètres d'intégration temporelle
  type(mnu_spat)        :: defspat    ! paramètres d'intégration spatiale
                                      !   cf définitions variables globales
  type(mnu_mesh)        :: defmesh    ! type de maillage
  character             :: typ_mesh   ! type de maillage (cf VARCOM)
                                      !   S : multibloc structuré
                                      !   U : non structuré
                                      !   H : hybride
  integer               :: mpi_cpu    ! numéro de CPU chargé du calcul

  type(st_strmesh), dimension(:), pointer &
                        :: str_mesh   ! maillage multibloc structuré
  type(st_ustmesh)      :: ust_mesh   ! maillage non structuré
  type(st_field), dimension(:), pointer &
                        :: field      ! tableau des champs

  integer               :: ncoupling  ! nombre d'échanges avec d'autres zones
  type(mnu_zonecoupling), dimension(:), pointer &
                        :: coupling   !definition des raccords avec d'autres zones
endtype st_zone


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_zone
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure ZONE
!------------------------------------------------------------------------------!
subroutine delete_zone(zone)
implicit none
type(st_zone)  :: zone
integer        :: i     

  print*,'destruction de zone interne / mesh :',zone%nmesh_str, zone%nmesh_ust !! DEBUG
  if (zone%nmesh_str >= 1) then
    do i = 1, zone%nmesh_str
      call delete(zone%str_mesh(i))   
    enddo 
    deallocate(zone%str_mesh)
  endif
  
  call delete(zone%defsolver)
  
  if (zone%nmesh_ust >= 1) then
    print*,"desallocation ust_mesh" !! DEBUG
    call delete(zone%ust_mesh)
  endif
  
!  if (zone%ncoupling >= 1) then
    print*,"desallocation tableau coupling" !! DEBUG
    do i = 1, zone%ncoupling
      print*,"desallocation coupling ",i !! DEBUG
      call delete(zone%coupling(i))
    enddo  
    deallocate(zone%coupling)
!  endif

  do i = 1, zone%ndom
    print*,"desallocation champ ",i !! DEBUG
    call delete(zone%field(i))
  enddo
  deallocate(zone%field)

  print*,'fin de destruction de zone interne' !! DEBUG

endsubroutine delete_zone




endmodule DEFZONE

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2002 : création du module
! juin 2003 : structuration des champs par type (scalaire, vecteur...)
! juil 2003 : delete zone%defsolver
!------------------------------------------------------------------------------!
