!------------------------------------------------------------------------------!
! MODULE : CGNS_STRUCT                    Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : 
!   Définition des structures de données pour la lecture et la gestion
!   de fichiers CGNS
!
! Defauts/Limitations/Divers :
!   La notion de ZONE pour le maillage CGNS est différent de la ZONE pour TYPHON
!   En CGNS, une ZONE est une entité de maillage (ensemble non structuré ou bloc
!   structuré). Il y a en général plusieurs ZONEs CGNS pour définir un maillage
!   (structuré), alors que cela ne représentera qu'une seule ZONE dans TYPHON.
!
!------------------------------------------------------------------------------!

module CGNS_STRUCT   

use TYPHMAKE             ! Définition de la précision
use CGNSLIB             ! Définition des constantes pour les types CGNS
use GEO3D            ! MES! Compilation conditionnelle ? avec GEO3D_dp

!use mod_nuage_de_points         ! TYPE INCLUS: NUAGE DE POINTS  
!use mod_connectivite            ! TYPE INCLUS: CONNECTIVITE   

implicit none         

! -- Variables globales du module -------------------------------------------

!integer, parameter :: cgnslen   = 30  ! défini dans CGNSLIB
integer, parameter :: maxconnect = 8   ! nombre maximum de sommets par élément


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! ST_CGNS_USTCONNECT : Définition de la connectivité
!   Sommets, faces, cellules
!------------------------------------------------------------------------------!
type st_cgns_ustconnect
  integer                 :: nbnodes     ! nombre de d'ensemble connectivités
  integer                 :: ideb, ifin  ! indice de début et de fin
  integer                 :: type        ! type d'éléments (cf CGNSLIB)
  integer                 :: imesh       ! type géométrique (1D, 2D ou 3D)
  integer                 :: nbfils      ! nombre de connectivités par ensemble
                                         !   selon le type
  integer, dimension(:,:), pointer &
                          :: fils        ! définition de la connectivité
endtype st_cgns_ustconnect


!------------------------------------------------------------------------------!
! ST_CGNS_VTEX : structure réceptrice des sommets des maillages
!------------------------------------------------------------------------------!
type st_cgns_vtex
  integer                 :: ni, nj, nk   ! nombre de sommets
  type(v3d), dimension(:,:,:), pointer &
                          :: vertex       ! liste des sommets
endtype st_cgns_vtex


!------------------------------------------------------------------------------!
! ST_CGNS_PATCH : structure de définition de patch
!------------------------------------------------------------------------------!
type st_cgns_patch
  integer                          :: nbvtex   ! nombre de sommets
endtype st_cgns_patch


!------------------------------------------------------------------------------!
! ST_CGNS_BOCO : structure de définition de condition aux limites
!------------------------------------------------------------------------------!
type st_cgns_boco
  character(len=cgnslen)  :: nom          ! nom de la condition aux limites
  character(len=cgnslen)  :: family       ! nom de la condition aux limites
  integer                 :: nvtex        ! nombre de sommets
  integer, dimension(:), pointer &
                          :: ivtex        ! liste des sommets 
                                          ! (pointeurs entiers dans zone%mesh)
endtype st_cgns_boco


!------------------------------------------------------------------------------!
! ST_CGNS_ZONE : structure réceptrice des données par zone
!------------------------------------------------------------------------------!
type st_cgns_zone
  character(len=cgnslen)  :: nom          ! nom de la zone
  integer                 :: imesh        ! type de maillage (2: 2D, 3: 3D) IDEM BASE
  integer                 :: type         ! type de zone (structurée ou non)
  type(st_cgns_vtex)      :: mesh
  integer                 :: ncellfam, &  ! nombre de familles de connectivités de 
                             nfacefam, &  ! (   cellules, faces,   bords)
                             nedgefam     ! (3D: volume,  surface, ligne)
                                          ! (2D: surface, ligne,   X)
  type(st_cgns_ustconnect), dimension(:), pointer &
                          :: cellfam, &   ! sections de connectivité par type d'élément
                             facefam, &   ! (cellules, faces, bords)
                             edgefam
  integer                 :: npatch       ! nombre de patchs (connectivité en structuré)
  type(st_cgns_patch), dimension(:), pointer &
                          :: patch        ! patch de maillage structuré
  integer                 :: nboco        ! nombre de conditions aux limites 
  type(st_cgns_boco), dimension(:), pointer &
                          :: boco         ! liste des conditions aux limites
endtype st_cgns_zone


!------------------------------------------------------------------------------!
! ST_CGNS_BASE : structure réceptrice des données par base
!------------------------------------------------------------------------------!
type st_cgns_base  
  character(len=cgnslen) :: nom        ! nom de la base
  integer                 :: imesh     ! type de maillage (2: 2D, 3: 3D)
  integer                 :: igeo      ! nombre de coordonnées
  integer                 :: nzone     ! nombre de zones
  integer                 :: nzone_str ! nombre de zones structurées
  integer                 :: nzone_ust ! nombre de zones non structurées
  type(st_cgns_zone), dimension(:), pointer &
                          :: zone   ! liste des zones
endtype st_cgns_base  


!------------------------------------------------------------------------------!
! ST_CGNS_WORLD : structure principale réceptrice des données du fichier CGNS
!------------------------------------------------------------------------------!
type st_cgns_world
  character(len=cgnslen) :: nom    ! nom DU JEU DE DONNEES  
  integer                 :: nbase  ! nombre de bases
  type(st_cgns_base), dimension(:), pointer &
                          :: base   ! liste des bases
endtype st_cgns_world





endmodule CGNS_STRUCT
