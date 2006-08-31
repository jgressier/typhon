!------------------------------------------------------------------------------!
! MODULE : CGNS_STRUCT                    Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Definition des structures de donnees pour la lecture et la gestion
!   de fichiers CGNS
!
! Defauts/Limitations/Divers :
!   La notion de ZONE pour le maillage CGNS est different de la ZONE pour TYPHON
!   En CGNS, une ZONE est une entite de maillage (ensemble non structure ou bloc
!   structure). Il y a en general plusieurs ZONEs CGNS pour definir un maillage
!   (structure), alors que cela ne representera qu'une seule ZONE dans TYPHON.
!
!------------------------------------------------------------------------------!

module CGNS_STRUCT   

use TYPHMAKE         ! Definition de la precision
use CGNSLIB          ! Definition des constantes pour les types CGNS
use GEO3D            ! MES! Compilation conditionnelle ? avec GEO3D_dp
use CONNECTIVITY


implicit none         

! -- Variables globales du module -------------------------------------------

!integer, parameter :: cgnslen   = 30  ! defini dans CGNSLIB
integer, parameter :: maxconnect = 8   ! nombre maximum de sommets par element


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! ST_CGNS_USTCONNECT : Definition de la connectivite
!   Sommets, faces, cellules
!------------------------------------------------------------------------------!
type st_cgns_ustconnect
  integer                 :: nbnodes     ! nombre de d'ensemble connectivites
  integer                 :: ideb, ifin  ! indice de debut et de fin
  integer                 :: type        ! type d'elements (cf CGNSLIB)
  integer                 :: imesh       ! type geometrique (1D, 2D ou 3D)
  integer                 :: nbfils      ! nombre de connectivites par ensemble
                                         !   selon le type
  integer, dimension(:,:), pointer &
                          :: fils        ! definition de la connectivite
endtype st_cgns_ustconnect


!------------------------------------------------------------------------------!
! ST_CGNS_VTEX : structure receptrice des sommets des maillages
!------------------------------------------------------------------------------!
type st_cgns_vtex
  integer                 :: ni, nj, nk   ! nombre de sommets
  type(v3d), dimension(:,:,:), pointer &
                          :: vertex       ! liste des sommets
endtype st_cgns_vtex


!------------------------------------------------------------------------------!
! ST_CGNS_PATCH : structure de definition de patch
!------------------------------------------------------------------------------!
type st_cgns_patch
  integer                          :: nbvtex   ! nombre de sommets
endtype st_cgns_patch


!------------------------------------------------------------------------------!
! ST_CGNS_BOCO : structure de definition de condition aux limites
!------------------------------------------------------------------------------!
type st_cgns_boco
  character(len=cgnslen)  :: nom            ! nom de la condition aux limites
  character(len=cgnslen)  :: family         ! nom de la condition aux limites
  integer                 :: gridlocation   ! type de connectivite (cf CGNSLIB)
  type(st_elemc)          :: list           ! liste de noeuds ou de face
  !integer                 :: nvtex          ! nombre de sommets
  !integer, dimension(:), pointer &
  !                        :: ivtex          ! liste des sommets 
  !                                          ! (pointeurs entiers dans zone%mesh)
endtype st_cgns_boco


!------------------------------------------------------------------------------!
! ST_CGNS_ZONE : structure receptrice des donnees par zone
!------------------------------------------------------------------------------!
type st_cgns_zone
  character(len=cgnslen)  :: nom          ! nom de la zone
  integer                 :: imesh        ! type de maillage (2: 2D, 3: 3D) IDEM BASE
  integer                 :: type         ! type de zone (structuree ou non)
  type(st_cgns_vtex)      :: mesh
  integer                 :: ncellfam, &  ! nombre de familles de connectivites de 
                             nfacefam, &  ! (   cellules, faces,   bords)
                             nedgefam     ! (3D: volume,  surface, ligne)
                                          ! (2D: surface, ligne,   X)
  type(st_cgns_ustconnect), dimension(:), pointer &
                          :: cellfam, &   ! sections de connectivite par type d'element
                             facefam, &   ! (cellules, faces, bords)
                             edgefam
  integer                 :: npatch       ! nombre de patchs (connectivite en structure)
  type(st_cgns_patch), dimension(:), pointer &
                          :: patch        ! patch de maillage structure
  integer                 :: nboco        ! nombre de conditions aux limites 
  type(st_cgns_boco), dimension(:), pointer &
                          :: boco         ! liste des conditions aux limites
endtype st_cgns_zone


!------------------------------------------------------------------------------!
! ST_CGNS_BASE : structure receptrice des donnees par base
!------------------------------------------------------------------------------!
type st_cgns_base  
  character(len=cgnslen) :: nom        ! nom de la base
  integer                 :: imesh     ! type de maillage (2: 2D, 3: 3D)
  integer                 :: igeo      ! nombre de coordonnees
  integer                 :: nzone     ! nombre de zones
  integer                 :: nzone_str ! nombre de zones structurees
  integer                 :: nzone_ust ! nombre de zones non structurees
  type(st_cgns_zone), dimension(:), pointer &
                          :: zone   ! liste des zones
endtype st_cgns_base  


!------------------------------------------------------------------------------!
! ST_CGNS_WORLD : structure principale receptrice des donnees du fichier CGNS
!------------------------------------------------------------------------------!
type st_cgns_world
  character(len=cgnslen) :: nom    ! nom DU JEU DE DONNEES  
  integer                 :: nbase  ! nombre de bases
  type(st_cgns_base), dimension(:), pointer &
                          :: base   ! liste des bases
endtype st_cgns_world


contains 

!------------------------------------------------------------------------------!
! delete_cgns_world
!------------------------------------------------------------------------------!
subroutine delete_cgns_world(cgw)
implicit none
type(st_cgns_world) :: cgw
integer             :: i

  if (associated(cgw%base)) then
    do i = 1, cgw%nbase
      call delete_cgns_base(cgw%base(i))
    enddo
    deallocate(cgw%base)
  endif

endsubroutine delete_cgns_world


!------------------------------------------------------------------------------!
! delete_cgns_base
!------------------------------------------------------------------------------!
subroutine delete_cgns_base(cgb)
implicit none
type(st_cgns_base) :: cgb
integer             :: i

  if (associated(cgb%zone)) then
    do i = 1, cgb%nzone
      call delete_cgns_zone(cgb%zone(i))
    enddo
    deallocate(cgb%zone)
  endif

endsubroutine delete_cgns_base


!------------------------------------------------------------------------------!
! delete_cgns_zone
!------------------------------------------------------------------------------!
subroutine delete_cgns_zone(cgz)
implicit none
type(st_cgns_zone) :: cgz
integer             :: i

  if (associated(cgz%cellfam)) then
    do i = 1, cgz%ncellfam
      call delete_cgns_ustconnect(cgz%cellfam(i))
    enddo
    deallocate(cgz%cellfam)
  endif

  if (associated(cgz%facefam)) then
    do i = 1, cgz%nfacefam
      call delete_cgns_ustconnect(cgz%facefam(i))
    enddo
    deallocate(cgz%facefam)
  endif

  if (associated(cgz%edgefam)) then
    do i = 1, cgz%nedgefam
      call delete_cgns_ustconnect(cgz%edgefam(i))
    enddo
    deallocate(cgz%edgefam)
  endif

  call delete_cgns_vtex(cgz%mesh)

  if (associated(cgz%boco)) then
    do i = 1, cgz%nboco
      !call delete_cgns_boco(cgz%boco(i))
    enddo
    deallocate(cgz%boco)
  endif

endsubroutine delete_cgns_zone


!------------------------------------------------------------------------------!
! delete_cgns_ustconnect
!------------------------------------------------------------------------------!
subroutine delete_cgns_ustconnect(fam)
implicit none
type(st_cgns_ustconnect) :: fam
integer             :: i

  if (associated(fam%fils)) then
    deallocate(fam%fils)
  endif

endsubroutine delete_cgns_ustconnect


!------------------------------------------------------------------------------!
! delete_cgns_vtex
!------------------------------------------------------------------------------!
subroutine delete_cgns_vtex(mesh)
implicit none
type(st_cgns_vtex) :: mesh
integer             :: i

  if (associated(mesh%vertex)) then
    deallocate(mesh%vertex)
  endif

endsubroutine delete_cgns_vtex


!------------------------------------------------------------------------------!
! delete_cgns_boco
!------------------------------------------------------------------------------!
subroutine delete_cgns_boco(boco)
implicit none
type(st_cgns_boco) :: boco
integer             :: i

  call delete(boco%list)

endsubroutine delete_cgns_boco


endmodule CGNS_STRUCT

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : creation du module, structure pour la lecture CGNS
! juin 2004 : modificiation de la structure pour lecture des BOCO
!------------------------------------------------------------------------------!
