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

use VEC3D     
use CONNECTIVITY
use ELEMVTEX

implicit none         

include 'cgnslib_f.h'

! -- Variables globales du module -------------------------------------------

integer, parameter :: cgnslen    = len(ElementTypeName(0)) ! use string definition in cngslib_f.h
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


contains 

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

!------------------------------------------------------------------------------!
! Function : transfer element TYPE
!------------------------------------------------------------------------------!
integer function nvtex_cgnselement(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(NODE)
  nvtex_cgnselement = 1
case(BAR_2)
  nvtex_cgnselement = 2
case(TRI_3)
  nvtex_cgnselement = 3
case(quad_4)
  nvtex_cgnselement = 4
case(tetra_4)
  nvtex_cgnselement = 4
case(pyra_5)
  nvtex_cgnselement = 5
case(penta_6)
  nvtex_cgnselement = 6
case(hexa_8)
  nvtex_cgnselement = 8
case default
  nvtex_cgnselement = -1
endselect

endfunction nvtex_cgnselement

!------------------------------------------------------------------------------!
! Function : transfer element TYPE
!------------------------------------------------------------------------------!
integer function typhon2cgns_elemtype(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(elem_bar2)
  typhon2cgns_elemtype = BAR_2
case(elem_tri3)
  typhon2cgns_elemtype = TRI_3
case(elem_quad4)
  typhon2cgns_elemtype = QUAD_4
case(elem_ngon)
  typhon2cgns_elemtype = NGON_N
case(elem_tetra4)
  typhon2cgns_elemtype = TETRA_4
case(elem_pyra5)
  typhon2cgns_elemtype = PYRA_5
case(elem_penta6)
  typhon2cgns_elemtype = PENTA_6
case(elem_hexa8)
  typhon2cgns_elemtype = HEXA_8
case default
  typhon2cgns_elemtype = -1
endselect

endfunction typhon2cgns_elemtype

!------------------------------------------------------------------------------!
! Function : transfer element TYPE
!------------------------------------------------------------------------------!
integer function cgns2typhon_elemtype(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(bar_2)
  cgns2typhon_elemtype = elem_BAR2
case(tri_3)
  cgns2typhon_elemtype = elem_TRI3
case(quad_4)
  cgns2typhon_elemtype = elem_QUAD4
case(ngon_n)
  cgns2typhon_elemtype = elem_NGON
case(tetra_4)
  cgns2typhon_elemtype = elem_TETRA4
case(pyra_5)
  cgns2typhon_elemtype = elem_PYRA5
case(penta_6)
  cgns2typhon_elemtype = elem_PENTA6
case(hexa_8)
  cgns2typhon_elemtype = elem_HEXA8
case default
  cgns2typhon_elemtype = -1
endselect

endfunction cgns2typhon_elemtype


endmodule CGNS_STRUCT

!------------------------------------------------------------------------------!
! History
!
! nov  2002 : creation du module, structure pour la lecture CGNS
! juin 2004 : modificiation de la structure pour lecture des BOCO
!------------------------------------------------------------------------------!
