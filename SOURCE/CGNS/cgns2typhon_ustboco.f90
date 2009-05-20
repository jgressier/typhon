!------------------------------------------------------------------------------!
! Procedure : cgns2typhon_ustboco.f90     Auteur : J. Gressier
!                                         Date   : Fevrier 2003
! Fonction                                Modif  : (cf historique)
!   Creation des conditions aux limites par conversion CGNS->TYPHON
!   Test de coherence entre les faces CGNS et les faces creees dans TYPHON
!
! Defauts/Limitations/Divers :
!   ATTENTION : les faces sont supposees ordonnees (faces limites en fin de tab)
!
!------------------------------------------------------------------------------!
subroutine cgns2typhon_ustboco(cgnszone, umesh) 

use TYPHMAKE      ! definitions generales 
use CGNS_STRUCT   ! Definition des structures CGNS
use USTMESH       ! Definition des structures maillage non structure
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- INPUTS --
type(st_cgns_zone)  :: cgnszone        ! zone CGNS contenant conditions aux limites

! -- INPUTS/OUTPUTS --
type(st_ustmesh)    :: umesh           ! unstructured mesh

! -- Internal variables --
integer, dimension(:), allocatable &
                    :: listface        ! liste provisoire de faces
integer             :: nface_int       ! nombre de faces internes
integer             :: nface_lim       ! nombre de faces limites
integer             :: ib, if, nf, iib

! -- BODY --

! -- Creation des conditions aux limites --

allocate(listface(umesh%nface_lim))   ! allocation provisoire avec marges

call createboco(umesh, cgnszone%nboco)

do ib = 1, cgnszone%nboco

  call print_info(8,"  . linking boundary condition marks"//strof(ib,3)//"/"//trim(strof(cgnszone%nboco))// &
                    ": "//trim(cgnszone%boco(ib)%family))

  select case(cgnszone%boco(ib)%gridlocation)
  case(vertex)
    call print_info(20,'      vertex tagging method')
    call seek_bcface_vtex(ib, cgnszone%boco(ib), umesh, listface)
  case(facecenter)
    call print_info(20,'      face tagging method')
    call seek_bcface_face(ib, cgnszone%boco(ib), cgnszone%nfacefam , &
                          cgnszone%facefam(1:cgnszone%nfacefam), umesh, listface)
  case default
    call erreur("boundary condition build","tagging method unknown")
  endselect
  
enddo

! --- Check empty boco marks ---

nf  = 0
iib = 0

do ib = 1, umesh%nboco
  if (umesh%boco(ib)%nface /= 0) then
    iib = iib + 1
    nf  = nf + umesh%boco(ib)%nface
    if (iib < ib) umesh%boco(iib) = umesh%boco(ib)   ! if staggered, then transfer
  else
    call delete_ustboco(umesh%boco(ib))
  endif
enddo

umesh%nboco = iib

nf = 0
do ib = 1, umesh%nboco
  print*,ib, umesh%boco(ib)%nface
  nf = nf + umesh%boco(ib)%nface
enddo

! --- check bounding faces == tagged faces ---

if (nf /= umesh%nface_lim) &
  call erreur("BOCO tagging", "tagged faces number does not correspond to boundaring faces number")

! desallocation

deallocate(listface)

!-------------------------
endsubroutine cgns2typhon_ustboco

!------------------------------------------------------------------------------!
! Changes history
!
! fev  2003: creation
! june 2004: construction des connectivites BOCO-> faces, generalisation
!            procedure intrinseque transferee dans USTMESH
! Nov  2007: check empty boco marks
!------------------------------------------------------------------------------!
