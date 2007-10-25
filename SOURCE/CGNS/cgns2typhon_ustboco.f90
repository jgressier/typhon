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
subroutine cgns2typhon_ustboco(cgnszone, mesh) 

use TYPHMAKE      ! definitions generales 
use CGNS_STRUCT   ! Definition des structures CGNS
use USTMESH       ! Definition des structures maillage non structure
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrees --
type(st_cgns_zone)  :: cgnszone        ! zone CGNS contenant conditions aux limites

! -- Entrees/Sorties --
type(st_ustmesh)    :: mesh            ! connectivites et conditions aux limites

! -- Variables internes --
integer, dimension(:), allocatable &
                    :: listface        ! liste provisoire de faces
integer             :: nface_int       ! nombre de faces internes
integer             :: nface_lim       ! nombre de faces limites
integer             :: ib, if, nf

! -- Debut de procedure

! -- Creation des conditions aux limites --

allocate(listface(mesh%nface_lim))   ! allocation provisoire avec marges
mesh%nboco = cgnszone%nboco         
allocate(mesh%boco(mesh%nboco))    ! allocation des conditions aux limites TYPHON


do ib = 1, cgnszone%nboco

  call print_info(8,"  . apply boundary condition: "//trim(cgnszone%boco(ib)%family))

  call 

  select case(cgnszone%boco(ib)%gridlocation)
  case(vertex)
    call print_info(20,'     vertex tagging')
    call seek_bcface_vtex(ib, cgnszone%boco(ib), mesh, listface)
  case(facecenter)
    call print_info(20,'     face tagging')
    call seek_bcface_face(ib, cgnszone%boco(ib), cgnszone%nfacefam , &
                          cgnszone%facefam(1:cgnszone%nfacefam), mesh, listface)
  case default
    call erreur("boundary condition build","tagging method unknown")
  endselect
  
enddo

! -- Verification du nombre total de faces limites affectees aux conditions aux limites

nf = 0
do ib = 1, cgnszone%nboco
  nf = nf + mesh%boco(ib)%nface
enddo

if (nf /= mesh%nface_lim) &
  call erreur("BOCO tagging", "tagged faces do not correspond to boundaring faces")

! desallocation

deallocate(listface)

!-------------------------
endsubroutine cgns2typhon_ustboco

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2003 : creation de la procedure 
! juin 2004 : construction des connectivites BOCO-> faces, generalisation
!             procedure intrinseque transferee dans USTMESH
!------------------------------------------------------------------------------!
