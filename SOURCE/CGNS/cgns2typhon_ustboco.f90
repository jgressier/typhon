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

  select case(cgnszone%boco(ib)%gridlocation)
  case(vertex)
    print*,'     vertex tagging'
    call seek_bcface_vtex(ib, cgnszone%boco(ib), mesh, listface)
  case(facecenter)
    print*,'     face tagging'
    call seek_bcface_face(ib, cgnszone%boco(ib), cgnszone%nfacefam , &
                          cgnszone%facefam(1:cgnszone%nfacefam), mesh, listface)
  case default
    call erreur("construction de condition limite","type de specification non admis")
  endselect
  
enddo

! -- Verification du nombre total de faces limites affectees aux conditions aux limites

nf = 0
do ib = 1, cgnszone%nboco
  print*,ib,mesh%boco(ib)%nface
  nf = nf + mesh%boco(ib)%nface
enddo

print*,'nfaceboco/nfacelim:',nf, mesh%nface_lim
print*,'mesh faces:',mesh%nface_int, mesh%nface_lim, mesh%nface
if (nf /= mesh%nface_lim) call erreur("Conditions aux limites",&
  "le nombre de faces affectees et existantes ne correspondent pas")

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
