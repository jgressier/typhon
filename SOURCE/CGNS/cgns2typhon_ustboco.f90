!------------------------------------------------------------------------------!
! Procedure : cgns2typhon_ustboco.f90     Auteur : J. Gressier
!                                         Date   : Février 2003
! Fonction                                Modif  : (cf historique)
!   Création des conditions aux limites par conversion CGNS->TYPHON
!   Test de cohérence entre les faces CGNS et les faces créées dans TYPHON
!
! Defauts/Limitations/Divers :
!   ATTENTION : les faces sont supposées ordonnées (faces limites en fin de tab)
!
!------------------------------------------------------------------------------!
subroutine cgns2typhon_ustboco(cgnszone, mesh) 

use TYPHMAKE      ! définitions générales 
use CGNS_STRUCT   ! Définition des structures CGNS
use USTMESH       ! Définition des structures maillage non structuré
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrées --
type(st_cgns_zone)  :: cgnszone        ! zone CGNS contenant conditions aux limites

! -- Entrées/Sorties --
type(st_ustmesh)    :: mesh            ! connectivités et conditions aux limites

! -- Variables internes --
integer, dimension(:), allocatable &
                    :: listface        ! liste provisoire de faces
integer             :: nface_int       ! nombre de faces internes
integer             :: nface_lim       ! nombre de faces limites
integer             :: ib, if, nf

! -- Début de procédure

! -- Création des conditions aux limites --

allocate(listface(mesh%nface_lim))   ! allocation provisoire avec marges
mesh%nboco = cgnszone%nboco         
allocate(mesh%boco(mesh%nboco))    ! allocation des conditions aux limites TYPHON


do ib = 1, cgnszone%nboco

  call print_info(8,"  . traitement des conditions aux limites : "//trim(cgnszone%boco(ib)%family))

  select case(cgnszone%boco(ib)%gridlocation)
  case(vertex)
    call seek_bcface_vtex(ib, cgnszone%boco(ib), mesh, listface)
  case(facecenter)
    call seek_bcface_face(ib, cgnszone%boco(ib), cgnszone%nfacefam , &
                          cgnszone%facefam(:), mesh, listface)
  case default
    call erreur("construction de condition limite","type de spécification non admis")
  endselect
  
enddo

! -- Vérification du nombre total de faces limites affectées aux conditions aux limites

nf = 0
do ib = 1, cgnszone%nboco
  nf = nf + mesh%boco(ib)%nface
enddo

if (nf /= mesh%nface_lim) call erreur("Conditions aux limites",&
  "le nombre de faces affectées et existantes ne correspondent pas")

! desallocation

deallocate(listface)

!-------------------------
endsubroutine cgns2typhon_ustboco

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2003 : création de la procédure 
! juin 2004 : construction des connectivités BOCO-> faces, généralisation
!             procédure intrinsèque transférée dans USTMESH
!------------------------------------------------------------------------------!
