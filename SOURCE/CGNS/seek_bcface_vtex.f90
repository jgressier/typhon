!------------------------------------------------------------------------------!
! Procedure : seek_bcface_vtex.f90        Auteur : J. Gressier
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf historique)
!   Recherche des faces a partir des listes de VERTEX marques
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine seek_bcface_vtex(ib, cgnsboco, mesh, listface) 

use TYPHMAKE      ! definitions generales 
use CGNS_STRUCT   ! Definition des structures CGNS
use USTMESH       ! Definition des structures maillage non structure
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrees --
integer             :: ib              ! indice de condition limite
type(st_cgns_boco)  :: cgnsboco        ! zone CGNS contenant conditions aux limites

! -- Entrees/Sorties --
type(st_ustmesh)    :: mesh            ! connectivites et conditions aux limites

! -- Variables internes --
integer, dimension(*) :: listface      ! tableau de travail
integer               :: if, nf

! -- Debut de procedure

! -- Creation des conditions aux limites --

nf = 0  

! recherche des faces limites concernees

do if = mesh%nface_int+1, mesh%nface_int+mesh%nface_lim

  if (face_invtexlist(mesh%facevtex%nbfils, mesh%facevtex%fils(if,:), &
                      cgnsboco%list%nbfils, cgnsboco%list%fils)) then
    nf = nf + 1
    listface(nf) = if
  endif
enddo

call print_info(20,'     '//trim(strof(nf))//' tagged faces')
call new_ustboco(mesh%boco(ib), cgnsboco%family, nf)
mesh%boco(ib)%iface(1:nf) = listface(1:nf)
  
!-------------------------
endsubroutine seek_bcface_vtex

!------------------------------------------------------------------------------!
! Changes history
!
! june 2004: creation
!------------------------------------------------------------------------------!
