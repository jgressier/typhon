!------------------------------------------------------------------------------!
! Procedure : seek_bcface_vtex.f90        Auteur : J. Gressier
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf historique)
!   Recherche des faces a partir des listes de VERTEX marques
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine seek_bcface_vtex(ib, cgnsboco, umesh, listface) 

use TYPHMAKE      ! definitions generales 
use CGNS_STRUCT   ! Definition des structures CGNS
use USTMESH       ! Definition des structures maillage non structure
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrees --
integer             :: ib              ! indice de condition limite
type(st_cgns_boco)  :: cgnsboco        ! zone CGNS contenant conditions aux limites

! -- Entrees/Sorties --
type(st_ustmesh)    :: umesh           ! unstructured mesh

! -- Variables internes --
integer, dimension(*) :: listface      ! tableau de travail
integer               :: if, nf

! -- Debut de procedure

! -- Creation des conditions aux limites --

nf = 0  

! recherche des faces limites concernees

do if = umesh%nface_int+1, umesh%nface_int+umesh%nface_lim

  if (face_invtexlist(umesh%facevtex%nbfils, umesh%facevtex%fils(if,:), &
                      cgnsboco%list%nbfils, cgnsboco%list%fils)) then
    nf = nf + 1
    listface(nf) = if
  endif
enddo

call print_info(20,'      '//trim(strof(nf))//' mesh faces tagged')
call new_ustboco(umesh%boco(ib), cgnsboco%family, nf)
umesh%boco(ib)%iface(1:nf) = listface(1:nf)
  
!-------------------------
endsubroutine seek_bcface_vtex

!------------------------------------------------------------------------------!
! Changes history
!
! june 2004: creation
!------------------------------------------------------------------------------!
