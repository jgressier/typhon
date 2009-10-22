!------------------------------------------------------------------------------!
! Procedure : flux_to_res                 Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Calcul de residu a partir des flux
!
! Defauts/Limitations/Divers :
!   ATTENTION : le residu en sortie est different selon le traitement implicite
!               ou non
!
!------------------------------------------------------------------------------!
subroutine flux_to_res(dtloc, umesh, flux, residu, trait_jac, jacL, jacR)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use DEFFIELD
use GEO3D
use MATRIX_ARRAY

implicit none

! -- INPUTS --
type(st_ustmesh)      :: umesh                   ! mesh properties
real(krp)             :: dtloc(1:umesh%ncell)    ! local ou global time step
type(st_genericfield) :: flux                    ! face flux
logical               :: trait_jac               ! need to parse jacobians or not

! -- OUTPUTS --
type(st_genericfield)   :: residu            ! champ de residus
type(st_mattab)         :: jacL, jacR

! -- Declaration des variables internes --
real(krp), allocatable :: surf(:)         ! intermediate surface
real(krp), allocatable :: vol(:)          ! intermediate volume
integer               :: i_f, i            ! index de face
integer               :: ic1, ic2         ! index de cellules
integer               :: ip               ! index de variables
integer               :: ib               ! index de conditions aux limites
integer               :: dim              ! dimension
integer               :: ic               ! index de couplage

! -- Debut de la procedure --

allocate(surf(umesh%nface))

do i_f = 1, umesh%nface
  surf(i_f) = umesh%mesh%iface(i_f,1,1)%surface
enddo

! -- flux surfaciques -> flux de surfaces --

do ip = 1, flux%nscal
  do i_f = 1, umesh%nface
    flux%tabscal(ip)%scal(i_f) = surf(i_f) * flux%tabscal(ip)%scal(i_f)
  enddo
enddo

do ip = 1, flux%nvect
  !flux%tabvect(ip)%vect(:) = surf(:) * flux%tabvect(ip)%vect(:)
  call scale(flux%tabvect(ip)%vect, surf(:))
enddo

! -- idem traitement des jacobiennes

if (trait_jac) then
  dim = jacL%dim
  do i_f = 1, umesh%nface
    jacL%mat(1:dim,1:dim,i_f) = surf(i_f) * jacL%mat(1:dim,1:dim,i_f)
    jacR%mat(1:dim,1:dim,i_f) = surf(i_f) * jacR%mat(1:dim,1:dim,i_f)
  enddo
endif

deallocate(surf)

! -- calcul des residus --

call init_genericfield(residu, 0._krp, v3d(0._krp, 0._krp, 0._krp))

do i_f = 1, umesh%nface
  ic1 = umesh%facecell%fils(i_f,1)
  ic2 = umesh%facecell%fils(i_f,2)

  do ip = 1, residu%nscal
    residu%tabscal(ip)%scal(ic1) = residu%tabscal(ip)%scal(ic1) - flux%tabscal(ip)%scal(i_f)
    residu%tabscal(ip)%scal(ic2) = residu%tabscal(ip)%scal(ic2) + flux%tabscal(ip)%scal(i_f)
  enddo
  do ip = 1, residu%nvect
    call shift_sub(residu%tabvect(ip)%vect(ic1), flux%tabvect(ip)%vect(i_f))
    call shift_add(residu%tabvect(ip)%vect(ic2), flux%tabvect(ip)%vect(i_f))
    !residu%tabvect(ip)%vect(ic1) = residu%tabvect(ip)%vect(ic1) - flux%tabvect(ip)%vect(i_f)
    !residu%tabvect(ip)%vect(ic2) = residu%tabvect(ip)%vect(ic2) + flux%tabvect(ip)%vect(i_f)
  enddo
enddo

! ??? creation de procedure intrinseques ? // optimisation

do ic1 = 1, umesh%ncell_int
  do ip = 1, residu%nscal
    residu%tabscal(ip)%scal(ic1) = (dtloc(ic1)/ umesh%mesh%volume(ic1,1,1)) * residu%tabscal(ip)%scal(ic1)
  enddo
  do ip = 1, residu%nvect
    residu%tabvect(ip)%vect(ic1) = (dtloc(ic1)/ umesh%mesh%volume(ic1,1,1)) * residu%tabvect(ip)%vect(ic1)
  enddo
enddo


endsubroutine flux_to_res

!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : creation de la procedure
! sept 2005 : local time stepping
!------------------------------------------------------------------------------!
