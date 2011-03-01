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

use OUTPUT
use PACKET
use USTMESH
use DEFFIELD
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

! -- Private DATA --
real(krp)             :: surf(face_buffer) ! intermediate surface
integer               :: ifa, i            ! index de face
integer               :: ic1, ic2         ! index de cellules
integer               :: ip               ! index de variables
integer               :: ib               ! block index
integer               :: dim              ! dimension
integer               :: buf, nblock      ! buffer size 
integer, pointer      :: ista(:), iend(:) ! starting and ending index

! -- BODY --

call new_buf_index(umesh%nface, face_buffer, nblock, ista, iend)

!$OMP PARALLEL DO private(surf, ifa, buf) shared(flux)
do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  do ifa = ista(ib), iend(ib)
    surf(ifa-ista(ib)+1) = umesh%mesh%iface(ifa,1,1)%surface
  enddo

  ! -- flux surfaciques -> flux de surfaces --

  do ip = 1, flux%nscal
    do ifa = ista(ib), iend(ib)
      flux%tabscal(ip)%scal(ifa) = surf(ifa-ista(ib)+1) * flux%tabscal(ip)%scal(ifa)
    enddo
  enddo

  do ip = 1, flux%nvect
    do ifa = ista(ib), iend(ib)
      flux%tabvect(ip)%vect(ifa) = surf(ifa-ista(ib)+1) * flux%tabvect(ip)%vect(ifa)
    enddo
    !call scale(flux%tabvect(ip)%vect, surf(:))
  enddo

  ! -- idem traitement des jacobiennes

  if (trait_jac) then
    dim = jacL%dim
    do ifa = ista(ib), iend(ib)
      jacL%mat(1:dim,1:dim,ifa) = surf(ifa-ista(ib)+1) * jacL%mat(1:dim,1:dim,ifa)
      jacR%mat(1:dim,1:dim,ifa) = surf(ifa-ista(ib)+1) * jacR%mat(1:dim,1:dim,ifa)
    enddo
  endif

enddo
!$OMP END PARALLEL DO

! -- calcul des residus --

!!$OMP PARALLEL DO private(ic1, ic2) shared(residu, flux)  !!! bug when using OMP here
do ifa = 1, umesh%nface
  ic1 = umesh%facecell%fils(ifa,1)
  ic2 = umesh%facecell%fils(ifa,2)

  do ip = 1, residu%nscal
    residu%tabscal(ip)%scal(ic1) = residu%tabscal(ip)%scal(ic1) - flux%tabscal(ip)%scal(ifa)
    residu%tabscal(ip)%scal(ic2) = residu%tabscal(ip)%scal(ic2) + flux%tabscal(ip)%scal(ifa)
  enddo
  do ip = 1, residu%nvect
    call shift_sub(residu%tabvect(ip)%vect(ic1), flux%tabvect(ip)%vect(ifa))
    call shift_add(residu%tabvect(ip)%vect(ic2), flux%tabvect(ip)%vect(ifa))
    !residu%tabvect(ip)%vect(ic1) = residu%tabvect(ip)%vect(ic1) - flux%tabvect(ip)%vect(ifa)
    !residu%tabvect(ip)%vect(ic2) = residu%tabvect(ip)%vect(ic2) + flux%tabvect(ip)%vect(ifa)
  enddo
enddo
!!$OMP END PARALLEL DO

deallocate(ista, iend)

! ??? creation de procedure intrinseques ? // optimisation

!$OMP PARALLEL DO private(ic1, ip)
do ic1 = 1, umesh%ncell_int
  do ip = 1, residu%nscal
    residu%tabscal(ip)%scal(ic1) = (dtloc(ic1)/ umesh%mesh%volume(ic1,1,1)) * residu%tabscal(ip)%scal(ic1)
  enddo
  do ip = 1, residu%nvect
    residu%tabvect(ip)%vect(ic1) = (dtloc(ic1)/ umesh%mesh%volume(ic1,1,1)) * residu%tabvect(ip)%vect(ic1)
  enddo
enddo
!$OMP END PARALLEL DO

endsubroutine flux_to_res
!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : creation de la procedure
! sept 2005 : local time stepping
!------------------------------------------------------------------------------!
