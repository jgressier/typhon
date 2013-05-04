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
use VARCOM
use VEC3D

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
real(krp)             :: coef(cell_buffer) ! intermediate residual coefficient
integer               :: ifa, i            ! index de face
integer               :: ic1, ic2         ! index de cellules
integer               :: ip               ! index de variables
integer               :: ib, icolor       ! block index
integer               :: dim              ! dimension
integer               :: buf, nblock      ! buffer size 
integer, pointer      :: ista(:), iend(:) ! starting and ending index

! -- BODY --

call new_buf_index(umesh%nface, face_buffer, nblock, ista, iend, nthread)

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

deallocate(ista, iend)

! -- calcul des residus --

do icolor = 1, umesh%colors%nbnodes

!$OMP PARALLEL DO private(ic1, ic2, ifa, ip) shared(residu, flux, umesh) 
do i = 1, umesh%colors%node(icolor)%nelem
  ifa = umesh%colors%node(icolor)%elem(i)
  ic1 = umesh%facecell%fils(ifa,1)
  ic2 = umesh%facecell%fils(ifa,2)
  !print*,icolor, i, ifa, ic1, ic2

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
!$OMP END PARALLEL DO

!deallocate(ista, iend)
enddo ! color

!!$OMP PARALLEL DO private(ic1, ip)
!do ic1 = 1, umesh%ncell_int
!   do ip = 1, residu%nscal
!    residu%tabscal(ip)%scal(ic1) = (dtloc(ic1)/ umesh%mesh%volume(ic1,1,1)) * residu%tabscal(ip)%scal(ic1)
!   enddo
!   do ip = 1, residu%nvect
!    residu%tabvect(ip)%vect(ic1) = (dtloc(ic1)/ umesh%mesh%volume(ic1,1,1)) * residu%tabvect(ip)%vect(ic1)
!   enddo
!enddo
!!$OMP END PARALLEL DO
 
call new_buf_index(umesh%ncell_int, cell_buffer, nblock, ista, iend, nthread)

!$OMP PARALLEL DO private(ic1, ip, buf, coef) shared(residu)
do ib = 1, nblock

  buf         = iend(ib)-ista(ib)+1
  coef(1:buf) = dtloc(ista(ib):iend(ib))/ umesh%mesh%volume(ista(ib):iend(ib),1,1)

  do ip = 1, residu%nscal
    residu%tabscal(ip)%scal(ista(ib):iend(ib)) = coef(1:buf) * residu%tabscal(ip)%scal(ista(ib):iend(ib))
  enddo
  do ip = 1, residu%nvect
    call scale(residu%tabvect(ip)%vect(ista(ib):iend(ib)), coef(1:buf))
  enddo
  
enddo
!$OMP END PARALLEL DO

deallocate(ista, iend)

endsubroutine flux_to_res
!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : creation de la procedure
! sept 2005 : local time stepping
!------------------------------------------------------------------------------!
