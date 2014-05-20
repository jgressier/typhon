!------------------------------------------------------------------------------!
! Procedure : integ_ustboco
!
! Fonction
!   
!
!------------------------------------------------------------------------------!
subroutine integ_ustboco(umesh, field, flux)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
use MENU_GEN
use VEC3D
!$ use OMP_LIB

implicit none

! -- INPUTS --
type(st_field)        :: field            ! conservative, primitive and gradients fields
type(st_genericfield) :: flux             ! Face explicit fluxes (from Qn states)

! -- INPUTS/OUTPUTS --
type(st_ustmesh) :: umesh      ! unstructured domain + BOCO

! -- Internal variables --
integer                :: ig, ib, if, i
integer                :: nsca, nvec, nten, isca, ivec
real(krp)              :: surf
integer                :: ithread
real(krp), allocatable :: avgsca(:,:), fluxsca(:,:)
type(v3d), allocatable :: avgvec(:,:), fluxvec(:,:)

! -- BODY --

!---------------------------------------------------
! initialization

nsca = field%nscal
nvec = field%nvect
nten = 0

if (nsca /= 0) then
  allocate( avgsca(nsca, nthread))
  allocate(fluxsca(nsca, nthread))
endif
if (nvec /= 0) then
  allocate( avgvec(nvec, nthread))
  allocate(fluxvec(nvec, nthread))
endif

!---------------------------------------------------
! loop on BOCO

do ib = 1, umesh%nboco          ! loop on boco in grid

  !call init_genericfield(umesh%boco(ib)%avg_quant, 0._krp, v3d(0._krp, 0._krp, 0._krp))
  !call init_genericfield(umesh%boco(ib)%sum_flux,  0._krp, v3d(0._krp, 0._krp, 0._krp))
  if (nsca /= 0) then
    avgsca (1:nsca, 1:nthread) = 0._krp
    fluxsca(1:nsca, 1:nthread) = 0._krp
  endif
  if (nvec /= 0) then
    avgvec (1:nvec, 1:nthread) = v3d(0._krp, 0._krp, 0._krp)
    fluxvec(1:nvec, 1:nthread) = v3d(0._krp, 0._krp, 0._krp)
  endif
  
  !$OMP PARALLEL & 
  !$OMP   private(isca, ivec, if, ig, surf, ithread) &
  !$OMP   shared (nsca, nvec, avgsca, avgvec, fluxsca, fluxvec, ib)
  ithread = 1
  !$ ithread = OMP_GET_THREAD_NUM()+1
  !$OMP DO
  do i = 1, umesh%boco(ib)%nface

    if   = umesh%boco(ib)%iface(i)        ! index of i-th boco face
    ig   = umesh%facecell%fils(if, 2)     ! associated ghost state
    surf = umesh%mesh%face_surf(if)

    do isca = 1, nsca
      avgsca(isca,ithread)  = avgsca(isca,ithread)  + surf * field%etatprim%tabscal(isca)%scal(ig)
      fluxsca(isca,ithread) = fluxsca(isca,ithread) + flux%tabscal(isca)%scal(if)
    enddo
    do ivec = 1, nvec
      avgvec(ivec,ithread)  = avgvec(ivec,ithread)  + surf * field%etatprim%tabvect(ivec)%vect(ig)
      fluxvec(ivec,ithread) = fluxvec(ivec,ithread) + flux%tabvect(ivec)%vect(if)
    enddo
  enddo
  !$OMP END PARALLEL
  
  do isca = 1, nsca
    umesh%boco(ib)%avg_quant%tabscal(isca)%scal(1) = sum(avgsca(isca,1:nthread)) / umesh%boco(ib)%area
    umesh%boco(ib)%sum_flux%tabscal(isca)%scal(1)  = sum(fluxsca(isca,1:nthread))
  enddo
  do ivec = 1, nvec
    umesh%boco(ib)%avg_quant%tabvect(ivec)%vect(1) = sum(avgvec(ivec,1:nthread)) / umesh%boco(ib)%area
    umesh%boco(ib)%sum_flux%tabvect(ivec)%vect(1)  = sum(fluxvec(ivec,1:nthread)) 
  enddo

enddo

if (nsca /= 0) then
  deallocate( avgsca)
  deallocate(fluxsca)
endif
if (nvec /= 0) then
  deallocate( avgvec)
  deallocate(fluxvec)
endif

endsubroutine integ_ustboco
!------------------------------------------------------------------------------!
! changes history
!
! Apr  2008: created
! May  2013: OMP parallelization
!------------------------------------------------------------------------------!
