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

implicit none

! -- INPUTS --
type(st_field)        :: field            ! conservative, primitive and gradients fields
type(st_genericfield) :: flux             ! Face explicit fluxes (from Qn states)

! -- INPUTS/OUTPUTS --
type(st_ustmesh) :: umesh      ! unstructured domain + BOCO

! -- Internal variables --
integer                :: ig, ib, if, i
integer                :: nsca, nvec, nten, isca, ivec

! -- BODY --

nsca = field%nscal
nvec = field%nvect
nten = 0

do ib = 1, umesh%nboco          ! loop on boco in grid

  call init_genericfield(umesh%boco(ib)%avg_quant, 0._krp, v3d(0._krp, 0._krp, 0._krp))
  call init_genericfield(umesh%boco(ib)%sum_flux,  0._krp, v3d(0._krp, 0._krp, 0._krp))
  
  do i = 1, umesh%boco(ib)%nface

    if = umesh%boco(ib)%iface(i)        ! index of i-th boco face
    ig = umesh%facecell%fils(if, 2)     ! associated ghost state
    
    do isca = 1, nsca
      umesh%boco(ib)%avg_quant%tabscal(isca)%scal(1) = umesh%boco(ib)%avg_quant%tabscal(isca)%scal(1) &
                       + umesh%mesh%iface(if, 1, 1)%surface * field%etatprim%tabscal(isca)%scal(ig)
      umesh%boco(ib)%sum_flux%tabscal(isca)%scal(1) = umesh%boco(ib)%sum_flux%tabscal(isca)%scal(1) &
                       + flux%tabscal(isca)%scal(if)
    enddo
    do ivec = 1, nvec
      umesh%boco(ib)%avg_quant%tabvect(ivec)%vect(1) = umesh%boco(ib)%avg_quant%tabvect(ivec)%vect(1) &
                       + (umesh%mesh%iface(if, 1, 1)%surface * field%etatprim%tabvect(ivec)%vect(ig))
      umesh%boco(ib)%sum_flux%tabvect(ivec)%vect(1) = umesh%boco(ib)%sum_flux%tabvect(ivec)%vect(1) &
                       + flux%tabvect(ivec)%vect(if)
    enddo
  enddo

  do isca = 1, nsca
    umesh%boco(ib)%avg_quant%tabscal(isca)%scal(1) = umesh%boco(ib)%avg_quant%tabscal(isca)%scal(1) &
         / umesh%boco(ib)%area
  enddo
  do ivec = 1, nvec
    umesh%boco(ib)%avg_quant%tabvect(ivec)%vect(1) = umesh%boco(ib)%avg_quant%tabvect(ivec)%vect(1) &
         / umesh%boco(ib)%area
  enddo

enddo


endsubroutine integ_ustboco
!------------------------------------------------------------------------------!
! changes history
!
! Apr  2008: created
!------------------------------------------------------------------------------!
