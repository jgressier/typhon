!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_flux           Authors: J. Gressier/E. Radenac
!                                         Date   : Juin 2004
!> @brief Computation of non-uniform boundary conditions for solid, wall with set heat flux
!------------------------------------------------------------------------------!
subroutine setboco_kdif_flux(curtime, unif, ustboco, umesh, bccon, defsolver, bckdif, defspat)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use MGRID
use MENU_NUM
use FCT_EVAL
use FCT_ENV

implicit none

! -- Inputs --
real(krp)          :: curtime          ! current time
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! boundary conditions
type(st_ustmesh)   :: umesh            ! unstructured mesh
type(mnu_solver)   :: defsolver        ! kind of solver
type(st_boco_kdif) :: bckdif           ! parameters and fluxes (field or constant)
type(mnu_spat)     :: defspat

! -- Outputs --
type(st_bccon) :: bccon  ! pointer of send or receive fields

! -- Internal variables --
integer          :: ifb, if, ip, iv  ! index of list, index de boundary face and parameters
integer          :: nface
integer          :: ic, ighost    ! index of inner cells and factice cells
type(v3d)        :: cgface, cg, normale ! face, cell center, face normale
real(krp)        :: d             ! distance cell - boundary face
real(krp)        :: conduct       ! conductivity
type(v3d)        :: gradT         ! temperature gradient
type(v3d)        :: dc            ! vector cell center - its projection 
                                  ! on the face normale
real(krp)        :: gTdc          ! scalar product gradT.dc
real(krp), allocatable :: lflux(:)

! -- BODY --

nface = ustboco%nface 

allocate(lflux(nface))
call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

select case(unif)
case(uniform)
  do ifb = 1, nface
     if     = ustboco%iface(ifb)
     call fct_env_set_real(blank_env, "x", umesh%mesh%face_center(if,1)%x)
     call fct_env_set_real(blank_env, "y", umesh%mesh%face_center(if,1)%y)
     call fct_env_set_real(blank_env, "z", umesh%mesh%face_center(if,1)%z)
     call fct_env_set_real(blank_env, "t", curtime)
     call fct_eval_real(blank_env, bckdif%wall_flux, lflux(ifb))
  enddo
  lflux = -lflux  !!! USER flux is entering the domain (so, must be reversed)

case(nonuniform)
  lflux(1:nface) = bckdif%flux_nunif(1:nface)
case default
  call error_stop("unknown definition of boco flux computation (kdif)")
endselect

call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL

select case(bccon%bccon_mode)
!--- State BC ----------------------------------------------------------
case(bccon_cell_state, bccon_face_state)

do ifb = 1, nface
  do iv = 1, bccon%frecv%nscal
    bccon%frecv%tabscal(iv)%scal(bccon%irecv(ifb)) = bccon%fsend%tabscal(iv)%scal(bccon%isend(ifb)) 
  enddo
enddo

!--- Gradients BC ----------------------------------------------------------
case(bccon_cell_grad, bccon_face_grad)

do ifb = 1, nface

  if     = ustboco%iface(ifb)
  ic     = bccon%isend(ifb)
  ighost = bccon%irecv(ifb)

  ! Computation of distance cell center - face center
  cgface = umesh%mesh%face_center(if,1)
  !cg     = umesh%mesh%centre(ic,1,1)
  normale= umesh%mesh%face_normal(if,1)
  ! d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
  d = abs( (cgface - cg).scal.normale )

  ! Conductivity
  conduct = valeur_loi(defsolver%defkdif%materiau%Kd, bccon%fsend%tabscal(1)%scal(ic))

  ! Heat flux
  ustboco%bocofield%tabscal(1)%scal(ifb) = ustboco%bocofield%tabscal(1)%scal(ifb) + lflux(ifb)

  ! Approximated temperature in factice cell, for computation of gradients
  !dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale
  !if (defspat%calc_grad) then
  !  gradT = champ%gradient%tabvect(1)%vect(ic)
  !  gTdc = gradT .scal. dc
  !  champ%etatprim%tabscal(1)%scal(ighost) = &
  !         champ%etatprim%tabscal(1)%scal(ic) + gTdc - lflux(ifb)*d/conduct
  !else
  !d = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
  bccon%frecv%tabscal(1)%scal(ighost) = bccon%fsend%tabscal(1)%scal(ic) 

enddo

deallocate(lflux)

case default
  call error_stop("Internal error: unknown connection mode (setboco_kdif_flux)")
endselect

endsubroutine setboco_kdif_flux

!------------------------------------------------------------------------------!
! Change history
!
! june 2004 : creation 
! july 2004 : merge of uniform and non-uniform boco settings
! Mar  2008 : use of FCT function
! May  2008 : delete use of gradient in temperature estimate
! Feb  2014 : weak BC, temperature is no more computed to estimate right gradient
!------------------------------------------------------------------------------!
