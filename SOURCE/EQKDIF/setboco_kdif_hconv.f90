!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_hconv          Authors: J. Gressier/E. Radenac
!                                         Date   : Juin 2004
! Function                                Modif  : (cf Historique)
!   Computation of non uniform convection boundary conditions for 
!   the solid
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_kdif_hconv(curtime, unif, ustboco, umesh, bccon, defsolver, bckdif, defspat)

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
type(mnu_solver)   :: defsolver        ! solver
type(st_boco_kdif) :: bckdif           ! parameters and fluxes (field or constant)
type(mnu_spat)     :: defspat

! -- Outputs --
type(st_bccon) :: bccon  ! pointer of send or receive fields

! -- Internal variables --
integer          :: ifb, if, ip   ! index of list, boundary face and parameters
integer          :: nface
integer          :: ic, ighost    ! index of inner cell and factice cell
type(v3d)        :: cgface, cg, normale ! face, cell center, face normale
real(krp)        :: d             ! distance cell - boundary face
real(krp)        :: conduct       ! conductivity
real(krp)        :: gTdc          ! scalar product gradT.dc
type(v3d)        :: gradT         ! temperature gradient
type(v3d)        :: dc            ! vector cell center - its projection on the face normale
real(krp), pointer :: hloc(:), tloc(:)

! -- BODY --

nface = ustboco%nface 

allocate(hloc(nface))
allocate(tloc(nface))
call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

select case(unif)
case(uniform)
  
  do ifb = 1, nface
     if     = ustboco%iface(ifb)
     call fct_env_set_real(blank_env, "x", umesh%mesh%iface(if,1,1)%centre%x)
     call fct_env_set_real(blank_env, "y", umesh%mesh%iface(if,1,1)%centre%y)
     call fct_env_set_real(blank_env, "z", umesh%mesh%iface(if,1,1)%centre%z)
     call fct_env_set_real(blank_env, "t", curtime)
     call fct_eval_real(blank_env, bckdif%h_conv, hloc(ifb))
  enddo
  tloc(1:nface) = bckdif%temp_conv

case(nonuniform)
  hloc(1:nface) = bckdif%h_nunif(1:nface)
  tloc(1:nface) = bckdif%tconv_nunif(1:nface)

case default
  call erreur("boco flux computation","unknown definition")
endselect

call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL

!-------------------------------------------------------------
! APPLY CONVECTION CONDITION

do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ic     = bccon%isend(ifb)
  ighost = bccon%irecv(ifb)

  ! Computation of distance cell center - face center
  cgface = umesh%mesh%iface(if,1,1)%centre
  ! cg     = umesh%mesh%centre(ic,1,1)
  normale= umesh%mesh%iface(if,1,1)%normale
  !! d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
  ! d = abs((cgface - cg).scal.normale)
  ! dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale

  ! Conductivity
  conduct = valeur_loi(defsolver%defkdif%materiau%Kd, bccon%fsend%tabscal(1)%scal(ic))

  ! Approximate computation of temperature in fictive cells
  ! (for computation of gradients)
  !if (defspat%calc_cellgrad) then
  !  gradT = champ%gradient%tabvect(1)%vect(ic)
  !  gTdc = gradT .scal. dc
  !  champ%etatprim%tabscal(1)%scal(ighost) = ( (conduct/d) * &
  !       (champ%etatprim%tabscal(1)%scal(ic) + gTdc ) + &
  !       hloc(ifb)*tloc(ifb) ) / &
  !       (conduct/d+hloc(ifb))
  !else
  !  d = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
    bccon%frecv%tabscal(1)%scal(ighost) = &
      ( (conduct/d) * bccon%fsend%tabscal(1)%scal(ic) + &
      hloc(ifb)*tloc(ifb) ) / (conduct/d+hloc(ifb))
  !endif

  ! Heat flux
  ustboco%bocofield%tabscal(1)%scal(ifb) = ustboco%bocofield%tabscal(1)%scal(ifb) &
                                          + hloc(ifb)*(bccon%fsend%tabscal(1)%scal(ighost) - tloc(ifb))

  call error_stop("DEV: kdif hconv BC must be RE IMPLEMENTED")

enddo

deallocate(hloc, tloc)

endsubroutine setboco_kdif_hconv

!------------------------------------------------------------------------------!
! Change history
!
! june 2004 : creation
! july 2004 : merge of uniform and non-uniform boco settings
! Mar  2008 : use of FCT function
!------------------------------------------------------------------------------!

