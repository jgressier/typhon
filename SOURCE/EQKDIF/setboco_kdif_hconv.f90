!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_hconv          Authors: J. Gressier/E. Radenac
!                                         Date   : Juin 2004
! Function                                Modif  : (cf Historique)
!   Computation of non uniform convection boundary conditions for 
!   the solid
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_kdif_hconv(unif, ustboco, ustdom, champ, flux, defsolver, bckdif, defspat)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use DEFFIELD 
use MENU_NUM

implicit none

! -- Inputs --
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! boundary conditions
type(st_ustmesh)   :: ustdom           ! unstructured mesh
type(mnu_solver)   :: defsolver        ! solver
type(st_boco_kdif) :: bckdif           ! parameters and fluxes (field or constant)
type(mnu_spat)     :: defspat

! -- Outputs --
type(st_field)   :: champ            ! field
real(krp), dimension(ustboco%nface) &
                 :: flux             ! flux

! -- Internal variables --
integer          :: ifb, if, ip   ! index of list, boundary face and parameters
integer          :: ic, ighost    ! index of inner cell and factice cell
type(v3d)        :: cgface, cg, normale ! face, cell center, face normale
real(krp)        :: d             ! distance cell - boundary face
real(krp)        :: conduct       ! conductivity
real(krp)        :: gTdc          ! scalar product gradT.dc
type(v3d)        :: gradT         ! temperature gradient
type(v3d)        :: dc            ! vector cell center - its projection 
                                  ! on the face normale
! -- BODY --

if (unif == uniform) then
!-----------------------------------------
! Uniform case
!-----------------------------------------
do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ic     = ustdom%facecell%fils(if,1)
  ighost = ustdom%facecell%fils(if,2)

  ! Computation of distance cell center - face center
  cgface = ustdom%mesh%iface(if,1,1)%centre
  cg     = ustdom%mesh%centre(ic,1,1)
  normale= ustdom%mesh%iface(if,1,1)%normale
! d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
  d = abs((cgface - cg).scal.normale)
  dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale

  ! Conductivity
  conduct = valeur_loi(defsolver%defkdif%materiau%Kd, champ%etatprim%tabscal(1)%scal(ic))

  ! Approximate computation of temperature in factice cells
  ! (for computation of gradients)
  if (defspat%calc_grad) then
    gradT = champ%gradient%tabvect(1)%vect(ic)
    gTdc = gradT .scal. dc
    champ%etatprim%tabscal(1)%scal(ighost) = ( (conduct/d) * &
         (champ%etatprim%tabscal(1)%scal(ic) + gTdc ) + &
         bckdif%h_conv*bckdif%temp_conv ) / &
         (conduct/d+bckdif%h_conv)
  else
    d = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
    champ%etatprim%tabscal(1)%scal(ighost) = &
      ( (conduct/d) * champ%etatprim%tabscal(1)%scal(ic) + &
      bckdif%h_conv*bckdif%temp_conv ) / (conduct/d+bckdif%h_conv)
  endif

  ! Heat flux
  flux(ifb) = bckdif%h_conv*(champ%etatprim%tabscal(1)%scal(ighost) - bckdif%temp_conv)

enddo

else

!-----------------------------------------
! Non-uniform case
!-----------------------------------------
do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ic     = ustdom%facecell%fils(if,1)
  ighost = ustdom%facecell%fils(if,2)

  ! Computation of distance cell center - face center
  cgface = ustdom%mesh%iface(if,1,1)%centre
  cg     = ustdom%mesh%centre(ic,1,1)
  normale= ustdom%mesh%iface(if,1,1)%normale
! d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
  d = abs((cgface - cg).scal.normale)

  ! Conductivity
  conduct = valeur_loi(defsolver%defkdif%materiau%Kd, champ%etatprim%tabscal(1)%scal(ic))

  ! Approximate computation of temperature in factice cells
  ! (for computation of gradients)
  if (defspat%calc_grad) then
    gradT = champ%gradient%tabvect(1)%vect(ic)
    gTdc = gradT .scal. dc
    champ%etatprim%tabscal(1)%scal(ighost) = ( (conduct/d) * &
         (champ%etatprim%tabscal(1)%scal(ic) + gTdc ) + &
         bckdif%h_nunif(ifb)*bckdif%tconv_nunif(ifb) ) / &
         (conduct/d+bckdif%h_nunif(ifb))
  else
    d = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
    champ%etatprim%tabscal(1)%scal(ighost) = &
      ( (conduct/d) * champ%etatprim%tabscal(1)%scal(ic) + &
      bckdif%h_nunif(ifb)*bckdif%tconv_nunif(ifb) ) / &
      (conduct/d+bckdif%h_nunif(ifb))
  endif

  ! Heat flux
  flux(ifb) = bckdif%h_nunif(ifb)*(champ%etatprim%tabscal(1)%scal(ighost) &
                                       - bckdif%tconv_nunif(ifb))
enddo

endif

endsubroutine setboco_kdif_hconv

!------------------------------------------------------------------------------!
! Change history
!
! june 2004 : creation
! july 2004 : merge of uniform and non-uniform boco settings
!------------------------------------------------------------------------------!

