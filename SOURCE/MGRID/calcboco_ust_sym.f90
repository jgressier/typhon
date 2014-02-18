!------------------------------------------------------------------------------!
! Procedure : calcboco_ust_sym            Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (see history)
!   Boundary condition calculation (local symmetry)
!
! Defauts/Limitations/Divers :
!   ATTENTION : le calcul des conditions aux limites doit se faire sur les
!     variables primitives
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust_sym(curtime, defboco, defale, defmrf, ustboco, umesh, bccon)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use MENU_ALE
use MESHMRF
use USTMESH
use DEFFIELD
use QUANTITY

implicit none

! -- INPUTS --
type(mnu_boco)   :: defboco          ! parametres de conditions aux limites
type(mnu_ale)    :: defale           ! ALE parametres
type(mnu_mrf)    :: defmrf           ! MRF parametres
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh            ! unstructured mesh
real(krp)        :: curtime          ! current time

! -- Inputs/Outputs --
type(st_bccon) :: bccon

! -- Internal variables --
integer    :: ifb, if, ip      ! index de liste, index de face limite, et parametre
integer    :: icell, ighost    ! index de cellule interieure, et de cellule fictive
type(v3d)  :: fn, dfc, dgc, vc
real(krp)  :: rap 
type(v3d)  :: wallvelocity

! -- BODY --

do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  icell  = bccon%isend(ifb)
  ighost = bccon%irecv(ifb)
  fn     = umesh%mesh%iface(if,1,1)%normale                               ! normale face
  
  do ip = 1, bccon%fsend%nscal
    bccon%frecv%tabscal(ip)%scal(ighost) = bccon%fsend%tabscal(ip)%scal(icell) 
  enddo
  
  ! assume symmetric positions of nodes
  !dfc = umesh%mesh%iface(if,1,1)%centre - umesh%mesh%centre(icell,1,1) ! dist ctr. face - cell
  !dgc = umesh%mesh%centre(ighost,1,1)   - umesh%mesh%centre(icell,1,1) ! dist ghostcell - cell
  !rap = (dgc.scal.fn)/(dfc.scal.fn)

  wallvelocity = v3d_zero
  call calc_wallvelocity(defale, defmrf, wallvelocity, umesh%mesh%iface(if,1,1), if, curtime)

  !! ip = 1 ! DEV: only velocity for the moment, will have to change in the future!
  do ip = 1, bccon%fsend%nvect
    vc = bccon%fsend%tabvect(ip)%vect(icell)
    if (bccon%fsend%tabvect(ip)%quantity_id == qv_velocity) then    
      bccon%frecv%tabvect(ip)%vect(ighost) = vc - (2._krp*((vc-wallvelocity).scal.fn))*fn
    else
      bccon%frecv%tabvect(ip)%vect(ighost) = vc - (2._krp*(vc.scal.fn))*fn
    endif
  enddo

  do ip = 1, bccon%fsend%ntens
    bccon%frecv%tabtens(ip)%tens(ighost) = bccon%fsend%tabtens(ip)%tens(icell)
    call t3d_sym(bccon%frecv%tabtens(ip)%tens(ighost), fn)
  enddo

enddo

endsubroutine calcboco_ust_sym
!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : creation
! Feb  2014 : generalized symmetry (scalar, vector, tensors)
!------------------------------------------------------------------------------!
