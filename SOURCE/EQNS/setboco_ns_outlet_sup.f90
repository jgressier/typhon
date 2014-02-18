!------------------------------------------------------------------------------!
! Procedure : setboco_ns_outlet_sup       Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf Historique)
!   Computation of supersonic inlet boundary conditions
!   
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_outlet_sup(defns, unif, bc_ns, ustboco, umesh, bccon, curtime)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use MGRID 

implicit none

! -- INPUTS --
real(krp)        :: curtime
type(mnu_ns)     :: defns            ! solver parameters
integer          :: unif             ! uniform or not
type(st_boco_ns) :: bc_ns            ! parameters (field or constant)
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh            ! maillage non structure

! -- OUTPUTS --
type(st_bccon) :: bccon  ! pointer of send or receive fields

! -- Internal variables --
integer         :: ifb, if, ip, ic, is      ! index de liste, index de face limite et parametres
integer              :: nf, ib, nblock, buf            ! block index and number of blocks
integer, pointer     :: ista(:), iend(:)           ! starting and ending index

! -- BODY --

if (unif /= uniform) call error_stop("Development: non uniform condition not implemented")

call new_buf_index(ustboco%nface, fct_buffer, nblock, ista, iend, nthread)

select case(bccon%bccon_mode)
! --- State BC --------------------------------------
case(bccon_cell_state, bccon_face_state)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, is, buf) &
!$OMP shared(ista, iend, nblock) 
!$OMP DO
block: do ib = 1, nblock
  do ifb = ista(ib), iend(ib)
    if = ustboco%iface(ifb)
    is = bccon%isend(ifb)
    ic = bccon%irecv(ifb)
    bccon%frecv%tabscal(1)%scal(ic) = bccon%fsend%tabscal(1)%scal(is)
    bccon%frecv%tabscal(2)%scal(ic) = bccon%fsend%tabscal(2)%scal(is)
    bccon%frecv%tabvect(1)%vect(ic) = bccon%fsend%tabvect(1)%vect(is)
  enddo

enddo block
!$OMP END DO
!$OMP END PARALLEL

! --- Gradient BC --------------------------------------
case(bccon_cell_grad, bccon_face_grad)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, is, buf) &
!$OMP shared(ista, iend, nblock) 
  
!$OMP DO
do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1
  do ifb = 1, buf
    if = ustboco%iface(ista(ib)+ifb-1)
    is = bccon%isend(ifb)
    ic = bccon%irecv(ifb)
    bccon%frecv%tabvect(1)%vect(ic) = bccon%fsend%tabvect(1)%vect(is) 
    bccon%frecv%tabvect(2)%vect(ic) = bccon%fsend%tabvect(2)%vect(is) 
    bccon%frecv%tabtens(1)%tens(ic) = bccon%fsend%tabtens(1)%tens(is) 
  enddo

enddo
!$OMP END DO
!$OMP END PARALLEL

! ------------------------------------------------------
case default
  call error_stop("Internal error: unknown connection mode (setboco_ns_inlet_sub)")
endselect

deallocate(ista, iend)

endsubroutine setboco_ns_outlet_sup
!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation
!------------------------------------------------------------------------------!
