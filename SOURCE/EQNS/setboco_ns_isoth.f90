!------------------------------------------------------------------------------!
! Procedure : setboco_ns_isoth            Auteur : J. Gressier/E. Radenac
!                                         Date   : June 2005
!> @brief Calcul des conditions aux limites non uniformes pour la conduction de la chaleur, mur isotherme
!------------------------------------------------------------------------------!
subroutine setboco_ns_isoth(defns, defale, defmrf, unif, ustboco, umesh, bccon, bcns, curtime)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use MENU_ALE
use MESHMRF
use USTMESH
use MGRID 

implicit none

! -- INPUTS --
type(mnu_ns)       :: defns            ! solver parameters
type(mnu_ale)      :: defale           ! ALE parametres
type(mnu_mrf)      :: defmrf           ! MRF parametres
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! boundary condition location
type(st_ustmesh)   :: umesh            ! unstructured mesh
type(st_boco_ns)   :: bcns             ! parameters and temperature (field or constant)
real(krp)          :: curtime          ! current time

! -- OUTPUTS --
type(st_bccon) :: bccon  ! pointer of send or receive fields

! -- Internal variables --
integer    :: ifb, if, ip, nf, is  ! index de liste, index de face limite et parametres
integer    :: ic, ighost   ! index de cellule interieure, et de cellule fictive
real(krp)  :: r_PG         ! perfect gas constant
real(krp)  :: gPdc, temp, wtemp(ustboco%nface)
type(v3d)  :: cgface, cg, normale ! face, cell center, face normale
type(v3d)  :: dc           ! vector cell center - its projection 
                           ! on the face normale
type(v3d)  :: wallvelocity
integer                          :: ib, nblock, buf            ! block index and number of blocks
integer, pointer                 :: ista(:), iend(:)           ! starting and ending index

! -- BODY --

r_PG = defns%properties(1)%r_const        ! perfect gas constant
nf   = ustboco%nface

select case(unif)
case(uniform)  
  wtemp(1:nf) = bcns%temp_wall
case(nonuniform)
  wtemp(1:nf) = bcns%temp(1:nf)
case default
  call error_stop("unknown definition of boco flux computation (ns)")
endselect

call new_buf_index(nf, fct_buffer, nblock, ista, iend, nthread)

select case(bccon%bccon_mode)
! --- State BC --------------------------------------
case(bccon_cell_state, bccon_face_state)

  do ifb = 1, nf
    if     = ustboco%iface(ifb)
    ighost = bccon%irecv(ifb)
    ic     = bccon%isend(ifb)

    ! extrapolated temperature on ghost cell (supposed symmetrical)
    temp = 2._krp*wtemp(ifb) - bccon%fsend%tabscal(2)%scal(ic) / &
           ( bccon%fsend%tabscal(1)%scal(ic) * r_PG )
    ! pressure
    bccon%frecv%tabscal(2)%scal(ighost) = bccon%fsend%tabscal(2)%scal(ic)! 
    ! density
    bccon%frecv%tabscal(1)%scal(ighost) = &
                bccon%fsend%tabscal(2)%scal(ighost)/(r_PG*temp) 
    ! velocity
    !bccon%fxx%tabvect(1)%vect(ighost) = v3d(0._krp,0._krp,0._krp)
    wallvelocity = bcns%wall_velocity
    call calc_wallvelocity(defale, defmrf, wallvelocity, umesh%mesh%face_center(if,1), if, curtime)
    bccon%frecv%tabvect(1)%vect(ighost) = (2._krp*wallvelocity) - bccon%fsend%tabvect(1)%vect(ic)

  enddo

! --- Gradient BC --------------------------------------
case(bccon_cell_grad, bccon_face_grad)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, is, buf) &
!$OMP shared(ista, iend, nblock) 
!$OMP DO
do ib = 1, nblock
  do ifb = ista(ib), iend(ib)
    if = ustboco%iface(ifb)
    is = bccon%isend(ifb)
    ic = bccon%irecv(ifb)
    bccon%frecv%tabvect(1)%vect(ic) =  bccon%fsend%tabvect(1)%vect(is)   ! grad rho
    bccon%frecv%tabvect(2)%vect(ic) = -bccon%fsend%tabvect(2)%vect(is)   ! grad p - to be made symmetric
    bccon%frecv%tabtens(1)%tens(ic) =  bccon%fsend%tabtens(1)%tens(is)   ! grad V
    call v3d_sym(bccon%frecv%tabvect(2)%vect(ic), umesh%mesh%face_normal(if,1))
  enddo
enddo
!$OMP END DO
!$OMP END PARALLEL

! ------------------------------------------------------
case default
  call error_stop("Internal error: unknown connection mode (setboco_ns_inlet_sup)")
endselect

deallocate(ista, iend)



endsubroutine setboco_ns_isoth
!------------------------------------------------------------------------------!
! Changes history
!
! jun 2005: creation
! sept 2005: changed to ghost cell (velocity is symmetrical)
! Apr  2011: wall velocity updated to account for MRF and ALE (A.Gardi)
!------------------------------------------------------------------------------!
