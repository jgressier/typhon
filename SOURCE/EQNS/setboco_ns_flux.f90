!------------------------------------------------------------------------------!
! Procedure : setboco_ns_flux             Auteur : J. Gressier/E. Radenac
!                                         Date   : June 2005
!> @brief Calcul des conditions aux limites non uniformes pour la conduction de la chaleur, mur à flux imposé
!------------------------------------------------------------------------------!
subroutine setboco_ns_flux(defns, defale, defmrf, unif, ustboco, umesh, bccon, bcns, curtime)

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
integer    :: ifb, if, ip, ii, is  ! index de liste, index de face limite et parametres
integer    :: ic, ighost   ! index de cellule interieure, et de cellule fictive
integer    :: nf
real(krp)  :: r_PG, cp, gam, gsgmu     ! perfect gas constant, heat capacity
real(krp)  :: temp, conduct, d, gTdc, gPdc
real(krp), dimension(1) &
           :: TH, mu       ! cell temperature , viscocity
type(v3d)  :: cgface, cg, normale ! face, cell center, face normale
type(v3d)  :: gradT        ! temperature gradient
type(v3d)  :: dc           ! vector cell center - its projection 
                           ! on the face normale
integer              :: ib, nblock, buf            ! block index and number of blocks
integer, pointer     :: ista(:), iend(:)           ! starting and ending index
real(krp), allocatable :: lflux(:)
type(v3d)  :: wallvelocity

! -- BODY --

nf = ustboco%nface
allocate(lflux(nf))

gam   = defns%properties(1)%gamma
gsgmu = gam/(gam-1._krp)
r_PG = defns%properties(1)%r_const    ! perfect gas constant
cp   = gsgmu*r_PG                     ! heat capacity

select case(unif)
case(uniform)  
  lflux = bcns%flux  !!! USER flux is entering the domain (so, must be reversed)
                       ! already reversed in def_boco_ns
case(nonuniform)
  lflux(1:nf) = bcns%flux_nunif(1:nf)
case default
  call error_stop("unknown definition of boco flux computation (ns)")
endselect

call new_buf_index(nf, fct_buffer, nblock, ista, iend, nthread)

select case(bccon%bccon_mode)
! --- State BC --------------------------------------
case(bccon_cell_state)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, ii, buf, cgface, cg, normale, d, TH, conduct, mu, temp, wallvelocity) &
!$OMP shared(ista, iend, nblock, gam, cp, r_PG) 
!$OMP DO
do ib = 1, nblock
  buf = iend(ib)-ista(ib)+1
  do ifb = ista(ib), iend(ib)
    if   = ustboco%iface(ifb)
    ighost = bccon%irecv(ifb)
    ic     = bccon%isend(ifb)

    cgface = umesh%mesh%face_center(if,1)
    cg     = umesh%mesh%centre(ic,1,1)
    normale= umesh%mesh%face_normal(if,1)
    d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
  
    TH(1) = bccon%fsend%tabscal(2)%scal(ic) / (r_PG * bccon%fsend%tabscal(1)%scal(ic) )

    call calc_viscosity(defns%properties(1), bccon%fsend%tabscal(1)%scal(ic:ic), TH(1:1), mu(1:1))

    conduct = mu(1) * cp / defns%properties(1)%prandtl

    temp = bccon%fsend%tabscal(2)%scal(ic) / &
       ( bccon%fsend%tabscal(1)%scal(ic) * r_PG ) - lflux(ifb)*d/conduct

    ! heat flux
    ustboco%bocofield%tabscal(1)%scal(ifb) = ustboco%bocofield%tabscal(1)%scal(ifb) + lflux(ifb)
    ! pressure
    bccon%frecv%tabscal(2)%scal(ighost) = bccon%fsend%tabscal(2)%scal(ic)
    ! density
    bccon%frecv%tabscal(1)%scal(ighost) = bccon%fsend%tabscal(2)%scal(ighost)/(r_PG*temp) 
    ! velocity
    wallvelocity = bcns%wall_velocity
    call calc_wallvelocity(defale, defmrf, wallvelocity, umesh%mesh%face_center(if,1), if, curtime)
    bccon%frecv%tabvect(1)%vect(ighost) = (2._krp*wallvelocity) - bccon%fsend%tabvect(1)%vect(ic)
  enddo ! loop in face packet
enddo ! loop on blocks
!$OMP END DO
!$OMP END PARALLEL

! --- State BC --------------------------------------
case(bccon_face_state)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, ii, buf) &
!$OMP shared(ista, iend, nblock, gam) 
!$OMP DO
do ib = 1, nblock
  buf = iend(ib)-ista(ib)+1
  do ifb = ista(ib), iend(ib)
    if   = ustboco%iface(ifb)
    ighost = bccon%irecv(ifb)
    ic     = bccon%isend(ifb)

    ! heat flux for flux condition
    ustboco%bocofield%tabscal(1)%scal(ifb) = ustboco%bocofield%tabscal(1)%scal(ifb) + lflux(ifb)

    bccon%frecv%tabscal(1)%scal(ighost) = bccon%fsend%tabscal(2)%scal(ic)  ! density / weak BC
    bccon%frecv%tabscal(2)%scal(ighost) = bccon%fsend%tabscal(2)%scal(ic)  ! pressure
    ! velocity
    wallvelocity = bcns%wall_velocity
    call calc_wallvelocity(defale, defmrf, wallvelocity, umesh%mesh%face_center(if,1), if, curtime)
    bccon%frecv%tabvect(1)%vect(ighost) = (2._krp*wallvelocity) - bccon%fsend%tabvect(1)%vect(ic)
    
  enddo ! loop in face packet
enddo ! loop on blocks
!$OMP END DO
!$OMP END PARALLEL

! --- Gradient BC --------------------------------------
case(bccon_cell_grad, bccon_face_grad)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, is, buf) &
!$OMP shared(ista, iend, nblock) 
!$OMP DO
do ib = 1, nblock
  do ifb = ista(ib), iend(ifb)
    if = ustboco%iface(ifb)
    is = bccon%isend(ifb)
    ic = bccon%irecv(ifb)
    bccon%frecv%tabvect(1)%vect(ic) = -bccon%fsend%tabvect(1)%vect(is) ! impose flux or FLUX BC ?
    bccon%frecv%tabvect(2)%vect(ic) = -bccon%fsend%tabvect(2)%vect(is) 
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
deallocate(lflux)

endsubroutine setboco_ns_flux
!------------------------------------------------------------------------------!
! Changes history
!
! jun  2005: creation
! sept 2005: changed to ghost cell (velocity is symmetrical)
! June 2009: simplification and bug correction (must not use gradients)
! Apr  2011: wall velocity updated to account for MRF and ALE (A.Gardi)
!------------------------------------------------------------------------------!
