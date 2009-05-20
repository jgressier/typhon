!------------------------------------------------------------------------------!
! Procedure : ns_bocoflux                    Authors : J. Gressier / E. Radenac
!                                            Created : August 2005
! Function                                   Modif   : (cf history)
!   Computation of VISCOUS flux for NS equations
!
!------------------------------------------------------------------------------!
subroutine ns_bocoflux(defsolver, umesh, flux, field, defspat)
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D
use TENSOR3
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)        :: defsolver        ! solver
type(st_ustmesh)        :: umesh            ! unstructured mesh
type(st_field)          :: field            
type(mnu_spat)          :: defspat          ! spatial integration parameters

! -- Outputs --
type(st_genericfield)   :: flux             ! physical flux

! -- Internal variables --
integer                 :: ifb, if, ib, idef ! index de liste, index de face limite et parametres
integer                 :: icl, icr
real(krp), dimension(1) :: dHR, dHL          ! cell to face distance
type(v3d), dimension(1) :: vLR               ! cell to cell vector
type(v3d), dimension(1) :: gradTL, gradTR    ! left, right temp grad
real(krp), dimension(1) :: TL, TR            ! left, right temperatures
real(krp), dimension(1) :: TH, mu, gradTH    ! temperature at H
real(krp), dimension(1) :: rhoL, rhoR, rhoH  ! densities
type(st_face), dimension(1) :: face          ! geomtrical face array
real(krp)                   :: r_PG, cp, conduct
real(krp)                   :: id  

! -- BODY --

do ib = 1, umesh%nboco

  idef = umesh%boco(ib)%idefboco   ! index of boco definition in defsolver structure
  if (idef <= 0) cycle               ! if index <= 0, internal boco (connection...)

  !---------------------------------------------------------------------
  ! assign flux as already computed flux in bocofield

  select case(defsolver%boco(idef)%typ_boco)

  case(bc_wall_adiab, bc_wall_flux, bc_wall_hconv, bc_wall_hgen) 

    do ifb = 1, umesh%boco(ib)%nface
      if  = umesh%boco(ib)%iface(ifb)
      icl = umesh%facecell%fils(if,1)
      icr = umesh%facecell%fils(if,2)

      r_PG = defsolver%defns%properties(1)%r_const  ! perfect gas constant
      cp = defsolver%defns%properties(1)%gamma * r_PG / &
           (defsolver%defns%properties(1)%gamma - 1)    ! heat capacity

      dHL(1) = abs(umesh%mesh%iface(if,1,1)%centre - &
                  umesh%mesh%centre(icl,1,1))
      dHR(1) = abs(umesh%mesh%iface(if,1,1)%centre - &
                  umesh%mesh%centre(icr,1,1))
      id      = 1._krp/(dHL(1) + dHR(1))
      dHL(1) = id*dHL(1)
      dHR(1) = id*dHR(1)
      vLR(1) = umesh%mesh%centre(icr,1,1) - &
               umesh%mesh%centre(icl,1,1)
      ! DEV / OPT : calcul de distance au carre si c'est la seule utilisee
      ! pour eviter sqrt()**2
      !dLR = abs(vLR)

      rhoL(1) = field%etatprim%tabscal(1)%scal(icl)
      rhoR(1) = field%etatprim%tabscal(1)%scal(icr)
      TL(1)   = field%etatprim%tabscal(2)%scal(icl)/ (rhoL(1) * r_PG)
      TR(1)   = field%etatprim%tabscal(2)%scal(icr)/ (rhoR(1) * r_PG)
      TH(1)   = dHR(1)*TL(1)   + dHL(1)*TR(1)
      rhoH(1) = dHR(1)*rhoL(1) + dHL(1)*rhoR(1)

      ! computation of temperature gradient : 
      ! grad(T) = 1/(density*r)*grad(P) - P/(r*density**2)*grad(density)
      gradTL(1) = 1._krp/ (rhoL(1) * r_PG) * field%gradient%tabvect(2)%vect(icl) - &
           field%etatprim%tabscal(2)%scal(icl) / (rhoL(1)**2 *r_PG) * field%gradient%tabvect(1)%vect(icl)
      gradTR(1) = 1._krp/ (rhoR(1) * r_PG) * field%gradient%tabvect(2)%vect(icr) - &
           field%etatprim%tabscal(2)%scal(icr) / (rhoR(1)**2 *r_PG) * field%gradient%tabvect(1)%vect(icr)

      call calc_viscosity(defsolver%defns%properties(1), rhoH(1:1), TH(1:1), mu(1:1))

      ! temperature gradient at the face
      face(1) = umesh%mesh%iface(if,1,1)
      call interp_facegradn_scal(1,defspat%sch_dis,dHL,dHR,vLR,&
                               face,TL,TR,gradTL,gradTR,gradTH)
 
      ! viscous, heat flux
      ! thermal conductivity
      conduct = mu(1) * cp / defsolver%defns%properties(1)%prandtl

      flux%tabscal(2)%scal(if) = flux%tabscal(2)%scal(if) + &
         conduct * gradTH(1) + umesh%boco(ib)%bocofield%tabscal(1)%scal(ifb)
    enddo
  endselect

enddo

endsubroutine ns_bocoflux

!------------------------------------------------------------------------------!
! Changes history
!
! Aug  2005 : creation
!------------------------------------------------------------------------------!
