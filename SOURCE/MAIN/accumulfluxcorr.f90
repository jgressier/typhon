!------------------------------------------------------------------------------!
! Procedure : accumulfluxcorr            Auteur : E.Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!   Cumulation of fluxes during a cycle, for conservative corrections of
!   interface energy
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine accumulfluxcorr(dtloc, def_solver, domainenboco, domaineboco, &
                           nface, flux, ncoupling, coupling, field, umesh, &
                           defspat)
                           

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use MENU_ZONECOUPLING
use MENU_NUM

implicit none

! -- Declaration des entrees --
type(st_ustmesh) :: umesh            ! unstructured domain
real(krp)        :: dtloc(1:umesh%ncell)        ! CFL time step
type(mnu_solver) :: def_solver       ! solver properties
integer          :: domainenboco     ! number of domain boundary conditions
type(st_ustboco), dimension(1:domainenboco) &
                 :: domaineboco      ! domain boundary conditions
integer          :: nface            ! number of faces in domain
real(krp), dimension(1:nface) &
                 :: flux
integer          :: ncoupling        ! number of couplings in zone
type(mnu_spat)   :: defspat          ! spatial integration parameters
type(st_field)   :: field            ! field

! -- Declaration des entrees/sorties --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! coupling data

! -- Declaration des variables internes --
real(krp), parameter                :: theta = 1._krp
real(krp), dimension(1) :: dHR, dHL   ! cell to face distance
type(v3d), dimension(1) :: vLR        ! cell to cell vector
type(v3d), dimension(1) :: gradTL, gradTR  ! left, right temp grad
real(krp), dimension(1) :: TL, TR     ! left, right temperatures
real(krp), dimension(1) :: TH, mu, gradTH ! temperature at H
real(krp), dimension(1) :: rhoL, rhoR, rhoH
type(st_face), dimension(1) :: face       ! geomtrical face array
real(krp)                           :: r_PG, cp, conduct
real(krp)                           :: id
integer(kip)                        :: if, ib, i, ic
real(krp)                           :: rflux, etatcons       

! -- Debut de la procedure --

! Accumulation of fluxes at the interface
do ic =1, ncoupling
  do ib =1, domainenboco
    if (samestring(coupling(ic)%family, domaineboco(ib)%family)) then
      do i = 1, domaineboco(ib)%nface
        if = domaineboco(ib)%iface(i)

        select case(def_solver%typ_solver)
        case(solKDIF)
          ! the computed flux is used
          rflux = flux(if)
          etatcons = coupling(ic)%zcoupling%etatcons%tabscal(1)%scal(i)
          coupling(ic)%zcoupling%etatcons%tabscal(1)%scal(i) = etatcons + &
                                                               rflux * dtloc(i)

        case(solNS)
          ! the heat diffusion flux is re-computed

          r_PG = def_solver%defns%properties(1)%r_const  ! perfect gas constant
          cp = def_solver%defns%properties(1)%gamma * r_PG / &
               (def_solver%defns%properties(1)%gamma - 1)    ! heat capacity

          dHL(1) = abs(umesh%mesh%iface(if,1,1)%centre - &
                    umesh%mesh%centre(umesh%facecell%fils(if,1),1,1))
          dHR(1) = abs(umesh%mesh%iface(if,1,1)%centre - &
                    umesh%mesh%centre(umesh%facecell%fils(if,2),1,1))
          id      = 1._krp/(dHL(1) + dHR(1))
          dHL(1) = id*dHL(1)
          dHR(1) = id*dHR(1)
          vLR(1) = umesh%mesh%centre(umesh%facecell%fils(if,2),1,1) - &
                   umesh%mesh%centre(umesh%facecell%fils(if,1),1,1)
          ! DEV / OPT : calcul de distance au carre si c'est la seule utilisee
          ! pour eviter sqrt()**2
          !dLR = abs(vLR)

          rhoL(1) = field%etatprim%tabscal(1)%scal(umesh%facecell%fils(if,1))
          TL(1)   = field%etatprim%tabscal(2)%scal(umesh%facecell%fils(if,1)) / (rhoL(1) * r_PG)
          rhoR(1) = field%etatprim%tabscal(1)%scal(umesh%facecell%fils(if,2))
          TR(1)   = field%etatprim%tabscal(2)%scal(umesh%facecell%fils(if,2)) / (rhoR(1) * r_PG)

          TH(1)   = dHR(1)*TL(1)   + dHL(1)*TR(1)
          rhoH(1) = dHR(1)*rhoL(1) + dHL(1)*rhoR(1)

          ! computation of temperature gradient : 
          ! grad(T) = 1/(density*r)*grad(P) - P/(r*density**2)*grad(density)
          gradTL(1) = 1._krp/(rhoL(1) * r_PG) * field%gradient%tabvect(2)%vect(umesh%facecell%fils(if,1)) - &
               field%etatprim%tabscal(2)%scal(umesh%facecell%fils(if,1)) / (rhoL(1)**2 *r_PG) &
               * field%gradient%tabvect(1)%vect(umesh%facecell%fils(if,1))
          gradTR(1) = 1._krp/(rhoR(1) * r_PG) * field%gradient%tabvect(2)%vect(umesh%facecell%fils(if,2)) - &
         field%etatprim%tabscal(2)%scal(umesh%facecell%fils(if,2)) / (rhoR(1)**2 *r_PG) &
         * field%gradient%tabvect(1)%vect(umesh%facecell%fils(if,2))

          call calc_viscosity(def_solver%defns%properties(1), rhoH(1:1), TH(1:1), mu(1:1))

        ! temperature gradient at the face
        face(1) = umesh%mesh%iface(if,1,1)
        call interp_facegradn_scal(1,defspat%sch_dis,dHL,dHR,vLR,&
                                   face,TL,TR,gradTL,gradTR,gradTH)

        ! viscous, heat flux
        ! thermal conductivity
        conduct = mu(1) * cp / def_solver%defns%properties(1)%prandtl
        rflux = - conduct * gradTH(1) * &
                  umesh%mesh%iface(if,1,1)%surface
        etatcons = coupling(ic)%zcoupling%etatcons%tabscal(1)%scal(i)
        coupling(ic)%zcoupling%etatcons%tabscal(1)%scal(i) = etatcons + &
                                                             rflux * dtloc(i)

      case default
        call erreur("Incoherence interne (accumulfluxcorr)", &
                    "type de solveur inconnu")
      endselect 
      enddo
    endif
  enddo
enddo

endsubroutine accumulfluxcorr

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!
