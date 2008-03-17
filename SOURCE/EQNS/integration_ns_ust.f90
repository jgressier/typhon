!------------------------------------------------------------------------------!
! Procedure : integration_ns_ust  
!      
! Fonction  
!   NS flux computation 
!   - packet based computation
!   - high order interpolation
!
!------------------------------------------------------------------------------!
subroutine integration_ns_ust(defsolver, defspat, domaine, field, flux, &
                              calc_jac, jacL, jacR)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use EQNS
use MATRIX_ARRAY

implicit none

! -- INPUTS --
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(mnu_spat)   :: defspat          ! parametres d'integration spatiale
type(st_ustmesh) :: domaine          ! domaine non structure a integrer
logical          :: calc_jac         ! choix de calcul de la jacobienne

! -- INPUTS/OUTPUTS --
type(st_field)   :: field            ! champ des valeurs et residus

! -- OUTPUTS --
type(st_genericfield)   :: flux        ! flux physiques
type(st_mattab)         :: jacL, jacR  ! jacobiennes associees (gauche et droite)

! -- Internal variables --
logical :: gradneeded           ! use gradients or not
integer :: if, nfb              ! index de face et taille de bloc courant
integer :: nbuf                 ! taille de buffer 
integer :: ib, nbloc            ! index de bloc et nombre de blocs
integer :: ideb, ifin           ! index de debut et fin de bloc
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule a gauche et a droite
type(st_genericfield) :: cell_l, cell_r       ! buffer sized, state extrapolation at face
type(st_genericfield) :: gradL, gradR         ! block size arrays of gradients
type(v3d), dimension(:), allocatable &
                      :: cg_l, cg_r           ! tableau des centres de cellules a gauche et a droite   

! -- BODY --

! On peut ici decouper la maillage complet en blocs de taille fixe pour optimiser
! l'encombrement memoire et la vectorisation

call calc_buffer(domaine%nface, cell_buffer, nbloc, nbuf, nfb)

call new(cell_l, nbuf, field%etatprim%nscal, field%etatprim%nvect, field%etatprim%ntens)
call new(cell_r, nbuf, field%etatprim%nscal, field%etatprim%nvect, field%etatprim%ntens)
allocate(  cg_l(nbuf),   cg_r(nbuf))
call new(gradL, nbuf, field%gradient%nscal, field%gradient%nvect, field%gradient%ntens)
call new(gradR, nbuf, field%gradient%nscal, field%gradient%nvect, field%gradient%ntens)

ideb = 1

do ib = 1, nbloc

  ifin = ideb+nfb-1

  select case(defspat%method)

  case(hres_none)
    
    ! -- no extrapolation, only direct copy of cell values --

    call distrib_field(field%etatprim, domaine%facecell, ideb, ifin, &
                       cell_l, cell_r, 1)
  
 
  !----------------------------------------------------------------------
  ! HIGH ORDER states interpolation
  !----------------------------------------------------------------------
  case(hres_muscl)

    call hres_ns_muscl(defspat, nfb, ideb, domaine,      &
                       field%etatprim, field%gradient,   &
                       cell_l, cell_r)

  case(hres_musclfast)

    call hres_ns_musclfast(defspat, nfb, ideb, domaine,      &
                           field%etatprim, field%gradient,   &
                           cell_l, cell_r)

  case(hres_muscluns)

    call hres_ns_muscluns(defspat, nfb, ideb, domaine,      &
                          field%etatprim, field%gradient,   &
                          cell_l, cell_r)

  case(hres_svm)

    call hres_ns_svm(defspat, nfb, ideb, domaine, field%etatprim, cell_l, cell_r)

  case default
    call erreur("flux computation","unknown high resolution method")
  endselect

  ! --- POST-LIMITATION ---

  select case(defspat%postlimiter)
  case(postlim_none)
    ! NOTHING TO DO
  case(postlim_monotonic)
    call postlimit_monotonic(defspat, nfb, ideb, domaine, &
                             field%etatprim, cell_l, cell_r)
  case default
    call erreur("flux computation","unknown POST-LIMITATION method")
  endselect


  !----------------------------------------------------------------------
  ! computation of INVISCID fluxes
  !----------------------------------------------------------------------

  call calc_flux_inviscid(defsolver, defspat,                             &
                          nfb, ideb, domaine%mesh%iface(ideb:ifin, 1, 1), &
                          cell_l, cell_r, flux, calc_jac, jacL, jacR)

  !----------------------------------------------------------------------
  ! computation of VISCOUS fluxes
  !----------------------------------------------------------------------
  select case(defsolver%defns%typ_fluid)

  case(eqEULER)
    ! nothing to do

  case(eqNSLAM)
    ! -- redirection of cell centers 
    cg_l(1:nfb) = domaine%mesh%centre(domaine%facecell%fils(ideb:ifin,1), 1, 1)
    cg_r(1:nfb) = domaine%mesh%centre(domaine%facecell%fils(ideb:ifin,2), 1, 1)

    ! -- redirection of gradients
    call distrib_field(field%gradient, domaine%facecell, ideb, ifin, &
                       gradL, gradR, 1)
    call calc_flux_viscous(defsolver, defspat,                        &
                           nfb, ideb, domaine%mesh%iface(ideb:ifin, 1, 1), &
                           cg_l, cg_r,                                &
                           cell_l, cell_r, gradL, gradR, flux,        &
                           calc_jac, jacL, jacR)
  case(eqRANS)
    call erreur("development", "turbulence modeling not implemented")   

  case default
    call erreur("viscous flux computation", "unknown model")
  endselect

  !----------------------------------------------------------------------
  ! end of block

  ideb = ideb + nfb
  nfb  = nbuf         ! tous les blocs suivants sont de taille nbuf
  
enddo

!-------------------------------------------------------------
! flux assignment or modification on boundary conditions

call ns_bocoflux(defsolver, domaine, flux, field, defspat)

call delete(cell_l)
call delete(cell_r)
deallocate(cg_l, cg_r)
call delete(gradL)
call delete(gradR)

endsubroutine integration_ns_ust

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : created, basic calls
! nov  2004 : high order interpolation
! feb  2005 : call to viscous flux computation
! Nov  2007 : add post limitation
!------------------------------------------------------------------------------!
