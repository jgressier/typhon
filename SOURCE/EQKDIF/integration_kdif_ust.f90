!------------------------------------------------------------------------------!
! Procedure : integration_kdif_ust              Authors : J. Gressier
!                                               Created : April 2003
! Fonction                                      Modif  : (cf history)
!   Given field and boundary conditions, Computation of
!   - explicit fluxes
!   - jacobian matrices (if needed)
!
!------------------------------------------------------------------------------!
subroutine integration_kdif_ust(dt, defsolver, defspat, domaine, field, flux, &
                                calc_jac, jacL, jacR)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use EQKDIF

implicit none

! -- Declaration des entrees --
real(krp)        :: dt               ! pas de temps CFL
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(mnu_spat)   :: defspat          ! parametres d'integration spatiale
type(st_ustmesh) :: domaine          ! domaine non structure a integrer
logical          :: calc_jac         ! choix de calcul de la jacobienne

! -- Declaration des entrees/sorties --
type(st_field)   :: field            ! champ des valeurs et residus

! -- Declaration des sorties --
type(st_genericfield)   :: flux        ! flux physiques
real(krp), dimension(*) :: jacL, jacR  ! jacobiennes associees (gauche et droite)

! -- Declaration des variables internes --
integer :: if, nfb              ! index de face et taille de bloc courant
integer :: nbuf                 ! taille de buffer 
integer :: ib, nbloc            ! index de bloc et nombre de blocs
integer :: ideb, ifin           ! index de debut et fin de bloc
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule a gauche et a droite
type(st_kdifetat), dimension(:), allocatable & 
        :: cell_l, cell_r       ! tableau de cellules a gauche et a droite
type(v3d), dimension(:), allocatable &
        :: grad_l, grad_r       ! tableau des gradients
type(v3d), dimension(:), allocatable &
        :: cg_l, cg_r           ! tableau des centres de cellules a gauche et a droite   

! -- Debut de la procedure --

! On peut ici decouper la maillage complet en blocs de taille fixe pour optimiser
! l'encombrement memoire et la vectorisation

! nombre de blocs (<= taille_buffer) necessaires pour domaine%nface
nbloc = 1 + (domaine%nface-1) / taille_buffer
nbuf  = 1 + (domaine%nface-1) / nbloc          ! taille de bloc buffer
nfb   = 1 + mod(domaine%nface-1, nbuf)         ! taille de 1er bloc peut etre <> de nbuf

!print*,"!!! DEBUG integration kdif", domaine%nface, nbloc, nbuf, nfb, taille_buffer

! il sera a tester l'utilisation de tableaux de champs generiques plutôt que
! des definitions type d'etat specifiques (st_kdifetat)

allocate(grad_l(nbuf), grad_r(nbuf))
allocate(cell_l(nbuf), cell_r(nbuf))
allocate(  cg_l(nbuf),   cg_r(nbuf))

ideb = 1

do ib = 1, nbloc

  !print*,"!!! DEBUG integration bloc,",ib," de",ideb," a",ideb+nfb-1
  !! DEV : optimisation ? 13% du temps de calcul en explicite !!!

  do it = 1, nfb
    if  = ideb+it-1
    icl = domaine%facecell%fils(if,1)
    icr = domaine%facecell%fils(if,2)
    grad_l(it) = field%gradient%tabvect(1)%vect(icl)
    grad_r(it) = field%gradient%tabvect(1)%vect(icr)
    cell_l(it)%temperature = field%etatprim%tabscal(1)%scal(icl)
    cell_r(it)%temperature = field%etatprim%tabscal(1)%scal(icr)
    cg_l(it)   = domaine%mesh%centre(icl, 1, 1)
    cg_r(it)   = domaine%mesh%centre(icr, 1, 1)
  enddo

  ! - dans une version ulterieure, il sera necessaire de faire intervenir les gradients
  ! - l'acces au tableau flux n'est pas programme de maniere generale !!! DEV

  ifin = ideb+nfb-1

  ! ATTENTION : le flux n'est passe ici que pour UN SEUL scalaire

  call calc_kdif_flux(defsolver, defspat,                             &
                      nfb, domaine%mesh%iface(ideb:ifin, 1, 1),       &
                      cg_l, cell_l, grad_l, cg_r, cell_r, grad_r,     &
                      flux%tabscal(1)%scal(ideb:ifin),                &
                      calc_jac, jacL(ideb:ifin), jacR(ideb:ifin))

  ideb = ideb + nfb
  nfb  = nbuf         ! tous les blocs suivants sont de taille nbuf
  
enddo

!-------------------------------------------------------------
! flux assignment or modification on boundary conditions

call kdif_bocoflux(defsolver, domaine, flux, field%etatprim)

!-------------------------------------------------------------
deallocate(grad_l, grad_r, cell_l, cell_r, cg_l, cg_r)

endsubroutine integration_kdif_ust

!------------------------------------------------------------------------------!
! Change history
!
! apr  2003 : created
! june 2003 : update management of conservative and primitive variables
! oct  2003 : gradients added in left and right distribution
! apr  2004 : jacobian matrices computation for implicit solver
!------------------------------------------------------------------------------!
