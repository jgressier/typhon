!------------------------------------------------------------------------------!
! Procedure : integration_ns_ust          Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (see history)
!   Integration d'un domaine non structuré
!   Le corps de la routine consiste à distribuer les états et les gradients
!   sur chaque face.
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_ns_ust(dt, defsolver, defspat, domaine, field, flux, &
                              calc_jac, jacL, jacR)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use EQNS

implicit none

! -- Declaration des entrées --
real(krp)        :: dt               ! pas de temps CFL
type(mnu_solver) :: defsolver        ! type d'équation à résoudre
type(mnu_spat)   :: defspat          ! paramètres d'intégration spatiale
type(st_ustmesh) :: domaine          ! domaine non structuré à intégrer
logical          :: calc_jac         ! choix de calcul de la jacobienne

! -- Declaration des entrées/sorties --
type(st_field)   :: field            ! champ des valeurs et résidus

! -- Declaration des sorties --
type(st_genericfield)   :: flux        ! flux physiques
real(krp), dimension(*) :: jacL, jacR  ! jacobiennes associées (gauche et droite)

! -- Declaration des variables internes --
integer :: if, nfb              ! index de face et taille de bloc courant
integer :: nbuf                 ! taille de buffer 
integer :: ib, nbloc            ! index de bloc et nombre de blocs
integer :: ideb, ifin           ! index de début et fin de bloc
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule à gauche et à droite
type(st_nsetat), dimension(:), allocatable & 
        :: cell_l, cell_r       ! tableau de cellules à gauche et à droite
type(v3d), dimension(:), allocatable &
        :: grad_l, grad_r       ! tableau des gradients
type(v3d), dimension(:), allocatable &
        :: cg_l, cg_r           ! tableau des centres de cellules à gauche et à droite   

! -- Debut de la procedure --

! On peut ici découper la maillage complet en blocs de taille fixé pour optimiser
! l'encombrement mémoire et la vectorisation

! nombre de blocs (<= taille_buffer) nécessaires pour domaine%nface
nbloc = 1 + (domaine%nface-1) / taille_buffer
nbuf  = 1 + (domaine%nface-1) / nbloc          ! taille de bloc buffer
nfb   = 1 + mod(domaine%nface-1, nbuf)         ! taille de 1er bloc peut être <> de nbuf

! il sera à tester l'utilisation de tableaux de champs génériques plutôt que
! des définitions type d'état spécifiques (st_nsetat)

allocate(grad_l(nbuf), grad_r(nbuf))
allocate(cell_l(nbuf), cell_r(nbuf))
allocate(  cg_l(nbuf),   cg_r(nbuf))

ideb = 1

do ib = 1, nbloc

  do it = 1, nfb
    if  = ideb+it-1
    icl = domaine%facecell%fils(if,1)
    icr = domaine%facecell%fils(if,2)
    !grad_l(it) = field%gradient%tabvect(1)%vect(icl)
    !grad_r(it) = field%gradient%tabvect(1)%vect(icr)
    cell_l(it)%density  = field%etatprim%tabscal(1)%scal(icl)
    cell_r(it)%density  = field%etatprim%tabscal(1)%scal(icr)
    cell_l(it)%pressure = field%etatprim%tabscal(2)%scal(icl)
    cell_r(it)%pressure = field%etatprim%tabscal(2)%scal(icr)
    cell_l(it)%velocity = field%etatprim%tabvect(1)%vect(icl)
    cell_r(it)%velocity = field%etatprim%tabvect(1)%vect(icr)
    cg_l(it)   = domaine%mesh%centre(icl, 1, 1)
    cg_r(it)   = domaine%mesh%centre(icr, 1, 1)
  enddo

  ! - dans une version ultérieure, il sera nécessaire de faire intervenir les gradients
  ! - l'accès au tableau flux n'est pas programmé de manière générale !!! DEV

  ifin = ideb+nfb-1

  select case(defspat%sch_hyp)
  case(hlle)
    call calc_flux_hlle(defsolver, defspat,                             &
                        nfb, domaine%mesh%iface(ideb:ifin, 1, 1),       &
                        cg_l, cell_l, cg_r, cell_r, flux, ideb,         &
                        calc_jac, jacL(ideb:ifin), jacR(ideb:ifin))
  case default
    call erreur("erreur","schéma numérique non implémenté (calcul de flux)")
  endselect

  ideb = ideb + nfb
  nfb  = nbuf         ! tous les blocs suivants sont de taille nbuf
  
enddo

deallocate(grad_l, grad_r, cell_l, cell_r, cg_l, cg_r)

call fluxlimite(defsolver, domaine, flux)

endsubroutine integration_ns_ust

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : création de la procédure
!------------------------------------------------------------------------------!
