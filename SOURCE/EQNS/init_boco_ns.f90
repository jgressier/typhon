!------------------------------------------------------------------------------!
! Procedure : init_boco_ns                Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : 
!   Traitement des parametres du fichier menu principal
!   Initialisation des conditions limites
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_boco_ns(defsolver, umesh)

use TYPHMAKE
use DEFFIELD
use MENU_NS
use USTMESH
use MENU_SOLVER
use VARCOM

implicit none

! -- Declaration des entrees --
type(st_ustmesh)  :: umesh

! -- Declaration des entrees/sorties --
type(mnu_solver)  :: defsolver

! -- Declaration des variables internes --
integer :: iboco, i, idef


! -- Debut de la procedure --

! On parcourt toutes les conditions limites du domaine

do iboco = 1, umesh%nboco 

  idef = umesh%boco(iboco)%idefboco

  if (idef > 0) then

    select case(defsolver%boco(idef)%typ_boco) 
    case(bc_inlet_sub, bc_inlet_sup, bc_outlet_sub, bc_outlet_sup)
      ! nothing to do
    case(bc_wall_isoth)
      ! nothing to do
    case(bc_wall_flux, bc_wall_hconv, bc_wall_hgen)
      umesh%boco(iboco)%bocofield => insert_newgfield(umesh%boco(iboco)%bocofield, umesh%boco(iboco)%nface*defsolver%nsim, 1, 0, 0)
    !case default
    !  call erreur("Internal error","unknown boundary condition (init_boco_ns)")
    endselect

  !!! DEV !!! must locate array in umesh%boco instead of defsolver

  !-------------------------------------------------
  ! Condition de Dirichlet 
  !-------------------------------------------------
  ! Cas d'existence d'un tableau de temperatures
  if(defsolver%boco(idef)%boco_ns%alloctemp) then
    allocate(defsolver%boco(idef)%boco_ns%temp(umesh%boco(iboco)%nface))

    ! Cas d'existence d'un fichier de temperatures limites :
    if(defsolver%boco(idef)%boco_ns%tempfile .ne. cnull) then
      open(unit=1002, file = trim(defsolver%boco(idef)%boco_ns%tempfile), form="formatted")
      read(1002,*)  (defsolver%boco(idef)%boco_ns%temp(i),i = 1, umesh%boco(iboco)%nface) 
      close(1002)
    endif

  endif

  !-------------------------------------------------
  ! Condition de Von Neumann
  !-------------------------------------------------
  ! Cas d'existence d'un tableau de flux
  if(defsolver%boco(idef)%boco_ns%allocflux) then
    allocate(defsolver%boco(idef)%boco_ns%flux_nunif(umesh%boco(iboco)%nface))

    ! Cas d'existence d'un fichier de flux limites :
    if(defsolver%boco(idef)%boco_ns%fluxfile .ne. cnull) then
      open(unit=1002, file = trim(defsolver%boco(idef)%boco_ns%fluxfile), form="formatted")
      read(1002,*)  (defsolver%boco(idef)%boco_ns%flux_nunif(i),i = 1, umesh%boco(iboco)%nface) 
      close(1002)
      ! convention de flux sortant dans le code / CL : flux entrant pour l'utilisateur
      defsolver%boco(idef)%boco_ns%flux_nunif(:) = &
        - defsolver%boco(idef)%boco_ns%flux_nunif(:)
    endif
  endif

  !-------------------------------------------------
  ! Condition de convection
  !-------------------------------------------------
  ! Cas d'existence de tableaux de coefficients et temperatures de convection
  if(defsolver%boco(idef)%boco_ns%allochconv) then
    allocate(defsolver%boco(idef)%boco_ns%h_nunif(umesh%boco(iboco)%nface))
    allocate(defsolver%boco(idef)%boco_ns%tconv_nunif(umesh%boco(iboco)%nface))

  endif

  endif

enddo

endsubroutine init_boco_ns

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation de la routine
! Oct  2005 : check if idef > 0 (otherwise connection boco)
!------------------------------------------------------------------------------!


