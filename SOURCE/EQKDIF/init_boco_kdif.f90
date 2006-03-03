!------------------------------------------------------------------------------!
! Procedure : init_boco_kdif                   Authors : J. Gressier/E. Radenac
!                                              Created : Nov 2003
! Fonction                                   
!   Traitement des parametres du fichier menu principal
!   Initialisation des conditions limites
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_boco_kdif(defsolver, ustdom)

use TYPHMAKE
use DEFFIELD
use MENU_KDIF
use USTMESH
use MENU_SOLVER
use VARCOM

implicit none

! -- Declaration des entrees --
type(st_ustmesh)  :: ustdom

! -- Declaration des entrees/sorties --
type(mnu_solver)  :: defsolver

! -- Declaration des variables internes --
integer :: iboco, i, idef

! -- Debut de la procedure --

! On parcourt toutes les conditions limites du domaine

do iboco = 1, ustdom%nboco 

  idef = ustdom%boco(iboco)%idefboco

  if (idef > 0) then

  !-------------------------------------------------
  ! Condition de Dirichlet idef
  !-------------------------------------------------
  ! Cas d'existence d'un tableau de temperatures
  if(defsolver%boco(idef)%boco_kdif%alloctemp) then
    allocate(defsolver%boco(idef)%boco_kdif%temp(ustdom%boco(iboco)%nface))

    ! Cas d'existence d'un fichier de temperatures limites :
    if(defsolver%boco(idef)%boco_kdif%tempfile .ne. cnull) then
      open(unit=1002, file = trim(defsolver%boco(idef)%boco_kdif%tempfile), form="formatted")
      read(1002,*)  (defsolver%boco(idef)%boco_kdif%temp(i),i = 1, ustdom%boco(iboco)%nface) 
      close(1002)
    endif

  endif

  !-------------------------------------------------
  ! Condition de Von Neumann
  !-------------------------------------------------
  ! Cas d'existence d'un tableau de flux
  if(defsolver%boco(idef)%boco_kdif%allocflux) then
    allocate(defsolver%boco(idef)%boco_kdif%flux_nunif(ustdom%boco(iboco)%nface))

    ! Cas d'existence d'un fichier de flux limites :
    if(defsolver%boco(idef)%boco_kdif%fluxfile .ne. cnull) then
      open(unit=1002, file = trim(defsolver%boco(idef)%boco_kdif%fluxfile), form="formatted")
      read(1002,*)  (defsolver%boco(idef)%boco_kdif%flux_nunif(i),i = 1, ustdom%boco(iboco)%nface) 
      close(1002)
      ! convention de flux sortant dans le code / CL : flux entrant pour l'utilisateur
      defsolver%boco(idef)%boco_kdif%flux_nunif(:) = &
        - defsolver%boco(idef)%boco_kdif%flux_nunif(:)
    endif
  endif

  !-------------------------------------------------
  ! Condition de convection
  !-------------------------------------------------
  ! Cas d'existence de tableaux de coefficients et temperatures de convection
  if(defsolver%boco(idef)%boco_kdif%allochconv) then
    allocate(defsolver%boco(idef)%boco_kdif%h_nunif(ustdom%boco(iboco)%nface))
    allocate(defsolver%boco(idef)%boco_kdif%tconv_nunif(ustdom%boco(iboco)%nface))

    ! Cas d'existence de fichiers de coefficients et temperatures :
    if(defsolver%boco(idef)%boco_kdif%hfile .ne. cnull) then
      open(unit=1002, file = trim(defsolver%boco(idef)%boco_kdif%hfile), form="formatted")
      read(1002,*)  (defsolver%boco(idef)%boco_kdif%h_nunif(i),i = 1, ustdom%boco(iboco)%nface) 
      close(1002)
      !do i=1,ustdom%boco(iboco)%nface
      ! convention de flux sortant dans le code / CL : flux entrant pour l'utilisateur
      !defsolver%boco(idef)%boco_kdif%h_nunif(:) = &
      !  - defsolver%boco(idef)%boco_kdif%h_nunif(:)
      !enddo
    endif

    if(defsolver%boco(idef)%boco_kdif%tconvfile .ne. cnull) then
      open(unit=1002, file = trim(defsolver%boco(idef)%boco_kdif%tconvfile), form="formatted")
      read(1002,*)  (defsolver%boco(idef)%boco_kdif%tconv_nunif(i),i = 1, ustdom%boco(iboco)%nface) 
      close(1002)
    endif

  endif

  endif ! idef check

enddo

!-------------------------------------------------
! computation of radiating coefficients
!-------------------------------------------------

call print_info(10,"* Initialization of view factors (radiating exchange)")
call init_viewfactor(defsolver, ustdom)

!-------------------------------------------------

endsubroutine init_boco_kdif

!------------------------------------------------------------------------------!
! Change history
!
! nov  2003 : creation de la routine
! june 2004 : conditions limites non uniformes de Neumann et convection
!------------------------------------------------------------------------------!


