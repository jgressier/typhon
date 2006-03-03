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
subroutine init_boco_ns(defsolver, ustdom)

use TYPHMAKE
use DEFFIELD
use MENU_NS
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
  ! Condition de Dirichlet 
  !-------------------------------------------------
  ! Cas d'existence d'un tableau de temperatures
  if(defsolver%boco(idef)%boco_ns%alloctemp) then
    allocate(defsolver%boco(idef)%boco_ns%temp(ustdom%boco(iboco)%nface))

    ! Cas d'existence d'un fichier de temperatures limites :
    if(defsolver%boco(idef)%boco_ns%tempfile .ne. cnull) then
      open(unit=1002, file = trim(defsolver%boco(idef)%boco_ns%tempfile), form="formatted")
      read(1002,*)  (defsolver%boco(idef)%boco_ns%temp(i),i = 1, ustdom%boco(iboco)%nface) 
      close(1002)
    endif

  endif

  !-------------------------------------------------
  ! Condition de Von Neumann
  !-------------------------------------------------
  ! Cas d'existence d'un tableau de flux
  if(defsolver%boco(idef)%boco_ns%allocflux) then
    allocate(defsolver%boco(idef)%boco_ns%flux_nunif(ustdom%boco(iboco)%nface))

    ! Cas d'existence d'un fichier de flux limites :
    if(defsolver%boco(idef)%boco_ns%fluxfile .ne. cnull) then
      open(unit=1002, file = trim(defsolver%boco(idef)%boco_ns%fluxfile), form="formatted")
      read(1002,*)  (defsolver%boco(idef)%boco_ns%flux_nunif(i),i = 1, ustdom%boco(iboco)%nface) 
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
    allocate(defsolver%boco(idef)%boco_ns%h_nunif(ustdom%boco(iboco)%nface))
    allocate(defsolver%boco(idef)%boco_ns%tconv_nunif(ustdom%boco(iboco)%nface))

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


