!------------------------------------------------------------------------------!
! Procedure : init_boco_kdif              Auteur : J. Gressier/E. Radenac
!                                         Date   : Nov 2003
! Fonction                                Modif  : 
!   Traitement des paramètres du fichier menu principal
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

! -- Declaration des entrées --
type(st_ustmesh)  :: ustdom

! -- Declaration des entrées/sorties --
type(mnu_solver)  :: defsolver

! -- Declaration des variables internes --
integer :: iboco,i

! -- Debut de la procedure --

! On parcourt toutes les conditions limites du domaine
do iboco = 1, ustdom%nboco 

  ! Cas d'existence d'un tableau de températures
  if(defsolver%boco(ustdom%boco(iboco)%idefboco)%boco_kdif%alloctemp == .true.) then
    allocate(defsolver%boco(ustdom%boco(iboco)%idefboco)%boco_kdif%temp(ustdom%boco(iboco)%nface))

    ! Cas d'existence d'un fichier de températures limites :
    if(defsolver%boco(ustdom%boco(iboco)%idefboco)%boco_kdif%tempfile .ne. cnull) then
      open(unit=1002, file = defsolver%boco(ustdom%boco(iboco)%idefboco)%boco_kdif%tempfile, form="formatted")
      read(1002,*),  (defsolver%boco(ustdom%boco(iboco)%idefboco)%boco_kdif%temp(i),i = 1, ustdom%boco(iboco)%nface) 
      close(1002)
    endif

  endif

  if(defsolver%boco(ustdom%boco(iboco)%idefboco)%boco_kdif%allocflux == .true.) then
    ! Cas d'existence d'un tableau de flux
    allocate(defsolver%boco(ustdom%boco(iboco)%idefboco)%boco_kdif%flux_nunif(ustdom%boco(iboco)%nface))
  endif

enddo

endsubroutine init_boco_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov 2003 (v0.1.2) : création de la routine
!------------------------------------------------------------------------------!


