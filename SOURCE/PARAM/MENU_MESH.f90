!------------------------------------------------------------------------------!
! MODULE : MENU_MESH                      Authors : J. Gressier
!                                         Created : November 2002
! Fonction
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la lecture de maillage
!
!------------------------------------------------------------------------------!

module MENU_MESH

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------

integer(kpp), parameter :: split_none     = 0
integer(kpp), parameter :: split_svm2quad = 21
integer(kpp), parameter :: split_svm2tri  = 22

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_SVMmesh : Options for  Spectral Volume Method
!------------------------------------------------------------------------------!
type mnu_svmmesh
  integer(kpp)   :: cv_split        ! number of Control Volume (CV subcell) in a SV
  integer(kpp)   :: svface_split    ! number of subface (CV face) by original face
  integer(kpp)   :: intnode         ! number of internal added nodes for cell splitting
  integer(kpp)   :: internal_faces  ! number of internal faces (by cell)
  integer(kpp)   :: nb_facepoints   ! number of integration points by face
endtype mnu_svmmesh


!------------------------------------------------------------------------------!
! structure MNU_MESH : parametres pour la distribution entre processeurs
!------------------------------------------------------------------------------!
type mnu_mesh
  character             :: format      ! cf VARCOM
  character(len=strlen) :: fichier     ! nom de fichier
  real(krp)             :: scale       ! scale factor
  integer(kpp)          :: splitmesh   ! split method
  type(mnu_svmmesh)     :: svm         ! svm specific parameters
endtype mnu_mesh



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


subroutine init_svmmesh(method, svmmesh)
implicit none
! -- parameters --
integer(kpp)      :: method
type(mnu_svmmesh) :: svmmesh
! -- body --

select case(method)
case(split_svm2quad)
  svmmesh%cv_split       = 3  ! nb of CV in SV
  svmmesh%intnode        = 1  ! nb of internal added nodes for cell splitting
  svmmesh%svface_split   = 2  ! nb of CV face per SV face
  svmmesh%internal_faces = 3  ! number of internal faces (by cell)
  svmmesh%nb_facepoints  = 1  ! number of integration points by face
case default
endselect

endsubroutine init_svmmesh


endmodule MENU_MESH
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : created
! sept 2005 : add scale factor
!------------------------------------------------------------------------------!



