!------------------------------------------------------------------------------!
! MODULE : BASEFIELD                       Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : (cf historique)
!   Bibliotheque de procedures et fonctions pour la gestion des champs
!   des differents solveurs
!
! Defauts/Limitations/Divers :
! Historique :
!
!------------------------------------------------------------------------------!

module BASEFIELD

use TYPHMAKE     ! Definition de la precision
!use OUTPUT
use GEO3D        ! 
use TENSOR3      ! 

implicit none

! -- Variables globales du module -------------------------------------------

integer, parameter :: nghostcell = 1

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_SCAFIELD : Champ physique de scalaire
!------------------------------------------------------------------------------!

type st_scafield
  integer :: dim                            ! nombre de cellules
  real(krp), dimension(:), pointer :: scal  ! champ du scalaire
endtype

!------------------------------------------------------------------------------!
! Definition de la structure ST_VECFIELD : Champ physique de vecteurs
!------------------------------------------------------------------------------!

type st_vecfield
  integer :: dim                            ! nombre de cellules
  type(v3d), dimension(:), pointer :: vect  ! champ du vecteur
endtype

!------------------------------------------------------------------------------!
! Definition de la structure ST_TENFIELD : Champ physique de tenseurs
!------------------------------------------------------------------------------!

type st_tenfield
  integer :: dim                            ! nombre de cellules
  type(t3d), dimension(:), pointer :: tens  ! champ du vecteur
endtype

! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_scafield, new_vecfield, new_tenfield
endinterface

interface delete
  module procedure delete_scafield, delete_vecfield, delete_tenfield
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure SCAFIELD
!------------------------------------------------------------------------------!
subroutine new_scafield(scafield, dim)
implicit none
type(st_scafield) :: scafield          ! champ a creer
integer           :: dim               ! dimension

  scafield%dim = dim
  if (scafield%dim > 0) then
  allocate(scafield%scal(scafield%dim))
  endif

endsubroutine new_scafield


!------------------------------------------------------------------------------!
! Procedure : deallocation d'une structure SCAFIELD
!------------------------------------------------------------------------------!
subroutine delete_scafield(scafield)
implicit none
type(st_scafield) :: scafield     

  deallocate(scafield%scal)

endsubroutine delete_scafield


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure VECFIELD
!------------------------------------------------------------------------------!
subroutine new_vecfield(vecfield, dim)
implicit none
type(st_vecfield) :: vecfield          ! champ a creer
integer           :: dim               ! dimension

  vecfield%dim = dim
  if (vecfield%dim > 0) then
    allocate(vecfield%vect(dim))
  endif

endsubroutine new_vecfield


!------------------------------------------------------------------------------!
! Procedure : deallocation d'une structure VECFIELD
!------------------------------------------------------------------------------!
subroutine delete_vecfield(vecfield)
implicit none
type(st_vecfield) :: vecfield        

  deallocate(vecfield%vect)

endsubroutine delete_vecfield


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure TENFIELD
!------------------------------------------------------------------------------!
subroutine new_tenfield(tenfield, dim)
implicit none
type(st_tenfield) :: tenfield          ! champ a creer
integer           :: dim               ! dimension

  tenfield%dim = dim
  if (tenfield%dim > 0) then
    allocate(tenfield%tens(dim))
  endif

endsubroutine new_tenfield


!------------------------------------------------------------------------------!
! Procedure : deallocation d'une structure TENFIELD
!------------------------------------------------------------------------------!
subroutine delete_tenfield(tenfield)
implicit none
type(st_tenfield) :: tenfield        

  deallocate(tenfield%tens)

endsubroutine delete_tenfield


endmodule BASEFIELD

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2002 : creation du module
! juin 2003 : structuration des champs par type (scalaire, vecteur...)
! DEV: interface champ/tableau
! DEV: decoupage en MGFIELD et MZFIELD pour fonctions haut et bas niveau
! juin 2004 : procedures insert_newgfield et delete_chainedgfield
! nov  2004 : split GENFIELD -> GENFIELD / BASEFIELD
!------------------------------------------------------------------------------!

