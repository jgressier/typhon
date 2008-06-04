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

interface scale
  module procedure scaf_scale, vecf_scale, tenf_scale
endinterface

interface xeqxpy
  module procedure scaf_xeqxpy, vecf_xeqxpy, tenf_xeqxpy
endinterface

interface xeqxpay
  module procedure scaf_xeqxpay, vecf_xeqxpay, tenf_xeqxpay
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

!------------------------------------------------------------------------------
!  Procédure : transfert de champ scalaire : rscafield reçoit iscafield
!------------------------------------------------------------------------------
subroutine transfer_scafield(rscafield,iscafield)
implicit none
type(st_scafield) :: iscafield, rscafield
integer           :: i

rscafield%dim= iscafield%dim
do i = 1, iscafield%dim
  rscafield%scal(i) = iscafield%scal(i)
enddo

endsubroutine transfer_scafield


!------------------------------------------------------------------------------
! Procédure : transfert de champ vectoriel : rvecfield reçoit ivecfield
!------------------------------------------------------------------------------
subroutine transfer_vecfield(rvecfield,ivecfield)
implicit none
type(st_vecfield) :: ivecfield, rvecfield
integer           :: i

rvecfield%dim= ivecfield%dim
do i = 1, ivecfield%dim
  rvecfield%vect(i)= ivecfield%vect(i)
enddo

endsubroutine transfer_vecfield

!------------------------------------------------------------------------------
! Procédure : transfert de champ tensoriel : rtenfield reçoit itenfield
!------------------------------------------------------------------------------
subroutine transfer_tenfield(rtenfield,itenfield)
implicit none
type(st_tenfield) :: itenfield, rtenfield
integer           :: i

rtenfield%dim= itenfield%dim
do i = 1, itenfield%dim
  rtenfield%tens(i)= itenfield%tens(i)
enddo

endsubroutine transfer_tenfield

!------------------------------------------------------------------------------
! Computing routine : scaf_scale : X = a*X
!------------------------------------------------------------------------------
subroutine scaf_scale(x, a)
implicit none
type(st_scafield) :: x
real(krp)         :: a
integer           :: i

do i = 1, x%dim
  x%scal(i) = a*x%scal(i) 
enddo

endsubroutine scaf_scale

!------------------------------------------------------------------------------
! Computing routine : vecf_scale : X = a*X
!------------------------------------------------------------------------------
subroutine vecf_scale(x, a)
implicit none
type(st_vecfield) :: x
real(krp)         :: a

  call scale(x%vect(1:x%dim), a)

endsubroutine vecf_scale

!------------------------------------------------------------------------------
! Computing routine : vecf_scale : X = a*X
!------------------------------------------------------------------------------
subroutine tenf_scale(x, a)
implicit none
type(st_tenfield) :: x
integer           :: i 
real(krp)         :: a

  do i = 1, x%dim
    x%tens(i)%mat = a*x%tens(i)%mat 
  enddo

endsubroutine tenf_scale

!------------------------------------------------------------------------------
! Computing routine : scaf_xeqxpy : X = X + Y
!------------------------------------------------------------------------------
subroutine scaf_xeqxpy(x, y)
implicit none
type(st_scafield) :: x, y
integer           :: i

do i = 1, x%dim
  x%scal(i) = x%scal(i) + y%scal(i) 
enddo

endsubroutine scaf_xeqxpy

!------------------------------------------------------------------------------
! Computing routine : vecf_xeqxpy : X = X + Y
!------------------------------------------------------------------------------
subroutine vecf_xeqxpy(x, y)
implicit none
type(st_vecfield) :: x, y

  call shift_add(x%vect(1:x%dim), y%vect(1:x%dim))

endsubroutine vecf_xeqxpy

!------------------------------------------------------------------------------
! Computing routine : vecf_xeqxpy : X = X + Y
!------------------------------------------------------------------------------
subroutine tenf_xeqxpy(x, y)
implicit none
type(st_tenfield) :: x, y
integer           :: i 

  do i = 1, x%dim
    x%tens(i)%mat = x%tens(i)%mat + y%tens(i)%mat 
  enddo

endsubroutine tenf_xeqxpy

!------------------------------------------------------------------------------
! Computing routine : scaf_xeqxpay : X = X + a*Y
!------------------------------------------------------------------------------
subroutine scaf_xeqxpay(x, a, y)
implicit none
type(st_scafield) :: x, y
real(krp)         :: a
integer           :: i

do i = 1, x%dim
  x%scal(i) = x%scal(i) + a*y%scal(i) 
enddo

endsubroutine scaf_xeqxpay

!------------------------------------------------------------------------------
! Computing routine : vecf_xeqxpay : X = X + a*Y
!------------------------------------------------------------------------------
subroutine vecf_xeqxpay(x, a, y)
implicit none
type(st_vecfield) :: x, y
real(krp)         :: a
integer           :: i

do i = 1, x%dim
  x%vect(i) = x%vect(i) + a*y%vect(i)
enddo
!call shift_add(x%vect(1:x%dim), a*y%vect(1:x%dim))

endsubroutine vecf_xeqxpay

!------------------------------------------------------------------------------
! Computing routine : vecf_xeqxpay : X = X + a*Y
!------------------------------------------------------------------------------
subroutine tenf_xeqxpay(x, a, y)
implicit none
type(st_tenfield) :: x, y
integer           :: i 
real(krp)         :: a

  do i = 1, x%dim
    x%tens(i)%mat = x%tens(i)%mat + a*y%tens(i)%mat 
  enddo

endsubroutine tenf_xeqxpay




endmodule BASEFIELD

!------------------------------------------------------------------------------!
! Changes history
!
! oct  2002 : creation du module
! juin 2003 : structuration des champs par type (scalaire, vecteur...)
! DEV: interface champ/tableau
! DEV: decoupage en MGFIELD et MZFIELD pour fonctions haut et bas niveau
! juin 2004 : procedures insert_newgfield et delete_chainedgfield
! nov  2004 : split GENFIELD -> GENFIELD / BASEFIELD
! Nov  2007 : X = X + Y ; X = X + A*Y routines
!------------------------------------------------------------------------------!

