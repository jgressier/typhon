!------------------------------------------------------------------------------!
! MODULE : GENFIELD                       Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : (cf historique)
!   Bibliotheque de procedures et fonctions pour la gestion des champs
!   des differents solveurs
!
! Defauts/Limitations/Divers :
! Historique :
!
!------------------------------------------------------------------------------!

module GENFIELD

use TYPHMAKE     ! Definition de la precision
!use OUTPUT
use GEO3D        ! 
use TENSOR3      ! 
use BASEFIELD

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_GENERICFIELD : Champ physique generique
!------------------------------------------------------------------------------!

type st_genericfield
  integer      :: nscal, nvect, ntens        ! dimension de champs
  integer      :: dim                        ! nombre de valeurs par champ
  type(st_genericfield),           pointer :: next      ! pointeur de liste chainee
  type(st_scafield), dimension(:), pointer :: tabscal   ! champs des scalaires
  type(st_vecfield), dimension(:), pointer :: tabvect   ! champs des vecteurs
  type(st_tenfield), dimension(:), pointer :: tabtens   ! champs des tenseurs
endtype st_genericfield


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_genericfield, new_genericfield_st
endinterface

interface delete
  module procedure delete_genericfield
endinterface

interface insert
  module procedure insert_newgfield
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure GENERICFIELD
!------------------------------------------------------------------------------!
subroutine new_genericfield(gfield, dim, n_scal, n_vect, n_tens)
implicit none 
type(st_genericfield) :: gfield                  ! champ a creer
integer               :: dim                     ! nombre de cellules des champs
integer               :: n_scal, n_vect, n_tens  ! nombre de scalaires, vecteurs et tenseurs
integer               :: i

!print*, "DEBUG NEW GENERIC FIELD"
  gfield%dim       = dim
  gfield%nscal     = n_scal
  gfield%nvect     = n_vect
  gfield%ntens     = n_tens
  
  if (gfield%nscal > 0) then
    allocate(gfield%tabscal(n_scal))          ! allocation du tableau de champs scalaires
    do i = 1, n_scal
      
      call new(gfield%tabscal(i), gfield%dim)  ! allocation champ par champ
    enddo
  endif
  
  if (gfield%nvect > 0) then
    allocate(gfield%tabvect(n_vect))          ! allocation du tableau de champs vecteurs
    do i = 1, n_vect
      call new(gfield%tabvect(i), gfield%dim)  ! allocation champ par champ
    enddo
  endif

  if (gfield%ntens > 0) then
    allocate(gfield%tabtens(n_tens))          ! allocation du tableau de champs tenseurs
    do i = 1, n_tens
      call new(gfield%tabtens(i), gfield%dim)  ! allocation champ par champ
    enddo
  endif

endsubroutine new_genericfield


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure FIELD a partir d'une autre structure
!------------------------------------------------------------------------------!
subroutine new_genericfield_st(newfield, oldfield)
implicit none
type(st_genericfield) :: newfield, oldfield     ! champ a creer, et champ d'origine

  call new(newfield, oldfield%dim, oldfield%nscal, oldfield%nvect, oldfield%ntens)

endsubroutine new_genericfield_st


!------------------------------------------------------------------------------!
! Procedure : initialisation d'une structure GENERICFIELD
!------------------------------------------------------------------------------!
subroutine init_genericfield(gfield, scal, vect)
implicit none
type(st_genericfield) :: gfield     ! champ a creer, et champ d'origine
real(krp)             :: scal       ! scalaire pour initialisation
type(v3d)             :: vect       ! vecteur  pour initialisation
integer               :: i

  do i = 1, gfield%nscal
    gfield%tabscal(i)%scal(:) = scal
  enddo

  do i = 1, gfield%nvect
    gfield%tabvect(i)%vect(:) = vect
  enddo

  do i = 1, gfield%ntens
    gfield%tabtens(i)%tens(:) = t3d(0._krp)
  enddo

endsubroutine init_genericfield


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure GENERICFIELD
!------------------------------------------------------------------------------!
subroutine delete_genericfield(gfield)
implicit none
type(st_genericfield) :: gfield
integer               :: i
!print*, "DEBUG DELETE GFIELD", gfield%nscal, gfield%nvect, gfield%ntens 
  if (gfield%nscal > 0) then
    do i = 1, gfield%nscal
      !!print*, "delete scalaire ",i
      call delete(gfield%tabscal(i))
    enddo
    deallocate(gfield%tabscal)
  endif
  !! print*, "fin delete scalaire "

  if (gfield%nvect > 0) then
    do i = 1, gfield%nvect
      call delete(gfield%tabvect(i))
    enddo
    deallocate(gfield%tabvect)
  endif

  if (gfield%ntens > 0) then
    do i = 1, gfield%ntens
      call delete(gfield%tabtens(i))
    enddo
    deallocate(gfield%tabtens)
  endif

endsubroutine delete_genericfield


!------------------------------------------------------------------------------!
! Procedure : creation et lien chaine d'une structure GENERICFIELD
!------------------------------------------------------------------------------!
function insert_newgfield(gfield,dim,nscal,nvect,ntens) result(pgfield)
implicit none
type(st_genericfield), pointer :: pgfield
type(st_genericfield), target  :: gfield
integer                        :: dim,nscal,nvect,ntens

  allocate(pgfield)
  call new(pgfield,dim,nscal,nvect,ntens)
  pgfield%next => gfield  

endfunction insert_newgfield


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une liste chainee de structure GENERICFIELD
!------------------------------------------------------------------------------!
subroutine delete_chainedgfield(gfield)
implicit none
type(st_genericfield), target  :: gfield
type(st_genericfield), pointer :: pgfield, dgfield

  pgfield => gfield
  do while(associated(pgfield))
    dgfield => pgfield
    pgfield => pgfield%next
    call delete(dgfield)
  enddo

endsubroutine delete_chainedgfield


endmodule GENFIELD

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2002 : creation du module
! juin 2003 : structuration des champs par type (scalaire, vecteur...)
! DEV: interface champ/tableau
! DEV: decoupage en MGFIELD et MZFIELD pour fonctions haut et bas niveau
! juin 2004 : procedures insert_newgfield et delete_chainedgfield
! nov  2004 : split DEFFIELD -> DEFFIELD / GENFIELD / BASEFIELD
!------------------------------------------------------------------------------!

