!------------------------------------------------------------------------------!
! MODULE : GENFIELD                                  Authors : J. Gressier
!                                                    Created : Octobre 2002
! Fonction  
!   Library to manage scalar, vector, tensor fields
!   des differents solveurs
!
! Defauts/Limitations/Divers :
! Historique :
!
!------------------------------------------------------------------------------!

module GENFIELD

use TYPHMAKE     ! Definition de la precision
use OUTPUT
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

interface size_elem
  module procedure size_elem_gfield
endinterface

interface size_tot
  module procedure size_tot_gfield
endinterface

interface packst
  module procedure pack_gfield
endinterface

interface unpackst
  module procedure unpack_gfield
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

  gfield%dim       = dim
  gfield%nscal     = n_scal
  gfield%nvect     = n_vect
  gfield%ntens     = n_tens

  nullify(gfield%next)
  
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

  if (gfield%nscal > 0) then
    do i = 1, gfield%nscal
      call delete(gfield%tabscal(i))
    enddo
    deallocate(gfield%tabscal)
  endif

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


!------------------------------------------------------------------------------!
! Procedure : convert generic field into real array
!------------------------------------------------------------------------------!
subroutine pack_gfield(gfield, tab, n)
implicit none
type(st_genericfield) :: gfield
real(krp)             :: tab(n)
integer(kip)          :: n
integer(kip) :: iv, i, sze, dim, tot, ideb

  sze = size_elem(gfield)
  dim = gfield%dim
  tot = sze*dim

  if (tot /= size(tab)) &
    call erreur("Development", "non consistent sizes of generic field of real array")

  do iv = 1, gfield%nscal
    tab(iv:tot:sze) = gfield%tabscal(iv)%scal(1:dim)
  enddo
  do iv = 1, gfield%nvect
    ideb = gfield%nscal+3*(iv-1)+1
    do i = 1, dim
      tab(ideb  +(i-1)*sze) = gfield%tabvect(iv)%vect(i)%x
      tab(ideb+1+(i-1)*sze) = gfield%tabvect(iv)%vect(i)%y
      tab(ideb+2+(i-1)*sze) = gfield%tabvect(iv)%vect(i)%z
    enddo
  enddo

  if (gfield%ntens /= 0) &
    call erreur("Development", "not yet supposed to pack generic fields with tensor fields")

endsubroutine pack_gfield


!------------------------------------------------------------------------------!
! Procedure : convert generic field into real array
!------------------------------------------------------------------------------!
subroutine unpack_gfield(tab, gfield, n)
implicit none
type(st_genericfield) :: gfield
real(krp)             :: tab(n)
integer(kip)          :: n
integer(kip) :: iv, i, sze, dim, tot, ideb

  sze = size_elem(gfield)
  dim = gfield%dim
  tot = sze*dim

  if (tot /= size(tab)) &
    call erreur("Development", "non consistent sizes of generic field of real array")

  do iv = 1, gfield%nscal
    gfield%tabscal(iv)%scal(1:dim) = tab(iv:tot:sze) 
  enddo
  do iv = 1, gfield%nvect
    ideb = gfield%nscal+3*(iv-1)+1
    do i = 1, dim
      gfield%tabvect(iv)%vect(i)%x = tab(ideb  +(i-1)*sze)
      gfield%tabvect(iv)%vect(i)%y = tab(ideb+1+(i-1)*sze)
      gfield%tabvect(iv)%vect(i)%z = tab(ideb+2+(i-1)*sze) 
    enddo
  enddo

  if (gfield%ntens /= 0) &
    call erreur("Development", "not yet supposed to unpack generic fields with tensor fields")

endsubroutine unpack_gfield


!------------------------------------------------------------------------------!
! Function : compute size of an element of generic field
!------------------------------------------------------------------------------!
integer(kip) function size_elem_gfield(gfield)
implicit none
type(st_genericfield) :: gfield

  size_elem_gfield = gfield%nscal + 3*gfield%nvect + 9*gfield%ntens

end function size_elem_gfield

!------------------------------------------------------------------------------!
! Function : compute size of the full generic field
!------------------------------------------------------------------------------!
integer(kip) function size_tot_gfield(gfield)
implicit none
type(st_genericfield) :: gfield

  size_tot_gfield = (gfield%nscal + 3*gfield%nvect + 9*gfield%ntens)*gfield%dim

end function size_tot_gfield


endmodule GENFIELD

!------------------------------------------------------------------------------!
! Changes history
!
! oct  2002 : creation du module
! juin 2003 : structuration des champs par type (scalaire, vecteur...)
! juin 2004 : procedures insert_newgfield et delete_chainedgfield
! nov  2004 : split DEFFIELD -> DEFFIELD / GENFIELD / BASEFIELD
! aug  2005 : size of field, 
!------------------------------------------------------------------------------!

