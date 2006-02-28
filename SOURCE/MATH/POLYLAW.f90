!------------------------------------------------------------------------------!
! MODULE : MOD_LOIPOLY
!
!------------------------------------------------------------------------------!
module MOD_LOIPOLY

implicit none

!------------------------------------------------------------------------------!
! ST_LOIPOLY : structure de definition  d'une loi polynomiale
!------------------------------------------------------------------------------!
type st_loipoly
  integer       :: ordre     ! ordre du polynome
  real, pointer :: coef(:)   ! val. des coef (0:ordre) (ordre croissant exposant)
endtype st_loipoly


! -- INTERFACES -------------------------------------------------------------

interface new       ! constructeur
  module procedure new_loipoly
endinterface

interface delete    ! destructeur
  module procedure delete_loipoly
endinterface

interface eval      ! nom generique pour l'evaluation des lois (ici polynomiale)
  module procedure eval_loipoly, eval_loipoly_t
endinterface

interface eval_integrale      ! nom generique pour l'evaluation des lois (ici polynomiale)
  module procedure eval_integrale_loipoly, eval_integrale_loipoly_t
endinterface

! -- Procedures, Fonctions et Operateurs ------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure LOIPOLY
!------------------------------------------------------------------------------!
subroutine new_loipoly(loi, n)
implicit none
type(st_loipoly)  :: loi 
integer           :: n

  loi%ordre = n
  allocate(loi%coef(0:n))
 
endsubroutine new_loipoly


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure LOIPOLY par une constante
!------------------------------------------------------------------------------!
subroutine new_loipoly_cst(loi, a)
implicit none
type(st_loipoly)  :: loi 
real              :: a

  loi%ordre   = 0
  allocate(loi%coef(0:0))
  loi%coef(0) = a
 
endsubroutine new_loipoly_cst


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure LOIPOLY
!------------------------------------------------------------------------------!
subroutine delete_loipoly(loi)
implicit none
type(st_loipoly)  :: loi     

  deallocate(loi%coef)
 
endsubroutine delete_loipoly


!------------------------------------------------------------------------------!
! Fonction : Calcul de la valeur de la loi au point x de la variable
!------------------------------------------------------------------------------!
function eval_loipoly(loi, x) result (sca)

implicit none
! -- Entrees --
type(st_loipoly) :: loi
real             :: x
! -- Sortie --
real             :: sca
! -- variables internes --
integer :: i

! Debut de procedure

  sca = loi%coef(loi%ordre)

  if (loi%ordre >= 1) then                              ! si polynome au moins degre 1
    do i = loi%ordre-1, 0, -1                           !   calcul par methode Horner
      sca = (sca * x) + loi%coef(i)
    enddo
  endif

endfunction eval_loipoly


!------------------------------------------------------------------------------!
! Fonction : Calcul de la loi au point x (vecteur)
!------------------------------------------------------------------------------!
function eval_loipoly_t(loi, x) result (vect)

implicit none
! -- Entrees --
type(st_loipoly) :: loi
real             :: x(:)
! -- Sortie --
real             :: vect(size(x))
! -- variables internes --
integer :: i, n

! Debut de procedure

  n = size(x)

  vect(1:n) = loi%coef(loi%ordre)

  if (loi%ordre >= 1) then                              ! si polynome au moins degre 1
    do i = loi%ordre-1, 0, -1                           !   calcul par methode Horner
      vect(1:n) = (vect(1:n) * x(1:n)) + loi%coef(i)
    enddo
  endif

endfunction eval_loipoly_t


!------------------------------------------------------------------------------!
! Fonction : Calcul de la valeur de la primitive de la loi au point x de la variable
!            ( primitive par défaut à F(0) = 0)
!------------------------------------------------------------------------------!
function eval_integrale_loipoly(loi, x) result (sca)

implicit none
! -- Entrees --
type(st_loipoly) :: loi
real             :: x
! -- Sortie --
real             :: sca
! -- variables internes --
integer :: i

! Debut de procedure

  sca = loi%coef(loi%ordre) / (loi%ordre+1.)

  if (loi%ordre >= 1) then                              ! si polynome au moins degre 1
    do i = loi%ordre-1, 0, -1                           !   calcul par methode Horner
      sca = (sca * x) + loi%coef(i)/(i+1.)              !   x^n -> x^(n+1)/(n+1)
    enddo
  endif
  sca = sca * x

endfunction eval_integrale_loipoly


!------------------------------------------------------------------------------!
! Fonction : Calcul de la primitive de la loi au point x (vecteur)
!            ( primitive par défaut à F(0) = 0)
!------------------------------------------------------------------------------!
function eval_integrale_loipoly_t(loi, x) result (vect)

implicit none
! -- Entrees --
type(st_loipoly) :: loi
real             :: x(:)
! -- Sortie --
real             :: vect(size(x))
! -- variables internes --
integer :: i, n

! Debut de procedure

  n = size(x)

  vect(1:n) = loi%coef(loi%ordre) / (loi%ordre+1.)

  if (loi%ordre >= 1) then                              ! si polynome au moins degre 1
    do i = loi%ordre-1, 0, -1                           !   calcul par methode Horner
      vect(1:n) = (vect(1:n) * x(1:n)) + loi%coef(i)/(i+1.)
    enddo
  endif
  vect(1:n) = (vect(1:n) * x(1:n))

endfunction eval_integrale_loipoly_t


!------------------------------------------------------------------------------!
endmodule MOD_LOIPOLY


