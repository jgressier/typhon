!------------------------------------------------------------------------------!
! MODULE : GEO3D                          Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  : Juillet 2003 (cf historique)
!   Bibliotheque de procedures et fonctions pour le calcul géométrique 3D
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module GEO3D

use TYPHMAKE

! -- DECLARATIONS -----------------------------------------------------------

type v3d
  real(krp) :: x, y, z
endtype

! -- INTERFACES -------------------------------------------------------------

interface abs
  module procedure v3d_norme
endinterface

interface operator(+)
  module procedure v3d_addition
endinterface

interface operator(-)
  module procedure v3d_substraction, v3d_opp
endinterface

interface operator(*)
  module procedure v3d_multiplysp, v3d_multiplydp
endinterface

interface operator(/)
  module procedure v3d_divisionsp, v3d_divisiondp
endinterface

interface operator(.scal.)
  module procedure v3d_scalar_product
endinterface

interface operator(.vect.)
  module procedure v3d_vectorial_product
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Fonction : calcul de somme de vecteur
!------------------------------------------------------------------------------!
type(v3d) function v3d_addition(v1, v2)
implicit none
type(v3d), intent(in) :: v1, v2

  v3d_addition%x = v1%x + v2%x 
  v3d_addition%y = v1%y + v2%y 
  v3d_addition%z = v1%z + v2%z 

endfunction v3d_addition

!------------------------------------------------------------------------------!
! Fonction : calcul différence de vecteur
!------------------------------------------------------------------------------!
type(v3d) function v3d_substraction(v1, v2)
implicit none
type(v3d), intent(in) :: v1, v2

  v3d_substraction%x = v1%x - v2%x 
  v3d_substraction%y = v1%y - v2%y 
  v3d_substraction%z = v1%z - v2%z 

endfunction v3d_substraction

!------------------------------------------------------------------------------!
! Fonction : calcul de l'opposé d'un vecteur
!------------------------------------------------------------------------------!
type(v3d) function v3d_opp(v)
implicit none
type(v3d), intent(in) :: v

  v3d_opp%x = - v%x 
  v3d_opp%y = - v%y 
  v3d_opp%z = - v%z 

endfunction v3d_opp

!------------------------------------------------------------------------------!
! Fonction : calcul de multiplication de vecteur par réel
!------------------------------------------------------------------------------!
type(v3d) function v3d_multiplysp(x, v)
implicit none
real(4),   intent(in) :: x
type(v3d), intent(in) :: v

  v3d_multiplysp%x = x * v%x 
  v3d_multiplysp%y = x * v%y 
  v3d_multiplysp%z = x * v%z 

endfunction v3d_multiplysp

!------------------------------------------------------------------------------!
! Fonction : calcul de multiplication de vecteur par réel
!------------------------------------------------------------------------------!
type(v3d) function v3d_multiplydp(x, v)
implicit none
real(8),   intent(in) :: x
type(v3d), intent(in) :: v

  v3d_multiplydp%x = x * v%x 
  v3d_multiplydp%y = x * v%y 
  v3d_multiplydp%z = x * v%z 

endfunction v3d_multiplydp

!------------------------------------------------------------------------------!
! Fonction : calcul de division de vecteur par réel
!------------------------------------------------------------------------------!
type(v3d) function v3d_divisionsp(v,x)
implicit none
real(4),   intent(in) :: x
type(v3d), intent(in) :: v

  v3d_divisionsp%x = v%x / x 
  v3d_divisionsp%y = v%y / x
  v3d_divisionsp%z = v%z / x 

endfunction v3d_divisionsp

!------------------------------------------------------------------------------!
! Fonction : calcul de division de vecteur par réel
!------------------------------------------------------------------------------!
type(v3d) function v3d_divisiondp(v,x)
implicit none
real(8),   intent(in) :: x
type(v3d), intent(in) :: v

  v3d_divisiondp%x = v%x / x 
  v3d_divisiondp%y = v%y / x
  v3d_divisiondp%z = v%z / x 

endfunction v3d_divisiondp

!------------------------------------------------------------------------------!
! Fonction : norme de vecteur
!------------------------------------------------------------------------------!
real(krp) function v3d_norme(v)
implicit none
type(v3d), intent(in) :: v

  v3d_norme = sqrt(v%x*v%x + v%y*v%y + v%z*v%z)

endfunction v3d_norme

!------------------------------------------------------------------------------!
! Fonction : calcul de produit scalaire
!------------------------------------------------------------------------------!
real(krp) function v3d_scalar_product(v1, v2)
implicit none
type(v3d), intent(in) :: v1, v2

  v3d_scalar_product = v1%x*v2%x + v1%y*v2%y + v1%z*v2%z

endfunction v3d_scalar_product

!------------------------------------------------------------------------------!
! Fonction : produit vectoriel
!------------------------------------------------------------------------------!
type(v3d) function v3d_vectorial_product(v1, v2)
implicit none
type(v3d), intent(in) :: v1, v2

  v3d_vectorial_product%x = v1%y*v2%z - v1%z*v2%y
  v3d_vectorial_product%y = v1%z*v2%x - v1%x*v2%z
  v3d_vectorial_product%z = v1%x*v2%y - v1%y*v2%x

endfunction v3d_vectorial_product




endmodule GEO3D

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai  2002 (v0.0.1b): création du module
! Juil 2003          : compatibilité des opérateurs toute précision
!------------------------------------------------------------------------------!

