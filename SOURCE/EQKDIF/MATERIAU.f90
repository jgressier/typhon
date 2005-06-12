!------------------------------------------------------------------------------!
! MODULE : MATERIAU                                 Authors : J. Gressier
!                                                   Created : May 2002
! Fonction                                          Modif   : (see history)
!   Heat Transfer solver, Physical Model Definitions
!
!------------------------------------------------------------------------------!
module MATERIAU

use TYPHMAKE     ! variable accuracy
use MATER_LOI    ! definition generale d'une loi de variation des parametres

implicit none

! -- Constants  -------------------------------------------

real(krp), parameter :: stefan_cst = 5.6703E-8_krp

! -- Parameters -------------------------------------------

character, parameter :: mat_LIN  = 'L'   ! materiau a proprietes constantes
character, parameter :: mat_KNL  = 'K'   ! materiau a cp constant et conductivite non lineaire
character, parameter :: mat_XMAT = 'X'   ! materiau a proprietes specifiques

!------------------------------------------------------------------------------!
!    DECLARATIONS
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! ST_MATERIAU : structure pour la definition des equations
!------------------------------------------------------------------------------!
type st_materiau
  character(len=30) :: nom         ! material name
  character         :: type        ! cf constants
  real(krp)         :: Cp          ! Calorific capacity
  type(st_loi)      :: Energie     ! Energy (fct of temperature)
  type(st_loi)      :: Kd          ! thermal conductivity
  logical           :: isotropic   ! if isotropic or not
endtype st_materiau


! -- INTERFACES -------------------------------------------------------------


! -- Procedures, Fonctions et Operateurs ------------------------------------

!------------------------------------------------------------------------------!
!    IMPLEMENTATION 
!------------------------------------------------------------------------------!
!contains


endmodule MATERIAU

!------------------------------------------------------------------------------!
! Change history
!
! may  2002 : creation
! apr  2003 : specification of material type (LIN, KNL, XMAT)
! june 2005 : non isotropic material 
!------------------------------------------------------------------------------!
