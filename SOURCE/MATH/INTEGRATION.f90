!------------------------------------------------------------------------------!
! MODULE : INTEGRATION                    Auteur : J. Gressier
!                                         Date   : Avril 2002
! Fonction                                Modif  : 
!   Bibliotheque de procedures pour l'integration de fonction
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module INTEGRATION

use MATH

implicit none

! -- Variables globales du module -------------------------------------------


!------------------------------------------------------------------------------!
!    DECLARATIONS
!------------------------------------------------------------------------------!


! -- INTERFACES -------------------------------------------------------------

interface integ_trap
  module procedure integ_trapsp, integ_trapdp
endinterface

interface integ_trap_cyl
  module procedure integ_trap_cylsp, integ_trap_cyldp
endinterface

! -- Procedures, Fonctions et Operateurs ------------------------------------
!

!------------------------------------------------------------------------------!
!    IMPLEMENTATION 
!------------------------------------------------------------------------------!
contains

!------------------------------------------------------------------------------!
! Procedure : integ_err                   Auteur : J. Gressier
!                                         Date   : Avril 2002
! Fonction                                Modif  :
!   Gestion des erreurs de la librairie INTEGRATION
!
!------------------------------------------------------------------------------!
subroutine integ_err(message)
  implicit none 
! -- Declaration des Parametres --
  character(len=*) :: message
! -- Debut de la procedure --
  print*,'** librairie INTEGRATION - erreur : ' // message // ' **'
  stop
endsubroutine integ_err
!------------------------------------------------------------------------------!
  

!------------------------------------------------------------------------------!
! Fonction : integ_trapsp                  Auteur : J. Gressier
!                                         Date   : Avril 2002
! Fonction                                Modif  :
!   Integration par la methode des trapezes a partir de
!   deux couples de valeurs (x,y) 
!
! Defauts/Limitations/Divers :
!   L'integration est d'ordre 2
!
!------------------------------------------------------------------------------!
function integ_trapsp(cp1, cp2)
implicit none 
! -- Declaration des entrees --
real, dimension(2) :: cp1, cp2    ! donnees pour la definition de la droite
! -- Declaration des sorties --
real               :: integ_trapsp
! -- Debut de la procedure --

  integ_trapsp = .5*(cp1(2) + cp2(2)) * (cp2(1)-cp1(1))
  
endfunction integ_trapsp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Fonction : integ_trapdp                  Auteur : J. Gressier
!                                         Date   : Avril 2002
! Fonction                                Modif  :
!   Integration par la methode des trapezes a partir de
!   deux couples de valeurs (x,y) 
!
! Defauts/Limitations/Divers :
!   L'integration est d'ordre 2
!
!------------------------------------------------------------------------------!
function integ_trapdp(cp1, cp2)
implicit none 
! -- Declaration des entrees --
double precision, dimension(2) :: cp1, cp2    ! donnees pour la definition de la droite
! -- Declaration des sorties --
double precision               :: integ_trapdp
! -- Debut de la procedure --

  integ_trapdp = .5*(cp1(2) + cp2(2)) * (cp2(1)-cp1(1))
  
endfunction integ_trapdp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Fonction : integ_trap_cylsp                  Auteur : J. Gressier
!                                         Date   : Avril 2002
! Fonction                                Modif  :
!   Integration par la methode des trapezes a partir de
!   deux couples de valeurs (r,y) dans un repere cylindrique
!
! Defauts/Limitations/Divers :
!   L'integration est d'ordre 2
!
!------------------------------------------------------------------------------!
function integ_trap_cylsp(cp1, cp2)
implicit none 
! -- Declaration des entrees --
real, dimension(2) :: cp1, cp2    ! donnees pour la definition de la droite
! -- Declaration des sorties --
real               :: integ_trap_cylsp
! -- Debut de la procedure --

  integ_trap_cylsp = .5*(cp1(2) + cp2(2)) * pi * (cp2(1)**2-cp1(1)**2)
  
endfunction integ_trap_cylsp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Fonction : integ_trap_cyldp             Auteur : J. Gressier
!                                         Date   : Avril 2002
! Fonction                                Modif  :
!   Integration par la methode des trapezes a partir de
!   deux couples de valeurs (r,y) dans un repere cylindrique
!
! Defauts/Limitations/Divers :
!   L'integration est d'ordre 2
!
!------------------------------------------------------------------------------!
function integ_trap_cyldp(cp1, cp2)
implicit none 
! -- Declaration des entrees --
double precision, dimension(2) :: cp1, cp2    ! donnees pour la definition de la droite
! -- Declaration des sorties --
double precision               :: integ_trap_cyldp
! -- Debut de la procedure --

  integ_trap_cyldp = .5*(cp1(2) + cp2(2)) * pi * (cp2(1)**2-cp1(1)**2)
  
endfunction integ_trap_cyldp
!------------------------------------------------------------------------------!




endmodule INTEGRATION

