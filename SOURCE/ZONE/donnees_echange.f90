!------------------------------------------------------------------------------!
! Procedure : donnees_echange             Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Ecrire les donnees variables dans le temps dans une structure 
!   donnees_echange_inst
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine donnees_echange(solvercoupling, donnees_echange_inst1, zone1, &
                           nbc1, donnees_echange_inst2, zone2, nbc2, ncoupl2)

use TYPHMAKE
use OUTPUT
use GEO3D
use DEFZONE
use DEFFIELD
use VARCOM

implicit none

! -- Declaration des entrées --
integer                 :: solvercoupling
type(st_zone)           :: zone1, zone2
integer                 :: nbc1, nbc2 ! numéro (identité) de la CL
integer                 :: ncoupl2

! -- Declaration donnees_echange    
type(st_genericfield) :: donnees_echange_inst1, donnees_echange_inst2

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(solvercoupling)
  
  case(kdif_kdif)
  call ech_data_kdif(donnees_echange_inst1, zone1, nbc1, &
                     donnees_echange_inst2, zone2, nbc2, ncoupl2)
  
  case(kdif_ns)
  call ech_data_kdif(donnees_echange_inst1, zone1, nbc1, &
                     donnees_echange_inst2, zone2, nbc2, ncoupl2)

  case(ns_ns)
  call erreur("incohérence interne (donnees_echange)", "cas non implémenté")

  case default
  call erreur("incohérence interne (donnees_echange)", &
              "couplage de solvers inconnu")

endselect

endsubroutine donnees_echange
