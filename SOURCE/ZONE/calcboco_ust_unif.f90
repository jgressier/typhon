!------------------------------------------------------------------------------!
! Procedure : calcboco_ust_unif           Auteur : J. Gressier/E. Radenac
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites (uniformes) pour maillage non structuré
!   Aiguillage des appel selon le type de solveur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust_unif(defboco, ustboco, ustdom, champ, typsolver)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
!use MENU_ZONECOUPLING
use DEFZONE

implicit none

! -- Declaration des entrées --
type(mnu_boco)   :: defboco          ! paramètres de conditions aux limites
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: ustdom           ! maillage non structuré
integer          :: typsolver        ! type du solver

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des états

! -- Declaration des variables internes --


! -- Debut de la procedure --

select case(typsolver)
    case(solKDIF)
        call calcboco_kdif_ust_unif(defboco, ustboco, ustdom, champ)
    case default
       call erreur("incohérence interne (def_boco)","solveur inconnu")
endselect

endsubroutine calcboco_ust_unif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! novembre 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!
