!------------------------------------------------------------------------------!
! PROGRAM : TYPHON                        Auteur : J. Gressier
!                                         Date   : Juillet 2002
! 
! Plateforme de résolution de systèmes d'équations 
! par discrétisation Volumes Finis / Singularités
!   
! Historique des versions (cf fichier VERSIONS)
!
!------------------------------------------------------------------------------!
 
program main

use TYPHMAKE     ! définition de la précision par défaut
use VARCOM      ! définition des types internes et variables globales
use OUTPUT      ! définition des procédures et unités pour les sorties
use MODWORLD        ! Définition des structures pour le maillage

implicit none

! -- Variables locale --
type(st_world) :: loc_world      ! structure encapsulant toutes les données TYPHON

! -- Debut de la procedure --

call init_output()

!###### ENTETE

call print_info(0,"")
call print_info(0,"******************************************************")
write(str_w,*)    "* TYPHON V ",version,"                                    *"
call print_info(0,adjustl(str_w))
call print_info(0,"******************************************************")

call init_varcom()

!###### LECTURE MENU ET TRAITEMENT DES PARAMETRES

call def_param(loc_world)

!###### LECTURE MAILLAGE et Génération des structures de données

call print_etape("> LECTURE : maillages et condition aux limites")
call lecture_maillage(loc_world)

!###### INITIALISATION

call print_etape("> INITIALISATION")
call init_world(loc_world)

!###### INTEGRATION ET RESOLUTION

call print_etape("> INTEGRATION")
call integration(loc_world)

!###### FIN D'EXECUTION

call output_result(loc_world,end_calc) !DEV2602 call output_result(loc_world)

call delete(loc_world)

!###### Desallocation

call print_etape("> Fin du calcul")

!#########
endprogram
