!------------------------------------------------------------------------------!
! PROGRAM : TYPHON                        Auteur : J. Gressier
!                                         Date   : Juillet 2002
! 
! Plateforme de resolution de systemes d'equations 
! par discretisation Volumes Finis / Singularites
!   
! Historique des versions (cf fichier VERSIONS)
!
!------------------------------------------------------------------------------!
 
program main

use TYPHMAKE    ! definition de la precision par defaut
use VARCOM      ! definition des types internes et variables globales
use OUTPUT      ! definition des procedures et unites pour les sorties
use MODWORLD    ! Definition des structures pour le maillage

implicit none

! -- Variables locale --
type(st_world) :: loc_world      ! structure encapsulant toutes les donnees TYPHON

! -- Debut de la procedure --

call init_exch_protocol(loc_world%info)
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

!###### LECTURE MAILLAGE et Generation des structures de donnees

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
call finalize_exch(loc_world%info)

!#########
endprogram
