!------------------------------------------------------------------------------!
! PROGRAM : TYPHON                              Authors : J. Gressier (admin)
!                                               see http://typhon.sf.net
!                                               Created : July 2002
! 
! Plateforme de resolution de systemes d'equations 
! par discretisation Volumes Finis / Singularites
!   
! Historique des versions (cf fichier VERSIONS)
!
!------------------------------------------------------------------------------!
 
program main

use TYPHMAKE    ! default accuracy
use VARCOM      ! common variables and types
use OUTPUT      ! definition des procedures et unites pour les sorties
use MODWORLD    ! global data & structure for the computation
use MENU_GEN    ! general parameters for the project

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

call print_etape("> INPUTS : mesh and boundary conditions")
call lecture_maillage(loc_world)

!###### INITIALISATION

call print_etape("> INITIALISATION")
call init_world(loc_world)

!###### INTEGRATION ET RESOLUTION

select case(loc_world%prj%action)
case(act_compute)
  call print_etape("> INTEGRATION")
  call integration(loc_world)
case(act_analyse)
  call print_etape("> ANALYSIS & REPORT")
  call analyse(loc_world)
case default
  call erreur("Developement", "Unexpected ACTION parameter")
endselect

!###### FIN D'EXECUTION

call output_result(loc_world, end_calc) 

call delete(loc_world)

!###### Desallocation

call print_etape("> End of process")
call finalize_exch(loc_world%info)

!#########
endprogram

!------------------------------------------------------------------------------!
! Change history
!
! july  2002 : creation
! may   2005 : add switch option to "analyse" instead of "integration"
!------------------------------------------------------------------------------!
