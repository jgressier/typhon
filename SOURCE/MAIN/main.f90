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
use TIMER
use MODWORLD    ! global data & structure for the computation
use MENU_GEN    ! general parameters for the project

implicit none

! -- Variables locale --

type(st_world) :: loc_world      ! structure encapsulant toutes les donnees TYPHON
integer        :: itimer_init, itimer_tot

character(len=6), parameter :: version = "0.5.0"

include 'svnrev.h'

! -- BODY --

call init_exch_protocol(loc_world%info)
call init_output()

!###### ENTETE

call print_info(0,"")
call print_info(0,"******************************************************")
write(str_w,*)    "  TYPHON V ",version,' (',trim(svnrev),')'
call print_info(0,adjustl(str_w))
call print_info(0,"******************************************************")

itimer_tot  = realtime_start()
itimer_init = realtime_start()

call init_varcom()
if (omp_run) call print_info(0, "Open-MP computation "//trim(strof(nthread))//" threads")

!###### PARAMETERS PARSING 

call def_param(loc_world)

!###### MPI STRATEGY: distribute procs

if (mpi_run) then
  call print_etape("> MPI STRATEGY: preprocessing")
  call mpi_strategy_pre(loc_world)
endif

!###### MESH READING AND DATA STRUCTURE DEFINITION

call print_etape("> INPUTS : mesh and boundary conditions")
call readallmesh(loc_world)

!###### INITIALISATION

call print_etape("> INITIALIZATION")
call init_world(loc_world)

!###### MPI STRATEGY: distribute procs

if (mpi_run) then
  call print_etape("> MPI STRATEGY: postprocessing")
  call mpi_strategy_post(loc_world)
endif

write(str_w, "(a,e13.4)") "> user initialization time: ", realtime_stop(itimer_init)
call print_info(10, str_w)

!###### INTEGRATION ET RESOLUTION

select case(loc_world%prj%action)
case(act_compute)
  call print_etape("> INTEGRATION")
  call integration(loc_world)
case(act_analyse)
  call print_etape("> ANALYSIS & REPORT")
  call analyse(loc_world)
case default
  call erreur("Development", "Unexpected ACTION parameter")
endselect

!###### FIN D'EXECUTION

call output_result(loc_world, end_calc) 

call delete(loc_world)

!###### Desallocation

call print_etape("> End of process")
write(str_w, "(a,e13.4)") "> total user time: ", realtime_stop(itimer_tot)

call finalize_exch(loc_world%info)

!#########
endprogram

!------------------------------------------------------------------------------!
! Change history
!
! july  2002 : creation
! may   2005 : add switch option to "analyse" instead of "integration"
!------------------------------------------------------------------------------!
