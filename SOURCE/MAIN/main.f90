!------------------------------------------------------------------------------!
! PROGRAM : TYPHON
!
! Plateforme de resolution de systemes d'equations 
! par discretisation Volumes Finis
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

#ifdef MPICOMPIL
include 'mpif.h'
#endif /*MPICOMPIL*/

! -- Variables locales --

type(st_world) :: world      ! structure encapsulant toutes les donnees TYPHON
integer        :: itimer_init, itimer_tot

integer        :: ierr

character(len=6), parameter :: version = "0.5.0"

include 'svnrev.h'

! -- BODY --

#ifdef MPICOMPIL

  mpi_run  = .true.
  call print_info(5,"init MPI exchanges")

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myprocid,     ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, world%info%nbproc, ierr)
  myprocid   = myprocid+1    ! mpi_comm_rank starts from 0
  tympi_real = MPI_REAL8
  tympi_int  = MPI_INTEGER4  

#else  /*NO MPICOMPIL*/
  mpi_run           = .false.
  world%info%nbproc = 1
  myprocid          = 0
#endif /*MPICOMPIL*/

call init_output()

!###### ENTETE

call print_info(0,"")
call print_info(0,"******************************************************")
call print_info(0,"TYPHON V "//trim(version)//" ("//trim(svnrev)//")")
call print_info(0,"******************************************************")

itimer_tot  = realtime_start()
itimer_init = realtime_start()

call init_varcom()
if (omp_run) call print_info(0, "Open-MP computation "//trim(strof(nthread))//" threads")

!###### PARAMETERS PARSING 

call def_param(world)

!###### MPI STRATEGY: distribute procs

!if (mpi_run) then ! to be run even for omp and seq to initialize nbprocs
  call print_etape("> MPI STRATEGY: preprocessing")
  call mpi_strategy_pre(world)
!endif

!###### MESH READING AND DATA STRUCTURE DEFINITION

call print_etape("> INPUTS : mesh and boundary conditions")
call readallmesh(world)

!###### INITIALISATION

call print_etape("> INITIALIZATION")
call init_world(world)

!###### MPI STRATEGY: distribute procs

if (mpi_run) then
  call print_etape("> MPI STRATEGY: postprocessing")
  call mpi_strategy_post(world)
endif

write(str_w, "(a,e13.4)") "> user initialization time: ", realtime_stop(itimer_init)
call print_info(10, str_w)

!###### INTEGRATION ET RESOLUTION

select case(world%prj%action)
case(act_compute, act_restart)
  call print_etape("> INTEGRATION")
  call integration(world)
case(act_analyze)
  call print_etape("> ANALYSIS & REPORT")
  call analyse(world)
case default
  call error_stop("Development: Unexpected ACTION parameter: "//trim(strof(world%prj%action)))
endselect

!###### FIN D'EXECUTION

call output_result(world, end_calc) 

call delete(world)

!###### Desallocation

call print_etape("> End of process")
write(str_w, "(a,e13.4)") "> total user time: ", realtime_stop(itimer_tot)

#ifdef MPICOMPIL
! call finalize_exch(world%info)
call print_info(5,"finalize MPI exchanges")
call MPI_Finalize(ierr)
#endif /*MPICOMPIL*/

!#########
endprogram

!------------------------------------------------------------------------------!
! Change history
!
! july  2002 : creation
! may   2005 : add switch option to "analyse" instead of "integration"
!------------------------------------------------------------------------------!
