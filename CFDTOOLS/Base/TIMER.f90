!----------------------------------------------------------------------------------------
! MODULE : TIMER
! 
! Function
!   Routines to compute cpu and real/user elapsed time
!
!   to use (cpu or real time):
!
!   itimer = cputime_start()          ! starts clock and get timer index
!     ... WORK ...
!   cputime = cputime_stop(itimer)    ! stops clock and return time
!----------------------------------------------------------------------------------------
module TIMER

use IOCFD

! -- PUBLIC Variables -------------------------------------------

! -- PRIVATE Variables -------------------------------------------

integer, private, pointer   :: realtimer(:)
real(8), private, pointer   :: cputimer(:)
integer, private, parameter :: buffer = 20

! -- interface DECLARATIONS -----------------------------------------------------------

contains

!----------------------------------------------------------------------------------------
! start system clock reading and provide index for stopping
!----------------------------------------------------------------------------------------
integer function realtime_start()
implicit none
integer          :: new_index, dim
integer, pointer :: ptimer(:)

  if (associated(realtimer)) then
    new_index = minloc(realtimer(:), dim=1)   ! look for free timer
    if (new_index == 0) then                  ! -- not found
      dim = size(realtimer)
      allocate(ptimer(dim+buffer))
      ptimer(1:dim)  = realtimer(1:dim)
      ptimer(dim+1:) = 0
      deallocate(realtimer)
      realtimer => ptimer
      new_index = dim+1
    endif
  else
    allocate(realtimer(buffer))
    realtimer(:) = 0
    new_index = 1  
  endif
  call system_clock(count=realtimer(new_index))
  realtime_start = new_index
  
endfunction realtime_start

!----------------------------------------------------------------------------------------
! return elapsed time in seconds
!----------------------------------------------------------------------------------------
real function realtime_query(itimer)
implicit none
integer, intent(in) :: itimer
integer             ::newtimer, timerrate, timermax

  if (itimer <= 0) then
    call cfd_error("TIMER/realtime_query: bad (negative or null) timer index")
  elseif (itimer > size(realtimer)) then
    call cfd_error("TIMER/realtime_query: non existing timer index")
  else
    call system_clock(count=newtimer, count_rate=timerrate, count_max=timermax)
    if (newtimer <= realtimer(itimer)) newtimer = newtimer+timermax
    realtime_query = real(newtimer - realtimer(itimer))/timerrate
  endif
  
endfunction realtime_query


!----------------------------------------------------------------------------------------
! stop system clock reading, free timer, and return elapsed time in seconds
!----------------------------------------------------------------------------------------
real function realtime_stop(itimer)
implicit none
integer, intent(in) :: itimer
integer             ::newtimer, timerrate, timermax

  if (itimer <= 0) then
    call cfd_error("TIMER/realtime_stop: bad (negative or null) timer index")
  elseif (itimer > size(realtimer)) then
    call cfd_error("TIMER/realtime_stop: non existing timer index")
  else
    call system_clock(count=newtimer, count_rate=timerrate, count_max=timermax)
    if (newtimer <= realtimer(itimer)) newtimer = newtimer+timermax
    realtime_stop = real(newtimer - realtimer(itimer))/timerrate
    realtimer(itimer) = 0
  endif
  
endfunction realtime_stop

!----------------------------------------------------------------------------------------
! start system clock reading and provide index for stopping
!----------------------------------------------------------------------------------------
integer function cputime_start()
implicit none
integer          :: new_index, dim
real(8), pointer :: ptimer(:)

  if (associated(cputimer)) then
    new_index = minloc(cputimer(:), dim=1)   ! look for free timer
    if (new_index == 0) then                 ! -- not found
      dim = size(cputimer)
      allocate(ptimer(dim+buffer))
      ptimer(1:dim)  = cputimer(1:dim)
      ptimer(dim+1:) = 0
      deallocate(cputimer)
      cputimer => ptimer
      new_index = dim+1
    endif
  else
    allocate(cputimer(buffer))
    cputimer(:) = 0
    new_index = 1  
  endif
  call cpu_time(cputimer(new_index))
  cputime_start = new_index
  
endfunction cputime_start


!----------------------------------------------------------------------------------------
! stop system clock reading, free timer, and return elapsed time in seconds
!----------------------------------------------------------------------------------------
real function cputime_stop(itimer)
implicit none
integer, intent(in) :: itimer
real(8)             :: timer

  if (itimer <= 0) then
    call cfd_error("TIMER/cputime_stop: bad (negative or null) timer index")
  elseif (itimer > size(cputimer)) then
    call cfd_error("TIMER/cputime_stop: non existing timer index")
  else
    call cpu_time(timer)
    cputime_stop = timer - cputimer(itimer)
    cputimer(itimer) = 0.
  endif
  
endfunction cputime_stop


!----------------------------------------------------------------------------------------
endmodule TIMER
!------------------------------------------------------------------------------!
! Changes history
!
! Aug  2012: creation
!------------------------------------------------------------------------------!
