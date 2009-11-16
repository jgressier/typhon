!------------------------------------------------------------------------------!
! MODULE : QUANTITY
!------------------------------------------------------------------------------!
module QUANTITY

use STRING
use IOCFD

implicit none

! -- Quantity id definition --

integer, parameter :: qs_density         = 001
integer, parameter :: qs_pressure        = 002
integer, parameter :: qs_temperature     = 003 
integer, parameter :: qs_density_tot     = 011
integer, parameter :: qs_pressure_tot    = 012
integer, parameter :: qs_temperature_tot = 013 
integer, parameter :: qs_energy_int      = 015 
integer, parameter :: qs_energy_tot      = 016 
integer, parameter :: qs_enthalpy_int    = 017 
integer, parameter :: qs_enthalpy_tot    = 018 
integer, parameter :: qs_soundspeed      = 020 
integer, parameter :: qs_entropy         = 021 
integer, parameter :: qs_mach            = 030
integer, parameter :: qv_velocity        = 101
integer, parameter :: qv_momentum        = 102
integer, parameter :: qv_dynalpy         = 103
integer, parameter :: qv_stress          = 105 


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Function : get id (integer) from quantity string
!------------------------------------------------------------------------------!
integer function quantity_id(str)
implicit none
character(len=*) str

  quantity_id = -1

  ! quantites scalaires
  if (samestring(str, "RHO" ))          quantity_id = qs_density
  if (samestring(str, "DENSITY" ))      quantity_id = qs_density
  if (samestring(str, "PS" ))           quantity_id = qs_pressure
  if (samestring(str, "PRESSURE" ))     quantity_id = qs_pressure
  if (samestring(str, "TS" ))           quantity_id = qs_temperature
  if (samestring(str, "TEMPERATURE" ))  quantity_id = qs_temperature
  if (samestring(str, "RHOI" ))         quantity_id = qs_density_tot
  if (samestring(str, "PI" ))           quantity_id = qs_pressure_tot
  if (samestring(str, "TI" ))           quantity_id = qs_temperature_tot
  if (samestring(str, "E" ))            quantity_id = qs_energy_int
  if (samestring(str, "H" ))            quantity_id = qs_enthalpy_int
  if (samestring(str, "EI" ))           quantity_id = qs_energy_tot
  if (samestring(str, "HI" ))           quantity_id = qs_enthalpy_tot
  if (samestring(str, "ASOUND" ))       quantity_id = qs_soundspeed
  if (samestring(str, "A" ))            quantity_id = qs_soundspeed
  if (samestring(str, "C" ))            quantity_id = qs_soundspeed
  if (samestring(str, "S" ))            quantity_id = qs_entropy
  if (samestring(str, "M" ))            quantity_id = qs_entropy
  if (samestring(str, "MACH" ))         quantity_id = qs_mach

  ! quantites vectorielles
  if (samestring(str, "VELOCITY" ))     quantity_id = qv_velocity
  if (samestring(str, "V" ))            quantity_id = qv_velocity
  if (samestring(str, "MOMENTUM" ))     quantity_id = qv_momentum
  if (samestring(str, "DYNALPY" ))      quantity_id = qv_dynalpy
  if (samestring(str, "STRESS" ))       quantity_id = qv_stress

  if (quantity_id == -1) call cfd_error("unknown quantity "//trim(str))

endfunction quantity_id

!------------------------------------------------------------------------------!
! Fonction : get "official" name from quantity id
!------------------------------------------------------------------------------!
function quantity_name(id) result(strout)
implicit none
integer           :: id
character(len=30) :: strout

select case(id)
case(qs_density)
  strout = "RHO"
case(qs_pressure)
  strout = "PS"
case(qs_temperature)
  strout = "TS"
case(qs_density_tot)
  strout = "RHOI"
case(qs_pressure_tot)
  strout = "PI"
case(qs_temperature_tot)
  strout = "TI"
case(qs_energy_int)
  strout = "E"
case(qs_enthalpy_int)
  strout = "H"
case(qs_energy_tot)
  strout = "EI"
case(qs_enthalpy_tot)
  strout = "HI"
case(qs_soundspeed)
  strout = "ASOUND"
case(qs_entropy)
  strout = "S"
case(qs_mach)
  strout = "M"
case(qv_velocity)
  strout = "V"
case(qv_momentum)
  strout = "RHOV"
case(qv_dynalpy)
  strout = "DYNALPY"
case(qv_stress)
  strout = "TAUW"
case default
  call cfd_error("unknown quantity id="//trim(strof(id)))
endselect

endfunction quantity_name

!------------------------------------------------------------------------------!
! Fonction : get "official" name from quantity id
!------------------------------------------------------------------------------!
function quantity_cgnsname(id) result(strout)
implicit none
integer           :: id
character(len=30) :: strout

select case(id)
case(qs_density)
  strout = "Density"
case(qs_pressure)
  strout = "Pressure"
case(qs_temperature)
  strout = "Temperature"
case(qs_density_tot)
  strout = "RHOI"
case(qs_pressure_tot)
  strout = "PI"
case(qs_temperature_tot)
  strout = "TI"
case(qs_energy_int)
  strout = "E"
case(qs_enthalpy_int)
  strout = "H"
case(qs_energy_tot)
  strout = "EI"
case(qs_enthalpy_tot)
  strout = "HI"
case(qs_soundspeed)
  strout = "ASOUND"
case(qs_entropy)
  strout = "S"
case(qs_mach)
  strout = "M"
case(qv_velocity)
  strout = "Velocity"
case(qv_momentum)
  strout = "RHOV"
case(qv_dynalpy)
  strout = "DYNALPY"
case(qv_stress)
  strout = "TAUW"
case default
  call cfd_error("unknown quantity id="//trim(strof(id)))
endselect

endfunction quantity_cgnsname


endmodule QUANTITY

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2009 : transfer quantity names from TYPHON/PARAM/MENU_SOLVER
!------------------------------------------------------------------------------!

