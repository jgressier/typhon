!------------------------------------------------------------------------------!
! TYMONITOR
!
!------------------------------------------------------------------------------!
program tymonitor

use IOCFD
use IO_UNIT
use STRING
use VEC3D
use USTMESH
use XBIN_IO
use TYPHON_FMT
use TYFMT_MESH
use TYFMT_SOL
use DEFPROBE
use PROBECALC
use FCT_PARSER
use MESHPARAMS
use MESHCONNECT
use MESHGEOM

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs
character(len=160) :: inputfile, filename, str_opt, str_val
!------------------------------------------------------------------------------!
type(st_deftyphon)         :: deftyphon
type(st_ustmesh)           :: umesh
type(st_genericfield)      :: gfield
type(st_defprobe), pointer :: probe(:)
integer                    :: imin, imax, isize
type(mnu_mesh)             :: defmesh
!---------------------------
integer               :: iunit1
integer(kip)          :: nprobe, ip, icycle, iarg
logical               :: needgeom, cyclesuffix, axi
!------------------------------------------------------------------------------!

call print_cfdtools_header("tymonitor")

!------------------------------
! parse arguments

! default values

inputfile   = ""
nprobe      = 0
needgeom    = .true.
cyclesuffix = .false.
axi         = .false.
imin        = 1
imax        = 1     ! if no imax option, only one monitors computation with default cycle index
isize       = 4

nargs    = command_argument_count()
iarg     = 1

if (nargs >= 3) then
  allocate(probe(nargs/2))
else
  call cfdtool_error("not enough arguments")
endif

do while (iarg <= nargs)
  call get_command_argument(iarg, str_opt)
  iarg = iarg + 1
  if (str_opt == "-avg")  then
    nprobe = nprobe + 1
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    call create_probe(probe(nprobe), vol_average, str_val)
  elseif (str_opt == "-min")  then
    nprobe = nprobe + 1
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    call create_probe(probe(nprobe), vol_min, str_val)
  elseif (str_opt == "-max")  then
    nprobe = nprobe + 1
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    call create_probe(probe(nprobe), vol_max, str_val)
  elseif (str_opt == "-axi")  then
    axi = .true.
  elseif (str_opt == "-imin")  then
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    read(str_val, *) imin
    if (imin < 0) call cfdtool_error("need positive imin index")
  elseif (str_opt == "-imax")  then
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    read(str_val, *) imax
    cyclesuffix = .true.
    if (imax < imin) call cfdtool_error("need imax > imin")
  elseif (str_opt == "-isize")  then
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    read(str_val, *) isize
  else
    inputfile = basename(trim(str_opt), xtyext_sol)
  endif
enddo

if (inputfile == "") then
  call cfdtool_error("missing filename")
endif

!------------------------------------------------------------
! read mesh and solution
!------------------------------------------------------------

do icycle = imin, imax

  if (cyclesuffix) then
    filename = trim(inputfile)//"."//strof_full_int(icycle, isize)//"."//xtyext_sol
  else
    filename = trim(inputfile)//"."//xtyext_sol
  endif

  print*,'* Opening TYPHON file: '//trim(filename)

  iunit1 = getnew_io_unit()
  call typhon_openread(iunit1, trim(filename), deftyphon)

  call typhonread_ustmesh(deftyphon, umesh)
  !call delete_ustmesh_subelements(umesh)

  if (needgeom) then
    select case(umesh%elemdim)
    case(2)
      if (axi) then
        defmesh%geo = geo_2Daxi
      else
        defmesh%geo = geo_2D
      endif
    case(3)
      defmesh%geo = geo_3D
    case default
      call cfd_error("unknown mesh dimension")
    endselect

    call create_face_connect(.false., umesh)
    call new_mesh(umesh%mesh, umesh%ncell, umesh%nface, 0)
    call calc_meshgeom(defmesh, umesh)
  endif
  
  call typhonread_sol(deftyphon, umesh, gfield)

  do ip = 1, nprobe
    call prb_vol_init(probe(ip))
    call prb_vol_calc(probe(ip), umesh, gfield)
    write(probe(ip)%unit,'(i5,e16.8)') icycle, probe(ip)%result  
  enddo

  call delete(umesh)
  call delete(gfield)
  call close_io_unit(iunit1)

enddo

!------------------------------
! close files and end program

do ip = 1, nprobe
  call close_io_unit(probe(ip)%unit)
enddo
deallocate(probe)
print*,'done.'

contains

!------------------------------------------------------------------------------!
subroutine cfdtool_error(str)
  implicit none
  character(len=*) :: str
  print*,'command line: tymonitor [options] -[mon] "monitor=equation" inputfile[.tys]'
  print*
  print*,'where -[mon] option:'
  print*,'  -avg "monitor=equation" : volume weighted average of monitor'
  print*,'  -min "monitor=equation" : minimum value of monitor'
  print*,'  -max "monitor=equation" : maximum value of monitor'
  print*,'"monitor"  is the monitor name, outputs are written to "monitor.tmon"'
  print*,'"equation" is the expression of monitor, depending on x,y,z and field variables'
  print*
  print*,'where [options] could be:'
  print*,'  -axi         : enforce 2D axisymmetric mesh (metric calculation)'
  print*,'  -imin  index : starting index solution (default: 1)'
  print*,'  -imax  index : ending   index solution'
  print*,'  -isize size  : size of index suffix    (default: 4)'
  print*
  print*,'parsed files will be'
  print*,'  inputfile.tys          if -imax has not been defined'
  print*,'  inputfile.nnnn.tys     if -imax has been defined'
  call cfd_error(str)
endsubroutine cfdtool_error

!------------------------------------------------------------------------------!
subroutine create_probe(probe, type, str)
  implicit none
  type(st_defprobe) :: probe
  integer(kpp)      :: type
  character(len=*)  :: str
! -- private --
  integer :: ieq, info
! -- BODY --
  ieq = index(str, '=')
  if (ieq < 2)              call cfdtool_error("missing '=' to define monitor name")
  if (ieq == len_trim(str)) call cfdtool_error("missing equation to define monitor")
  probe%name = str(1:ieq-1)
  probe%type = type
  probe%unit = getnew_io_unit()
  open(unit = probe%unit, file=trim(probe%name)//".tmon", form = "formatted")
  call convert_to_funct(str(ieq+1:len_trim(str)), probe%quantity, info)  
  if (info /= 0) &
       call cfd_error("unable to process symbolic function from "//trim(str))
endsubroutine create_probe

endprogram tymonitor
!------------------------------------------------------------------------------!
! Changes
!
! June  2011: created
!------------------------------------------------------------------------------!
