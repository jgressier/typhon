!------------------------------------------------------------------------------!
! Procedure : def_mesh                              Authors : J. Gressier
!
! Fonction                                          Modif  : (see history)
!   Parse main file parameters / Mesh definition
!
!------------------------------------------------------------------------------!
subroutine def_mesh(prj, block, defmesh)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use STRING
use MATH
use VEC3D
use MESHPARAMS
use MENU_GEN
use FCT_PARSER

implicit none

! -- INPUTS --
type(mnu_project)      :: prj
type(rpmblock), target :: block

! -- OUTPUTS --
type(mnu_mesh)     :: defmesh

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i, ip, info
real(krp)                :: x
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

call print_info(5,"- mesh definition")

defmesh%geo = prj%typ_coord   ! transfer 2D/2DAXI/3D property

! -- Recherche du BLOCK:MESH

pblock => block
call seekrpmblock(pblock, "MESH", 0, pcour, nkey)

if (nkey /= 1) call error_stop("Parameter parsing: BLOCK:MESH not found (or conflicts)")

! -- lecture du format

if (prj%action == act_restart) then
  defmesh%format   = fmt_TYPHON
  defmesh%filename = "restart.tys"
  defmesh%scaling  = .false.
  defmesh%morphing = .false.
  defmesh%defsplit%splitmesh = split_none
else

call rpmgetkeyvalstr(pcour, "FORMAT", str)
defmesh%format = cnull

#ifdef CGNS
if (samestring(str,"CGNS"))      defmesh%format = fmt_CGNS
#else /*CGNS*/
if (samestring(str,"CGNS")) then
  call error_stop("Parameter parsing: CGNS format was not activated at configure time")
endif
#endif/*CGNS*/
if (samestring(str,"INTERNAL"))  defmesh%format = fmt_TYPHON
if (samestring(str,"TYPHON"))    defmesh%format = fmt_TYPHON
if (samestring(str,"TYM"))       defmesh%format = fmt_TYPHON
if (samestring(str,"AUTOBLOCK")) defmesh%format = fmt_AUTOBLOCK

select case(defmesh%format)
#ifdef CGNS
case(fmt_CGNS)
  call print_info(20, "  . mesh format : CGNS")
  call rpmgetkeyvalint(pcour, "CGNSBASE", defmesh%icgnsbase, 1)
  call rpmgetkeyvalint(pcour, "CGNSZONE", defmesh%icgnszone, 1)
  call rpmgetkeyvalstr(pcour, "FILE", str)
  defmesh%filename = str
#endif/*CGNS*/
case(fmt_TYPHON)
  call print_info(20, "  . mesh format : TYPHON internal")
  call rpmgetkeyvalstr(pcour, "FILE", str)
  defmesh%filename = str
case(fmt_AUTOBLOCK)
  call print_info(20, "  . mesh format : AUTOBLOCK")
  call rpmgetkeyvalint(pcour, "NI", defmesh%ni)
  call rpmgetkeyvalint(pcour, "NJ", defmesh%nj)
  call rpmgetkeyvalint(pcour, "NK", defmesh%nk, 0_kip)
  call rpmgetkeyvalreal(pcour, "LX", defmesh%lx, 1._krp)
  call rpmgetkeyvalreal(pcour, "LY", defmesh%ly, 1._krp)
  call rpmgetkeyvalreal(pcour, "LZ", defmesh%lz, 1._krp)
case default
  call error_stop("Parameters parsing: unknown mesh format -> "//trim(str))
endselect

! -- read mesh file name --

call print_info(20, "  . mesh file   : "//trim(defmesh%filename))

! -- read scale factor (default 1.)

defmesh%scaling = rpm_existkey(pcour, "SCALE")

if (defmesh%scaling) then
  call rpmgetkeyvalreal(pcour, "SCALE", defmesh%scale)
  call print_info(20, "  . scale factor: "//strof(defmesh%scale))
endif

! -- read morphing functions

defmesh%morphing =    rpm_existkey(pcour, "MORPH_X") &
                  .or.rpm_existkey(pcour, "MORPH_Y") &
                  .or.rpm_existkey(pcour, "MORPH_Z")

if (defmesh%morphing) then
  call print_info(20, "  . mesh morphing")
  call rpmgetkeyvalstr(pcour, "MORPH_X", str, "X")
  call print_info(20, "    MORPH_X = "//trim(str))
  call convert_to_funct(str, defmesh%morph_x, info)
  if (info /= 0) call error_stop("Problem when parsing "//trim(str))
  call rpmgetkeyvalstr(pcour, "MORPH_Y", str, "Y")
  call print_info(20, "    MORPH_Y = "//trim(str))
  call convert_to_funct(str, defmesh%morph_y, info)
  if (info /= 0) call error_stop("Problem when parsing "//trim(str))
  call rpmgetkeyvalstr(pcour, "MORPH_Z", str, "Z")
  call print_info(20, "    MORPH_Z = "//trim(str))
  call convert_to_funct(str, defmesh%morph_z, info)
  if (info /= 0) call error_stop("Problem when parsing "//trim(str))
endif

if (defmesh%scaling.and.defmesh%morphing) then
  call error_stop("cannot use simultaneously SCALE and MORPH_* functions")
endif

! -- read split method --

defmesh%defsplit%splitmesh = -1

call rpmgetkeyvalstr(pcour, "SPLIT", str, "NONE")
if (samestring(str,"NONE"))      defmesh%defsplit%splitmesh = split_none
if (samestring(str,"SVM2TRI"))   defmesh%defsplit%splitmesh = split_svm2tri
if (samestring(str,"SVM2QUAD"))  defmesh%defsplit%splitmesh = split_svm2quad
if (samestring(str,"SVM3WANG"))  defmesh%defsplit%splitmesh = split_svm3wang
if (samestring(str,"SVM3KRIS"))  defmesh%defsplit%splitmesh = split_svm3kris
if (samestring(str,"SVM3KRIS2")) defmesh%defsplit%splitmesh = split_svm3kris2
if (samestring(str,"SVM4WANG"))  defmesh%defsplit%splitmesh = split_svm4wang
if (samestring(str,"SVM4KRIS"))  defmesh%defsplit%splitmesh = split_svm4kris
if (samestring(str,"SVM4KRIS2")) defmesh%defsplit%splitmesh = split_svm4kris2
if (samestring(str,"ISO-TRI"))  then
  defmesh%defsplit%splitmesh = split_iso_tri
  call rpmgetkeyvalint(pcour, "NSPLIT", defmesh%defsplit%nsplit, 1)
endif
if (samestring(str,"ISO-QUAD"))  then
  defmesh%defsplit%splitmesh = split_quad2x2
  call rpmgetkeyvalint(pcour, "NSPLIT", defmesh%defsplit%nsplit, 1)
endif
if (samestring(str,"QUAD2X2"))  then
  defmesh%defsplit%splitmesh = split_quad2x2
  call rpmgetkeyvalint(pcour, "NSPLIT", defmesh%defsplit%nsplit, 1)
endif
if (samestring(str,"QUAD3X3"))  then
  defmesh%defsplit%splitmesh  = split_quad3x3
  defmesh%defsplit%splitparam = 1._krp/3._krp
  call rpmgetkeyvalint(pcour, "NSPLIT", defmesh%defsplit%nsplit, 1)
endif

select case(defmesh%defsplit%splitmesh)
case(split_none)
  ! nothing to write
case(split_svm2quad)
  call print_info(20, "  . split mesh : SVM2 based (3 quads)")
case(split_svm3wang)
  call print_info(20, "  . split mesh : SVM3 WANG ORIGINAL")
case(split_svm3kris)
  call print_info(20, "  . split mesh : SVM3 KRIS OPTIMIZED")
case(split_svm3kris2)
  call print_info(20, "  . split mesh : SVM3 KRIS OPTIMIZED 2")
case(split_svm4wang)
  call print_info(20, "  . split mesh : SVM4 WANG ORIGINAL")
case(split_svm4kris)
  call print_info(20, "  . split mesh : SVM4 KRIS OPTIMIZED")
case(split_svm4kris2)
  call print_info(20, "  . split mesh : SVM4 KRIS OPTIMIZED 2")
case(split_iso_tri)
  call print_info(20, "  . split mesh : ISO TRI REFINEMENT")
case(split_quad2x2)
  call print_info(20, "  . split mesh : QUAD2x2 REFINEMENT")
case(split_quad3x3)
  call print_info(20, "  . split mesh : QUAD3x3 REFINEMENT")
case default
  call error_stop("Parameters parsing: unknown splitmesh parameter -> "//trim(str))
endselect

endif ! no restart

defmesh%nfgauss = 1

! ----------------------------------------------------------------------------
! Periodicity BLOCKS

pblock => block
call seekrpmblock(pblock, "PERIODICITY", 0, pcour, defmesh%nperiodicity)

if (defmesh%nperiodicity >= 1) then

  allocate(defmesh%periodicity(defmesh%nperiodicity))

  do ip = 1, defmesh%nperiodicity

    call seekrpmblock(pblock, "PERIODICITY", ip, pcour, nkey)

    call rpmgetkeyvalstr(pcour, "NAME", str)
    defmesh%periodicity(ip)%name = trim(str)

    call rpmgetkeyvalstr(pcour, "TYPE", str)

    defmesh%periodicity(ip)%type = inull
    if (samestring(str, "TRANSLATION" )) defmesh%periodicity(ip)%type = per_trans
    if (samestring(str, "ROTATION" ))    defmesh%periodicity(ip)%type = per_rot

    call print_info(20, "  . periodicity definition, type "//str(1:12)//": "//trim(defmesh%periodicity(ip)%name))

    select case(defmesh%periodicity(ip)%type)

    case(per_trans)
      call rpmgetkeyvalstr(pcour, "TRANSLATION", str)
      defmesh%periodicity(ip)%distance = v3d_of(str, info)

    case(per_rot)
      call rpmgetkeyvalstr (pcour, "ROTATION_CENTER", str, "(0., 0., 0.)")
      defmesh%periodicity(ip)%origin = v3d_of(str, info)
      call rpmgetkeyvalstr (pcour, "ROTATION_AXIS", str)
      defmesh%periodicity(ip)%axis = v3d_of(str, info)
      defmesh%periodicity(ip)%axis = defmesh%periodicity(ip)%axis / abs(defmesh%periodicity(ip)%axis)
      if (rpm_existkey(pcour, "ROTATION_ANGLE")) then
       call rpmgetkeyvalreal(pcour, "ROTATION_ANGLE", x)
       defmesh%periodicity(ip)%angle = x/180._krp*PIcst
      else
       call rpmgetkeyvalreal(pcour, "ROTATION_NUMBER", x)
       defmesh%periodicity(ip)%angle = 2._krp*PIcst/x
      endif
    case default
      call error_stop("Parameters parsing: unknown periodicity model")
    endselect

  enddo
endif

endsubroutine def_mesh
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002: created
! sept 2005: add scale factor
! oct  2007: splitting option
! June 2010: TYPHON internal format
! Feb  2013: add auto-blocking
!------------------------------------------------------------------------------!
