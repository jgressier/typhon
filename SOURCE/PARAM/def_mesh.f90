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

if (nkey /= 1) call error_stop("parameter parsing: BLOCK:MESH not found (or conflicts)")

! -- lecture du format

call rpmgetkeyvalstr(pcour, "FORMAT", str)
defmesh%format = cnull

if (samestring(str,"CGNS"))     defmesh%format = fmt_CGNS
if (samestring(str,"INTERNAL")) defmesh%format = fmt_TYPHON
if (samestring(str,"TYPHON"))   defmesh%format = fmt_TYPHON
if (samestring(str,"TYM"))      defmesh%format = fmt_TYPHON

select case(defmesh%format)
case(fmt_CGNS)
  call print_info(20, "  . mesh format : CGNS")
  call rpmgetkeyvalint(pcour, "CGNSBASE", defmesh%icgnsbase, 1)
  call rpmgetkeyvalint(pcour, "CGNSZONE", defmesh%icgnszone, 1)
case(fmt_TYPHON)
  call print_info(20, "  . mesh format : TYPHON internal")
case default
  call error_stop("parameters parsing: unknown mesh format -> "//trim(str))
endselect

! -- read mesh file name --

call rpmgetkeyvalstr(pcour, "FILE", str)
call print_info(20, "  . mesh file   : "//trim(str))
defmesh%filename = str

! -- read scale factor (default 1.) or morphing functions

defmesh%scaling = rpm_existkey(pcour, "SCALE")

if (defmesh%scaling) then
  call rpmgetkeyvalreal(pcour, "SCALE", defmesh%scale)
  call print_info(20, "  . scale factor: "//strof(defmesh%scale))
endif

defmesh%morphing =    rpm_existkey(pcour, "MORPH_X") &
                  .or.rpm_existkey(pcour, "MORPH_Y") &
                  .or.rpm_existkey(pcour, "MORPH_Z") 

if (defmesh%morphing) then
  call print_info(20, "  . mesh morphing")
  call rpmgetkeyvalstr(pcour, "MORPH_X", str, "X")
  call print_info(20, "    MORPH_X = "//trim(str))
  call convert_to_funct(str, defmesh%morph_x, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  call rpmgetkeyvalstr(pcour, "MORPH_Y", str, "Y")
  call print_info(20, "    MORPH_Y = "//trim(str))
  call convert_to_funct(str, defmesh%morph_y, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  call rpmgetkeyvalstr(pcour, "MORPH_Z", str, "Z")
  call print_info(20, "    MORPH_Z = "//trim(str))
  call convert_to_funct(str, defmesh%morph_z, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
endif

if (defmesh%scaling.and.defmesh%morphing) then
  call error_stop("cannot use simultaneously SCALE and MORPH_* functions")
endif

! -- read split method --

call rpmgetkeyvalstr(pcour, "SPLIT", str, "NONE")
if (samestring(str,"NONE"))      defmesh%splitmesh = split_none
if (samestring(str,"SVM2TRI"))   defmesh%splitmesh = split_svm2tri
if (samestring(str,"SVM2QUAD"))  defmesh%splitmesh = split_svm2quad
if (samestring(str,"SVM3WANG"))  defmesh%splitmesh = split_svm3wang
if (samestring(str,"SVM3KRIS"))  defmesh%splitmesh = split_svm3kris
if (samestring(str,"SVM3KRIS2")) defmesh%splitmesh = split_svm3kris2
if (samestring(str,"SVM4WANG"))  defmesh%splitmesh = split_svm4wang
if (samestring(str,"SVM4KRIS"))  defmesh%splitmesh = split_svm4kris
if (samestring(str,"SVM4KRIS2")) defmesh%splitmesh = split_svm4kris2
if (samestring(str,"ISO-TRI"))  then 
  defmesh%splitmesh = split_iso_tri
  call rpmgetkeyvalint(pcour, "NSPLIT", defmesh%nsplit, 1)
endif
if (samestring(str,"ISO-QUAD"))  then 
  defmesh%splitmesh = split_iso_quad
  call rpmgetkeyvalint(pcour, "NSPLIT", defmesh%nsplit, 1)  
endif

select case(defmesh%splitmesh)
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
case(split_iso_quad)
  call print_info(20, "  . split mesh : ISO QUAD REFINEMENT")
case default
  call error_stop("parameters parsing: unknown splitmesh parameter -> "//trim(str))
endselect

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
      call error_stop("parameters parsing: periodicity model")
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
!------------------------------------------------------------------------------!
