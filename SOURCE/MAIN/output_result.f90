!------------------------------------------------------------------------------!
! Procedure : output_result               Auteur : J. Gressier
!                                         Date   : Decembre 2002
! Fonction                                Modif  :
!   Ecriture fichier des champs de chaque zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine output_result(world, position)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MODINFO
use MENU_GEN
use STRING

implicit none

! -- Inputs --
type(st_world) :: world
integer        :: position

! -- Ouputs --

! -- Internal variables --
integer               :: i
character(len=strlen) :: nom       ! file name

! -- BODY --

do i = 1, world%noutput

  if ((position == end_calc).or. &
      (mod(world%info%icycle, world%output(i)%period) == 0)) then

  select case(position)
  case(end_calc)
    nom = trim(world%output(i)%fichier)
  case(end_cycle)
    nom = trim(world%output(i)%fichier)//"_cyc"//strof_full_int(world%info%icycle, 4)
  case(in_cycle)
  case default
  endselect  

  select case(world%output(i)%format)

  case(fmt_VTK)

    nom = trim(nom)
    call print_info(2,"* write VTK file: " // trim(nom))
    call output_vtk(nom, world, world%output(i)%type, position, i) 

  case(fmt_VTKBIN)

    nom = trim(nom)
    call print_info(2,"* write VTK Binary file: " // trim(nom))
    call output_vtkbin(nom, world, world%output(i)%type, position, i) 

  case(fmt_TECPLOT)

    nom = trim(nom)//".dat"
    call print_info(2,"* write TECPLOT file: " // trim(nom))
    call output_tecplot(nom, world, world%output(i)%type, position, i) 

  case(fmt_VIGIE)
    call erreur("Development","VIGIE format not implemented")

  case(fmt_CGNS)
    call erreur("Development","CGNS format not implemented")

  case default
    call erreur("Internal error","unknown output parameter")

  endselect

  endif

enddo


endsubroutine output_result
!------------------------------------------------------------------------------!
! Changes history
!
!------------------------------------------------------------------------------!
