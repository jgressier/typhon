!------------------------------------------------------------------------------!
! Procedure : readcgnsfile                Auteur : J. Gressier

! Fonction
!   Read a cgns file (one base, and one zone)
!
!------------------------------------------------------------------------------!
subroutine readcgnsfile(defmesh, world) 

use MENU_MESH
use CGNSLIB       ! definition des mots-clefs
use CGNS_STRUCT   ! Definition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- INPUTS --
type(mnu_mesh)      :: defmesh

! -- OUTPUTS --
type(st_cgns_world) :: world      ! typhon cgns structures

! -- Internal variables --
integer       :: unit             ! Io unit number
integer       :: ier              ! code d'erreur
integer       :: i

! -- BODY --
   
! --- open CGNS file ---

call print_info(5, "* READING CGNS MESH: "//trim(defmesh%filename))

call cg_open_f(trim(defmesh%filename), MODE_READ, unit, ier)

if (ier /= 0) call erreur("CGNS IO","cannot open CGNS file "//trim(defmesh%filename))
   
! --- read base structure ---

call cg_nbases_f(unit, world%nbase, ier)

write(str_w,'(a,i2,a,a)') ".",world%nbase,"base(s) in CGNS file ",trim(defmesh%filename)
call print_info(8, adjustl(str_w))

if (ier /= 0) call erreur("CGNS IO","cannot count bases")

! --- Allocation et Lecture des bases ---

world%nbase = 1
allocate(world%base(world%nbase))

call readcgnsbase(unit, defmesh%icgnsbase, world%base(1))

! --- fermeture du fichier ---

call cg_close_f(unit, ier)
call print_info(8, "CGNS file "//trim(defmesh%filename)//" closed")



!-------------------------
endsubroutine readcgnsfile
