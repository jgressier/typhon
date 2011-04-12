!------------------------------------------------------------------------------!
! MODULE : MENU_ALE                       Author : A. Gardi
!                                         Date   : Mar 2011
! Fonction                                Modif  : (cf changes history)
!   Definition of ALE mesh movement parameters and routines
!------------------------------------------------------------------------------!
module MENU_ALE

use TYPHMAKE
use VEC3D
use FCT_ENV
use FCT_EVAL

implicit none

! -- Global variables -------------------------------------------

! -- type of ALE definition --

integer(kpp), parameter :: ale_none      = 0
integer(kpp), parameter :: ale_global    = 1
integer(kpp), parameter :: ale_body      = 5

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_ALE : ALE mesh movement parameters
!------------------------------------------------------------------------------!
type mnu_ale
  character(len=shortname) :: name
  integer(kpp)             :: type
  type(st_fct_node)        :: movement_x, movement_y, movement_z, movement_theta
  character(len=shortname) :: moving_body      ! name of the moving body boundary in the mesh
  integer                  :: idboco_body      ! id of the moving body among the boco definitions
  type(v3d)                :: body_centre      ! centre of the moving body
  real(krp)                :: body_maxradius   ! maximum dimension of the body / 2
  real(krp)                :: closest_boundary ! radial distance of the closest volume boundary
  real(krp), dimension(:), pointer :: weight ! weight function for the BODY interpolated mesh movement
  type(v3d), dimension(:), pointer :: original_vertex, old_facecentres, face_velocity
endtype mnu_ale

! -- INTERFACES -------------------------------------------------------------

! -- Fonctions et Operateurs ------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------

contains

!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!
subroutine init_defale(defale)
implicit none
! --- IN/OUTPUTS ---
type(mnu_ale)               :: defale
! -- BODY --
  defale%name = ""
  defale%type = ale_none
  defale%idboco_body = -1
  defale%body_centre = v3d(-huge(1._krp),-huge(1._krp),-huge(1._krp))
  defale%body_maxradius = -1._krp
  defale%closest_boundary = -1._krp
  nullify(defale%weight)
  nullify(defale%original_vertex)
  nullify(defale%old_facecentres)
  nullify(defale%face_velocity)
endsubroutine init_defale


endmodule MENU_ALE
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2011 : created
!------------------------------------------------------------------------------!
