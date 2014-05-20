!------------------------------------------------------------------------------!
! MODULE : MESHALE                        Author : A. Gardi
!                                         Date   : Mar 2011
!> @brief Definition of ALE mesh movement parameters and routines
!------------------------------------------------------------------------------!
module MESHALE

use USTMESH
use MENU_SOLVER
use MENU_ALE
use VEC3D
use FCT_ENV
use FCT_EVAL
use OUTPUT
use MESHGEOM

implicit none

! -- Global variables -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------

! -- Functions et Operators ------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------

contains

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
subroutine ale_initcycle(umesh, defale)
implicit none
! --- INPUTS ---
type(st_ustmesh)            :: umesh
! --- IN/OUTPUTS ---
type(mnu_ale)               :: defale
! -- Internal variables --
integer                     :: ifc, ic

! -- BODY --
! Initialization (ONLY THE VERY FIRST CYCLE)
if (.not.associated(defale%original_vertex)) then
  allocate(defale%original_vertex(1:umesh%nvtex))
  defale%original_vertex = umesh%mesh%vertex(:,1,1)
  allocate(defale%old_facecentres(1:umesh%nface))
  allocate(defale%old_volume(1:umesh%ncell))
  allocate(defale%face_velocity(1:umesh%nface))
endif

! Saving the old face centres, for face velocity evaluation (ALL CYCLES)
do ifc = 1, umesh%nface
  defale%old_facecentres(ifc) = umesh%mesh%face_center(ifc,1)
enddo

! Saving the old volumes, for conservative balance correction (ALL CYCLES)
do ic = 1, umesh%ncell_int
  defale%old_volume(ic) = umesh%mesh%volume(ic,1,1)
enddo

endsubroutine ale_initcycle

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
subroutine ale_calcweight(umesh, defale)
implicit none
! --- INPUTS ---
type(st_ustmesh)            :: umesh
! --- IN/OUTPUTS ---
type(mnu_ale)               :: defale
! -- Internal variables --
integer                     :: ib, ind, ifa, ifc, iv, ivb
integer                     :: nbodyvtex, nboundvtex
integer, dimension(:), allocatable :: bodyvtex, boundvtex
real(krp)                   :: distance

! -- BODY --
if (.not. associated(defale%weight)) then
  defale%idboco_body = -1
  do ib = 1,umesh%nboco
    if (samestring(defale%moving_body, umesh%boco(ib)%family)) then
      defale%idboco_body = ib
      exit
    endif
  enddo
  if (defale%idboco_body <= 0) then
    call error_stop("ale_calcweight: MOVING BODY name not found among mesh boundaries!")
  else
    call print_info(20,"ALE: MOVING_BODY name found between mesh boundaries!")
  endif

  ! body vertex indexes vector calculation
  allocate(bodyvtex(umesh%boco(defale%idboco_body)%nface * umesh%facevtex%nbfils))
  bodyvtex(:) = 0
  ind = 0
  do ifa = 1, umesh%boco(defale%idboco_body)%nface
    ifc = umesh%boco(defale%idboco_body)%iface(ifa)
    do iv = 1, umesh%facevtex%nbfils
      ivb = umesh%facevtex%fils(ifc,iv)
      if (ivb > 0) then
        ind = ind + 1
        bodyvtex(ind) = ivb
      endif
    enddo
  enddo
  nbodyvtex = ind !count(bodyvtex(:) > 0)

  ! boundary vertex indexes vector calculation
  allocate(boundvtex(umesh%nface_lim * umesh%facevtex%nbfils - nbodyvtex))
  boundvtex(:) = 0
  ind = 0
  do ib = 1, umesh%nboco
    if (ib /= defale%idboco_body) then
      do ifa = 1, umesh%boco(ib)%nface
        ifc = umesh%boco(ib)%iface(ifa)
        do iv = 1, umesh%facevtex%nbfils
          ivb = umesh%facevtex%fils(ifc, iv)
          if (ivb > 0) then
            ind = ind + 1
            boundvtex(ind) = ivb
          endif
        enddo
      enddo
    else
      cycle
    endif
  enddo
  nboundvtex = ind !count(boundvtex(:) > 0)

  ! body_centre
  if (abs(defale%body_centre) >= (huge(1._krp)/2)) then
    do iv = 1, nbodyvtex
      defale%body_centre%x = defale%body_centre%x + (umesh%mesh%vertex(bodyvtex(iv),1,1)%x/nbodyvtex)
      defale%body_centre%y = defale%body_centre%y + (umesh%mesh%vertex(bodyvtex(iv),1,1)%y/nbodyvtex)
      defale%body_centre%z = defale%body_centre%z + (umesh%mesh%vertex(bodyvtex(iv),1,1)%z/nbodyvtex)
    enddo
  endif

  ! body_maxradius
  if (defale%body_maxradius <= 0._krp) then
    do iv = 1, nbodyvtex
      if (abs(umesh%mesh%vertex(bodyvtex(iv),1,1)-defale%body_centre) > defale%body_maxradius) then
          defale%body_maxradius = abs(umesh%mesh%vertex(bodyvtex(iv),1,1)-defale%body_centre)
      endif
    enddo
  endif

  ! closest_boundary
  if (defale%closest_boundary <= 0._krp) then
    defale%closest_boundary = huge(1._krp)
    do iv = 1, nboundvtex
      if (abs(umesh%mesh%vertex(boundvtex(iv),1,1)-defale%body_centre) < defale%closest_boundary) then
        defale%closest_boundary = abs(umesh%mesh%vertex(boundvtex(iv),1,1)-defale%body_centre)
      endif
    enddo
  endif

  ! weight field calculation
  allocate(defale%weight(umesh%nvtex))
  do iv = 1, umesh%nvtex
    distance = abs(umesh%mesh%vertex(iv,1,1)-defale%body_centre)
    ! weighting:  w = 1
    !                 for distance < body_max_radius
    !             w = 0
    !                 for distance > closest_boundary
    !             w = 1 - (distance - body_max_radius) / (closest_boundary - body_max_radius)
    !                 for body_max_radius < distance < closest_boundary
    if (distance <= defale%body_maxradius) then
      defale%weight(iv)=1
    elseif (distance >= defale%closest_boundary) then
      defale%weight(iv)=0
    else
      defale%weight(iv)= (defale%closest_boundary - distance)/(defale%closest_boundary - defale%body_maxradius)
      !1-((distance-defale%body_maxradius)/(defale%closest_boundary-defale%body_maxradius))**2
    endif
  enddo
  deallocate(bodyvtex)
  deallocate(boundvtex)
  ! DEBUG:
  !write(str_w,'(a, I8, a, a)') "idboco_body = ", defale%idboco_body, ", boco family name = ", umesh%boco(defale%idboco_body)%family
  !call print_info(20, str_w)
  !write(str_w,'(a,3e12.4,a,e12.4,a,e12.4)') "body_centre = ", defale%body_centre, &
  !     ", body_maxradius = ", defale%body_maxradius, ", closest_boundary = ", defale%closest_boundary
  !call print_info(20, str_w)
  call print_info(20,"ALE: MOVING_BODY Weight field evaluation completed!")
endif

endsubroutine ale_calcweight
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
! -----------------------------------------------------------------------------!
subroutine ale_update_geoparam(umesh, defsolver)
! --- IN/OUTPUTS ---
type(st_ustmesh)            :: umesh
type(mnu_solver)            :: defsolver
! -- Internal variables --
integer                     :: ib
! -- BODY --

  call calc_meshgeom(defsolver%defmesh, umesh)

  do ib = 1, umesh%nboco
    select case(defsolver%boco(umesh%boco(ib)%idefboco)%typ_calc)
    case(bc_calc_ghostcell)
      call update_ustboco_ghostcell(ib, defsolver%boco(umesh%boco(ib)%idefboco), umesh)
    case(bc_calc_ghostface)
      call update_ustboco_ghostface(ib, defsolver%boco(umesh%boco(ib)%idefboco), umesh)
    case(bc_calc_singpanel)
      !call update_ustboco_singpanel(ib, defboco(umesh%boco(ib)%idefboco), grid)
      call error_stop("ale_update_geomparam: no way for me to update the thermal boundary conditions")
    case(bc_calc_kutta)
      !call update_ustboco_kutta(ib, defboco(umesh%boco(ib)%idefboco), grid)
      call error_stop("ale_update_geomparam: no way for me to update the thermal boundary conditions")
    case default
      ! DEV: PERIODICITY boco drops here! Something to investigate further... ???
      call error_stop("ale_update_geomparam: boundary conditions definitions were lost at some point in the past. Update impossible")
    endselect
  enddo

  call calc_ust_checkface(umesh%facecell, umesh%mesh)

  call calc_mesh_info(umesh%mesh)

endsubroutine ale_update_geoparam
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
subroutine ale_fincycle(umesh, defsolver, gradcond_computed, dt)
implicit none
! --- INPUTS ---
real(krp)                 :: dt
! --- IN/OUTPUTS ---
type(st_ustmesh)          :: umesh
type(mnu_solver)          :: defsolver
logical                   :: gradcond_computed
! -- Internal variables --
integer(kip)              :: ind

! -- BODY --
  ! Geometric and calculation parameters update
  gradcond_computed = .false.  ! so precalc_grad_lsq() is forced to update the gradient
  call ale_update_geoparam(umesh, defsolver)

  ! Face velocity evaluation
  do ind = 1, umesh%nface
    defsolver%defale%face_velocity(ind) = (umesh%mesh%face_center(ind,1) - defsolver%defale%old_facecentres(ind)) / dt
  enddo

endsubroutine ale_fincycle
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
! -----------------------------------------------------------------------------!
subroutine ale_meshupdate(umesh, defsolver, gradcond_computed, curtime, dt)
implicit none
! --- INPUTS ---
real(krp)                :: curtime, dt
type(mnu_solver)         :: defsolver
logical                  :: gradcond_computed
! --- IN/OUTPUTS ---
type(st_ustmesh)         :: umesh
! -- Internal variables --
type(v3d)                :: vertexmovement
real(krp)                :: deltatheta
type(st_fct_env)         :: blank_env
integer(kip)             :: ind

! -- BODY --

select case(defsolver%defale%type)

case(ale_none)
  ! nothing to do
case(ale_global)
  call ale_initcycle(umesh, defsolver%defale)

  ! Vertex movement
  call new_fct_env(blank_env)
  call fct_env_set_real(blank_env, "t", curtime)

  do ind = 1, umesh%nvtex
    call fct_env_set_real(blank_env, "x", defsolver%defale%original_vertex(ind)%x)
    call fct_env_set_real(blank_env, "y", defsolver%defale%original_vertex(ind)%y)
    call fct_env_set_real(blank_env, "z", defsolver%defale%original_vertex(ind)%z)
    call fct_eval_real(blank_env, defsolver%defale%movement_x, vertexmovement%x)
    call fct_eval_real(blank_env, defsolver%defale%movement_y, vertexmovement%y)
    call fct_eval_real(blank_env, defsolver%defale%movement_z, vertexmovement%z)
    
    ! (actual vertex movement)
    umesh%mesh%vertex(ind,1,1) = defsolver%defale%original_vertex(ind) + vertexmovement
  enddo
  call delete_fct_env(blank_env)

  call ale_fincycle(umesh, defsolver, gradcond_computed, dt)

case(ale_body)
  call ale_initcycle(umesh, defsolver%defale)

  call new_fct_env(blank_env)

  call ale_calcweight(umesh, defsolver%defale)

  ! Vertex movement
  call fct_env_set_real(blank_env, "t", curtime)
  call fct_eval_real(blank_env, defsolver%defale%movement_x, vertexmovement%x)
  call fct_eval_real(blank_env, defsolver%defale%movement_y, vertexmovement%y)
  call fct_eval_real(blank_env, defsolver%defale%movement_z, vertexmovement%z)
  call fct_eval_real(blank_env, defsolver%defale%movement_theta, deltatheta)

  do ind = 1, umesh%nvtex 
    ! (actual vertex movement)
    umesh%mesh%vertex(ind,1,1)%x = defsolver%defale%original_vertex(ind)%x &
          + defsolver%defale%weight(ind) * (vertexmovement%x - deltatheta*(umesh%mesh%vertex(ind,1,1)%y - defsolver%defale%body_centre%y))
    umesh%mesh%vertex(ind,1,1)%y = defsolver%defale%original_vertex(ind)%y &
          + defsolver%defale%weight(ind) * (vertexmovement%y + deltatheta*(umesh%mesh%vertex(ind,1,1)%x - defsolver%defale%body_centre%x))
    umesh%mesh%vertex(ind,1,1)%z = defsolver%defale%original_vertex(ind)%z + defsolver%defale%weight(ind) * vertexmovement%z
  enddo
  call delete_fct_env(blank_env)

  call ale_fincycle(umesh, defsolver, gradcond_computed, dt)

case default
  call cfd_error("unknown ALE parameter (ale_meshupdate)")
endselect

endsubroutine ale_meshupdate
! -----------------------------------------------------------------------------!

endmodule MESHALE
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2011 : created
!------------------------------------------------------------------------------!
