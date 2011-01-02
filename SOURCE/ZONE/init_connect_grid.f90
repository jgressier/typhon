!------------------------------------------------------------------------------!
! Procedure : init_connect_grid                         Authors : J. Gressier
!                                                       Created : March 2004
! Fonction
!   Calcul des connectivites supplementaires (conditions limites)
!
!------------------------------------------------------------------------------!
subroutine init_connect_grid(defsolver, grid)

use TYPHMAKE
use STRING
use VARCOM
use OUTPUT
use MGRID
use MENU_SOLVER

implicit none

! -- INPUTS --
type(mnu_solver) :: defsolver            ! parametres du solveur

! -- INPUTS/OUTPUTS --
type(st_grid)    :: grid                 ! maillage et connectivites

! -- OUTPUTS --

! -- Internal variables --
type(v3d), allocatable :: v1(:), v2(:)
integer,   allocatable :: i1(:), i2(:)
type(v3d)              :: dfc(1)
integer                :: ib, ib2, idef, nf, nf2     ! index de conditions aux limites et index de definition
integer                :: iface, ic1, ic2, icper, if, ifper
logical                :: same_name
type(st_ustboco), pointer :: boco

! -- BODY --

call print_info(10,"- Grid "//name(grid))

write(str_w,'(a,i8,a,i8,a,i6,a)') "  connectivity :",grid%umesh%ncell," total cells,",&
                                  grid%umesh%ncell_int," internal cells &",&
                                  grid%umesh%ncell_lim," ghost (BC) cells"
call print_info(10, str_w)
write(str_w,'(a,i8,a,i8,a,i6,a)') "  connectivity :",grid%umesh%nface," total faces,",&
                                  grid%umesh%nface_int," internal faces &",&
                                  grid%umesh%nface_lim," boundary faces"
call print_info(10, str_w)

!------------------------------------------------------------------------
! computing CONNECTIVITY of boundary conditions

call print_info(8, ". initializing boundary faces to ghost cells")

! -- Definition des connectivites faces limites -> cellules limites

grid%umesh%ncell_lim = 0     ! initialisation du compteur de cellules limites


!---------------------------------------------------------------------------------------
! Loop on all grid boco 
!---------------------------------------------------------------------------------------
do ib = 1, grid%umesh%nboco

  if (grid%umesh%boco(ib)%nface == 0) cycle

  grid%umesh%boco(ib)%idefboco = inull         ! initialization to bad value

  !---------------------------------------------------------------------------------------
  ! ----- looking for predefined classical boco parameters ------

  same_name = .false.
  idef      = 0
  do while ((.not.same_name).and.(idef+1 <= defsolver%nboco))
    idef      = idef + 1
    same_name = samestring(grid%umesh%boco(ib)%family, defsolver%boco(idef)%family)
  enddo

  if (same_name) then

    grid%umesh%boco(ib)%idefboco = idef

    select case(defsolver%boco(idef)%typ_calc)
    case(bc_calc_ghostcell)
      call init_ustboco_ghostcell(ib, defsolver%boco(idef), grid%umesh)
    case(bc_calc_ghostface)
      call init_ustboco_ghostface(ib, defsolver%boco(idef), grid%umesh)
    case(bc_calc_singpanel)
      call init_ustboco_singpanel(ib, defsolver%boco(idef), grid)
    case(bc_calc_kutta)
      call init_ustboco_kutta(ib, defsolver%boco(idef), grid)
    case default
      call erreur("Internal error (init_connect_grid)","unknown type of boundary conditions")
    endselect

  endif

  !---------------------------------------------------------------------------------------
  ! ----- looking for predefined CONNECTION parameters (if not classical BOCO) ------

  if (grid%umesh%boco(ib)%idefboco == inull) then !(if not yet found)

    same_name = .false.
    idef      = 0
    do while ((.not.same_name).and.(idef+1 <= defsolver%nconnect))
      idef      = idef + 1
      same_name = samestring(grid%umesh%boco(ib)%family, defsolver%connect(idef)%fam1).or.&
                  samestring(grid%umesh%boco(ib)%family, defsolver%connect(idef)%fam2)
    enddo
    if (same_name) then
      grid%umesh%boco(ib)%idefboco        = defboco_connect
      grid%umesh%boco(ib)%gridcon%contype = gdcon_per_match
      grid%umesh%boco(ib)%gridcon%ilink   = idef            ! index in CONNECTION array
      if (samestring(grid%umesh%boco(ib)%family, defsolver%connect(idef)%fam1)) then
        grid%umesh%boco(ib)%gridcon%rlink   = 1._krp
      else
        grid%umesh%boco(ib)%gridcon%rlink   = -1._krp
      endif
    endif

  endif

  ! ----- not found ? ------

  if (grid%umesh%boco(ib)%idefboco == inull) then
    call erreur("Error in matching family names", &
                "impossible to find "//trim(grid%umesh%boco(ib)%family)// &
                " in the parameter definition")
  endif

enddo

!------------------------------------------------------------------------
! computing CONNECTIVITY between grids
!------------------------------------------------------------------------
!
call print_info(8, ". initializing connections")

do ib = 1, grid%umesh%nboco

  boco => grid%umesh%boco(ib)
  nf = boco%nface
  if (nf == 0) cycle

  if (boco%idefboco == defboco_connect) then      ! if boco type is CONNECTION
    select case(boco%gridcon%contype)

    case(gdcon_per_match)

      allocate(v1(nf))

      ! -- get BOCO1 centers and transfer via periodicity --

      call get_bocofacecenter(boco, grid%umesh, v1)
      call transloc_per(defsolver%defmesh%periodicity(defsolver%connect(boco%gridcon%ilink)%ilink), v1(1:nf), boco%gridcon%rlink)

      ! -- look for BOCO2 index --

       idef = 0
      do ib2 = 1, grid%umesh%nboco
        if (grid%umesh%boco(ib2)%idefboco /= defboco_connect) cycle
        if ((boco%gridcon%ilink == grid%umesh%boco(ib2)%gridcon%ilink).and. &         ! if same CONNECTION
            (boco%gridcon%rlink * grid%umesh%boco(ib2)%gridcon%rlink < 0)) then       ! and inverse direction
          if (idef == 0) then
            idef = ib2
          else
            call erreur("Error (init_connect_grid)", "too many matching families")
          endif
        endif
      enddo
      if (idef == 0) &
           call erreur("Error (init_connect_grid)", "no matching boco connection")

      ! -- get BOCO2 centers --

      ib2 = idef
      nf2 = grid%umesh%boco(ib2)%nface
      if (nf2 /= nf) &
           call erreur("Error (init_connect_grid)", "different number of faces for matching periodic conditions")

      allocate(v2(nf2))
      call get_bocofacecenter(grid%umesh%boco(ib2), grid%umesh, v2)

      allocate(i1(nf))
      allocate(i2(nf2))
      call matching_index(v1, v2, i1, i2)   ! 

!!$      ! instead of keeping matching index, 
!!$      ! one can only reorder one connectivity 
!!$      ! so that they automatically match
!!$
!!$      !grid%umesh%boco(ib2)%iface(1:nf2) = grid%umesh%boco(ib2)%iface(i2(1:nf2))

      ! -- init facecell connectivity (same as in init_ustboco_ghostcell) --

      do if = 1, boco%nface     
        grid%umesh%ncell_lim = grid%umesh%ncell_lim + 1     ! new ghost cell
        iface = grid%umesh%boco(ib)%iface(if)               ! face index
        ic1   = grid%umesh%facecell%fils(iface,1)           ! internal/reference cell index
        ic2   = grid%umesh%ncell_int + grid%umesh%ncell_lim ! ghost cell index

        if (grid%umesh%facecell%fils(iface,2) == 0) then
          grid%umesh%facecell%fils(iface,2) = ic2        ! affectation de la cellule fictive
        else
          call erreur("Error in computing connectivity", "Ghost cell has already been affected")
          
        endif   
      enddo
      
      ! -- direct periodic connection inside a zone
      
      allocate(boco%gridcon%i_param(1:nf))
      boco%gridcon%i_param(1:nf) = grid%umesh%boco(ib2)%iface(i1(1:nf))

      ! -- define ghost cells centers, volumes...

      do if = 1, boco%nface     
        iface = grid%umesh%boco(ib)%iface(if)               ! local face index
        ifper = boco%gridcon%i_param(if)                    ! periodic face index
        ic1   = grid%umesh%facecell%fils(iface,1)           ! internal/reference cell index
        ic2   = grid%umesh%facecell%fils(iface,2)           ! ghost cell index
        icper = grid%umesh%facecell%fils(ifper,1)           ! index of periodic internal cell

        ! face to cell vector on periodic face
        dfc   = grid%umesh%mesh%centre(icper,1,1) - grid%umesh%mesh%iface(ifper,1,1)%centre
        ! transform vector
        call transvec_per(defsolver%defmesh%periodicity(defsolver%connect(boco%gridcon%ilink)%ilink), dfc, -boco%gridcon%rlink)

        grid%umesh%mesh%volume(ic2,1,1) = grid%umesh%mesh%volume(icper,1,1)
        grid%umesh%mesh%centre(ic2,1,1) = grid%umesh%mesh%iface(iface,1,1)%centre + dfc(1)
      enddo

      ! -- 
      deallocate(i1, i2)
      deallocate(v1, v2)

      call print_info(8, "  - computed matching periodic connection: "&
                         //trim(boco%family)//"->"//trim(grid%umesh%boco(ib2)%family))

    case default 
      call erreur("Internal error (init_connect_grid)","unknown type of connection")
    endselect
  endif

enddo

endsubroutine init_connect_grid

!------------------------------------------------------------------------------!
! Changes history
!
! Mar 2004 : creation  (copied from init_connect_ust)
! Mar 2009 : initialization of connections (periodic)
!------------------------------------------------------------------------------!
