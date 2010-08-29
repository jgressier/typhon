!------------------------------------------------------------------------------!
! Procedure : ns_bocojacobian
!
! Function
!   Jacobian correction due to BOundary COnditions
!
!------------------------------------------------------------------------------!
subroutine ns_bocojacobian(defsolver, defspat, umesh, flux, prim, grad, jacL, jacR)
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D
use TENSOR3
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)        :: defsolver        ! solver
type(mnu_spat)          :: defspat          ! spatial integration parameters
type(st_ustmesh)        :: umesh            ! unstructured mesh
type(st_genericfield)   :: prim, grad
type(st_mattab)         :: jacL, jacR   ! jacobian matrices related to faces

! -- Outputs --
type(st_genericfield)   :: flux             ! physical flux

! -- Internal variables --
integer                 :: ifb, if, ib, idef ! index de liste, index de face limite et parametres
integer                 :: icl, icr, i, dim
real(krp)               :: dQrdQl(jacL%dim, jacL%dim)

real(krp)               :: id  

! -- BODY --

dim = jacL%dim

do ib = 1, umesh%nboco

  idef = umesh%boco(ib)%idefboco   ! index of boco definition in defsolver structure
  if (idef <= 0) cycle               ! if index <= 0, internal boco (connection...)

  !---------------------------------------------------------------------
  ! select case of boco type

  select case(defsolver%boco(idef)%typ_boco)

  case(bc_geo_sym) !--------------------- SYMMETRY

    do ifb = 1, umesh%boco(ib)%nface
      if  = umesh%boco(ib)%iface(ifb)
      icl = umesh%facecell%fils(if,1)
      icr = umesh%facecell%fils(if,2)
      dQrdQl(1:dim,  1:dim) = 0._krp
      do i = 1, dim
        dQrdQl(i, i) = 1._krp
      enddo
      dQrdQl(3:5, 3:5) = dQrdQl(3:5, 3:5)&
                   -2._krp*tab(umesh%mesh%iface(if,1,1)%normale.tens.umesh%mesh%iface(if,1,1)%normale)
      jacL%mat(1:dim, 1:dim, if) = jacL%mat(1:dim, 1:dim, if) +&
                            matmul(jacR%mat(1:dim, 1:dim, if), dQrdQl(1:dim, 1:dim) )
      jacR%mat(1:dim, 1:dim, if) = 0._krp
    enddo

  case(bc_inlet_sup) !--------------------- SUPERSONIC INLET
    !nothing to do: dQR/dQL = 0

  case(bc_outlet_sup) !--------------------- SUPERSONIC OUTLET

    do ifb = 1, umesh%boco(ib)%nface
      if  = umesh%boco(ib)%iface(ifb)
      icl = umesh%facecell%fils(if,1)
      icr = umesh%facecell%fils(if,2)
      dQrdQl(1:dim,  1:dim) = 0._krp
      do i = 1, dim
        dQrdQl(i, i) = 1._krp
      enddo
      jacL%mat(1:dim, 1:dim, if) = jacL%mat(1:dim, 1:dim, if) +&
                            matmul(jacR%mat(1:dim, 1:dim, if), dQrdQl(1:dim, 1:dim) )
      jacR%mat(1:dim, 1:dim, if) = 0._krp
    enddo


  endselect

enddo

endsubroutine ns_bocojacobian

!------------------------------------------------------------------------------!
! Changes history
!
! Aug  2010 : creation (bc_geo_sym, bc_inlet_sup, bc_outlet_sup)
!------------------------------------------------------------------------------!
