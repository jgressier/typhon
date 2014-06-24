!------------------------------------------------------------------------------!
! Procedure : calcboco_ust                             Authors : J. Gressier
!                                                      Created : April 2003
! Fonction 
!   Calcul des conditions aux limites pour maillage non structure
!   Aiguillage des appels selon le type (general ou par solveur)
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust(curtime, defsolver, defspat, umesh, bccon)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
!use MENU_ZONECOUPLING
use DEFZONE
use MGRID ! (for bccon type)

implicit none

! -- INPUTS --
real(krp)              :: curtime
type(mnu_solver)       :: defsolver        ! type d'equation a resoudre
type(mnu_spat)         :: defspat
type(st_ustmesh)       :: umesh             ! maillage en entree, champ en sortie

! -- Inputs/Outputs --
type(st_bccon)         :: bccon
!type(st_genericfield) :: fsend, frecv ! pointer of send or receive fields

! -- Internal variables --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef, nf                  ! index de definitions des conditions aux limites
integer :: nrac                      ! numero de raccord

! ----------------------------------- BODY -----------------------------------


!-----------------------------------------------------------
! only compute physical boundary conditions (idef >= 1)

do ib = 1, umesh%nboco

  idef = umesh%boco(ib)%idefboco
  if (idef >= 1) then

    nf        = umesh%boco(ib)%nface
    bccon%nf  = nf
    allocate(bccon%isend(nf))
    allocate(bccon%irecv(nf))

    select case(bccon%bccon_mode)
    case(bccon_cell_state, bccon_cell_grad)
      bccon%isend(1:nf) = umesh%facecell%fils(umesh%boco(ib)%iface(1:nf), 1)    ! index indirection: internal cell of current BC face
      bccon%irecv(1:nf) = umesh%facecell%fils(umesh%boco(ib)%iface(1:nf), 2)    ! index indirection: ghost    cell of current BC face
    case(bccon_face_state)
      bccon%isend(1:nf) = umesh%boco(ib)%iface(1:nf)
      bccon%irecv(1:nf) = umesh%boco(ib)%iface(1:nf)
    case(bccon_face_grad)
      call error_stop("Internal error: connection mode not yet implemented")
    case default
      call error_stop("Internal error: unknown connection mode")
    endselect

  ! --- Common (geometrical) BOCO ---

  select case(defsolver%boco(idef)%typ_boco)

  case(bc_geo_sym)
    call calcboco_ust_sym(curtime, defsolver%boco(idef), defsolver%defale, defsolver%defmrf, &
           umesh%boco(ib), umesh, bccon, defsolver%nsim)
    
  case(bc_geo_period)
    call error_stop("not expected to manage 'bc_geo_period' in this routine")
    
  case(bc_geo_extrapol)
    call calcboco_ust_extrapol(defsolver%boco(idef), umesh%boco(ib), umesh, bccon)

! PROVISOIRE : a retirer
  case(bc_connect_match)
    call error_stop("!!!DEV!!! 'bc_connection' : Cas non implemente")

  ! --- Solver dependent BOCO ---
  case default 

    select case(defsolver%typ_solver)
      case(solNS)
        call calcboco_ns(curtime, defsolver, defsolver%boco(idef), umesh%boco(ib), umesh, bccon)
      case(solKDIF)
        call calcboco_kdif(curtime, defsolver, defsolver%boco(idef), umesh%boco(ib), umesh, defspat, bccon)
      case(solVORTEX)
        ! rien a faire
      case default
        call error_stop("unknown solver (calcboco_ust)")
    endselect

  endselect

  deallocate(bccon%isend, bccon%irecv)
  
  endif

enddo

endsubroutine calcboco_ust
!------------------------------------------------------------------------------!
! Changes history
!
! avril 2003 : creation de la procedure
! july  2004 : simplification of call tree (uniform or not boundary conditions)
! oct   2004 : field chained list
! Oct   2005 : add connection computation (idef <= 0)
! Feb   2014 : cell/face state/gradient BC (bccon_mode)
!------------------------------------------------------------------------------!
