!------------------------------------------------------------------------------!
! Procedure : init_ustboco_ghostcell                 Authors : J. Gressier
!                                                    Created : Mars 2003
! Fonction  
!   Affectation des connectivites entre faces limites et cellules limites
!   pour le type "ghostcell" 
!
!------------------------------------------------------------------------------!
subroutine init_ustboco_ghostcell(ib, defboco, ust_mesh)

use TYPHMAKE
!use VARCOM
use OUTPUT
use USTMESH
use MENU_BOCO

implicit none

! -- INPUTS --
integer        :: ib                     ! numero de condition aux limites
type(mnu_boco) :: defboco                ! parametres du solveur

! -- INPUTS/OUTPUTS --
type(st_ustmesh) :: ust_mesh             ! maillage et connectivites

! -- OUTPUTS --

! -- Internal variables --
integer   :: if                          ! face index in boco list
integer   :: ic1, ic2, iface             ! cells and face indexes
type(v3d) :: fn, gc                      ! face normal vector, internal cell center

! -- Body --

! affectation de connectivite face limites -> cellules fictives
! la variable ust_mesh%ncell_lim contient le nombre courant de cellules limites affectees

! le tableau de cellules est cense pouvoir contenir le nombre de cellules fictives (test)

if ((ust_mesh%ncell_lim+ust_mesh%boco(ib)%nface)>(ust_mesh%ncell-ust_mesh%ncell_int)) then
  call erreur("Allocation","Pas assez de cellules allouees pour les cellules fictives")
endif

! -- boucle sur la liste des faces de la condition limite --

do if = 1, ust_mesh%boco(ib)%nface    
  
  ! --  affectation de connectivite face limites -> cellules fictives --

  ust_mesh%ncell_lim = ust_mesh%ncell_lim + 1       ! new ghost cell
  iface = ust_mesh%boco(ib)%iface(if)               ! face index
  ic1   = ust_mesh%facecell%fils(iface,1)           ! internal/reference cell index
  ic2   = ust_mesh%ncell_int + ust_mesh%ncell_lim   ! ghost cell index

  if (ust_mesh%facecell%fils(iface,2) == 0) then
    ust_mesh%facecell%fils(iface,2) = ic2        ! affectation de la cellule fictive
  else
    call erreur("Error in computing connectivity", "Ghost cell has been already affected")
  endif

  ! -- geometrical definition of ghost cell --

  select case(defboco%typ_boco)

  case(bc_geo_sym)
    fn = ust_mesh%mesh%iface(iface,1,1)%normale
    gc = ust_mesh%mesh%centre(ic1,1,1)
    ust_mesh%mesh%volume(ic2,1,1) = ust_mesh%mesh%volume(ic1,1,1)
    ust_mesh%mesh%centre(ic2,1,1) = gc + (2._krp*((ust_mesh%mesh%iface(iface,1,1)%centre-gc).scal.fn))*fn

  case(bc_wall_isoth, bc_wall_flux, bc_wall_adiab)
    ust_mesh%mesh%volume(ic2,1,1) = ust_mesh%mesh%volume(ic1,1,1)
    ust_mesh%mesh%centre(ic2,1,1) = 2._krp*ust_mesh%mesh%iface(iface,1,1)%centre - ust_mesh%mesh%centre(ic1,1,1)

  case(bc_geo_period)
    call erreur("Development", "Initialization of periodic boundary conditions not yet implemented")

  case default
    call erreur("Internal error", "unknown type of boundary conditions with ghostcell implementation")
  endselect

enddo

endsubroutine init_ustboco_ghostcell
!------------------------------------------------------------------------------!
! Changes history
!
! Aug  2005 : created (symmetry boundary condition)
!------------------------------------------------------------------------------!
