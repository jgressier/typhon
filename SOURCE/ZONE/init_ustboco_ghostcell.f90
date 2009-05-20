!------------------------------------------------------------------------------!
! Procedure : init_ustboco_ghostcell                 Authors : J. Gressier
!                                                    Created : Mars 2003
! Fonction  
!   Affectation des connectivites entre faces limites et cellules limites
!   pour le type "ghostcell" 
!
!------------------------------------------------------------------------------!
subroutine init_ustboco_ghostcell(ib, defboco, umesh)

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
type(st_ustmesh) :: umesh                ! unstructured mesh

! -- OUTPUTS --

! -- Internal variables --
integer   :: if                          ! face index in boco list
integer   :: ic1, ic2, iface             ! cells and face indexes
type(v3d) :: fn, gc                      ! face normal vector, internal cell center

! -- Body --

! affectation de connectivite face limites -> cellules fictives
! la variable umesh%ncell_lim contient le nombre courant de cellules limites affectees

! le tableau de cellules est cense pouvoir contenir le nombre de cellules fictives (test)

if ((umesh%ncell_lim+umesh%boco(ib)%nface)>(umesh%ncell-umesh%ncell_int)) then
  call erreur("Allocation","Pas assez de cellules allouees pour les cellules fictives")
endif

! -- boucle sur la liste des faces de la condition limite --

do if = 1, umesh%boco(ib)%nface    
  
  ! --  affectation de connectivite face limites -> cellules fictives --

  umesh%ncell_lim = umesh%ncell_lim + 1       ! new ghost cell
  iface = umesh%boco(ib)%iface(if)            ! face index
  ic1   = umesh%facecell%fils(iface,1)        ! internal/reference cell index
  ic2   = umesh%ncell_int + umesh%ncell_lim   ! ghost cell index

  if (umesh%facecell%fils(iface,2) == 0) then
    umesh%facecell%fils(iface,2) = ic2        ! affectation de la cellule fictive
  else
    call erreur("Error in computing connectivity", "Ghost cell has been already affected")
  endif

  ! -- geometrical definition of ghost cell --

  select case(defboco%typ_boco)

  case(bc_geo_sym)
    fn = umesh%mesh%iface(iface,1,1)%normale
    gc = umesh%mesh%centre(ic1,1,1)
    umesh%mesh%volume(ic2,1,1) = umesh%mesh%volume(ic1,1,1)
    umesh%mesh%centre(ic2,1,1) = gc + (2._krp*((umesh%mesh%iface(iface,1,1)%centre-gc).scal.fn))*fn

  case(bc_wall_isoth, bc_wall_flux, bc_wall_adiab)
    umesh%mesh%volume(ic2,1,1) = umesh%mesh%volume(ic1,1,1)
    umesh%mesh%centre(ic2,1,1) = 2._krp*umesh%mesh%iface(iface,1,1)%centre - umesh%mesh%centre(ic1,1,1)

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
