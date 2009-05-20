!------------------------------------------------------------------------------!
! Procedure : output_tecplot_ust  
!             
! Fonction    
!   TECPLOT export
!------------------------------------------------------------------------------!
subroutine output_tec_ust(uf, umesh, field, outp_typ, defsolver)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use DEFFIELD
use MENU_GEN
USE MENU_SOLVER

implicit none

! -- INPUTS --
integer          :: uf            ! unite d'ecriture
type(st_ustmesh) :: umesh         ! unstructured mesh
type(st_field)   :: field         ! champ de valeurs
integer          :: outp_typ      ! type de sortie
type(mnu_solver) :: defsolver     ! solver parameters

! -- OUTPUTS --

! -- LOCAL VARIABLES --

! -- BODY --

select case(outp_typ)

case(dataset_node)
  !call output_tec_ust_node(uf, umesh, field)
  call erreur("Development", "Option not available in these new releases")

case(dataset_cell)
  call output_tec_ust_ctr(uf, umesh, field, defsolver)

case default
  call erreur("Internal Error (output_tec_ust)", "unknown dataset location parameter")
endselect

endsubroutine output_tec_ust
!------------------------------------------------------------------------------!
! Changes history
!
! Dec  2002: created
!
!------------------------------------------------------------------------------!
