!------------------------------------------------------------------------------!
! Procedure : scale_mesh                  Authors : J. Gressier
!                                         Created : September 2005
! Fonction
!
!------------------------------------------------------------------------------!
subroutine scale_mesh(mesh, factor)

use TYPHMAKE
use OUTPUT
use MESHBASE
use GEO3D

implicit none

! -- Declaration des entrees --
real(krp) :: factor

! -- Declaration des entrees/sorties --
type(st_mesh)            :: mesh      ! entrees:vertex / sorties:iface

! -- Declaration des variables internes --


! -- Debut de la procedure --


call v3d_eq_mult_t(mesh%vertex(1:mesh%nvtex,1,1), factor)


endsubroutine scale_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Sept 2005 : creation
!------------------------------------------------------------------------------!
