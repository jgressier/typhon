!------------------------------------------------------------------------------!
! Procedure : udf_kdif_aniso                        Auteur : J. Gressier
!                                                   Date   : June 2005
! Fonction                                          Modif  : (cf history)
!   Calcul des flux de conduction de la chaleur : trois methodes
!
!------------------------------------------------------------------------------!
subroutine udf_kdif_aniso(nf, face, tens)

use TYPHMAKE
use MESHBASE
use TENSOR3

implicit none

! -- Declaration des entrees --
integer        :: nf                 ! nb of faces to compute
type(st_face)  :: face(1:nf)         ! geometrical faces

! -- Declaration des sorties --
type(t3d)      :: tens(1:nf) 

! -- Declaration des variables internes --
integer :: if

! -- Debut de la procedure --

do if = 1, nf
  tens(if)%mat(:,:) = 0._krp
  tens(if)%mat(1,1) = 1._krp
  tens(if)%mat(2,2) = 1._krp
  tens(if)%mat(3,3) = 1._krp
enddo

endsubroutine udf_kdif_aniso

!------------------------------------------------------------------------------!
! Change history
!
! june  2005 : creation
!------------------------------------------------------------------------------!
