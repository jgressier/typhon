!------------------------------------------------------------------------------!
! Procedure : cgns2typhon                 Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Conversion d'une structure CGNS complete dans la structure de donnees
!   de TYPHON.
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine cgns2typhon(cgnsworld, typhonworld) 

use CGNS_STRUCT   ! Definition des structures CGNS
use MODWORLD
use DEFZONE          ! Definition des structures TYPHON
use OUTPUT        ! Sorties standard TYPHON


implicit none 

! -- Entrees --
type(st_cgns_world) :: cgnsworld      ! structure des donnees CGNS

! -- Sorties --
type(st_world)      :: typhonworld    ! structure des donnees TYPHON

! -- Variables internes --
integer       :: i                ! indice courant

! -- Debut de procedure

call print_info(2, "* CONVERSION DES DONNEES CGNS -> TYPHON")

typhonworld%prj%nzone = cgnsworld%nbase
allocate(typhonworld%zone(typhonworld%prj%nzone))

do i = 1, typhonworld%prj%nzone

  call cgns2typhon_zone(cgnsworld%base(i), typhonworld%zone(i))

enddo

call print_info(8, "Fin de la conversion CGNS -> TYPHON")


!-------------------------
endsubroutine cgns2typhon
