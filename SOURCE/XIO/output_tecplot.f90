!------------------------------------------------------------------------------!
! Procedure : output_tecplot              Auteur : J. Gressier
!                                         Date   : Décembre 2002
! Fonction                                Modif  :
!   Ecriture fichier des champs de chaque zone au format TECPLOT
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_tecplot(nom, world, outp_typ)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées --
character(len=strlen) :: nom       ! nom du fichier
type(st_world)        :: world
integer               :: outp_typ

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: izone, i
integer :: info

! -- Debut de la procedure --


open(unit=uf_chpresu, file=trim(nom), form='formatted', iostat = info)

do izone = 1, world%prj%nzone

  write(uf_chpresu,'(a)') 'VARIABLES="X","Y","Z","T"'

  !do i = 1, world%zone(izone)%nmesh_str
  !  call output_tec_str(uf_chpresu, world%zone(izone)%str_mesh(i), &
  !                      world%zone(izone)%field)
  !enddo

  do i = 1, world%zone(izone)%nmesh_ust
    ! attention : ustmesh n'est pas un tableau
    call output_tec_ust(uf_chpresu, world%zone(izone)%ust_mesh, &
                        world%zone(izone)%field, outp_typ)
  enddo

enddo

close(uf_chpresu)

endsubroutine output_tecplot
