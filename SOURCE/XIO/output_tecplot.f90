!------------------------------------------------------------------------------!
! Procedure : output_tecplot              Auteur : J. Gressier
!                                         Date   : Décembre 2002
! Fonction                                Modif  : (cf historique)
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
integer               :: outp_typ  ! type de représentation (NODE/CENTER)

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer               :: izone, i, dim
integer               :: info
type(st_genericfield) :: vfield


! -- Debut de la procedure --

! DEVELOPPEMENT PROVISOIRE

open(unit=uf_chpresu, file=trim(nom), form='formatted', iostat = info)

do izone = 1, world%prj%nzone

  select case(world%zone(izone)%defsolver%typ_solver)

  case(solKDIF)

    write(uf_chpresu,'(a)') 'VARIABLES="X","Y","Z","T"'
    call output_tec_ust(uf_chpresu, world%zone(izone)%grid%umesh, &
                        world%zone(izone)%grid%field, outp_typ)

  case(solVORTEX)

    write(uf_chpresu,'(a)') 'VARIABLES="X","Y","Z","V","P"'

    !! DEV: allocation
    dim = world%zone(izone)%grid%umesh%nface
    call new(vfield, dim, 1, 2, 0)

    !! DEV: initialisation
    call init_genericfield(vfield, 0._krp, v3d(0._krp, 0._krp, 0._krp))

    do i = 1, vfield%dim
      vfield%tabvect(1)%vect(i) = world%zone(izone)%grid%umesh%mesh%iface(i,1,1)%centre &
                                  - .0000001*world%zone(izone)%grid%umesh%mesh%iface(i,1,1)%normale
    enddo

    call calc_induced_velocities(world%zone(izone)%defsolver,   &
                                 vfield%tabvect(1)%vect(1:dim), &
                                 vfield%tabvect(2)%vect(1:dim), dim)

    do i = 1, vfield%dim
      vfield%tabscal(1)%scal(i) = 1._krp - (abs(vfield%tabvect(2)%vect(i))/100._krp)**2
    enddo

    do i = 1, vfield%dim
      write(uf_chpresu, '(5(g16.8))') vfield%tabvect(1)%vect(i), &
                                      abs(vfield%tabvect(2)%vect(i)),&
                                      vfield%tabscal(1)%scal(i)
    enddo

    call delete(vfield)

  endselect

enddo ! fin boucle : zone

close(uf_chpresu)

endsubroutine output_tecplot

!------------------------------------------------------------------------------!
! Historique des modifications
!
! dec  2002 : création de la procédure
! avr  2004 : cas Vortex
!------------------------------------------------------------------------------!
