!------------------------------------------------------------------------------!
! Procedure : output_tecplot              Auteur : J. Gressier
!                                         Date   : Decembre 2002
! Fonction                                Modif  : (cf historique)
!   Ecriture fichier des champs de chaque zone au format TECPLOT
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
 
subroutine output_tecplot(nom, world, outp_typ, position, io) 

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MESHBASE

implicit none

! -- Declaration des entrees --
character(len=strlen) :: nom       ! nom du fichier
type(st_world)        :: world
integer               :: outp_typ
integer               :: position !DEV2602
integer               :: io       !DEV2602

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer               :: izone, i, dim, ufc, ir
integer               :: info
type(st_genericfield) :: vfield

! -- Debut de la procedure --
if (position == end_calc) then  !DEV2602

  if ((outp_typ == outp_NODE).or.(outp_typ == outp_CENTER)) then !DEV2602

    ! DEVELOPPEMENT PROVISOIRE
    open(unit=uf_chpresu, file=trim(nom), form='formatted', iostat = info)

    do izone = 1, world%prj%nzone

      select case(world%zone(izone)%defsolver%typ_solver)

      case(solKDIF)

        write(uf_chpresu,'(a)') 'VARIABLES="X","Y","Z","T"'
        call output_tec_ust(uf_chpresu, world%zone(izone)%grid%umesh, &
                            world%zone(izone)%grid%info%field_loc, outp_typ, &
                            world%zone(izone)%defsolver%typ_solver)

      case(solVORTEX)

        write(uf_chpresu,'(a)') 'VARIABLES="X","Y","Z","V","P"'

        !! DEV: allocation
        dim = world%zone(izone)%grid%umesh%nface
        call new(vfield, dim, 1, 2, 0)

        !! DEV: initialisation
        call init_genericfield(vfield, 0._krp, v3d(0._krp, 0._krp, 0._krp))


        do i = 1, vfield%dim
          vfield%tabvect(1)%vect(i) = world%zone(izone)%grid%umesh%mesh%iface(i,1,1)%centre &
                                      - .0000001_krp*world%zone(izone)%grid%umesh%mesh%iface(i,1,1)%normale
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

      case(solNS)

        write(uf_chpresu,'(a)') 'VARIABLES="X","Y","Z","u","v","w","P","T"'
        call output_tec_ust(uf_chpresu, world%zone(izone)%grid%umesh, &
                            world%zone(izone)%grid%info%field_loc, outp_typ, &
                            world%zone(izone)%defsolver%typ_solver, &
                            world%zone(izone)%defsolver%defns)


      endselect

    enddo ! fin boucle : zone

    close(uf_chpresu)

  endif !DEV2602

  if (outp_typ == outp_FLUX) then !DEV2602
    print*, "DEBUG output_tecplot compflux"
    close(uf_compflux)
  endif 

  if (outp_typ == outp_TEMPINTER) then !DEV1404
    print*, "DEBUG output_tecplot tempinter"
    close(uf_tempinter)
  endif 

  if (outp_typ == outp_COR) then !DEV2602
    print*, "DEBUG output_tecplot corr"
    ufc = uf_correction
    do ir = 1, world%prj%ncoupling
      close(ufc)
      ufc = ufc+1
      close(ufc)
      ufc = ufc+1
    enddo
  endif

else !DEV2602
  if (outp_typ == outp_FLUX) then !DEV2602
    call output_tec_flux(nom, world,io)
  endif
  if (outp_typ == outp_TEMPINTER) then !DEV1404
    call output_tec_temp(nom, world,io)
  endif
  if (outp_typ == outp_COR) then !DEV2602
    call output_tec_cor(nom, world,io)
  endif
endif !DEV2602

endsubroutine output_tecplot

!------------------------------------------------------------------------------!
! Historique des modifications
!
! dec  2002 : creation de la procedure
! avr  2004 : cas Vortex
! oct  2004 : field chained list
!------------------------------------------------------------------------------!
