!------------------------------------------------------------------------------!
! Procedure : output_tec_flux             Auteur : E. Radenac / J. Gressier
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : (cf Historique)
!   Affichage au format tecplot des corrections
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine output_tec_cor(nom, lworld, io)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees --
character(len=strlen) :: nom       ! nom du fichier
type(st_world)        :: lworld
integer               :: io        ! indice de la sortie

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: ir, if, ufc, ufc1, ufc2
integer   :: iz1, iz2, ncoupl1, ncoupl2,  nbc1, nbc2
real(krp) :: curtps

! -- Debut de la procedure --
curtps = lworld%info%curtps
ufc = uf_correction

do ir = 1, lworld%prj%ncoupling
  ! calcul des donnees de raccord : indices de raccord, de CL pour 
  ! les deux zones couplees
  call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

  if (curtps == 0) then
    open(unit = ufc, file = trim(lworld%output(io)%fichier)//trim(adjustl(strof(ir,3)))//'_'//&
                            trim(adjustl(strof(1,3)))//'.dat', form = 'formatted')
    write(ufc, '(a)') 'VARIABLES="t","CORRECTION"'
    write(ufc,*) 'ZONE T="'//trim(lworld%zone(iz1)%name)//' / '//trim(lworld%zone(iz2)%name)//'"'
    ufc1 = ufc
    ufc = ufc+1

    open(unit = ufc, file = trim(lworld%output(io)%fichier)//trim(adjustl(strof(ir,3)))//'_'//&
                            trim(adjustl(strof(2,3)))//'.dat', form = 'formatted')
    write(ufc, '(a)') 'VARIABLES="t","CORRECTION"'
    write(ufc,*) 'ZONE T="'//trim(lworld%zone(iz2)%name)//' / '//trim(lworld%zone(iz1)%name)//'"'
    ufc2 = ufc
    ufc = ufc+1
  else
    ufc1 = ufc
    ufc2 = ufc +1
    ufc = ufc + 2
  endif

  do if = 1, lworld%zone(iz1)%gridlist%first%umesh%boco(nbc1)%nface
    write(ufc1,'(2e18.8)') curtps, &
      lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(2)%scal(if)

    write(ufc2,'(2e18.8)') curtps, &
      lworld%zone(iz2)%coupling(ncoupl2)%zcoupling%etatcons%tabscal(2)%scal(if)
  enddo

enddo

endsubroutine output_tec_cor

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev 2004   : creation de la procedure
!------------------------------------------------------------------------------!
