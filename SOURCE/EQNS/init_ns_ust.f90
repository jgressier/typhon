!------------------------------------------------------------------------------!
! Procedure : init_ns_ust                 Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Initialization according parameters
!
! Defauts/Limitations/Divers :
!   CAUTION : only initialization of primitive variables
!
!------------------------------------------------------------------------------!
subroutine init_ns_ust(defns, initns, champ, mesh)

use TYPHMAKE
use DEFFIELD
use MENU_NS
use MENU_INIT

implicit none

! -- Declaration des entrees --
type(mnu_ns)     :: defns
type(st_init_ns) :: initns
type(st_mesh)    :: mesh

! -- Declaration des sorties --
type(st_field) :: champ

! -- Declaration des variables internes --
real(krp)       :: x
integer         :: ip, ic
type(st_nsetat) :: nspri

! -- Debut de la procedure --

!print*,'init ns',initns%ptot,  initns%ttot, initns%mach, initns%direction
nspri = pi_ti_mach_dir2nspri(defns%properties(1), initns%ptot,  initns%ttot, &
                                                  initns%mach, initns%direction) 
!print*,'init pri',nspri
do ic = 1, champ%etatprim%dim
!  x = mesh%centre(ic,1,1)%x
!  print*,'ic,x',ic,x
  champ%etatprim%tabscal(1)%scal(ic) = nspri%density  !*.01*x
  champ%etatprim%tabscal(2)%scal(ic) = nspri%pressure !*.01*x
  champ%etatprim%tabvect(1)%vect(ic) = nspri%velocity !+ v3d(x, 10*x, 0._krp)
enddo
!print*,'init_ns',nspri

!!if (champ%allocgrad) champ%gradient(:,:,:,:,:) = 0._krp

endsubroutine init_ns_ust

!------------------------------------------------------------------------------!
! Modification history
!
! july 2004 : creation & calculation of uniform primitive variables
!------------------------------------------------------------------------------!


