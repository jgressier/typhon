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
integer         :: ip, ic
type(st_nsetat) :: nspri

! -- Debut de la procedure --

print*,'init ns',initns%ptot,  initns%ttot, initns%mach, initns%direction
nspri = pi_ti_mach_dir2nspri(defns%properties(1), initns%ptot,  initns%ttot, &
                                                  initns%mach, initns%direction) 
print*,'init pri',nspri
champ%etatprim%tabscal(1)%scal(:) = nspri%density
champ%etatprim%tabscal(2)%scal(:) = nspri%pressure
champ%etatprim%tabvect(1)%vect(:) = nspri%velocity
print*,'init_ns',nspri

!!if (champ%allocgrad) champ%gradient(:,:,:,:,:) = 0._krp

endsubroutine init_ns_ust

!------------------------------------------------------------------------------!
! Modification history
!
! july 2004 : creation & calculation of uniform primitive variables
!------------------------------------------------------------------------------!


