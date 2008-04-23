!------------------------------------------------------------------------------!
! MODULE : MENU_INVERSE                              Author  : P. Reulet
!                                                    Created : Octobre 2007
! Fonction                                           Modif   : (cf History)
!   Structure definition for parameter input (calcul inverse)
!
!------------------------------------------------------------------------------!
module MENU_INVERSE

use TYPHMAKE     ! Definition de la precision
use VARCOM

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_INV : options numeriques du calcul inverse
!------------------------------------------------------------------------------!
type mnu_inv
  integer(kip)           :: ncyc_futur                ! number of prospective cycles
  integer(kip)           :: ncyc_sensi                ! periodicity of sensitivity computation
  real(krp)              :: ref_flux                  ! reference flux for sensitivity computation
  character (len=strlen) :: bc_unknown                ! name of BOCO of unknown FLUX
  character (len=strlen) :: bc_tmes                   ! name of BOCO of MEASURES

  character (len=strlen) :: dct_file                  ! filename
  character (len=strlen) :: tmes_file                 ! filename
  integer                ::  dct_funit                ! File Unit of DCT modes
  integer                :: tmes_funit                ! File Unit of Measured Quantities
  integer                :: iz_tmes, iz_unknown       ! zone index where there are tmes/unknown BOCO
  integer                :: ib_tmes, ib_unknown       ! boco index where there are tmes/unknown BOCO
  integer(kip)           :: nflux                     ! size of flux array

  ! DCT modes
  integer(kip)           :: ndctmode                  ! number of DCT modes
  real(krp)              :: xlim(2), ylim(2)          ! dimensions of DCT frame
  integer(kip), pointer  :: modes(:,:)                ! list of I and J DCT modes (1:ndctmode, 1:2)

  ! Measured quantities
  integer(kip)           :: nmes                      ! nombre de points de mesure
  real(krp), pointer     :: tmes_expe(:,:)            ! temperature de paroi mesuree
  real(krp), pointer     :: sensi(:,:,:)              ! matrice de sensibilite (nmode, nmes, nfut)
endtype mnu_inv



! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_mnu_inv
endinterface

! -- Fonctions et Operateurs ------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MNU_INV
!------------------------------------------------------------------------------!
subroutine delete_mnu_inv(definv)
implicit none
type(mnu_inv)  :: definv

  deallocate(definv%modes)
  deallocate(definv%tmes_expe)
  deallocate(definv%sensi)

endsubroutine delete_mnu_inv

endmodule MENU_INVERSE

!------------------------------------------------------------------------------!
! Change history
!
! oct  2007 : creation
!------------------------------------------------------------------------------!
