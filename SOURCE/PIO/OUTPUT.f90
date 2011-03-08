!------------------------------------------------------------------------------!
! MODULE : OUTPUT                         Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  :
!   Definition des unites d'entrees/sorties du programme TYPHON
!   Definition des procedures d'ecritures
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module OUTPUT

use IO_UNIT
use VARCOM
use STRING

implicit none

! -- Variables globales du module -------------------------------------------

logical :: debug_mode

! niveaux d'ecritures

integer  std_maxlevel ! profondeur d'affichage maximale en sortie standard
integer  log_maxlevel ! profondeur d'affichage maximale en fichier log

! unites d'entrees

integer  uf_stdin     ! entree standard

! unites de sorties

integer  uf_stdout    ! sortie standard (informations standard)
integer  uf_log       ! fichier log     (informations detaillees)
integer  uf_monres    ! residual monitor
integer  uf_monphy    ! physical value monitor
integer  uf_residu    ! residus
integer  uf_mesure    ! mesures diverses
integer  uf_chpresu   ! champs de resultats
integer  uf_compflux  ! comparaison des flux a l'interface
integer  uf_correction  ! correction  DEV2602
integer  uf_tempinter ! temperature interface DEV1404

! unites de entrees/sorties

integer  uf_reprise   ! fichier reprise

! divers

character(len=256) :: str_w   ! chaine provisoire pour l'ecriture sur unite
character(len=6), parameter :: str_std = "[STD] ", &   ! prefixe d'ecriture std/log
                               str_log = "      "      ! prefixe d'ecriture log/log

character, parameter :: newline_char = char(10)
character, parameter :: carriage_char = char(13)

! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------

interface erreur   ! for backward compatibility
  module procedure old_error
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : init_output                 Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Initialisation des constantes, niveaux et unites
!
!------------------------------------------------------------------------------!
subroutine init_output()
implicit none

! -- Debut de la procedure --

  std_maxlevel = 100  !  15   ! profondeur d'affichage maximale en sortie standard
  log_maxlevel = 100  ! profondeur d'affichage maximale en fichier log

  ! unites d'entrees
  uf_stdin   = 5     ! entree standard

  ! unites de sorties
  uf_stdout  = 6    ! sortie standard (informations standard)
  uf_log     = getnew_io_unit()    ! fichier log     (informations detaillees)
  uf_monres  = getnew_io_unit()   ! monres file : residual monitor
  uf_monphy  = getnew_io_unit()   ! monphy file : physical value monitor
  uf_residu  = getnew_io_unit()   ! residus
  uf_mesure  = getnew_io_unit()   ! mesures diverses
  if (uf_log    <= 0) call error_stop("IO unit management: cannot find free unit")
  if (uf_monres <= 0) call error_stop("IO unit management: cannot find free unit")
  if (uf_monphy <= 0) call error_stop("IO unit management: cannot find free unit")
  if (uf_residu <= 0) call error_stop("IO unit management: cannot find free unit")
  if (uf_mesure <= 0) call error_stop("IO unit management: cannot find free unit")
  uf_chpresu = 1055   ! champs resultats
  uf_compflux= 56   ! comparaison de flux a l'interface
  uf_tempinter=57   ! DEV1404
  uf_correction = 1000 !DEV2602

  open(unit=uf_log, file = "typhon.log", form="formatted")

  ! unites de entrees/sorties
  uf_reprise = getnew_io_unit()   ! fichier reprise

  debug_mode= .false.

endsubroutine init_output


!------------------------------------------------------------------------------!
! Procedure : print_etape                 Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  :
!   Ecriture en sortie standard et dans le fichier log des entetes de commandes
!
!------------------------------------------------------------------------------!
subroutine print_etape(str)
implicit none

! -- Declaration des entrees/sorties --
  character(len=*) str

! -- Debut de la procedure --

  call print_info(0,   "")
  call print_info(100, repeat('#',len_trim(str)+8))
  call print_info(0,   trim(str))

endsubroutine print_etape


!------------------------------------------------------------------------------!
! Procedure : print_warning               Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  :
!   Ecriture dans le fichier log des warnings
!
!------------------------------------------------------------------------------!
subroutine print_warning(str)
implicit none

! -- Declaration des entrees/sorties --
  character(len=*) str

! -- Debut de la procedure --

  write(uf_log,'(a,a)')   "[WARNING] ",trim(str)

endsubroutine print_warning


!------------------------------------------------------------------------------!
! Procedure : print_master 
! Fonction  : only master proc can write text
!------------------------------------------------------------------------------!
subroutine print_master(n, str)
implicit none
! -- INPUTS --
integer          n     ! niveau requis de l'ecriture
character(len=*) str   ! chaine a ecrire
! -- BODY --

if ((.not.mpi_run).or.(myprocid == 1)) then
  call print_info(n, str)
endif

endsubroutine print_master

!------------------------------------------------------------------------------!
! Procedure : print_info
! Fonction  : Write text on standart input with PROC number if necessary
!------------------------------------------------------------------------------!
subroutine print_info(n, str)
implicit none
! -- INPUTS --
integer          n        ! niveau requis de l'ecriture
character(len=*) str      ! chaine a ecrire
character(len=6) mpipre   ! chaine a ecrire
! -- BODY --

if (mpi_run) then
  mpipre = '['//strof_full_int(myprocid, 3)//'] '
else
  mpipre = ''
endif

if (n <= std_maxlevel) then
  write(uf_stdout,'(a,a)') trim(mpipre), trim(str)
  write(uf_log,   '(a,a)') trim(mpipre), trim(str)
elseif (n <= log_maxlevel) then
  write(uf_log,'(a,a)')    trim(mpipre), trim(str)
endif

endsubroutine print_info


!------------------------------------------------------------------------------!
! Procedure : error
! Fonction  : write error and stop executation
!------------------------------------------------------------------------------!
subroutine error_stop(str)
implicit none
! -- INPUTS --
character(len=*):: str
! -- BODY --

call print_info(1, str)
stop 1

endsubroutine error_stop

!------------------------------------------------------------------------------!
! Procedure : error (backward compatibility)
! Fonction  : write error and stop executation
!------------------------------------------------------------------------------!
subroutine old_error(str1, str2)
implicit none
! -- INPUTS --
character(len=*):: str1            ! chaine 1
character(len=*):: str2            ! chaine 2
! -- BODY --

call error_stop(trim(str1)//": "//trim(str2))

endsubroutine old_error


!------------------------------------------------------------------------------!
! Procedure : print_log                   Auteur : J. Gressier
 !                                         Date   : Juillet 2002
! Fonction                                Modif  :
!   Ecriture en sortie standart et dans le fichier log
!
!------------------------------------------------------------------------------!
subroutine print_log(str)
implicit none

! -- Declaration des entrees/sorties --
  character(len=*) str

! -- Debut de la procedure --

  call erreur("internal","print_log subroutine obsolete")
  write(uf_stdout,'(a)') trim(str)
  write(uf_log,'(a,a)')    "[OUT] ",trim(str)

endsubroutine print_log


!------------------------------------------------------------------------------!
! Procedure : print_debug                   Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  :
!   Ecriture en sortie standart et dans le fichier debug
!
!------------------------------------------------------------------------------!
subroutine print_debug(str)
implicit none

! -- Declaration des entrees/sorties --
  character(len=*) str

! -- Debut de la procedure --

  if (debug_mode) then
    write(uf_stdout,'(a,a)') "[DEBUG] ",trim(str)
    write(uf_log,   '(a,a)') "[DEBUG] ",trim(str)
  endif

endsubroutine print_debug


!------------------------------------------------------------------------------!
! Procedure : writestr
!------------------------------------------------------------------------------!
subroutine writestr(unit, str)
implicit none
integer,          intent(in) :: unit
character(len=*), intent(in) :: str

  write(unit) str//newline_char

endsubroutine writestr


!------------------------------------------------------------------------------!
! Procedure : writereturn
!------------------------------------------------------------------------------!
subroutine writereturn(unit)
implicit none
integer,          intent(in) :: unit

  write(unit) newline_char

endsubroutine writereturn


endmodule OUTPUT
!------------------------------------------------------------------------------!
! Change history
!
! July 2002 : creation
! Nov  2002 : print_info
! Aug  2005 : debug output
! Feb  2007 : English translation
!------------------------------------------------------------------------------!
