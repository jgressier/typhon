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

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------

! niveaux d'ecritures

integer  std_maxlevel ! profondeur d'affichage maximale en sortie standard
integer  log_maxlevel ! profondeur d'affichage maximale en fichier log

! unites d'entrees 

integer  uf_stdin     ! entree standard
integer  uf_menu      ! menus
integer  uf_mesh      ! maillages
integer  uf_cdlim     ! conditions aux limites

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

! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------


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
  uf_menu    = 10    ! menus
  uf_mesh    = 20    ! maillages
  uf_cdlim   = 30    ! conditions aux limites

  ! unites de sorties
  uf_stdout  = 6    ! sortie standard (informations standard)
  uf_log     = 9    ! fichier log     (informations detaillees)
  uf_monres  = 31   ! monres file : residual monitor
  uf_monphy  = 32   ! monphy file : physical value monitor
  uf_residu  = 40   ! residus
  uf_mesure  = 50   ! mesures diverses
  uf_chpresu = 55   ! champs resultats
  uf_compflux= 56   ! comparaison de flux a l'interface
  uf_tempinter=57   ! DEV1404
  uf_correction = 1000 !DEV2602

  open(unit=uf_log, file = "typhon.log", form="formatted")  

  ! unites de entrees/sorties 
  uf_reprise = 60   ! fichier reprise


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

  write(uf_log,'(aa)')   "[WARNING] ",trim(str)

endsubroutine print_warning
  

!------------------------------------------------------------------------------!
! Procedure : print_info                  Auteur : J. Gressier
!                                         Date   : November 2002
! Fonction                                Modif  :
!   Ecriture en sortie standard et dans le fichier log selon niveaux
!
!------------------------------------------------------------------------------!
subroutine print_info(n, str)
implicit none

! -- Declaration des entrees --
  integer          n     ! niveau requis de l'ecriture
  character(len=*) str   ! chaine a ecrire

! -- Debut de la procedure --

  if (n <= std_maxlevel) then
    write(uf_stdout,'(a)') trim(str)
    write(uf_log,'(aa)')    str_std, trim(str)
  elseif (n <= log_maxlevel) then
    write(uf_log,'(aa)')    str_log, trim(str)
  endif

endsubroutine print_info
  

!------------------------------------------------------------------------------!
! Procedure : print_stdout                Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  :
!   Ecriture en sortie standart et dans le fichier log
!
!------------------------------------------------------------------------------!
subroutine print_std(str)
implicit none

! -- Declaration des entrees/sorties --
  character(len=*) str

! -- Debut de la procedure --

  call erreur("interne","procedure print_std obsolete")
  write(uf_stdout,'(a)') trim(str)
  write(uf_log,'(aa)')    "[OUT] ",trim(str)

endsubroutine print_std
  

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

  call erreur("interne","procedure print_log obsolete")
  write(uf_stdout,'(a)') trim(str)
  write(uf_log,'(aa)')    "[OUT] ",trim(str)

endsubroutine print_log
  


endmodule OUTPUT
