!------------------------------------------------------------------------------!
! MODULE : OUTPUT                         Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  :
!   Définition des unités d'entrées/sorties du programme TYPHON
!   Définition des procédures d'écritures
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
 
module OUTPUT

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------

! niveaux d'écritures

integer  std_maxlevel ! profondeur d'affichage maximale en sortie standard
integer  log_maxlevel ! profondeur d'affichage maximale en fichier log

! unités d'entrées 

integer  uf_stdin     ! entrée standard
integer  uf_menu      ! menus
integer  uf_mesh      ! maillages
integer  uf_cdlim     ! conditions aux limites

! unités de sorties

integer  uf_stdout    ! sortie standard (informations standard)
integer  uf_log       ! fichier log     (informations détaillées)
integer  uf_monres    ! residual monitor
integer  uf_monphy    ! physical value monitor
integer  uf_residu    ! residus
integer  uf_mesure    ! mesures diverses
integer  uf_chpresu   ! champs de résultats
integer  uf_compflux  ! comparaison des flux à l'interface
integer  uf_correction  ! correction  DEV2602
integer  uf_tempinter ! température interface DEV1404

! unités de entrées/sorties 

integer  uf_reprise   ! fichier reprise

! divers

character(len=256) :: str_w   ! chaine provisoire pour l'écriture sur unité
character(len=6), parameter :: str_std = "[STD] ", &   ! préfixe d'écriture std/log
                               str_log = "      "      ! préfixe d'écriture log/log

! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : init_output                 Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Initialisation des constantes, niveaux et unités
!
!------------------------------------------------------------------------------!
subroutine init_output()
implicit none

! -- Debut de la procedure --

  std_maxlevel = 100  !  15   ! profondeur d'affichage maximale en sortie standard
  log_maxlevel = 100  ! profondeur d'affichage maximale en fichier log

  ! unités d'entrées 
  uf_stdin   = 5     ! entrée standard
  uf_menu    = 10    ! menus
  uf_mesh    = 20    ! maillages
  uf_cdlim   = 30    ! conditions aux limites

  ! unités de sorties
  uf_stdout  = 6    ! sortie standard (informations standard)
  uf_log     = 9    ! fichier log     (informations détaillées)
  uf_monres  = 31   ! monres file : residual monitor
  uf_monphy  = 32   ! monphy file : physical value monitor
  uf_residu  = 40   ! residus
  uf_mesure  = 50   ! mesures diverses
  uf_chpresu = 55   ! champs résultats
  uf_compflux= 56   ! comparaison de flux à l'interface
  uf_tempinter=57   ! DEV1404
  uf_correction = 1000 !DEV2602

  open(unit=uf_log, file = "typhon.log", form="formatted")  

  ! unités de entrées/sorties 
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

! -- Declaration des entrées/sorties --
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

! -- Declaration des entrées/sorties --
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

! -- Declaration des entrées --
  integer          n     ! niveau requis de l'écriture
  character(len=*) str   ! chaine à écrire

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

! -- Declaration des entrées/sorties --
  character(len=*) str

! -- Debut de la procedure --

  call erreur("interne","procédure print_std obsolète")
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

! -- Declaration des entrées/sorties --
  character(len=*) str

! -- Debut de la procedure --

  call erreur("interne","procédure print_log obsolète")
  write(uf_stdout,'(a)') trim(str)
  write(uf_log,'(aa)')    "[OUT] ",trim(str)

endsubroutine print_log
  


endmodule OUTPUT
