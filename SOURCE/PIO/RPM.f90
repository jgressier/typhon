!------------------------------------------------------------------------------!
! MODULE : RPM                            Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  : Novembre 2002 (cf historique)
!   Bibliotheque de procedures pour la lecture de fichier de parametres
!
! Defauts/Limitations/Divers :
!   cf VERSIONS
!------------------------------------------------------------------------------!
module RPM

!use PRECDEF
use STRING

implicit none

! -- Variables globales du module -------------------------------------------

integer,          parameter :: dimrpmlig      = 100
integer,          parameter :: dimrpmname     = 30
character(len=*), parameter :: rpmcommentchar = '!#%'
character,        parameter :: rpmquotechar   = '"'


!------------------------------------------------------------------------------!
!    DECLARATIONS
!------------------------------------------------------------------------------!

type rpmblock
  integer                   :: nblig     ! nombre de lignes du bloc
  character(len=dimrpmname) :: name      ! nom du bloc
  logical                   :: flagblock ! marqueur de lecture de bloc
  logical, dimension(:), pointer &
                            :: flagtxt   ! marqueur de lecture de ligne
  character(len=dimrpmlig), dimension(:), pointer &
                            :: txt       ! contenu du bloc, par ligne
  type(rpmblock), pointer   :: next      ! pointeur sur le bloc suivant
  type(rpmdata),  pointer   :: data      ! pointeur sur le premier bloc DATA
endtype

type rpmdata
  integer                :: nbvar       ! nombre de variables       
  integer                :: nbpts       ! nombre de points
  character(len=dimrpmname), dimension(:), pointer &
                         :: name        ! noms des variables
  real, dimension(:,:), pointer &
                         :: tab         ! tableau de valeurs (nbvar,nbpts)
  type(rpmdata), pointer :: next        ! pointeur sur un éventuel bloc suivant
endtype

! -- INTERFACES -------------------------------------------------------------

!interface new
!  module procedure new_rpmblock
!endinterface

interface seekrpmdata
  module procedure seekrpmdata
endinterface

interface rpmgetvalreal
  module procedure rpmgetvalrealsp, rpmgetvalrealdp
endinterface

interface rpmgetkeyvalreal
  module procedure rpmgetkeyvalrealsp, rpmgetkeyvalrealdp
endinterface

! -- Procédures, Fonctions et Operateurs ------------------------------------
!
! subroutine create_rpmblock  (pblock, name)
! subroutine create_rpmdata   (pdata)
! subroutine dealloc_rpmblock (firstblock)

! subroutine readrpmblock (nio, nerr, iaff, firstblock)
! subroutine readrpmdata  (nio, pdata, entete, iaff)

! subroutine seekinrpmblock (block, str, num, nlig, ntot)
! subroutine seekrpmdata    (block, strvar, num, data, ntot)
! subroutine printrpmblock  (iu, pblock)

! subroutine rpmgetvalreal (str, res)
! subroutine rpmgetvalstr  (str, res)
! subroutine rpmgetvalint  (str, res)

! subroutine rpmgetkeyvalreal (block, key, res)
! subroutine rpmgetkeyvalstr  (block, key, res)
! subroutine rpmgetkeyvalint  (block, key, res)
! 
!integer numvar_inrpmdata
!interface numvar_inrpmdata
!  function numvar_inrpmdata (strvar, data)
!    character(len=*)       :: strvar  ! nom de variable à chercher
!    type(rpmdata), pointer :: data    ! structure de données concernée
!    integer                :: numvar_inrpmdata
!  end
!endinterface

!------------------------------------------------------------------------------!
!    IMPLEMENTATION 
!------------------------------------------------------------------------------!
contains

!------------------------------------------------------------------------------!
! Les procédures des fichiers F90 sont incluses par commande INCLUDE
! L'implémentation doit être interne au module car elle affecte des pointeurs
! en tant que résultat (en particulier, allocation)
! Les déclarations doivent être explicites
!------------------------------------------------------------------------------!

  include "rpm_gestalloc.F90"

  include "rpm_read.F90"

  include "rpm_seek.F90"
 
  include "rpm_get.F90"

!------------------------------------------------------------------------------!


endmodule RPM

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2002 (v0.0.1b): création du module
! juil 2003          : ajout des procédures "get" double précision
!------------------------------------------------------------------------------!

