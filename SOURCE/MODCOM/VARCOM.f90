!----------------------------------------------------------------------------------------
! MODULE : VARCOM                         Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : (cf historique)
!   Variables globales du code TYPHON
!
! Defauts/Limitations/Divers :
!
!----------------------------------------------------------------------------------------
module VARCOM
 

use TYPHMAKE   ! Définition de la précision machine


! -- Variables globales du module -------------------------------------------

character(len=6), parameter :: version = "0.1.3b"

logical        :: mpi_run              ! calcul parallèle MPI ou non
character      :: memory_mode          ! mode d'économie mémoire
character      :: model_mode           ! mode de modélisation physique
integer        :: taille_buffer        ! taille de buffer pour la distribution des calculs


! -- CONSTANTES globales du module -------------------------------------------

! -- Définition des fonctionnalités

logical, parameter :: pass_kdif  = .true.
logical, parameter :: pass_ns    = .false.
logical, parameter :: pass_vort  = .true.
logical, parameter :: pass_mpi   = .false.
logical, parameter :: pass_amr   = .false.
logical, parameter :: pass_coupl_int = .true.
logical, parameter :: pass_coupl_ext = .false.

! -- Constantes "erreurs"

character, parameter :: cnull = ' '
integer,   parameter :: inull = 0

! -- Constantes de définition du mode de calcul cpu/mémoire --

character, parameter :: mode_normal = 'N' ! mode économie : normal
character, parameter :: save_mem    = 'M' ! mode économie : mémoire  minimale (recalcul)
character, parameter :: save_cpu    = 'C' ! mode économie : cpu time minimal  (mise en mémoire)

! -- Constantes de définition du type de modélisation (qualité/hypothèse) --

character, parameter :: model_max = 'X'   ! mode modélisation : strict (hypothèses minimales)
character, parameter :: model_hyp = 'H'   ! mode modélisation : avec hypothèses classiques
character, parameter :: model_sim = 'S'   ! mode modélisation : simpliste

! -- Constantes de définition du type de maillage --

character, parameter :: mshSTR = 'S'      ! maillage structuré
character, parameter :: mshUST = 'U'      ! maillage non structuré
character, parameter :: mshHYB = 'H'      ! maillage hybride

! -- Constantes de définition du type de solveur --

integer, parameter   :: solNS     = 10    ! Equations de Navier-Stokes (EQNS)
integer, parameter   :: solKDIF   = 20    ! Equation  de la chaleur    (EQKDIF)
integer, parameter   :: solVORTEX = 30    ! Méthode intégrale et lagrangienne VORTEX

!définitino de solVORTEX dans MENU_SOLVER

! -- Constantes pour le choix du paramètre "temps" (mnu_project)

character, parameter :: stationnaire   = 'S'
character, parameter :: instationnaire = 'I'
character, parameter :: periodique     = 'P'

! -- Constantes pour le choix du paramètre "typ_coord" (mnu_projMODCOM/VARCOM.f90ect)

character, parameter :: c2dplan  = 'P'
character, parameter :: c2daxi   = 'X'
character, parameter :: c3dgen   = 'G'

! -- Constantes de définition du format de maillage --

character, parameter   :: fmt_CGNS    = 'C'   ! format CGNS
character, parameter   :: fmt_TYPHMSH = 'M'   ! format CGNS
character, parameter   :: fmt_TECPLOT = 'T'   ! format TECPLOT (ascii)
character, parameter   :: fmt_VIGIE   = 'V'   ! format VIGIE
character, parameter   :: fmt_VTK     = 'K'   ! format VTK

! -- Constantes pour le choix du type de sortie des données (centres ou noeuds)

integer, parameter :: outp_NODE         = 01
integer, parameter :: outp_CENTER       = 02
integer, parameter :: outp_COR          = 03 !DEV2602
integer, parameter :: outp_FLUX         = 04 !DEV2602
integer, parameter :: outp_TEMPINTER    = 05 !DEV1404

! -- Constantes pour l'état d'avancement du calcul
integer, parameter :: end_calc          = 01 !DEV2602
integer, parameter :: in_cycle          = 02 !DEV2602

! -- Constantes de définition des conditions aux limites (physique) --

integer, parameter :: bc_connection     = 01
integer, parameter :: bc_coupling       = 02

integer, parameter :: bc_geo_sym        = 10
integer, parameter :: bc_geo_period     = 11
integer, parameter :: bc_geo_periodx    = 12
integer, parameter :: bc_geo_extrapol   = 13

integer, parameter :: bc_wall_adiab     = 20
integer, parameter :: bc_wall_isoth     = 21
integer, parameter :: bc_wall_flux      = 22
integer, parameter :: bc_wall_hconv     = 23

integer, parameter :: bc_farfield       = 30
integer, parameter :: bc_wall           = 32
integer, parameter :: bc_kutta          = 35

! -- Constantes de définition des conditions aux limites (calcul) --

integer, parameter :: bc_calc_ghostcell = 01   ! calcul par cellule fictive
integer, parameter :: bc_calc_ghostface = 02   ! calcul par cellule fictive sur la face
integer, parameter :: bc_calc_flux      = 03   ! calcul par flux, pas de point fictif
integer, parameter :: bc_calc_singpanel = 10   ! calcul implicite de singularités
integer, parameter :: bc_calc_kutta     = 15   ! calcul de condition kutta-joukowski
integer, parameter :: bc_calc_farfield  = 16   ! calcul de condition champ lointain

! -- Constantes de définition des paramètres de conditions aux limites --

integer, parameter :: extrap_quantity   = 1
integer, parameter :: extrap_gradient   = 2

! -- Constantes pour les types de connections ou couplages --
!    au niveau des faces communes du maillage

integer, parameter  :: mesh_match    = 01
integer, parameter  :: mesh_nonmatch = 03
integer, parameter  :: mesh_slide    = 04

! -- Constantes pour le choix du paramètre "typecalcul" (coupling)

integer, parameter  :: compact    = 01
integer, parameter  :: consistant = 02
integer, parameter  :: threed     = 03

! -- Constantes pour le choix du paramètre "mode" (coupling)

integer, parameter  :: fixed  = 01
integer, parameter  :: sensor = 02

! -- Constantes pour le choix du paramètre "solvercoupling" (zonecoupling)
integer, parameter  :: kdif_kdif = 01
integer, parameter  :: kdif_ns   = 02
integer, parameter  :: ns_ns     = 03

! -- Constantes pour le choix du paramètre typ_cor (MENU_ZONECOUPLING) : type de correction
integer, parameter  :: sans       = 01
integer, parameter  :: avant      = 02
integer, parameter  :: apres      = 03
integer, parameter  :: auto       = 04
integer, parameter  :: repart_reg = 05
integer, parameter  :: repart_geo = 06
integer, parameter  :: partiel    = 07
integer, parameter  :: bocoT      = 08
integer, parameter  :: bocoT2     = 09 !DEV1603

! -- Constantes pour le choix du paramètre "activite" (senseur)
!
!integer, parameter  :: fluxcomp = 1
!integer, parameter  :: tempevol = 2


! -- DECLARATIONS -----------------------------------------------------------

contains

!----------------------------------------------------------------------------------------
subroutine init_varcom()

  ! paramètres par défaut

  mpi_run       = .false.
  memory_mode   = mode_normal
  model_mode    = model_max
  taille_buffer = 64   ! 1024 ? 

  ! constantes

endsubroutine init_varcom



!----------------------------------------------------------------------------------------
endmodule VARCOM

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Oct  2002 : création du module
!------------------------------------------------------------------------------!
