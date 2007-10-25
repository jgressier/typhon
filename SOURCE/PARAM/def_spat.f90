!------------------------------------------------------------------------------!
! Procedure : def_spat                    Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_spat(block, defsolver, defspat)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block
type(mnu_solver)       :: defsolver

! -- Declaration des sorties --
type(mnu_spat) :: defspat

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

call print_info(5,"- Definition of spatial numerical parameters")

! -- Initialisation --

defspat%calc_grad = .false.

! -- Recherche du BLOCK:SPAT_PARAM --

pblock => block
call seekrpmblock(pblock, "SPAT_PARAM", 0, pcour, nkey)

! DEV : est-ce que la presence du bloc est obligatoire ?
if (nkey /= 1) call erreur("parameters parsing", &
                           "SPAT_PARAM block not found")

defspat%calc_grad = .false.

select case(defsolver%typ_solver)

case(solNS)

  !-----------------------------------------
  ! Euler numerical Scheme

  call rpmgetkeyvalstr(pcour, "SCHEME", str, "HLLC")
  defspat%sch_hyp = inull

  if (samestring(str,"ROE"))             defspat%sch_hyp = sch_roe
  if (samestring(str,"OSHER-NO"))        defspat%sch_hyp = sch_osher_no
  if (samestring(str,"OSHER-IO"))        defspat%sch_hyp = sch_osher_io
  if (samestring(str,"OSHER"))           defspat%sch_hyp = sch_osher_no
  if (samestring(str,"HUS"))             defspat%sch_hyp = sch_efmo
  if (samestring(str,"EFMO"))            defspat%sch_hyp = sch_efmo
  if (samestring(str,"HLL"))             defspat%sch_hyp = sch_hlle
  if (samestring(str,"HLLE"))            defspat%sch_hyp = sch_hlle
  if (samestring(str,"HLLK"))            defspat%sch_hyp = sch_hllk
  if (samestring(str,"HLLC"))            defspat%sch_hyp = sch_hllc
  if (samestring(str,"HLLCK"))           defspat%sch_hyp = sch_hllck
  if (samestring(str,"STEGER-WARMING"))  defspat%sch_hyp = sch_stegwarm
  if (samestring(str,"VANLEER"))         defspat%sch_hyp = sch_vanleer
  if (samestring(str,"EFM"))             defspat%sch_hyp = sch_efm
  if (samestring(str,"KFVS"))            defspat%sch_hyp = sch_efm
  if (samestring(str,"AUSMM"))           defspat%sch_hyp = sch_ausmm

  select case(defspat%sch_hyp)
  case(sch_roe)
    call print_info(7,"  numerical flux : Roe")
  case(sch_hlle)
    call print_info(7,"  numerical flux : HLLE")
  case(sch_hllc)
    call print_info(7,"  numerical flux : HLLC")
  case(sch_vanleer)
    call print_info(7,"  numerical flux : Van Leer")
  case(sch_ausmm)
    call print_info(7,"  numerical flux : AUSM (original scheme, Mach weighting)")
  case default
    call erreur("parameters parsing","unknown numerical scheme")
  endselect

  !-----------------------------------------
  ! Jacobian matrix (if needed)

  call rpmgetkeyvalstr(pcour, "JACOBIAN", str, "HLL")
  defspat%jac_hyp = inull

  if (samestring(str,"HLL"))        defspat%jac_hyp = jac_hll
  if (samestring(str,"HLL-DIAG"))   defspat%jac_hyp = jac_hlldiag
  if (samestring(str,"EFM"))        defspat%jac_hyp = jac_efm

  if (defspat%jac_hyp == inull) &
    call erreur("parameters parsing","unknown numerical scheme for jacobian matrices")

  !-----------------------------------------
  ! Dissipative flux method

  select case(defsolver%defns%typ_fluid)
  case(eqEULER)
  case(eqNSLAM, eqRANS)
    call get_dissipmethod(pcour, "DISSIPATIVE_FLUX", defspat)
  case default
    call erreur("parameters parsing","unexpected fluid dynamical model")
  endselect

  !-----------------------------------------
  ! High resolution method

  defspat%method = cnull
  call rpmgetkeyvalstr(pcour, "HIGHRES", str, "NONE")
  if (samestring(str,"NONE"))       defspat%method = hres_none
  if (samestring(str,"MUSCL"))      defspat%method = hres_muscl
  if (samestring(str,"MUSCL-FAST")) defspat%method = hres_musclfast
  if (samestring(str,"MUSCL-UNS"))  defspat%method = hres_muscluns
  if (samestring(str,"ENO"))        defspat%method = hres_eno
  if (samestring(str,"WENO"))       defspat%method = hres_weno
  if (samestring(str,"SVM"))        defspat%method = hres_svm
  if (samestring(str,"SDM"))        defspat%method = hres_sdm
  
  if (defspat%method == cnull) &
    call erreur("parameters parsing","unexpected high resolution method")
    
  select case(defspat%method)

  ! --------------- first order ---------------------
  case(hres_none)

    call print_info(7,"  No high order extension")

  ! --------------- MUSCL methods ---------------------
  case(hres_muscl, hres_musclfast, hres_muscluns)

    call print_info(7,"  MUSCL second order extension")

    ! -- High resolution order
    !call rpmgetkeyvalint(pcour, "ORDER", defspat%order, 2_kpp)

    ! -- High resolution gradient computation                  
    call get_gradientmethod(pcour, defspat)

    defspat%muscl%limiter = cnull
    call rpmgetkeyvalstr(pcour, "LIMITER", str, "VAN_ALBADA")
    if (samestring(str,"NONE"))        defspat%muscl%limiter = lim_none
    if (samestring(str,"MINMOD"))      defspat%muscl%limiter = lim_minmod
    if (samestring(str,"VAN_ALBADA"))  defspat%muscl%limiter = lim_albada
    if (samestring(str,"ALBADA"))      defspat%muscl%limiter = lim_albada
    if (samestring(str,"VAN_LEER"))    defspat%muscl%limiter = lim_vleer
    if (samestring(str,"VANLEER"))     defspat%muscl%limiter = lim_vleer
    if (samestring(str,"SUPERBEE"))    defspat%muscl%limiter = lim_sbee
    if (samestring(str,"KIM3"))        defspat%muscl%limiter = lim_kim3

    if (defspat%muscl%limiter == cnull) &
      call erreur("parameters parsing","unexpected high resolution limiter")

  ! --------------- SVM methods ---------------------
  case(hres_svm)

    call print_info(7,"  Spectral Volume method")

    call rpmgetkeyvalstr(pcour, "SVM", str, "2QUAD")
    if (samestring(str,"2"))       defspat%svm%sv_meth = svm_2quad
    if (samestring(str,"2TRI"))    defspat%svm%sv_meth = svm_2tri
    if (samestring(str,"2QUAD"))   defspat%svm%sv_meth = svm_2quad
  
    select case(defspat%svm%sv_meth)
    case(svm_2quad)
      call print_info(7,"    second order, splitted into quads (face split)")
      defspat%svm%cv_split       = 3  ! nb of CV in SV
      defspat%svm%intnode        = 1  ! nb of internal added nodes for cell splitting
      defspat%svm%svface_split   = 2  ! nb of CV face per SV face
      defspat%svm%internal_faces = 3  ! number of internal faces (by cell)
      !defspat%svm%external_faces = 6  ! number of external faces (by cell)
      defspat%svm%nb_facepoints  = 1  ! number of integration points by face
    !case(svm_2tri)
    !  call print_info(7,"    second order, splitted into tris (vertex split)")
    !  defspat%svm%intnode        = 1  ! number of internal added nodes for cell splitting
    !  defspat%svm%internal_faces = 3  ! number of internal faces (by cell)
    !  defspat%svm%external_faces = 3  ! number of external faces (by cell)
    !  defspat%svm%nb_facepoints  = 1  ! number of integration points by face
    case default
      call erreur("parameters parsing","unknown numerical scheme")
    endselect

  case default
    call erreur("parameters parsing","unexpected high resolution method (reading limiter)")
  endselect
  

case(solKDIF)

  ! -- Methode de calcul des flux dissipatifs --
  call get_dissipmethod(pcour, "DISSIPATIVE_FLUX", defspat)

case(solVORTEX)

case default
  call erreur("development","unknown solver (defspat)")
endselect


endsubroutine def_spat

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation, lecture de bloc vide
! oct  2003 : choix de la methode de calcul des flux dissipatifs
! mars 2004 : traitement dans le cas solVORTEX
! july 2004 : NS solver parameters
! nov  2004 : NS high resolution parameters
! jan  2006 : basic parameter routines moved to MENU_NUM
! apr  2007 : add SVM method parameters
!------------------------------------------------------------------------------!
