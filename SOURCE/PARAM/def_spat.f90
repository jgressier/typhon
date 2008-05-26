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
subroutine def_spat(block, defsolver, defspat, defmesh)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MENU_MESH

implicit none

! -- INPUTS --
type(rpmblock), target :: block
type(mnu_solver)       :: defsolver

! -- OUTPUTS --
type(mnu_spat) :: defspat
type(mnu_mesh) :: defmesh

! -- Internal variables --
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

  if (samestring(str,"RUSANOV"))         defspat%sch_hyp = sch_rusanov
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
  case(sch_rusanov)
    call print_info(7,"  numerical flux : Rusanov")
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
  if (samestring(str,"RUSANOV"))    defspat%jac_hyp = jac_rusanov
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

    call print_info(7,"  MUSCL second order extension: "//trim(str))

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
    call print_info(7,"    limiter     : "//trim(str))

    if (defspat%muscl%limiter == cnull) &
      call erreur("parameters parsing","unexpected high resolution limiter")

  ! --------------- SVM methods ---------------------
  case(hres_svm)

    call print_info(7,"  Spectral Volume method")

    call rpmgetkeyvalstr(pcour, "SVM", str, "2QUAD")
    if (samestring(str,"2")) then
       defspat%svm%sv_order = svm_2
       defspat%svm%sv_partition = svm_2quad
    endif
    if (samestring(str,"2TRI"))    then 
       defspat%svm%sv_order = svm_2
       defspat%svm%sv_partition = svm_2tri
    endif
    if (samestring(str,"2QUAD"))  then
       defspat%svm%sv_order = svm_2
       defspat%svm%sv_partition = svm_2quad
    endif 
    if (samestring(str,"3"))    then
       defspat%svm%sv_order = svm_3
       defspat%svm%sv_partition = svm_3wang
    endif 
    if (samestring(str,"3WANG")) then
       defspat%svm%sv_order = svm_3
       defspat%svm%sv_partition = svm_3wang
    endif 
    if (samestring(str,"3KRIS"))  then
       defspat%svm%sv_order = svm_3
       defspat%svm%sv_partition = svm_3kris
    endif   
    if (samestring(str,"3KRIS2"))  then
       defspat%svm%sv_order = svm_3
       defspat%svm%sv_partition = svm_3kris2
    endif  

    select case(defspat%svm%sv_partition)
    case(svm_2quad)
      call print_info(7,"    second order, splitted into quads (face split)")
      defmesh%splitmesh = split_svm2quad    
    case(svm_2tri)
      call print_info(7,"    second order, splitted into tris (vertex split)")
    case(svm_3wang)
      call print_info(7,"    third order, splitted into 3 quads and 3 pentagons: original partition by Wang")
      defmesh%splitmesh = split_svm3wang
    case(svm_3kris)
      call print_info(7,"    third order, splitted into 3 quads and 3 pentagons: 1st optimised partition by Abeele")
      defmesh%splitmesh = split_svm3kris
    case(svm_3kris2)
      call print_info(7,"    third order, splitted into 3 quads and 3 pentagons: 2nd optimised partition by Abeele")
      defmesh%splitmesh = split_svm3kris2
    case default
      call erreur("parameters parsing","unknown SVM method")
    endselect
    
    call init_svmparam(defspat%svm)
    call init_svmweights(defspat%svm)

    if (defspat%svm%sv_order.ge.3) then
    call rpmgetkeyvalstr(pcour, "SVMFLUX", str, "QUAD_Q")
    if (samestring(str,"QUAD_Q"))        defspat%svm%sv_flux = svm_fluxQ
    if (samestring(str,"QUAD_F"))      defspat%svm%sv_flux = svm_fluxF

  select case(defspat%svm%sv_flux)
    case(svm_fluxQ)
      call print_info(7,"  SVM flux on Gauss points: averaged quantities")
    case(svm_fluxF)
      call print_info(7,"  SVM flux on Gauss points: averaged fluxes")
    case default
      call erreur("parameters parsing","unknown method for SVM flux on Gauss points")
  endselect

    endif

  case default
    call erreur("parameters parsing","unexpected high resolution method (reading limiter)")
  endselect

  ! --- Post-Limitation method ---
 
  call rpmgetkeyvalstr(pcour, "POST-LIMITER", str, "NONE")
  if (samestring(str,"NONE"))        defspat%postlimiter = postlim_none
  if (samestring(str,"MONOTONIC0"))  defspat%postlimiter = postlim_monotonic0
  if (samestring(str,"MONOTONIC1"))  defspat%postlimiter = postlim_monotonic1
  if (samestring(str,"MONOTONIC2"))  defspat%postlimiter = postlim_monotonic2
  call print_info(7,"    post-limiter: "//trim(str))

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
