!------------------------------------------------------------------------------!
! Procedure : def_boco
!
! Fonction
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
!------------------------------------------------------------------------------!
subroutine def_boco(block, isolver, defsolver, zcoupling, ncoupling)
 
use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use MENU_ZONECOUPLING
use USTMESH

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block
integer                :: isolver
integer                :: ncoupling

! -- Declaration des sorties --
type(mnu_solver)                             :: defsolver
type(mnu_zonecoupling), dimension(ncoupling) :: zcoupling

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nboco          ! nombre de conditions aux limites
integer                  :: ib, nkey, iboco
integer                  :: izr            ! indice de parcours du tableau de raccords
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(5,"* Definition of Boundary Conditions (BOCO)")

! -- Recherche du BLOCK:BOCO

pblock => block
call seekrpmblock(pblock, "BOCO", 0, pcour, nboco)
defsolver%nboco = nboco

if (nboco < 1) then

  call print_warning("no BLOCK:BOCO found")

else

defsolver%nboco = nboco
allocate(defsolver%boco(nboco))
izr = 0 !initialisation

do ib = 1, nboco
  
  ! -- Initializations dependant on the solver
  select case(isolver)
  case(solKDIF)
    ! -- Initialization of boco table allocations :  FALSE
    defsolver%boco(ib)%boco_kdif%alloctemp = .false.
    defsolver%boco(ib)%boco_kdif%allocflux = .false.
    defsolver%boco(ib)%boco_kdif%allochconv = .false.

    ! -- Initialization of file names for temperature, flux
    defsolver%boco(ib)%boco_kdif%tempfile = cnull
    defsolver%boco(ib)%boco_kdif%fluxfile = cnull
    defsolver%boco(ib)%boco_kdif%hfile = cnull
    defsolver%boco(ib)%boco_kdif%tconvfile = cnull 
   
  case(solNS)
    ! -- Initialization of boco table allocations :  FALSE
    defsolver%boco(ib)%boco_ns%alloctemp = .false.
    defsolver%boco(ib)%boco_ns%allocflux = .false.
    defsolver%boco(ib)%boco_ns%allochconv = .false.

    ! -- Initialization of file names for temperature, flux
    defsolver%boco(ib)%boco_ns%tempfile = cnull
    defsolver%boco(ib)%boco_ns%fluxfile = cnull
    
  case(solVORTEX)
  case default
     call erreur("incoherence interne (def_boco)","solveur inconnu")
  endselect

  call seekrpmblock(pblock, "BOCO", ib, pcour, nkey)

  ! -- Determination du nom de famille

  call rpmgetkeyvalstr(pcour, "FAMILY", str)
  defsolver%boco(ib)%family = str

  ! -- Determination du type de condition aux limites 

  call rpmgetkeyvalstr(pcour, "TYPE", str)

  defsolver%boco(ib)%typ_boco = bocotype(str)

  if (defsolver%boco(ib)%typ_boco /= inull) then
    call print_info(8,"    family name "//defsolver%boco(ib)%family(1:12)//": condition "//trim(str))
  else
    call erreur("lecture de menu (def_boco)",trim(str)//" unknown boundary condition")
  endif

  ! -- Traitement du couplage

  if (samestring(str, "COUPLING")) then

    ! -- Condition aux limites necessairement non uniforme 
    defsolver%boco(ib)%boco_unif = nonuniform

    ! -- Resource allocation for boundary condition tables
    select case(isolver)
    case(solKDIF)
      defsolver%boco(ib)%boco_kdif%alloctemp = .true.
      defsolver%boco(ib)%boco_kdif%allocflux = .true.
      defsolver%boco(ib)%boco_kdif%allochconv = .true.    
   
    case(solNS)
      defsolver%boco(ib)%boco_ns%alloctemp = .true.
      defsolver%boco(ib)%boco_ns%allocflux = .true.
      defsolver%boco(ib)%boco_ns%allochconv = .true.    
    
    case(solVORTEX)
    case default
      call erreur("incoherence interne (def_boco)","solveur inconnu")
    endselect

    ! -- Incrementation : numero du raccord
    izr = izr + 1

    ! -- Determination de la zone connectee par le raccord
    call rpmgetkeyvalstr(pcour, "CONNZONE", str)
    zcoupling(izr)%connzone = str
    
    ! -- Determination du nom de la famille connectee par le raccord
    call rpmgetkeyvalstr(pcour, "CONNFAM", str)
    zcoupling(izr)%connfam = str
    
    ! -- Nom de la famille du raccord
    zcoupling(izr)%family = defsolver%boco(ib)%family
    
    ! -- Determination de la methode de calcul du raccord
    call rpmgetkeyvalstr(pcour, "METHOD", str)
  
    if (samestring(str, "FLUX" ))      defsolver%boco(ib)%typ_calc = bc_calc_flux
    if (samestring(str, "GHOSTFACE" )) defsolver%boco(ib)%typ_calc = bc_calc_ghostface
    if (samestring(str, "GHOSTCELL"))  defsolver%boco(ib)%typ_calc = bc_calc_ghostcell
    
    select case(defsolver%boco(ib)%typ_calc)
  
    case(bc_calc_flux) ! Methode du flux specifique
      call print_info(10,"    methode du flux specifique")
    case(bc_calc_ghostface) ! Methode du flux de face
      call print_info(10,"    methode du flux de face")
    case(bc_calc_ghostcell) ! Methode de la cellule fictive
      call print_info(10,"    methode de la cellule fictive")
    case default
      call erreur("lecture de menu","methode de calcul du raccord inconnue") 
    endselect

    ! -- Correction : repartition
    call rpmgetkeyvalstr(pcour, "CORRECTION", str, "AUTO")

    if (samestring(str, "AUTO"))          zcoupling(izr)%typ_cor = auto
    if (samestring(str, "BEF_EXCH"))      zcoupling(izr)%typ_cor = avant
    if (samestring(str, "AFT_EXCH"))      zcoupling(izr)%typ_cor = apres  
    if (samestring(str, "NO"))            zcoupling(izr)%typ_cor = sans 
    if (samestring(str, "SPLIT_REG"))     zcoupling(izr)%typ_cor = repart_reg
    if (samestring(str, "SPLIT_GEO"))     zcoupling(izr)%typ_cor = repart_geo
    if (samestring(str, "SPLIT_PARTIAL")) zcoupling(izr)%typ_cor = partiel
    if (samestring(str, "EQ_BOCOT"))      zcoupling(izr)%typ_cor = bocoT
    if (samestring(str, "NEQ_BOCOT"))     zcoupling(izr)%typ_cor = bocoT2 !DEV1603
    if (samestring(str, "DISTRIBUTED"))   zcoupling(izr)%typ_cor = distributed

    select case(zcoupling(izr)%typ_cor)

    case(auto) ! Application de la correction automatique
      call print_info(10,"    Correction automatique")
      zcoupling(izr)%partcor = 1
    case(avant) ! Application de la correction AVANT l'echange, en 1 fois
      call print_info(10,"    Correction AVANT")
      zcoupling(izr)%partcor = 1
    case(apres) ! Application de la correction APRES l'echange, en 1 fois
      call print_info(10,"    Correction APRES")
      zcoupling(izr)%partcor = 1
    case(sans)   ! Pas de correction
      call print_info(10,"    Pas de correction")
      zcoupling(izr)%partcor = 0
    case(repart_reg) ! repartition reguliere de la correction sur le nombre
                     ! d'iterations necessaires
      call print_info(10,"    Correction regulierement repartie sur plusieurs iterations")
      call rpmgetkeyvalreal(pcour, "ITER_PART", zcoupling(izr)%partcor)
    case(repart_geo) ! repartition geometrique de la correction 
      call print_info(10,"    Correction repartie selon une variation geometrique")
      call rpmgetkeyvalreal(pcour, "ITER_PART", zcoupling(izr)%partcor)
    case(partiel) ! correction partielle sur un cycle
      call print_info(10,"    Correction partielle")
      call rpmgetkeyvalreal(pcour, "ITER_PART", zcoupling(izr)%partcor)
    case(bocoT)   ! Correction sur la condition aux limites
      call print_info(10,"    Correction appliquee sur la condition limite d'interface")
      zcoupling(izr)%partcor = 1
    case(bocoT2)   ! Correction sur la condition aux limites
      call print_info(10,"    Correction appliquee sur la condition limite d'interface")
      zcoupling(izr)%partcor = 1
    case(distributed)   ! Correction ditributed uniformly all over the cycle
      call print_info(10,"    Correction distributed all over the cycle")
      zcoupling(izr)%partcor = 1
    case default
      call erreur("lecture de menu","type de correction inconnu") 
    endselect

    ! -- read radiating parameters --

    defsolver%boco(ib)%boco_kdif%radiating = -1_kpp
    call rpmgetkeyvalstr(pcour, "RADIATING", str, "NONE")
    if (samestring(str, "NONE"))    defsolver%boco(ib)%boco_kdif%radiating = rad_none
    if (samestring(str, "SIMPLE"))  defsolver%boco(ib)%boco_kdif%radiating = rad_direct
    if (samestring(str, "DIRECT"))  defsolver%boco(ib)%boco_kdif%radiating = rad_direct
    if (samestring(str, "COUPLED")) defsolver%boco(ib)%boco_kdif%radiating = rad_coupled
    if (defsolver%boco(ib)%boco_kdif%radiating == -1_kpp) then
      call erreur("parameter reading", "unknown option for RADIATING keyword")
    endif

    call rpmgetkeyvalreal(pcour, "EMMISSIVITY", defsolver%boco(ib)%boco_kdif%emmissivity, 1._krp)
    call rpmgetkeyvalreal(pcour, "RAD_TINF",    defsolver%boco(ib)%boco_kdif%rad_Tinf,    0._krp)

  ! -- Traitement des conditions aux limites non attachees a un couplage
  else 
    
    ! -- Determination de l'uniformite de la CL (par defaut : uniforme)
    call rpmgetkeyvalstr(pcour, "UNIFORMITY", str, "UNIFORM")
    defsolver%boco(ib)%boco_unif = inull
    if (samestring(str, "UNIFORM" )) defsolver%boco(ib)%boco_unif = uniform
    if (samestring(str, "NON_UNIFORM" )) defsolver%boco(ib)%boco_unif = nonuniform
    if (defsolver%boco(ib)%boco_unif == inull) &
    call erreur("lecture de menu (def_boco)","Uniformite de la CL mal definie")

    ! Traitement des conditions aux limites communes aux solveurs

    select case(defsolver%boco(ib)%typ_boco)

    case(bc_geo_sym) 
      !call erreur("Developpement","'bc_geo_sym' : Cas non implemente")
    
    case(bc_geo_period)
      call erreur("Developpement","'bc_geo_period' : Cas non implemente")
    
    case(bc_geo_extrapol)
      call rpmgetkeyvalstr(pcour, "ORDER", str, "QUANTITY")
      defsolver%boco(ib)%order_extrap = inull
      if (samestring(str, "QUANTITY" )) defsolver%boco(ib)%order_extrap = extrap_quantity
      if (samestring(str, "GRADIENT" )) defsolver%boco(ib)%order_extrap = extrap_gradient
      if (defsolver%boco(ib)%order_extrap == inull) &
        call erreur("lecture de menu (def_boco)","ordre d'extrapolation inconnu")
   
    case default    
      select case(isolver)
      case(solKDIF)
        call def_boco_kdif(pcour, defsolver%boco(ib)%typ_boco, &
                           defsolver%boco(ib)%boco_kdif, &
                           defsolver%boco(ib)%boco_unif)
      case(solNS)
        call def_boco_ns(pcour, defsolver%boco(ib)%typ_boco, &
                           defsolver%boco(ib)%boco_ns, &
                           defsolver%boco(ib)%boco_unif)
      case(solVORTEX)
        call def_boco_vortex(pcour, defsolver%boco(ib)%typ_boco, &
                           defsolver%boco(ib)%boco_vortex, &
                           defsolver%boco(ib)%boco_unif)
       case default
         call erreur("Internal error (def_boco)","unknown solver")
      endselect

    endselect

    call rpmgetkeyvalstr(pcour, "SAVE_HISTORY", str, "NONE")
    defsolver%boco(ib)%save_history = -1
    if (samestring(str, "NONE" ))           defsolver%boco(ib)%save_history = bchisto_none
    if (samestring(str, "QUANTITY" ))       defsolver%boco(ib)%save_history = bchisto_quantity
    if (samestring(str, "FLUX" ))           defsolver%boco(ib)%save_history = bchisto_flux
    if (samestring(str, "QUANTITY-FLUX" ))  defsolver%boco(ib)%save_history = bchisto_quantity + bchisto_flux
    if (defsolver%boco(ib)%save_history == -1) then
        call erreur("Internal error (def_boco)","unknown SAVE_HISTORY option")
    endif
    

    ! Initialisation de l'implementation de la condition aux limites
    defsolver%boco(ib)%typ_calc = bctype_of_boco(isolver, defsolver%boco(ib)%typ_boco)
  endif

enddo

endif

endsubroutine def_boco


!------------------------------------------------------------------------------!
! Changes History
!
! mars 2003 : creation de la routine
! fev  2004 : ajout des CL propres au solveur VORTEX (cf MENU_VORTEX)
! Apr  2008: boundary condition history
!------------------------------------------------------------------------------!


