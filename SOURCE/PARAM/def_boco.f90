!------------------------------------------------------------------------------!
! Procedure : def_boco                    Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf historique)
!   Traitement des param�tres du fichier menu principal
!   Param�tres principaux du projet
!
! Defauts/Limitations/Divers :
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

! -- Declaration des entr�es --
type(rpmblock), target :: block
integer                :: isolver
integer                :: ncoupling
!type(st_ustmesh)       :: ustdom

! -- Declaration des sorties --
type(mnu_solver)                             :: defsolver
type(mnu_zonecoupling), dimension(ncoupling) :: zcoupling

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nboco          ! nombre de conditions aux limites
integer                  :: ib, nkey, iboco
integer                  :: izr            ! indice de parcours du tableau de raccords
character(len=dimrpmlig) :: str            ! cha�ne RPM interm�diaire

! -- Debut de la procedure --

call print_info(5,"- D�finition des conditions aux limites")

! -- Recherche du BLOCK:BOCO

pblock => block
call seekrpmblock(pblock, "BOCO", 0, pcour, nboco)

if (nboco < 1) call erreur("lecture de menu", &
                           "Pas de d�finition de conditions aux limites (BOCO)")

defsolver%nboco = nboco
allocate(defsolver%boco(nboco))
izr = 0 !initialisation

do ib = 1, nboco

  ! -- Initialisation des allocations de tableaux de CL � FALSE
  defsolver%boco(ib)%boco_kdif%alloctemp = .false.
  defsolver%boco(ib)%boco_kdif%allocflux = .false.
  defsolver%boco(ib)%boco_kdif%allochconv = .false.

  ! -- Initialisation des noms de fichier de temp�rature, flux
  defsolver%boco(ib)%boco_kdif%tempfile = cnull
  defsolver%boco(ib)%boco_kdif%fluxfile = cnull
  defsolver%boco(ib)%boco_kdif%hfile = cnull
  defsolver%boco(ib)%boco_kdif%tconvfile = cnull

  call seekrpmblock(pblock, "BOCO", ib, pcour, nkey)

  ! -- D�termination du nom de famille

  call rpmgetkeyvalstr(pcour, "FAMILY", str)
  defsolver%boco(ib)%family = str

  ! -- D�termination du type de condition aux limites 

  call rpmgetkeyvalstr(pcour, "TYPE", str)

  defsolver%boco(ib)%typ_boco = bocotype(str)

  if (defsolver%boco(ib)%typ_boco /= inull) then
    call print_info(8,"    famille "//defsolver%boco(ib)%family(1:12)//": condition "//trim(str))
  else
    call erreur("lecture de menu (def_boco)",trim(str)//" condition aux limites inconnue")
  endif

  ! -- Traitement du couplage

  if (samestring(str, "COUPLING")) then

    ! -- Condition aux limites n�cessairement non uniforme 
    defsolver%boco(ib)%boco_unif = nonuniform

    ! -- Allocation m�moire pour les tableaux de conditions limites
    defsolver%boco(ib)%boco_kdif%alloctemp = .true.
    defsolver%boco(ib)%boco_kdif%allocflux = .true.
    defsolver%boco(ib)%boco_kdif%allochconv = .true.

    ! -- Incr�mentation : num�ro du raccord
    izr = izr + 1

    ! -- D�termination de la zone connect�e par le raccord
    call rpmgetkeyvalstr(pcour, "CONNZONE", str)
    zcoupling(izr)%connzone = str
    
    ! -- D�termination du nom de la famille connect�e par le raccord
    call rpmgetkeyvalstr(pcour, "CONNFAM", str)
    zcoupling(izr)%connfam = str
    
    ! -- Nom de la famille du raccord
    zcoupling(izr)%family = defsolver%boco(ib)%family
    
    ! -- D�termination de la m�thode de calcul du raccord
    call rpmgetkeyvalstr(pcour, "METHOD", str)
  
    if (samestring(str, "FLUX" ))      defsolver%boco(ib)%typ_calc = bc_calc_flux
    if (samestring(str, "GHOSTFACE" )) defsolver%boco(ib)%typ_calc = bc_calc_ghostface
    if (samestring(str, "GHOSTCELL"))  defsolver%boco(ib)%typ_calc = bc_calc_ghostcell
    
    select case(defsolver%boco(ib)%typ_calc)
  
    case(bc_calc_flux) ! M�thode du flux sp�cifique
      call print_info(10,"    m�thode du flux sp�cifique")
    case(bc_calc_ghostface) ! M�thode du flux de face
      call print_info(10,"    m�thode du flux de face")
    case(bc_calc_ghostcell) ! M�thode de la cellule fictive
      call print_info(10,"    m�thode de la cellule fictive")
    case default
      call erreur("lecture de menu","m�thode de calcul du raccord inconnue") 
    endselect

    ! -- Correction : r�partition
    call rpmgetkeyvalstr(pcour, "CORRECTION", str, "AUTO")

    if (samestring(str, "AUTO"))          zcoupling(izr)%typ_cor = auto
    if (samestring(str, "BEF_EXCH"))         zcoupling(izr)%typ_cor = avant
    if (samestring(str, "AFT_EXCH"))         zcoupling(izr)%typ_cor = apres  
    if (samestring(str, "NO"))            zcoupling(izr)%typ_cor = sans 
    if (samestring(str, "SPLIT_REG"))     zcoupling(izr)%typ_cor = repart_reg
    if (samestring(str, "SPLIT_GEO"))     zcoupling(izr)%typ_cor = repart_geo
    if (samestring(str, "SPLIT_PARTIAL")) zcoupling(izr)%typ_cor = partiel
    if (samestring(str, "EQ_BOCOT"))         zcoupling(izr)%typ_cor = bocoT
    if (samestring(str, "NEQ_BOCOT"))        zcoupling(izr)%typ_cor = bocoT2 !DEV1603

    select case(zcoupling(izr)%typ_cor)

    case(auto) ! Application de la correction automatique
      call print_info(10,"    Correction automatique")
      zcoupling(izr)%partcor = 1
    case(avant) ! Application de la correction AVANT l'�change, en 1 fois
      call print_info(10,"    Correction AVANT")
      zcoupling(izr)%partcor = 1
    case(apres) ! Application de la correction APRES l'�change, en 1 fois
      call print_info(10,"    Correction APRES")
      zcoupling(izr)%partcor = 1
    case(sans)   ! Pas de correction
      call print_info(10,"    Pas de correction")
      zcoupling(izr)%partcor = 0
    case(repart_reg) ! r�partition r�guli�re de la correction sur le nombre
                     ! d'it�rations n�cessaires
      call print_info(10,"    Correction r�guli�rement r�partie sur plusieurs it�rations")
      call rpmgetkeyvalreal(pcour, "ITER_PART", zcoupling(izr)%partcor)
    case(repart_geo) ! r�partition g�om�trique de la correction 
      call print_info(10,"    Correction r�partie selon une variation g�om�trique")
      call rpmgetkeyvalreal(pcour, "ITER_PART", zcoupling(izr)%partcor)
    case(partiel) ! correction partielle sur un cycle
      call print_info(10,"    Correction partielle")
      call rpmgetkeyvalreal(pcour, "ITER_PART", zcoupling(izr)%partcor)
    case(bocoT)   ! Correction sur la condition aux limites
      call print_info(10,"    Correction appliqu�e sur la condition limite d'interface")
      zcoupling(izr)%partcor = 1
    case(bocoT2)   ! Correction sur la condition aux limites
      call print_info(10,"    Correction appliqu�e sur la condition limite d'interface")
      zcoupling(izr)%partcor = 1

    case default
      call erreur("lecture de menu","type de correction inconnu") 
    endselect

  ! -- Traitement des conditions aux limites non attach�es � un couplage
  else 
    
    ! -- D�termination de l'uniformit� de la CL (par d�faut : uniforme)
    call rpmgetkeyvalstr(pcour, "UNIFORMITY", str, "UNIFORM")
    defsolver%boco(ib)%boco_unif = inull
    if (samestring(str, "UNIFORM" )) defsolver%boco(ib)%boco_unif = uniform
    if (samestring(str, "NON_UNIFORM" )) defsolver%boco(ib)%boco_unif = nonuniform
    if (defsolver%boco(ib)%boco_unif == inull) &
    call erreur("lecture de menu (def_boco)","Uniformit� de la CL mal d�finie")

    ! Traitement des conditions aux limites communes aux solveurs

    select case(defsolver%boco(ib)%typ_boco)

    case(bc_geo_sym) 
      !call erreur("D�veloppement","'bc_geo_sym' : Cas non impl�ment�")
    
    case(bc_geo_period)
      call erreur("D�veloppement","'bc_geo_period' : Cas non impl�ment�")
    
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
                           defsolver%boco(ib)%boco_ns)
      case(solVORTEX)
        call def_boco_vortex(pcour, defsolver%boco(ib)%typ_boco, &
                           defsolver%boco(ib)%boco_vortex, &
                           defsolver%boco(ib)%boco_unif)
       case default
         call erreur("incoh�rence interne (def_boco)","solveur inconnu")
      endselect

    endselect

    ! Initialisation de l'impl�mentation de la condition aux limites
    defsolver%boco(ib)%typ_calc = bctype_of_boco(isolver, defsolver%boco(ib)%typ_boco)
  endif

enddo

endsubroutine def_boco


!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : cr�ation de la routine
! fev  2004 : ajout des CL propres au solveur VORTEX (cf MENU_VORTEX)
!------------------------------------------------------------------------------!


