!------------------------------------------------------------------------------!
! Procedure : def_coupling                Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Traitement des paramètres du fichier menu principal
!   Paramètres principaux de couplage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_coupling(block, coupling, zone, nzone, icoupl)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_COUPLING
use DEFZONE

implicit none

! -- Declaration des entrées --
integer                :: nzone
type(st_zone), dimension(nzone) &
                       :: zone
type(rpmblock), target :: block
integer                :: icoupl

! -- Declaration des sorties --
type(mnu_coupling)     :: coupling

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire
integer                  :: iz             ! indice de boucle

! -- Debut de la procedure --
call print_info(2,"* Définition des conditions de calcul de couplage entre zones")

! -- Recherche du BLOCK:COUPLING 
pblock => block
call seekrpmblock(pblock, "COUPLING", icoupl, pcour, nkey)
                           
! -- Détermination des zones couplées

call rpmgetkeyvalstr(pcour, "ZONE1", str)
do iz = 1, nzone
  if (samestring(str, zone(iz)%nom)) then 
    coupling%zone1 = zone(iz)%id
  endif
enddo  

call rpmgetkeyvalstr(pcour, "ZONE2", str)
do iz = 1, nzone
  if (samestring(str, zone(iz)%nom)) then 
    coupling%zone2 = zone(iz)%id
  endif
enddo  

write(str_w,*) ". zone couplée 1  : numéro ",coupling%zone1
call print_info(8,adjustl(str_w))
write(str_w,*) ". zone couplée 2  : numéro ",coupling%zone2
call print_info(8,adjustl(str_w))

! -- Détermination de la configuration des maillages

call rpmgetkeyvalstr(pcour, "MESH", str)

if (samestring(str, "MATCHING" ))      coupling%typ_calc =mesh_match
if (samestring(str, "NONMATCHING" )) coupling%typ_calc = mesh_nonmatch
if (samestring(str, "SLIDING"))  coupling%typ_calc = mesh_slide

select case(coupling%typ_calc)

case(mesh_match) ! Maillages coincidents
  call print_info(10,"    maillages coincidents")
  
case(mesh_nonmatch) ! Maillages non coincidents
  call print_info(10,"    maillages non coincidents")

case(mesh_slide) ! Maillages glissants
  call print_info(10,"    maillages glissants")

case default
  call erreur("lecture de menu","configuration de maillages inconnue")
endselect

! -- Détermination des conditions limites de raccord

call rpmgetkeyvalstr(pcour, "BOCOTYPE", str, "DIRICHLET")

if (samestring(str, "DIRICHLET" ))   coupling%boco = couplingboco_TT
if (samestring(str, "CONVECTION" ))  coupling%boco = couplingboco_CC
if (samestring(str, "CONV_DIR" ))    coupling%boco = couplingboco_CT
if (samestring(str, "DIR_CONV" ))    coupling%boco = couplingboco_TC

select case(coupling%boco)

case(couplingboco_TT) ! Raccord par conditions de Dirichlet
  call print_info(10,"   raccord Dirichlet / Dirichlet")

case(couplingboco_CC) ! Raccord par conditions de convection
  call print_info(10,"   raccord convection / convection")

case(couplingboco_CT) ! Raccord par conditions de convection (zone1) / Dirichlet (zone2)
  call print_info(10,"   raccord convection / Dirichlet")

case(couplingboco_TC) ! Raccord par conditions de Dirichlet (zone1) / convection (zone2)
  call print_info(10,"   raccord Dirichlet / convection")

case default
  call erreur("lecture de menu","Conditions limites de raccord inconnues")
endselect

! -- Détermination du type de calcul d'interpolation

call rpmgetkeyvalstr(pcour, "INTERPOLATION", str)

if (samestring(str, "COMPACT" ))   coupling%typ_interpol = compact
if (samestring(str, "CONSISTANT" )) coupling%typ_interpol = consistant
if (samestring(str, "3D"))  coupling%typ_interpol = threed

select case(coupling%typ_interpol)

case(compact) ! Calcul par interpolation compacte
  call print_info(10,"    calcul par interpolation compacte")
  
case(consistant) ! Calcul par interpolation consistante
  call print_info(10,"    calcul par interpolation consistante")

case(threed) ! Interpolation 3D
  call print_info(10,"    calcul par interpolation 3D")

case default
  call erreur("lecture de menu","Type d'interpolation inconnu")
endselect

! -- Détermination du mode de declenchement du couplage

call rpmgetkeyvalstr(pcour, "MODE", str)

if (samestring(str, "FIXED" ))     coupling%period_mode = fixed
if (samestring(str, "SENSOR" ))    coupling%period_mode = sensor

select case(coupling%period_mode)

case(fixed) ! Pas de temps fixé
  call print_info(10,"    Déclenchement du couplage à pas de temps fixé")
  if (.not.(rpm_existkey(pcour,"PERIOD"))) then
    call erreur("lecture de menu","paramètre PERIOD manquant")
  endif
  call rpmgetkeyvalint(pcour, "PERIOD", coupling%n_tpsbase, 1) ! période fixée à 1 de manière
                                                               ! optionnelle
  
case(sensor) ! Avec senseur
  call print_info(10,"    Déclenchement du couplage par senseur")
  if (.not.(rpm_existkey(pcour,"SENSOR"))) then
    call erreur("lecture de menu","paramètre SENSOR manquant")
  endif
  
!  call rpmgetkeyvalstr(pcour, "SENSOR", str)
!  
!  if (samestring(str, "FLUXCOMPARISON" )) coupling%senseur%mode = fluxcomp
!  if (samestring(str, "TEMPEVOL"))  coupling%senseur%mode = tempevol
!  
!  select case(coupling%senseur%mode)
!  
!  case(fluxcomp) ! Senseur : comparaison de flux
!    call print_info(10,"    senseur par comparaison de flux")
!    if (.not.(rpm_existkey(pcour,"NMIN").or.rpm_existkey(pcour,"NMAX").or.&
!    		rpm_existkey(pcour,"ECARTFLUX"))) then
!      call erreur("lecture de menu","paramètre NMIN ou NMAX ou ECARTFLUX manquant")
!    endif
!    call rpmgetkeyvalint(pcour, "NMIN", coupling%senseur%nmin)
!    call rpmgetkeyvalint(pcour, "NMAX", coupling%senseur%nmax)
!    call rpmgetkeyvalreal(pcour, "ECARTFLUX", coupling%senseur%ecartflux)
!   
!  case(tempevol) ! Senseur : evolution de température
!    call print_info(10,"    senseur sur l'évolution de la température")
!    if (.not.(rpm_existkey(pcour,"NMIN").or.rpm_existkey(pcour,"NMAX").or.&
!    		rpm_existkey(pcour,"EPSILON"))) then
!      call erreur("lecture de menu","paramètre NMIN ou NMAX ou EPSILON manquant")
!    endif
!    call rpmgetkeyvalint(pcour, "NMIN", coupling%senseur%nmin)
!    call rpmgetkeyvalint(pcour, "NMAX", coupling%senseur%nmax)
!    call rpmgetkeyvalreal(pcour, "EPSILON", coupling%senseur%epsilon)
!
!  case default
!    call erreur("lecture de menu","Fonctionnement de senseur inconnu")
!
!  endselect
  
case default
  call erreur("lecture de menu","mode de déclenchement du couplage inconnu")
endselect


! -- Détermination de la valeur du coefficient de correction de flux 

call rpmgetkeyvalreal(pcour, "FLUXCORR_COEF", coupling%corcoef, 0.5_krp)


endsubroutine def_coupling

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai 2003 (v0.0.1b): création de la procédure
! oct 2003          : ajout coef correction de flux
!------------------------------------------------------------------------------!
