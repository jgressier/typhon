!------------------------------------------------------------------------------!
! Procedure : readcgnszone                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Lecture d'une zone d'un fichier CGNS
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readcgnszone(unit, ib, iz, zone) 

use CGNSLIB       ! définition des mots-clefs
use CGNS_STRUCT   ! Définition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrées --
integer             :: unit       ! numéro d'unité pour la lecture
integer             :: ib, iz     ! numéro de base et de zone

! -- Sorties --
type(st_cgns_zone)  :: zone       ! structure CGNS : zone

! -- Variables internes --
integer       :: size(3,3)        ! tableau d'informations de la zone
integer       :: ier              ! code d'erreur
integer       :: i, ibc           ! indice courant
integer       :: nmax_elem        ! nombre total d'éléments

! -- Début de procédure
   
! --- Lecture du type de zone ---
          
call cg_zone_type_f(unit, ib, iz, zone%type, ier)
if (ier /= 0) call erreur("Lecture CGNS","Problème à la lecture du type de zone")

select case(zone%type)
  case(Structured,Unstructured)
    write(str_w,'(a,i3,a,a)') "- Zone",iz,": type",ZoneTypeName(zone%type)
    call print_info(5, adjustl(str_w))
  case default
    call erreur("Lecture CGNS","zone "//trim(ZoneTypeName(zone%type))//" inconnue")
endselect

! --- Lecture des informations de la zone ---

call cg_zone_read_f(unit, ib, iz, zone%nom, size, ier)
if (ier /= 0)   call erreur("Lecture CGNS","Problème à la lecture de la zone")

select case(zone%type)

  case(Structured)     ! cas STRUCTURE
    ! size(1:3, 1:3) contient             (cf CGNS Mid-level Library)
    !     (1:3, 1) le nombre de sommets ni,nj,nk
    !     (1:3, 2) le nombre de cellules
    !     (1:3, 3) ?
    zone%mesh%ni = size(1,1)
    zone%mesh%nj = size(2,1)
    zone%mesh%nk = size(3,1)

  case(Unstructured)   ! cas NON STRUCTURE
    ! size(1:3, 1) contient             (cf CGNS Mid-level Library)
    !     (1, 1) le nombre de sommets
    !     (2, 1) le nombre de cellules
    !     (3, 1) le nombre de sommets aux limites
    ! ce sont en fait les trois premiers entiers du tableau sous forme linéaire
    zone%mesh%ni = size(1,1)
    zone%mesh%nj = 1
    zone%mesh%nk = 1
    nmax_elem    = size(2,1)

  case default
    call erreur("Gestion CGNS","type non reconnu")
endselect

! --- Allocation et Lecture des sommets du maillage ---

allocate(zone%mesh%vertex(zone%mesh%ni, zone%mesh%nj, zone%mesh%nk)) 
call readcgnsvtex(unit, ib, iz, zone%mesh)
   
! --- Lecture des connectivités  ---

select case(zone%type)

case(Structured)
  call erreur("Développement",&
              "la lecture de connectivité multibloc n'est pas implémentée")
  !call readcgns_strconnect(unit, ib, iz, zone, nmax_elem)

case(Unstructured)
  call readcgns_ustconnect(unit, ib, iz, zone, nmax_elem)

case default
  call erreur("Développement", "incohérence de programmation (zone%imesh)")
endselect

! --- Lecture des conditions aux limites ---

call cg_nbocos_f(unit, ib, iz, zone%nboco, ier)
if (ier /= 0) call erreur("Lecture CGNS","Problème à la lecture du nombre BoCo")

write(str_w,*) ". lecture de ",zone%nboco," conditions aux limites"
call print_info(5, adjustl(str_w))

allocate(zone%boco(zone%nboco))

do ibc = 1, zone%nboco

  select case(zone%type)

  case(Structured)
    call erreur("Développement",&
                "la lecture de connectivité multibloc n'est pas implémentée")
    !call readcgns_strboco(unit, ib, iz, ibc, zone%boco(ibc))

  case(Unstructured)
    call readcgns_ustboco(unit, ib, iz, ibc, zone%boco(ibc))

  case default
    call erreur("Développement", "incohérence de programmation (zone%imesh)")
  endselect


enddo




! --- fermeture du fichier ---

call cg_close_f(unit, ier)



!-------------------------
endsubroutine readcgnszone
