!------------------------------------------------------------------------------!
! Procedure : readcgns_ustconnect         Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Lecture des de la connectivite de tous les elements
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readcgns_ustconnect(unit, ib, iz, zone, nmax_elem)                 

use CGNSLIB       ! definition des mots-clefs
use CGNS_STRUCT   ! Definition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON

implicit none

! -- Entrees --
integer             :: unit       ! numero d'unite pour la lecture
integer             :: ib, iz     ! numero de base et de zone
integer             :: nmax_elem  ! nombre total d'elements

! -- Sorties --
type(st_cgns_zone)  :: zone       ! connectivite cellule->sommets
                                  !              faces  ->sommets
                                  ! dans la structure zone

! -- Variables internes --                                        
integer             :: ier        ! code erreur
integer             :: ifam, nfam ! indice de famille et nombre total de familles
integer             :: ideb, ifin ! indice des cellules repertoriees dans la section
integer             :: itype      ! type d'element
integer             :: nbd, ip    ! entiers non utilises en sortie
type(st_cgns_ustconnect), pointer &
                    :: pfam       ! pointeur sur une famille
integer, dimension(:,:), allocatable &
                    :: elem       ! tableau de connectivite intermediaire
character(len=cgnslen) :: nom     ! nom fantome

! -- Debut de procedure

write(str_w,'(a,i8,a)') "lecture des connectivites du maillage non structure :",nmax_elem," elements"
call print_info(5, adjustl(str_w))

! --- Lecture du nombre de sections ---
! (les cellules sont regroupees par section selon leur type)

call cg_nsections_f(unit, ib, iz, nfam, ier)
if (ier /= 0)   call erreur("Lecture CGNS","Probleme a la lecture du nombre de sections")

! --- Lecture des types de section et affectation aux familles ---

zone%ncellfam = 0
zone%nfacefam = 0
zone%nedgefam = 0
allocate(zone%cellfam(nfam))    ! on surdimensionne les listes de familles au nombre
allocate(zone%facefam(nfam))    ! de familles maximal alors que seule la somme doit
allocate(zone%edgefam(nfam))    ! faire "nfam"

do ifam = 1, nfam               ! Boucle sur l'ensemble des sections

  call cg_section_read_f(unit, ib, iz, ifam, nom, itype, ideb, ifin, nbd, ip, ier)
  if (ier /= 0) call erreur("Lecture CGNS",&
                            "Probleme a la lecture des informations de la section")

  select case(zone%imesh) ! TRAITEMENT SELON 2D OU 3D

  case(2) ! 2D
    select case(itype)
      case(NODE)
        zone%nedgefam = zone%nedgefam + 1
        pfam => zone%edgefam(zone%nedgefam)
      case(BAR_2,BAR_3)
        zone%nfacefam = zone%nfacefam + 1
        pfam => zone%facefam(zone%nfacefam)
      case(TRI_3,QUAD_4,TRI_6,QUAD_8,QUAD_9)
        zone%ncellfam = zone%ncellfam + 1
        pfam => zone%cellfam(zone%ncellfam)
      case(TETRA_4,PYRA_5,PENTA_6,HEXA_8,TETRA_10,PYRA_14,PENTA_15,PENTA_18,HEXA_20,HEXA_27)
        call erreur("Gestion CGNS", "Elements volumiques inattendus en 2D")
      case(MIXED, NGON_n)
        call erreur("Gestion CGNS", "Elements MIXED et NFON_n non traites")
      case default
        call erreur("Gestion CGNS", "Type d'element non reconnu dans CGNSLIB")
    endselect

  case(3) ! 3D
    select case(itype)
      case(NODE)
        call erreur("Gestion CGNS","Elements NODE inattendus en 3D")
      case(BAR_2,BAR_3)
        zone%nedgefam = zone%nedgefam + 1
        pfam => zone%edgefam(zone%nedgefam)
      case(TRI_3,QUAD_4,TRI_6,QUAD_8,QUAD_9)
        zone%nfacefam = zone%nfacefam + 1
        pfam => zone%facefam(zone%nfacefam)
      case(TETRA_4,PYRA_5,PENTA_6,HEXA_8,TETRA_10,PYRA_14,PENTA_15,PENTA_18,HEXA_20,HEXA_27)
        zone%ncellfam = zone%ncellfam + 1
        pfam => zone%cellfam(zone%ncellfam)
      case(MIXED, NGON_n)
        call erreur("Gestion CGNS", "Elements MIXED et NGON_n non traites")
      case default
        call erreur("Gestion CGNS", "Type d'element non reconnu dans CGNSLIB")
    endselect

  case default
    call erreur("Developpement", "incoherence de programmation (imesh)")

  endselect

  ! --- Lecture du nombre de sommets pour le type d'element itype // allocation ---
 
  call cg_npe_f(itype, pfam%nbfils, ier)
  if (ier /= 0)    call erreur("Lecture CGNS","Probleme a la lecture du type d'element")

  pfam%type    = itype
  pfam%ideb    = ideb
  pfam%ifin    = ifin
  pfam%nbnodes = ifin - ideb + 1
  allocate(pfam%fils(ideb:ifin, pfam%nbfils))
  
  write(str_w,'(a,a,a,i8,a,i2,a)') ". ",ElementTypeName(itype)(1:12),": ",&
                                   pfam%nbnodes," elements a", pfam%nbfils," sommets"
  call print_info(8, adjustl(str_w))

  allocate(elem(pfam%nbfils, pfam%nbnodes))  ! tableau intermediaire pour echanger les indices
  elem = 0
  call cg_elements_read_f(unit, ib, iz, ifam, elem, ip, ier)       ! lecture
  if (ier /= 0) call erreur("Lecture CGNS", &
                            "Probleme a la lecture de connectivite dans une section")
  pfam%fils(ideb:ifin, 1:pfam%nbfils) = transpose(elem)            ! echange des indices
  deallocate(elem)                                                 ! desallocation

  ! ---  ---
  ! Il est envisageable de renumeroter les famille par ensemble volumiques et surfaciques
  ! pour imposer une continuite de la numerotation. Mais cela n'est a priori pas obligatoire

enddo ! fin de la boucle sur les sections



!------------------------------
endsubroutine readcgns_ustconnect
