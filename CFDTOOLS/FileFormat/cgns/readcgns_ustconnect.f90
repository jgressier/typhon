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

use CGNS_STRUCT
use IOCFD
use STRING

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
character(len=100)     :: str_w   ! nom fantome

! -- BODY --

write(str_w,'(a,i8,a)') "reading element connectivity:",nmax_elem," elements"
call cfd_print(str_w)

! --- Lecture du nombre de sections ---
! (les cellules sont regroupees par section selon leur type)

call cg_nsections_f(unit, ib, iz, nfam, ier)
if (ier /= 0) call cfd_error("(CGNS) cannot read number of element sections")

! --- Lecture des types de section et affectation aux familles ---

zone%ncellfam = 0
zone%nfacefam = 0
zone%nedgefam = 0
allocate(zone%cellfam(nfam))    ! on surdimensionne les listes de familles au nombre
allocate(zone%facefam(nfam))    ! de familles maximal alors que seule la somme doit
allocate(zone%edgefam(nfam))    ! faire "nfam"

do ifam = 1, nfam               ! Boucle sur l'ensemble des sections

  call cg_section_read_f(unit, ib, iz, ifam, nom, itype, ideb, ifin, nbd, ip, ier)
  if (ier /= 0) call cfd_error("(CGNS) cannot read section information")

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
        call cfd_error("(CGNS) unexpected volumic element for a 2D problem")
      case(MIXED, NGON_n)
        call cfd_error("(CGNS) cannot read MIXED and NGON_n elements")
      case default
        call cfd_error("(CGNS) unknown type of element")
    endselect

  case(3) ! 3D
    select case(itype)
      case(NODE)
        call cfd_print("(CGNS) NODE elements ignored")
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
        call cfd_error("(CGNS) cannot read MIXED and NGON_n elements")
      case default
        call cfd_error("(CGNS) unknown type element")
     endselect

  case default
    call cfd_error("internal inconsistency (readcgns_ustconnect)")

  endselect

  ! --- Lecture du nombre de sommets pour le type d'element itype // allocation ---
 
  call cg_npe_f(itype, pfam%nbfils, ier)
  if (ier /= 0)    call cfd_error("(CGNS) cannot get number of vertex per element")

  pfam%type    = itype
  pfam%ideb    = ideb
  pfam%ifin    = ifin
  pfam%nbnodes = ifin - ideb + 1
  allocate(pfam%fils(ideb:ifin, pfam%nbfils))
  
  write(str_w,'(a,a,a,i8,a,i2,a)') ". ",ElementTypeName(itype)(1:10),":",&
                                   pfam%nbnodes," elements with", pfam%nbfils," vertices"
  call cfd_print(adjustl(str_w))

  allocate(elem(pfam%nbfils, pfam%nbnodes))  ! tableau intermediaire pour echanger les indices
  elem = 0
  call cg_elements_read_f(unit, ib, iz, ifam, elem, ip, ier)       ! lecture
  if (ier /= 0) call cfd_error("(CGNS) cannot read element->vertex connectivity")
  pfam%fils(ideb:ifin, 1:pfam%nbfils) = transpose(elem)            ! echange des indices
  deallocate(elem)                                                 ! desallocation

  ! ---  ---
  ! Il est envisageable de renumeroter les famille par ensemble volumiques et surfaciques
  ! pour imposer une continuite de la numerotation. Mais cela n'est a priori pas obligatoire

enddo ! fin de la boucle sur les sections



!------------------------------
endsubroutine readcgns_ustconnect
