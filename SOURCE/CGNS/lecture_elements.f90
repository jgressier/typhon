subroutine lecture_elements(index_cg, nb, nz, nbcell, origine)                  

use CGNSLIB               ! definition des mots-clefs
use mod_origine           ! structure de donnees receptrices du maillage CGNS
use mod_connectivite      ! structure generale pour la connectivite

implicit none

! -- Entrees --
integer     :: index_cg   ! numero d'unite du fichier CGNS
integer     :: nb, nz     ! index de base et de zone du fichier CGNS
integer     :: nbcell     ! nombre de cellules

! -- Sorties --
type(type_origine) :: origine     ! structure receptrice de la connectivite 

! -- Variables internes --                                        
integer           :: ier        ! code erreur
integer           :: ideb, ifin ! indice des cellules repertoriees dans la section
integer           :: itype      ! type de cellule
integer           :: nbd, ip    ! entiers non utilises en sortie
integer           :: nelem      ! nombre d'elements dans la section
integer           :: nbtot      ! nombre total de sections
integer           :: nbnodes    ! nombre de noeuds pour un type d'element donne
integer, dimension(:), allocatable &
                  :: typelem    ! tableau des types d'element
integer, dimension(:), allocatable &
                  :: typesect   ! flag de type de section i
                                !   0 : section a supprimer
                                !   1 : section LIMITE
                                !   2 : section ELEMENT
integer, dimension(:,:), allocatable &
                  :: elem       ! tableau de connectivite
real,    dimension(:),   allocatable &
                  :: v          ! tableau de valeurs intermediaires pour la lecture
integer           :: i, j, isect, ilim, last
character(len=32) :: nom

! -- Debut de procedure

! --- allocation  ---

print*
print*,"  * Lecture des connectivites :",nbcell,"elements"

! --- Lecture du nombre de sections ---
! ( les cellules sont regroupees par section selon leur type )

call cg_nsections_f(index_cg, nb, nz, nbtot, ier)
if (ier /= 0)   call erreur("Probleme a la lecture du nombre de sections")

! --- Lecture des types de section ---

allocate( typelem(nbtot))
allocate(typesect(nbtot))

do i = 1, nbtot
  call cg_section_read_f(index_cg, nb, nz, i, nom, typelem(i), ideb, ifin, nbd, ip, ier)
  if (ier /= 0) call erreur("Probleme a la lecture de section")
enddo

! --- Determination maillage 2D ou 3D ---

origine%m3d = .false.    ! par defaut

do i = 1, nbtot
  select case(typelem(i))
    case(NODE)
      ! call erreur("Element NODE inattendu")
    case(BAR_2)
    case(TRI_3, QUAD_4)
    case(TETRA_4, PYRA_5, PENTA_6, HEXA_8)
      origine%m3d = .true.
    case(BAR_3,TRI_6,QUAD_8,QUAD_9,TETRA_10,PYRA_14,PENTA_15,PENTA_18,HEXA_20,HEXA_27)
      call erreur("Element avec centres inter-sommets non geres par CGNS_CEDRE")
    case(MIXED, NGON_n)
      call erreur("Type d'element interdit dans CGNS_CEDRE")
    case default
      call erreur("Type d'element non reconnu dans CGNSLIB")
  endselect
enddo

! --- Lecture du nombre de sections CELLULES et sections LIMITES ---
! ( le type depend du maillage, 3D ou non )

origine%nbsect = 0
origine%nblim  = 0

do i = 1, nbtot
  select case(typelem(i))
    case(NODE)
      typesect(i) = 0    ! section a supprimer (ne pas lire)
    case(BAR_2)
      if (origine%m3d) then
        ! call erreur("Element BAR_2 inattendu dans un maillage 3D")
        typesect(i) = 0  ! section a supprimer (ne pas lire)
      else
        origine%nblim  = origine%nblim  + 1
        typesect(i)    = 1   ! section de type LIMITE
      endif
    case(TRI_3, QUAD_4)
      if (origine%m3d) then
        origine%nblim  = origine%nblim  + 1
        typesect(i)    = 1   ! section de type LIMITE
      else
        origine%nbsect = origine%nbsect + 1
        typesect(i)    = 2   ! section de type CELLULE
      endif
    case(TETRA_4, PYRA_5, PENTA_6, HEXA_8)
      if (origine%m3d) then
        origine%nbsect = origine%nbsect + 1
        typesect(i)    = 2   ! section de type CELLULE
      else
        call erreur("Element inattendu dans un maillage 2D")
      endif
    case default
      call erreur("Type d'element de CGNSLIB non traite par CGNS_CEDRE")
  endselect
enddo

print*,"    .",origine%nbsect,"section(s) de cellules"
print*,"    .",origine%nblim, "section(s) de faces limites"
allocate(origine%section(origine%nbsect))
allocate(origine%limite (origine%nblim))

! --- Boucle sur les sections (CELLULES et LIMITE) et lecture des connectivites  ---

nom   = ""
isect = 0     ! index de section CELLULE
ilim  = 0     ! index de section LIMITE

do i = 1, nbtot

  if (typesect(i) /= 0) then

    call cg_section_read_f(index_cg, nb, nz, i, nom, itype, ideb, ifin, nbd, ip, ier)
    if (ier /= 0) call erreur("Probleme a la lecture de section")
    nelem = ifin-ideb+1

    !--- Calcul des nombres d'elements et allocation ---
    select case(typesect(i))

    case(2) ! --- section type CELLULE ---
      isect = isect + 1
      print*
      print*,"    * section",i,"(",trim(nom),") de cellules      :",&
             nelem,trim(ElementTypeName(itype))
      origine%section(isect)%type = itype      ! type d'element
      origine%section(isect)%elements%nombre = nelem
      origine%section(isect)%nb              = nelem
      origine%section(isect)%ideb            = ideb
      origine%section(isect)%ifin            = ifin
      allocate(origine%section(isect)%elements%liste(ideb:ifin))

    case(1) ! --- section type LIMITE ---
      ilim = ilim + 1
      print*
      print*,"    * section",i,"(",trim(nom),") de faces limites :",&
             nelem,trim(ElementTypeName(itype))
      origine%limite(ilim)%type = itype      ! type d'element
      origine%limite(ilim)%elements%nombre = nelem
      origine%limite(ilim)%nb              = nelem
      origine%limite(ilim)%ideb            = ideb
      origine%limite(ilim)%ifin            = ifin
      allocate(origine%limite(ilim)%elements%liste(ideb:ifin))

    case default
      call erreur("Probleme de coherence interne CGNS_CEDRE")
    endselect

    ! --- Lecture du nombre de noeuds pour le type itype ---
 
    call cg_npe_f(itype, nbnodes, ier)
    if (ier /= 0)    call erreur("Probleme a la lecture du type d'element")
    if (nbnodes > maxconnect) &
      call erreur("Nombre de sommets maximal (8) depasse pour ce type")

    print*, "      . indices CGNS",ideb,"a",ifin,": lecture de connectivites"
  
    allocate(elem(nbnodes,ideb:ifin))
    elem = 0

    call cg_elements_read_f(index_cg, nb, nz, i, elem, ip, ier)
    if (ier /= 0) &
      call erreur("Probleme a la lecture de connectivite dans une section")

    ! --- Retranscription des donnees dans la structure origine ---

    select case(typesect(i))

    case(2) ! --- section type CELLULE ---
      do j = ideb, ifin
        ! allocation de (nbnodes) sommets par cellules 
        allocate(origine%section(isect)%elements%liste(j)%article(nbnodes)) 
      enddo 
      ! -- retranscription dans la structure "origine" --
      do j = ideb, ifin
        origine%section(isect)%elements%liste(j)%article(1:nbnodes) = elem(1:nbnodes,j)
      enddo

    case(1) ! --- section type LIMITE ---
      do j = ideb, ifin
        ! allocation de (nbnodes) sommets par cellules 
        allocate(origine%limite(ilim)%elements%liste(j)%article(nbnodes)) 
      enddo 
      ! -- retranscription dans la structure "origine" --
      do j = ideb, ifin
        origine%limite(ilim)%elements%liste(j)%article(1:nbnodes) = elem(1:nbnodes,j)
      enddo

    case default
      call erreur("Probleme de coherence interne CGNS_CEDRE")
    endselect

    ! --- desallocation ---
    deallocate(elem)

  endif ! test si section a supprimer

enddo ! boucle sur section

! --- Calcul des nouveaux index de cellules pour chacun des types ---
! (on renumerote les cellules pour avoir une continuite de la numerotation)

print*
print*,"  * Renumerotation continue des cellules et faces limites"

last = 0
do i = 1, origine%nbsect
  origine%section(i)%ideb = last + 1
  origine%section(i)%ifin = last + origine%section(i)%nb
  last = origine%section(i)%nb
enddo

last = 0
do i = 1, origine%nblim
  origine%limite(i)%ideb = last + 1
  origine%limite(i)%ifin = last + origine%limite(i)%nb
  last = origine%limite(i)%nb
enddo


! --- desallocation ---
deallocate(typelem, typesect)
   
!------------------------------
endsubroutine lecture_elements
