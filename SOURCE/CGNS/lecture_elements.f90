subroutine lecture_elements(index_cg, nb, nz, nbcell, origine)                  

use CGNSLIB               ! définition des mots-clefs
use mod_origine           ! structure de données réceptrices du maillage CGNS
use mod_connectivite      ! structure générale pour la connectivité

implicit none

! -- Entrées --
integer     :: index_cg   ! numéro d'unité du fichier CGNS
integer     :: nb, nz     ! index de base et de zone du fichier CGNS
integer     :: nbcell     ! nombre de cellules

! -- Sorties --
type(type_origine) :: origine     ! structure réceptrice de la connectivité 

! -- Variables internes --                                        
integer           :: ier        ! code erreur
integer           :: ideb, ifin ! indice des cellules répertoriées dans la section
integer           :: itype      ! type de cellule
integer           :: nbd, ip    ! entiers non utilisés en sortie
integer           :: nelem      ! nombre d'éléments dans la section
integer           :: nbtot      ! nombre total de sections
integer           :: nbnodes    ! nombre de noeuds pour un type d'élément donné
integer, dimension(:), allocatable &
                  :: typelem    ! tableau des types d'élément
integer, dimension(:), allocatable &
                  :: typesect   ! flag de type de section i
                                !   0 : section à supprimer
                                !   1 : section LIMITE
                                !   2 : section ELEMENT
integer, dimension(:,:), allocatable &
                  :: elem       ! tableau de connectivité
real,    dimension(:),   allocatable &
                  :: v          ! tableau de valeurs intermédiaires pour la lecture
integer           :: i, j, isect, ilim, last
character(len=32) :: nom

! -- Début de procédure

! --- allocation  ---

print*
print*,"  * Lecture des connectivités :",nbcell,"éléments"

! --- Lecture du nombre de sections ---
! ( les cellules sont regroupées par section selon leur type )

call cg_nsections_f(index_cg, nb, nz, nbtot, ier)
if (ier /= 0)   call erreur("Problème à la lecture du nombre de sections")

! --- Lecture des types de section ---

allocate( typelem(nbtot))
allocate(typesect(nbtot))

do i = 1, nbtot
  call cg_section_read_f(index_cg, nb, nz, i, nom, typelem(i), ideb, ifin, nbd, ip, ier)
  if (ier /= 0) call erreur("Problème à la lecture de section")
enddo

! --- Détermination maillage 2D ou 3D ---

origine%m3d = .false.    ! par défaut

do i = 1, nbtot
  select case(typelem(i))
    case(NODE)
      ! call erreur("Element NODE inattendu")
    case(BAR_2)
    case(TRI_3, QUAD_4)
    case(TETRA_4, PYRA_5, PENTA_6, HEXA_8)
      origine%m3d = .true.
    case(BAR_3,TRI_6,QUAD_8,QUAD_9,TETRA_10,PYRA_14,PENTA_15,PENTA_18,HEXA_20,HEXA_27)
      call erreur("Elément avec centres inter-sommets non gérés par CGNS_CEDRE")
    case(MIXED, NGON_n)
      call erreur("Type d'élément interdit dans CGNS_CEDRE")
    case default
      call erreur("Type d'élément non reconnu dans CGNSLIB")
  endselect
enddo

! --- Lecture du nombre de sections CELLULES et sections LIMITES ---
! ( le type dépend du maillage, 3D ou non )

origine%nbsect = 0
origine%nblim  = 0

do i = 1, nbtot
  select case(typelem(i))
    case(NODE)
      typesect(i) = 0    ! section à supprimer (ne pas lire)
    case(BAR_2)
      if (origine%m3d) then
        ! call erreur("Element BAR_2 inattendu dans un maillage 3D")
        typesect(i) = 0  ! section à supprimer (ne pas lire)
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
      call erreur("Type d'élément de CGNSLIB non traité par CGNS_CEDRE")
  endselect
enddo

print*,"    .",origine%nbsect,"section(s) de cellules"
print*,"    .",origine%nblim, "section(s) de faces limites"
allocate(origine%section(origine%nbsect))
allocate(origine%limite (origine%nblim))

! --- Boucle sur les sections (CELLULES et LIMITE) et lecture des connectivités  ---

nom   = ""
isect = 0     ! index de section CELLULE
ilim  = 0     ! index de section LIMITE

do i = 1, nbtot

  if (typesect(i) /= 0) then

    call cg_section_read_f(index_cg, nb, nz, i, nom, itype, ideb, ifin, nbd, ip, ier)
    if (ier /= 0) call erreur("Problème à la lecture de section")
    nelem = ifin-ideb+1

    !--- Calcul des nombres d'éléments et allocation ---
    select case(typesect(i))

    case(2) ! --- section type CELLULE ---
      isect = isect + 1
      print*
      print*,"    * section",i,"(",trim(nom),") de cellules      :",&
             nelem,trim(ElementTypeName(itype))
      origine%section(isect)%type = itype      ! type d'élément
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
      origine%limite(ilim)%type = itype      ! type d'élément
      origine%limite(ilim)%elements%nombre = nelem
      origine%limite(ilim)%nb              = nelem
      origine%limite(ilim)%ideb            = ideb
      origine%limite(ilim)%ifin            = ifin
      allocate(origine%limite(ilim)%elements%liste(ideb:ifin))

    case default
      call erreur("Problème de cohérence interne CGNS_CEDRE")
    endselect

    ! --- Lecture du nombre de noeuds pour le type itype ---
 
    call cg_npe_f(itype, nbnodes, ier)
    if (ier /= 0)    call erreur("Problème à la lecture du type d'élément")
    if (nbnodes > maxconnect) &
      call erreur("Nombre de sommets maximal (8) dépassé pour ce type")

    print*, "      . indices CGNS",ideb,"à",ifin,": lecture de connectivités"
  
    allocate(elem(nbnodes,ideb:ifin))
    elem = 0

    call cg_elements_read_f(index_cg, nb, nz, i, elem, ip, ier)
    if (ier /= 0) &
      call erreur("Problème à la lecture de connectivité dans une section")

    ! --- Retranscription des données dans la structure origine ---

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
      call erreur("Problème de cohérence interne CGNS_CEDRE")
    endselect

    ! --- désallocation ---
    deallocate(elem)

  endif ! test si section à supprimer

enddo ! boucle sur section

! --- Calcul des nouveaux index de cellules pour chacun des types ---
! (on renumérote les cellules pour avoir une continuité de la numérotation)

print*
print*,"  * Renumérotation continue des cellules et faces limites"

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


! --- désallocation ---
deallocate(typelem, typesect)
   
!------------------------------
endsubroutine lecture_elements
