!------------------------------------------------------------------------------!
! Procedure : seek_bcface_face.f90        Auteur : J. Gressier
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf historique)
!   Recherche des faces à partir des listes de FACES marquées
!
! Defauts/Limitations/Divers :
!   On passe par la reconstruction d'une liste de VERTEX marqués et on
!   utilise seek_bcface_vtex
!
!------------------------------------------------------------------------------!
subroutine seek_bcface_face(ib, cgnsboco, nfam, facevtex, mesh, listface) 

use TYPHMAKE      ! définitions générales 
use CONNECTIVITY
use CGNS_STRUCT   ! Définition des structures CGNS
use USTMESH       ! Définition des structures maillage non structuré
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrées --
integer             :: ib             ! indice de condition limite
integer             :: nfam           ! nombre de connectivité facevtex
type(st_cgns_boco)  :: cgnsboco       ! zone CGNS contenant conditions aux limites
type(st_cgns_ustconnect), dimension(1:nfam) &
                    :: facevtex

! -- Entrées/Sorties --
type(st_ustmesh)    :: mesh            ! connectivités et conditions aux limites

! -- Variables internes --
integer, dimension(*) :: listface      ! tableau de travail
type(st_cgns_boco)    :: cgboco        ! connectivité intermédiaire
logical, dimension(:), allocatable &
                      :: mkvtex        ! liste de vertex marqués
integer               :: iface, pface, ifam, pfam, iv, ivm, dim

! -- Début de procédure

allocate(mkvtex(mesh%mesh%nvtex))    ! allocation du tableau des vertex marqués
mkvtex(:) = .false.                  ! initialisation à "non marqué"

!print*,"DEBUG:",nfam

! -- marquage de sommets marqués par la liste de faces marquées --

do iface = 1, cgnsboco%list%nbfils  ! boucle sur la liste de faces marquées

  pface = cgnsboco%list%fils(iface)   ! index réel de face
  pfam  = 0                           ! recherche de famille
  do ifam = 1, nfam
    if ((pface >= facevtex(ifam)%ideb).and.(pface <= facevtex(ifam)%ifin)) pfam = ifam
  enddo

  if (pfam == 0) call erreur("Calcul de connectivité","face limite introuvable dans CGNS")

  ! on marque les sommets de la face trouvée
  do iv = 1, facevtex(pfam)%nbfils
    mkvtex(facevtex(pfam)%fils(pface,iv)) = .true.
  enddo

enddo

! -- construction d'une connectivité cgnsboco/vertex associée --

!dim = count(mkvtex==.true.)
dim = count(mkvtex)
cgboco%nom    = cgnsboco%nom
cgboco%family = cgnsboco%family
call new(cgboco%list, dim)

ivm = 0
do iv = 1, size(mkvtex)
  if (mkvtex(iv)) then
    ivm = ivm+1
    cgboco%list%fils(ivm) = iv
  endif
enddo

deallocate(mkvtex)

! -- contruction de la connectivité boco/face de TYPHON à partir des sommets --

call seek_bcface_vtex(ib, cgboco, mesh, listface)

call delete(cgboco%list)

  
!-------------------------
endsubroutine seek_bcface_face

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2004 : création de la procédure
!------------------------------------------------------------------------------!
