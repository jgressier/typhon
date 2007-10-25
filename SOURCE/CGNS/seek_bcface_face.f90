!------------------------------------------------------------------------------!
! Procedure : seek_bcface_face.f90        Auteur : J. Gressier
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf historique)
!   Recherche des faces a partir des listes de FACES marquees
!
! Defauts/Limitations/Divers :
!   On passe par la reconstruction d'une liste de VERTEX marques et on
!   utilise seek_bcface_vtex
!
!------------------------------------------------------------------------------!
subroutine seek_bcface_face(ib, cgnsboco, nfam, facevtex, mesh, listface) 

use TYPHMAKE      ! definitions generales 
use CONNECTIVITY
use CGNS_STRUCT   ! Definition des structures CGNS
use USTMESH       ! Definition des structures maillage non structure
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrees --
integer             :: ib             ! indice de condition limite
integer             :: nfam           ! nombre de connectivite facevtex
type(st_cgns_boco)  :: cgnsboco       ! zone CGNS contenant conditions aux limites
type(st_cgns_ustconnect), dimension(1:nfam) &
                    :: facevtex

! -- Entrees/Sorties --
type(st_ustmesh)    :: mesh            ! connectivites et conditions aux limites

! -- Variables internes --
integer, dimension(*) :: listface      ! tableau de travail
type(st_cgns_boco)    :: cgboco        ! connectivite intermediaire
logical, dimension(:), allocatable &
                      :: mkvtex        ! liste de vertex marques
integer               :: iface, pface, ifam, pfam, iv, ivm, dim

! -- Debut de procedure

allocate(mkvtex(mesh%mesh%nvtex))    ! allocation du tableau des vertex marques
mkvtex(:) = .false.                  ! initialisation a "non marque"

!print*,"DEBUG:",nfam

! -- marquage de sommets marques par la liste de faces marquees --

do iface = 1, cgnsboco%list%nbfils  ! boucle sur la liste de faces marquees

  pface = cgnsboco%list%fils(iface)   ! index reel de face
  pfam  = 0                           ! recherche de famille
  do ifam = 1, nfam
    if ((pface >= facevtex(ifam)%ideb).and.(pface <= facevtex(ifam)%ifin)) pfam = ifam
  enddo
  !print*,'pface',pface
  if (pfam == 0) call erreur("BOCO construction","unable to find tagged face in CGNS sections")

  ! on marque les sommets de la face trouvee
  do iv = 1, facevtex(pfam)%nbfils
    mkvtex(facevtex(pfam)%fils(pface,iv)) = .true.
  enddo

enddo

! -- construction d'une connectivite cgnsboco/vertex associee --

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

! -- contruction de la connectivite boco/face de TYPHON a partir des sommets --

call seek_bcface_vtex(ib, cgboco, mesh, listface)

call delete(cgboco%list)

  
!-------------------------
endsubroutine seek_bcface_face

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2004 : creation de la procedure
!------------------------------------------------------------------------------!
