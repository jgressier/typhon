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
subroutine seek_bcface_face(ib, cgnsboco, nfam, facevtex, umesh, listface) 

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
type(st_ustmesh)    :: umesh          ! unstructured mesh

! -- Variables internes --
integer, dimension(*) :: listface      ! tableau de travail
type(st_cgns_boco)    :: cgboco        ! connectivite intermediaire
logical, dimension(:), allocatable &
                      :: mkvtex        ! liste de vertex marques
integer               :: iface, pface, ifam, pfam, iv, ivm, dim, notfound

! -- BODY --

allocate(mkvtex(umesh%mesh%nvtex))    ! allocation du tableau des vertex marques
mkvtex(:) = .false.                  ! initialisation a "non marque"

notfound = 0

! -- marquage de sommets marques par la liste de faces marquees --

do iface = 1, cgnsboco%list%nbfils  ! boucle sur la liste de faces marquees

  pface = cgnsboco%list%fils(iface)   ! index reel de face
  pfam  = 0                           ! recherche de famille
  do ifam = 1, nfam
    if ((pface >= facevtex(ifam)%ideb).and.(pface <= facevtex(ifam)%ifin)) pfam = ifam
  enddo

  if (pfam == 0) then
    notfound = notfound + 1
  else
    ! on marque les sommets de la face trouvee
    do iv = 1, facevtex(pfam)%nbfils
      mkvtex(facevtex(pfam)%fils(pface,iv)) = .true.
    enddo
  endif

enddo

if (notfound /=0) then
  call print_info(20,"      some marked elements ("//trim(strof(notfound))//") have not been found in CGNS face sections")
endif

! -- construction d'une connectivite cgnsboco/vertex associee --

!dim = count(mkvtex==.true.)

dim = count(mkvtex)

if (dim == 0) then
  call print_info(20,"      no marked elements havebeen found in CGNS face sections, skipping BOCO mark...")
  call new_ustboco(umesh%boco(ib), cgboco%family, 0)
else
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

  ! -- contruction de la connectivite boco/face de TYPHON a partir des sommets --

  call seek_bcface_vtex(ib, cgboco, umesh, listface)

  call delete(cgboco%list)

endif

deallocate(mkvtex)
  
!-------------------------
endsubroutine seek_bcface_face

!------------------------------------------------------------------------------!
! Changes history
!
! juin 2004: creation de la procedure
! Nov  2007: improve BOCO reading (when sections are defined but useless)
!------------------------------------------------------------------------------!
