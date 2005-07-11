!------------------------------------------------------------------------------!
! Procedure : setboco_ns_isoth            Auteur : J. Gressier/E. Radenac
!                                         Date   : June 2005
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur, mur isotherme
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_isoth(defns,unif, ustboco, ustdom, fld, bcns)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Declaration des entrees --
type(mnu_ns)       :: defns            ! solver parameters
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! boundary condition location
type(st_ustmesh)   :: ustdom           ! unstructured mesh
type(st_boco_ns)   :: bcns             ! parameters and temperature (field or constant)

! -- Declaration des sorties --
type(st_field)   :: fld            ! fields

! -- Declaration des variables internes --
integer    :: ifb, if, ip  ! index de liste, index de face limite et parametres
integer    :: ic, ighost   ! index de cellule interieure, et de cellule fictive
real(krp)  :: r_PG         ! perfect gas constant
real(krp)  :: gPdc
type(v3d)  :: cgface, cg, normale ! face, cell center, face normale
type(v3d)  :: dc           ! vector cell center - its projection 
                           ! on the face normale

! -- Debut de la procedure --
r_PG = defns%properties(1)%r_const        ! perfect gas constant

if (unif == uniform) then

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = ustdom%facecell%fils(if,2)
    ic     = ustdom%facecell%fils(if,1)

    cgface = ustdom%mesh%iface(if,1,1)%centre
    cg     = ustdom%mesh%centre(ic,1,1)
    normale= ustdom%mesh%iface(if,1,1)%normale

    ! pressure
    !fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic)
    dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale
    gPdc = fld%gradient%tabvect(2)%vect(ic) .scal. dc
    fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic) + &
                                           gPdc
    ! density
    fld%etatprim%tabscal(1)%scal(ighost) = &
                fld%etatprim%tabscal(2)%scal(ighost)/(r_PG*bcns%temp_wall) 
    ! velocity
    fld%etatprim%tabvect(1)%vect(ighost) = v3d(0._krp,0._krp,0._krp)

  enddo

if     = ustboco%iface(1)
ighost = ustdom%facecell%fils(if,2)

else

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = ustdom%facecell%fils(if,2)
    ic     = ustdom%facecell%fils(if,1)

    cgface = ustdom%mesh%iface(if,1,1)%centre
    cg     = ustdom%mesh%centre(ic,1,1)
    normale= ustdom%mesh%iface(if,1,1)%normale

    ! pressure
    !fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic)
    dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale
    gPdc = fld%gradient%tabvect(2)%vect(ic) .scal. dc
    fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic) + &
                                           gPdc
    ! density
    fld%etatprim%tabscal(1)%scal(ighost) = &
                fld%etatprim%tabscal(2)%scal(ighost)/(r_PG*bcns%temp(ifb))
    ! velocity
    fld%etatprim%tabvect(1)%vect(ighost) = v3d(0._krp,0._krp,0._krp)  

  enddo

endif

endsubroutine setboco_ns_isoth

!------------------------------------------------------------------------------!
! Historique des modifications
!
! jun 2005: creation
!------------------------------------------------------------------------------!
