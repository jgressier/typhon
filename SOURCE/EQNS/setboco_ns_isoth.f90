!------------------------------------------------------------------------------!
! Procedure : setboco_ns_isoth            Auteur : J. Gressier/E. Radenac
!                                         Date   : June 2005
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur, mur isotherme
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_isoth(defns,unif, ustboco, umesh, fld, bcns)

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
type(st_ustmesh)   :: umesh            ! unstructured mesh
type(st_boco_ns)   :: bcns             ! parameters and temperature (field or constant)

! -- Declaration des sorties --
type(st_field)   :: fld            ! fields

! -- Declaration des variables internes --
integer    :: ifb, if, ip  ! index de liste, index de face limite et parametres
integer    :: ic, ighost   ! index de cellule interieure, et de cellule fictive
real(krp)  :: r_PG         ! perfect gas constant
real(krp)  :: gPdc, temp
type(v3d)  :: cgface, cg, normale ! face, cell center, face normale
type(v3d)  :: dc           ! vector cell center - its projection 
                           ! on the face normale

! -- Debut de la procedure --
r_PG = defns%properties(1)%r_const        ! perfect gas constant

if (unif == uniform) then

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = umesh%facecell%fils(if,2)
    ic     = umesh%facecell%fils(if,1)

    cgface = umesh%mesh%iface(if,1,1)%centre
    cg     = umesh%mesh%centre(ic,1,1)
    normale= umesh%mesh%iface(if,1,1)%normale

    ! extrapolated temperature on ghost cell (supposed symmetrical)
    temp = 2._krp*bcns%temp_wall - fld%etatprim%tabscal(2)%scal(ic) / &
           ( fld%etatprim%tabscal(1)%scal(ic) * r_PG )

    ! pressure
    !fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic)
    dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale
    gPdc = fld%gradient%tabvect(2)%vect(ic) .scal. dc
    fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic) + &
                                           gPdc
    ! density
    fld%etatprim%tabscal(1)%scal(ighost) = &
                fld%etatprim%tabscal(2)%scal(ighost)/(r_PG*temp) 
    ! velocity
    !fld%etatprim%tabvect(1)%vect(ighost) = v3d(0._krp,0._krp,0._krp)
    fld%etatprim%tabvect(1)%vect(ighost) = (2._krp*bcns%wall_velocity) - fld%etatprim%tabvect(1)%vect(ic)

  enddo

if     = ustboco%iface(1)
ighost = umesh%facecell%fils(if,2)

else

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = umesh%facecell%fils(if,2)
    ic     = umesh%facecell%fils(if,1)

    cgface = umesh%mesh%iface(if,1,1)%centre
    cg     = umesh%mesh%centre(ic,1,1)
    normale= umesh%mesh%iface(if,1,1)%normale

    ! extrapolated temperature on ghost cell (supposed symmetrical)
    temp = 2._krp*bcns%temp(ifb) - fld%etatprim%tabscal(2)%scal(ic) / &
           ( fld%etatprim%tabscal(1)%scal(ic) * r_PG )

    ! pressure
    !fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic)
    dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale
    gPdc = fld%gradient%tabvect(2)%vect(ic) .scal. dc
    fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic) + &
                                           gPdc
    ! density
    fld%etatprim%tabscal(1)%scal(ighost) = &
                fld%etatprim%tabscal(2)%scal(ighost)/(r_PG*temp)
    ! velocity
    !fld%etatprim%tabvect(1)%vect(ighost) = v3d(0._krp,0._krp,0._krp)  
    fld%etatprim%tabvect(1)%vect(ighost) = (2._krp*bcns%wall_velocity) - fld%etatprim%tabvect(1)%vect(ic)

  enddo

endif

endsubroutine setboco_ns_isoth

!------------------------------------------------------------------------------!
! Changes history
!
! jun 2005: creation
! sept 2005: changed to ghost cell (velocity is symmetrical)
!------------------------------------------------------------------------------!
