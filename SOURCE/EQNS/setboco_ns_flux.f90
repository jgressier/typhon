!------------------------------------------------------------------------------!
! Procedure : setboco_ns_flux             Auteur : J. Gressier/E. Radenac
!                                         Date   : June 2005
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur, mur à flux imposé
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_flux(defns,unif, ustboco, ustdom, fld, flux, bcns)
!subroutine setboco_ns_flux(defns,unif, ustboco, ustdom, fld, bcns)

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
real(krp), dimension(ustboco%nface) &
                 :: flux             ! flux

! -- Declaration des variables internes --
integer    :: ifb, if, ip  ! index de liste, index de face limite et parametres
integer    :: ic, ighost   ! index de cellule interieure, et de cellule fictive
real(krp)  :: r_PG, cp     ! perfect gas constant, heat capacity
real(krp)  :: temp, conduct, d, gTdc, gPdc
real(krp), dimension(1) &
           :: TH, mu       ! cell temperature , viscocity
type(v3d)  :: cgface, cg, normale ! face, cell center, face normale
type(v3d)  :: gradT        ! temperature gradient
type(v3d)  :: dc           ! vector cell center - its projection 
                           ! on the face normale
! -- Debut de la procedure --
r_PG = defns%properties(1)%r_const        ! perfect gas constant
cp = defns%properties(1)%gamma * r_PG / &
     (defns%properties(1)%gamma - 1)      ! heat capacity

if (unif == uniform) then

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = ustdom%facecell%fils(if,2)
    ic     = ustdom%facecell%fils(if,1)

    cgface = ustdom%mesh%iface(if,1,1)%centre
    cg     = ustdom%mesh%centre(ic,1,1)
    normale= ustdom%mesh%iface(if,1,1)%normale
    d    = (cgface - cg) .scal. (cgface - cg) / &
           (abs((cgface - cg).scal.normale))

    ! temperature
    select case(defns%typ_visc)
    case(visc_suth)
      TH(1) = fld%etatprim%tabscal(2)%scal(ic) / (r_PG * &
              fld%etatprim%tabscal(1)%scal(ic) )
      call calc_visc_suther(defns, 1, TH, mu, 1)
    case(visc_cst)
      mu(1) = defns%properties(1)%visc_dyn
    case(visc_lin)
      TH(1) = fld%etatprim%tabscal(2)%scal(ic) / (r_PG * &
              fld%etatprim%tabscal(1)%scal(ic) )
      mu(1) = defns%properties(1)%visc_dyn*TH(1)
    case default
      call erreur("viscosity computation","unknown kind of computation")
    endselect
    conduct = mu(1) * cp / defns%properties(1)%prandtl

    !temp = fld%etatprim%tabscal(2)%scal(ic) / &
    !       ( fld%etatprim%tabscal(1)%scal(ic) * r_PG ) - bcns%flux*d/conduct

    dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale
    gradT = 1/( fld%etatprim%tabscal(1)%scal(ic) * r_PG) * &
            fld%gradient%tabvect(2)%vect(ic) - &
            fld%etatprim%tabscal(2)%scal(ic) / &
            ( fld%etatprim%tabscal(1)%scal(ic)**2 * r_PG) * &
            fld%gradient%tabvect(1)%vect(ic)
    gTdc = gradT .scal. dc
    temp = fld%etatprim%tabscal(2)%scal(ic) / &
           ( fld%etatprim%tabscal(1)%scal(ic) * r_PG ) + gTdc - &
           2._krp*bcns%flux*abs( (cgface - cg).scal.normale )/conduct
    ! heat flux
    flux(ifb) = flux(ifb) + bcns%flux

    ! pressure
    !fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic)
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

else

  do ifb = 1, ustboco%nface
    if     = ustboco%iface(ifb)
    ighost = ustdom%facecell%fils(if,2)
    ic     = ustdom%facecell%fils(if,1)

    cgface = ustdom%mesh%iface(if,1,1)%centre
    cg     = ustdom%mesh%centre(ic,1,1)
    normale= ustdom%mesh%iface(if,1,1)%normale
    d    = (cgface - cg) .scal. (cgface - cg) / &
           (abs((cgface - cg).scal.normale))

    ! temperature
    select case(defns%typ_visc)
    case(visc_suth)
      TH(1) = fld%etatprim%tabscal(2)%scal(ic) / (r_PG * &
              fld%etatprim%tabscal(1)%scal(ic) )
      call calc_visc_suther(defns, 1, TH, mu, 1)
    case(visc_cst)
      mu(1) = defns%properties(1)%visc_dyn
    case(visc_lin)
      TH(1) = fld%etatprim%tabscal(2)%scal(ic) / (r_PG * &
              fld%etatprim%tabscal(1)%scal(ic) )
      mu(1) = defns%properties(1)%visc_dyn*TH(1)
    case default
      call erreur("viscosity computation","unknown kind of computation")
    endselect
    conduct = mu(1) * cp / defns%properties(1)%prandtl

    !temp = fld%etatprim%tabscal(2)%scal(ic) / &
    !       ( fld%etatprim%tabscal(1)%scal(ic) * r_PG ) - bcns%flux_nunif(ifb)*&
    !       d/conduct
    dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale
    gradT = 1/( fld%etatprim%tabscal(1)%scal(ic) * r_PG) * &
            fld%gradient%tabvect(2)%vect(ic) - &
            fld%etatprim%tabscal(2)%scal(ic) / &
            ( fld%etatprim%tabscal(1)%scal(ic)**2 * r_PG) * &
            fld%gradient%tabvect(1)%vect(ic)
    gTdc = gradT .scal. dc
    temp = fld%etatprim%tabscal(2)%scal(ic) / &
           ( fld%etatprim%tabscal(1)%scal(ic) * r_PG ) + gTdc - &
           2._krp*bcns%flux_nunif(ifb)*abs( (cgface - cg).scal.normale )/conduct

    ! heat flux
    flux(ifb) = flux(ifb) + bcns%flux_nunif(ifb)

    ! pressure
    !fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic)
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

endsubroutine setboco_ns_flux

!------------------------------------------------------------------------------!
! Changes history
!
! jun  2005: creation
! sept 2005: changed to ghost cell (velocity is symmetrical)
!------------------------------------------------------------------------------!
