!------------------------------------------------------------------------------!
! Procedure : donnees_echange             Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Ecrire les donnees variables dans le temps dans une structure 
!   donnees_echange_inst
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
 
subroutine donnees_echange(solvercoupling, donnees_echange_inst1, zone1, &
                           nbc1, donnees_echange_inst2, zone2, nbc2, ncoupl1, &
                           ncoupl2, typcor)

use TYPHMAKE
use OUTPUT
use GEO3D
use DEFZONE
use DEFFIELD
use MATER_LOI
use VARCOM

implicit none

! -- Declaration des entrees --
integer                 :: solvercoupling
type(st_zone)           :: zone1, zone2
integer                 :: nbc1, nbc2 ! numero (identite) de la CL
integer                 :: ncoupl1, ncoupl2
integer                 :: typcor

! -- Declaration donnees_echange    
type(st_genericfield) :: donnees_echange_inst1, donnees_echange_inst2

! -- Declaration des variables internes --
integer   :: i, icl1, icl2, if1, if2
real(krp) :: conduct, r_PG, cp
real(krp), dimension(1) &
          :: mu, TH  

! -- Debut de la procedure --
do i=1, zone1%grid%umesh%boco(nbc1)%nface
  
  if1 = zone1%grid%umesh%boco(nbc1)%iface(i)
  if2 = zone2%grid%umesh%boco(nbc2)%iface(zone2%coupling(ncoupl2)%zcoupling%connface(i))

  icl1 = zone1%grid%umesh%facecell%fils(if1,1)
  icl2 = zone2%grid%umesh%facecell%fils(if2,1)

  ! ZONE 1 !

  select case(zone1%defsolver%typ_solver)
  case(solKDIF)

    ! Computation of zone 1 conductivity
    select case(zone1%defsolver%defkdif%materiau%type)
    case(mat_LIN, mat_KNL)
      conduct = valeur_loi(zone1%defsolver%defkdif%materiau%Kd, &
                         zone1%grid%info%field_loc%etatprim%tabscal(1)%scal(icl1))
    case(mat_XMAT)
      call erreur("Calcul de materiau","Materiau non lineaire interdit")
    endselect

    ! Exchange data of zone 1

    if (typcor == bocoT) then
      !DEBUG
      print*, "correction BOCO"
      donnees_echange_inst1%tabscal(1)%scal(i) = &
             (zone1%grid%info%field_loc%etatcons%tabscal(1)%scal(icl1) - &
             (zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal(2)%scal(i) / &
             zone1%grid%umesh%mesh%volume(icl1,1,1)) ) / &
             zone1%defsolver%defkdif%materiau%Cp
      ! Correction  rest zero
      ! zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal(3)%scal(i) = 0
    else
      !donnees_echange_inst1%tabscal(1)%scal(i) = &
      !                              zone1%grid%field%etatprim%tabscal(1)%scal(icl1)
      donnees_echange_inst1%tabscal(1)%scal(i) = &
             zone1%grid%info%field_loc%etatcons%tabscal(1)%scal(icl1)/ &
             zone1%defsolver%defkdif%materiau%Cp
    endif

    donnees_echange_inst1%tabscal(2)%scal(i) = conduct
    !donnees_echange_inst1%tabvect(1)%vect(if) = zone1%grid%field%gradient%tabvect(1)%vect(icl)

  case(solNS)
    ! Computation of zone 1 conductivity
    r_PG = zone1%defsolver%defns%properties(1)%r_const        ! perfect gas constant
    cp = zone1%defsolver%defns%properties(1)%gamma * r_PG / &
       (zone1%defsolver%defns%properties(1)%gamma - 1)      ! heat capacity
    ! Temperature :
    TH(1) = zone1%grid%field%etatprim%tabscal(2)%scal(icl1) / (zone1%grid%field%etatprim%tabscal(1)%scal(icl1) * r_PG)
    ! Viscosity :
    select case(zone1%defsolver%defns%typ_visc)
    case(visc_suth)
      call calc_visc_suther(zone1%defsolver%defns, 1, TH, mu, 1)
    case default
      call erreur("viscosity computation","unknown kind of computation")
    endselect
    conduct = mu(1) * cp / zone1%defsolver%defns%properties(1)%prandtl

    ! Exchange data of zone 1
    if (typcor == bocoT) then
      !DEBUG
      print*, "correction BOCO"
      donnees_echange_inst1%tabscal(1)%scal(i) = TH(1) - &
             zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal(2)%scal(i) / &
             (zone1%grid%umesh%mesh%volume(icl1,1,1) * cp * &
             zone1%grid%field%etatprim%tabscal(1)%scal(icl1))
      ! Correction  rest zero
      ! zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal(3)%scal(i) = 0
    else
      donnees_echange_inst1%tabscal(1)%scal(i) = TH(1)
    endif

    donnees_echange_inst1%tabscal(2)%scal(i) = conduct

  endselect

  ! ZONE 2 !

  select case(zone2%defsolver%typ_solver)
  case(solKDIF)

    ! Computation of zone 2 conductivity
    select case(zone2%defsolver%defkdif%materiau%type)
    case(mat_LIN, mat_KNL)
      conduct = valeur_loi(zone2%defsolver%defkdif%materiau%Kd, &
                         zone2%grid%info%field_loc%etatprim%tabscal(1)%scal(icl2))
    case(mat_XMAT)
      call erreur("Calcul de materiau","Materiau non lineaire interdit")
    endselect

    ! Exchange data of zone 2

    if (typcor == bocoT) then
      donnees_echange_inst2%tabscal(1)%scal(i) = &
             (zone2%grid%info%field_loc%etatcons%tabscal(1)%scal(icl2) - &
             (zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal(2)%scal(i) / &
             zone2%grid%umesh%mesh%volume(icl2,1,1)) ) / &
             zone2%defsolver%defkdif%materiau%Cp
      ! Correction rest zero
      ! zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal(3)%scal(i) = 0
    else
      !donnees_echange_inst2%tabscal(1)%scal(i) = &
      !                              zone2%grid%field%etatprim%tabscal(1)%scal(icl2)
      donnees_echange_inst2%tabscal(1)%scal(i) = &
             zone2%grid%info%field_loc%etatcons%tabscal(1)%scal(icl2)/ &
             zone2%defsolver%defkdif%materiau%Cp
    endif

    donnees_echange_inst2%tabscal(2)%scal(i) = conduct
    !donnees_echange_inst2%tabvect(1)%vect(if) = zone2%grid%field%gradient%tabvect(1)%vect(icl)  

  case(solNS)
    ! Computation of zone 2 conductivity
    r_PG = zone2%defsolver%defns%properties(1)%r_const        ! perfect gas constant
    cp = zone2%defsolver%defns%properties(1)%gamma * r_PG / &
       (zone2%defsolver%defns%properties(1)%gamma - 1)      ! heat capacity
    ! Temperature :
    TH(1) = zone2%grid%field%etatprim%tabscal(2)%scal(icl2) / (zone2%grid%field%etatprim%tabscal(1)%scal(icl2) * r_PG)
    ! Viscosity :
    select case(zone2%defsolver%defns%typ_visc)
    case(visc_suth)
      call calc_visc_suther(zone2%defsolver%defns, 1, TH, mu, 1)
    case default
      call erreur("viscosity computation","unknown kind of computation")
    endselect
    conduct = mu(1) * cp / zone2%defsolver%defns%properties(1)%prandtl

    ! Exchange data of zone 2
    if (typcor == bocoT) then
      !DEBUG
      print*, "correction BOCO"
      donnees_echange_inst2%tabscal(1)%scal(i) = TH(1) - &
             zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal(2)%scal(i) / &
             (zone2%grid%umesh%mesh%volume(icl2,1,1) * cp * &
             zone2%grid%field%etatprim%tabscal(1)%scal(icl2))
      ! Correction  rest zero
      ! zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal(3)%scal(i) = 0
    else
      donnees_echange_inst2%tabscal(1)%scal(i) = TH(1)
    endif

    donnees_echange_inst2%tabscal(2)%scal(i) = conduct

  endselect

enddo

endsubroutine donnees_echange

!------------------------------------------------------------------------------!
! Historique des modifications
!
! june 2003 (v0.0.1b): creation of routine
! july 2003          : conductivity dependent on temperature
! oct  2004          : field chained list
! june 2005          : NS case : modification of name and folder : ech_data_kdif
!                      included into donnees_echange
!------------------------------------------------------------------------------!
