!-----------------------------------------------------------------------------!
! Procedure : output_tec_ust_boco         Auteur : J. Gressier & A. Gardi
!                                         Date   : Mars 2011
!> @brief Ecriture fichier des champs NON STRUCTURES de chaque zone au format TECPLOT
!>   Valeurs au centre des faces limites (BOCO)
!------------------------------------------------------------------------------!
subroutine output_tec_ust_boco(uf, umesh, field, defsolver)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use USTMESH
use DEFFIELD
use MENU_SOLVER

implicit none

! -- Declaration des entrees --
integer          :: uf            ! unite d'ecriture
type(st_ustmesh) :: umesh         ! unstructured mesh
type(st_field)   :: field         ! field of values
type(mnu_solver) :: defsolver     ! solver parameters

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: indboco, ifb, indf, icella, icellb
type(v3d) :: centre
real(krp) :: surface
real(krp) :: temperature

! -- Debut de la procedure --

write(uf,*) 'ZONE T="USTMESH"' !, F=FEPOINT, N=',umesh%nvtex,',E=',ncell

do indboco = 1, umesh%nboco
  write(uf,*) '# BOCO SET = ', umesh%boco(indboco)%family, ', TOTAL FACES = ', umesh%boco(indboco)%nface, ', R = ', defsolver%defns%properties(1)%r_const
  do ifb = 1, umesh%boco(indboco)%nface
    indf = umesh%boco(indboco)%iface(ifb)
    icella = umesh%facecell%fils(indf,1)
    icellb = umesh%facecell%fils(indf,2)
    centre = umesh%mesh%face_center(indf,1)
    surface = umesh%mesh%face_surf(indf)
    select case(defsolver%typ_solver)
    case(solKDIF)
      write(uf,'(5e18.8)') centre%x, centre%y, centre%z, ((field%etatprim%tabscal(1)%scal(icella)+field%etatprim%tabscal(1)%scal(icellb))/2), surface

    case(solNS)
!      write(uf, *) 'indboco=',indboco,', ifb=',ifb,', indf=',indf, ', icell=',icell
      temperature = (field%etatprim%tabscal(2)%scal(icella) + field%etatprim%tabscal(2)%scal(icellb)) / &  ! T = p / (rho * R) => T_ab = (p_a+p_b)/((rho_a+rho_b)*R)
                    ( (field%etatprim%tabscal(1)%scal(icella)+ field%etatprim%tabscal(1)%scal(icellb)) * &
                    defsolver%defns%properties(1)%r_const )
      write(uf,'(9e18.8)') centre%x, centre%y, centre%z, &
                ((field%etatprim%tabvect(1)%vect(icella)%x+field%etatprim%tabvect(1)%vect(icellb)%x)/2), &
                ((field%etatprim%tabvect(1)%vect(icella)%y+field%etatprim%tabvect(1)%vect(icellb)%y)/2), &
                ((field%etatprim%tabvect(1)%vect(icella)%z+field%etatprim%tabvect(1)%vect(icellb)%z)/2), &
                ((field%etatprim%tabscal(2)%scal(icella)+field%etatprim%tabscal(2)%scal(icellb))/2), &
                temperature, surface
    endselect
  enddo
enddo

endsubroutine output_tec_ust_boco
