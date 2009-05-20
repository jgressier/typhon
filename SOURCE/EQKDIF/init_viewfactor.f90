!------------------------------------------------------------------------------!
! Procedure : init_viewfactor                  Authors : J. Gressier
!                                              Date    : June 2005
! Fonction
!   Compute view factors for radiating coupling between faces with
!   the right option
!
!------------------------------------------------------------------------------!
subroutine init_viewfactor(defsolver, umesh)

use TYPHMAKE
use MENU_KDIF
use USTMESH
use MENU_SOLVER
use VARCOM
use MATH
use SPARSE_MAT

implicit none

! -- Declaration des entrees --
type(st_ustmesh)  :: umesh

! -- Declaration des entrees/sorties --
type(mnu_solver)  :: defsolver

! -- internal variables --
type(st_sdlu)          :: loc_vf          ! local view factor 
integer(kip)           :: nbc, ib1, ib2   ! number of boco families, index 1 & 2 of boco families
integer(kip)           :: nf1, nf2        ! number of faces in boco families 1 & 2
integer(kip)           :: if1, if2        ! index  of faces in boco families 1 & 2
integer(kip)           :: indf1, indf2    ! index  of faces of boco families 1 & 2 in ust domain
integer(kip)           :: ncp, icp        ! number of couples, index of couples
integer(kip)           :: parsed_cp       ! number of parsed couples of faces
integer(kip)           :: kept_cp         ! number of kept   couples of faces
real(krp)              :: tol, ndist2
type(v3d)              :: dist

! -- Body --

nbc       = umesh%nboco
parsed_cp = 0
kept_cp   = 0
tol       = defsolver%defkdif%tolerance

! all couple of bc families will be parsed (one time)

do ib1 = 1, nbc

  if (defsolver%boco(umesh%boco(ib1)%idefboco)%boco_kdif%radiating == rad_coupled) then
  
    !-----------------------------------------------------
    ! compute view factors on a boco family on itself

    nf1 = umesh%boco(ib1)%nface
    ncp = nf1*(nf1-1)/2            ! compute only the strictly upper part of the matrix

    call new(loc_vf, ncp, ncp)
    icp = 0

    do if1 = 1, nf1-1
      do if2 = if1+1, nf1
        icp = icp + 1
        indf1 = umesh%boco(ib1)%iface(if1)
        indf2 = umesh%boco(ib1)%iface(if2)   ! (!) same boco family
        loc_vf%couple%fils(icp, 1) = indf1
        loc_vf%couple%fils(icp, 2) = indf2
        dist   = umesh%mesh%iface(indf2,1,1)%centre - umesh%mesh%iface(indf1,1,1)%centre
        ndist2 = sqrabs(dist)
        loc_vf%value(icp) = - max(0._krp, umesh%mesh%iface(indf1,1,1)%normale.scal.dist) &
                             *min(0._krp, umesh%mesh%iface(indf2,1,1)%normale.scal.dist) 
        if (loc_vf%value(icp) <= tol*ndist2) loc_vf%value(icp) = 0._krp
        loc_vf%value(icp) = loc_vf%value(icp) / (pi*ndist2**2)
      enddo
    enddo

    icp = count(loc_vf%value > epsilon(tol))

    write(str_w,'(a,i6,a,i6,a)') "  ",icp/1000,"k /",ncp/1000,"k face couples for "//     &
                                 trim(defsolver%boco(umesh%boco(ib1)%idefboco)%family)//"/"//   &
                                 trim(defsolver%boco(umesh%boco(ib1)%idefboco)%family)
    call print_info(10, str_w)

    if (parsed_cp == 0) then
      call new(defsolver%defkdif%viewfactor, ncp, icp)
    else
      call realloc(defsolver%defkdif%viewfactor, parsed_cp+ncp, kept_cp+icp)
    endif

    icp = 0
    do if1 = 1, ncp
      if (loc_vf%value(if1) > epsilon(tol)) then
        icp = icp + 1
        defsolver%defkdif%viewfactor%couple%fils(kept_cp+icp, 1:2) = loc_vf%couple%fils(if1, 1:2)
        defsolver%defkdif%viewfactor%value      (kept_cp+icp)      = loc_vf%value(if1)
      endif
    enddo
    if (kept_cp+icp /= defsolver%defkdif%viewfactor%ncouple) &
      call erreur("Computation of coupled radiating faces", "bad filling of sparse matrix")
    
    call delete(loc_vf)
    kept_cp   = kept_cp   + icp
    parsed_cp = parsed_cp + ncp

    !-----------------------------------------------------
    ! compute view factors between different boco families

    do ib2 = ib1+1, nbc

      if (defsolver%boco(umesh%boco(ib2)%idefboco)%boco_kdif%radiating == rad_coupled) then

        nf2 = umesh%boco(ib2)%nface
        ncp = nf1*nf2

        call new(loc_vf, ncp, ncp)
        icp = 0

        do if1 = 1, nf1-1
          do if2 = if1+1, nf1
            icp = icp + 1
            indf1  = umesh%boco(ib1)%iface(if1)
            indf2  = umesh%boco(ib2)%iface(if2)
            loc_vf%couple%fils(icp, 1) = indf1
            loc_vf%couple%fils(icp, 2) = indf2
            dist   = umesh%mesh%iface(indf2,1,1)%centre - umesh%mesh%iface(indf1,1,1)%centre
            ndist2 = sqrabs(dist)
            loc_vf%value(icp) = - max(0._krp, umesh%mesh%iface(indf1,1,1)%normale.scal.dist) &
                                 *min(0._krp, umesh%mesh%iface(indf2,1,1)%normale.scal.dist) 
            if (loc_vf%value(icp) <= tol*ndist2) loc_vf%value(icp) = 0._krp
            loc_vf%value(icp) = loc_vf%value(icp) / (pi*ndist2**2)
          enddo
        enddo

        icp = count(loc_vf%value > epsilon(tol))

        write(str_w,'(a,i6,a,i6,a)') "  ",icp/1000,"k /",ncp/1000,"k face couples for "//     &
                                    trim(defsolver%boco(umesh%boco(ib1)%idefboco)%family)//"/"//   &
                                    trim(defsolver%boco(umesh%boco(ib2)%idefboco)%family)
        call print_info(10, str_w)

        call realloc(defsolver%defkdif%viewfactor, parsed_cp+ncp, kept_cp+icp)
        icp = 0
        do if1 = 1, ncp
          if (loc_vf%value(if1) > epsilon(tol)) then
            icp = icp + 1
            defsolver%defkdif%viewfactor%couple%fils(kept_cp+icp, 1:2) = loc_vf%couple%fils(if1, 1:2)
            defsolver%defkdif%viewfactor%value      (kept_cp+icp)      = loc_vf%value(if1)
          endif
        enddo
        if (kept_cp+icp /= defsolver%defkdif%viewfactor%ncouple) &
          call erreur("Computation of coupled radiating faces", "bad filling of sparse matrix")
    
        call delete(loc_vf)
        kept_cp   = kept_cp   + icp
        parsed_cp = parsed_cp + ncp

      endif

    enddo

  endif

enddo

write(str_w,'(a,i6,a,i6,a)') "  total number of face couples : ",kept_cp/1000," k/",parsed_cp/1000,"k"
call print_info(10, str_w)


endsubroutine init_viewfactor

!------------------------------------------------------------------------------!
! Change history
!
! june 2005 : created
!------------------------------------------------------------------------------!


