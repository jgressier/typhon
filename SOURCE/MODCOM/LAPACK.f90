!------------------------------------------------------------------------------!
! MODULE : LAPACK                         Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Interfaces pour les routines LAPACK
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module LAPACK

!------------------------------------------------------------------------------!
! Liste des routines interfaces
!------------------------------------------------------------------------------!
! GEGV  :
! GESV  :
! GESVX :
! GETRF : decomposition LU              (matrice generale)
! POTRF : decomposition de Choleski     (mat. sym. def. pos.)
! POTRS : resolution AX=B (apres POTRF)
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! GEGV : Calcul de valeurs propres generalisees a partir d'une matrice generale
!------------------------------------------------------------------------------!
  interface lapack_gegv

    subroutine sgegv(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr, work, lwork, info )
      character jobvl, jobvr
      integer   info, lda, ldb, ldvl, ldvr, lwork, n
      real      a(lda,*), alphai(*), alphar(*), b(ldb,*), &
                beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
    endsubroutine sgegv

    subroutine dgegv(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, &
                     beta, vl, ldvl, vr, ldvr, work, lwork, info )
      character        jobvl, jobvr
      integer          info, lda, ldb, ldvl, ldvr, lwork, n
      double precision a(lda,*), alphai(*), alphar(*), b(ldb,*), &
                       beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
    endsubroutine dgegv

    subroutine cgegv(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,  &
                     vl, ldvl, vr, ldvr, work, lwork, rwork, info )
      character jobvl, jobvr
      integer   lda, ldb, n, ldvl, ldvr, lwork, info
      real      rwork(:)
      complex   a(lda,*), b(ldb,*)
      complex   alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
    endsubroutine cgegv

    subroutine zgegv(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,  &
                     vl, ldvl, vr, ldvr, work, lwork, rwork, info)
      character  jobvl, jobvr
      integer    lda, ldb, n, ldvl, ldvr, lwork, info
      real(8)    rwork(*)
      complex(8) a(lda,*), b(ldb,*)
      complex(8) alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
    endsubroutine zgegv

  endinterface


!------------------------------------------------------------------------------!
! GESV : Resolution d'un systeme lineaire A.X=B
!   Decomposition LU de A, solution X dans B
!------------------------------------------------------------------------------!
  interface lapack_gesv
    ! Resolution d'un systeme lineaire A.X=B
    ! Decomposition LU de A, solution X dans B

    subroutine sgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer info, lda, ldb, n, nrhs, ipiv(*)
      real    a(lda,*), b(ldb,*)
    endsubroutine sgesv

    subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer info, lda, ldb, n, nrhs, ipiv(*)
      real(8) a(lda,*), b(ldb,*)
    endsubroutine dgesv

    subroutine cgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer info, lda, ldb, n, nrhs, ipiv(*)
      complex a(lda,*), b(ldb,*)
    endsubroutine cgesv

    subroutine zgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer    info, lda, ldb, n, nrhs, ipiv(*)
      complex(8) a(lda,*), b(ldb,*)
    endsubroutine zgesv

  endinterface


!------------------------------------------------------------------------------!
! GESVX : Resolution (expert) d'un systeme lineaire A.X=B
!   Decomposition LU de A, solution X dans B
!   Estimation des erreurs commises et calcul du nombre de condition
!------------------------------------------------------------------------------!
  interface lapack_gesvx

    subroutine sgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
                      r, c, b, ldb, x, ldx, rcond, ferr, berr, work,       &
                      irwork, info)
      character fact, trans, equed
      integer   info, lda, ldaf, ldb, ldx, n, nrhs, ipiv(*)
      real(4)   rcond, berr(*), c(*), ferr(*), r(*)
      integer   irwork(*)
      real(4)   a(lda,*), af(ldaf,*), b(ldb,*), work(*), x(ldx,*)
    endsubroutine sgesvx

    subroutine dgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
                      r, c, b, ldb, x, ldx, rcond, ferr, berr, work,       &
                      irwork, info)
      character fact, trans, equed
      integer   info, lda, ldaf, ldb, ldx, n, nrhs, ipiv(*)
      real(8)   rcond, berr(*), c(*), ferr(*), r(*)
      integer   irwork(*)
      real(8)   a(lda,*), af(ldaf,*), b(ldb,*), work(*), x(ldx,*)
    endsubroutine dgesvx

    subroutine cgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
                      r, c, b, ldb, x, ldx, rcond, ferr, berr, work,       &
                      irwork, info)
      character  fact, trans, equed
      integer    info, lda, ldaf, ldb, ldx, n, nrhs, ipiv(*)
      real(4)    rcond, berr(*), c(*), ferr(*), r(*)
      real(4)    irwork(*)
      complex(4) a(lda,*), af(ldaf,*), b(ldb,*), work(*), x(ldx,*)
    endsubroutine cgesvx

    subroutine zgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
                      r, c, b, ldb, x, ldx, rcond, ferr, berr, work,       &
                      irwork, info)
      character  fact, trans, equed
      integer    info, lda, ldaf, ldb, ldx, n, nrhs, ipiv(*)
      real(8)    rcond, berr(*), c(*), ferr(*), r(*)
      real(8)    irwork(*)
      complex(8) a(lda,*), af(ldaf,*), b(ldb,*), work(*), x(ldx,*)
    endsubroutine zgesvx

  endinterface


!------------------------------------------------------------------------------!
! GETRF : Decomposition LU d'une matrice generale
!------------------------------------------------------------------------------!
  interface lapack_getrf

    subroutine sgetrf(m, n, a, lda, piv, info)
      integer :: lda, m, n, info, piv(*)
      real    :: a(lda,*)
    endsubroutine sgetrf

    subroutine dgetrf(m, n, a, lda, piv, info)
      integer :: lda, m, n, info, piv(*)
      real(8) :: a(lda,*)
    endsubroutine dgetrf

    subroutine cgetrf(m, n, a, lda, piv, info)
      integer :: lda, m, n, info, piv(*)
      complex :: a(lda,*)
    endsubroutine cgetrf

    subroutine zgetrf(m, n, a, lda, piv, info)
      integer    :: lda, m, n, info, piv(*)
      complex(8) :: a(lda,*)
    endsubroutine zgetrf

  endinterface


!------------------------------------------------------------------------------!
! GETRS : Resolution A.X=B a partir d'une decomposition LU preliminaire
!------------------------------------------------------------------------------!
  interface lapack_getrs

    subroutine sgetrs(trans, n, nrhs, a, lda, piv, b, ldb, info)
      character :: trans
      integer   :: info, lda, ldb, n, nrhs, piv(*)
      real      :: a(lda,*), b(ldb,*)
    endsubroutine sgetrs

    subroutine dgetrs(trans, n, nrhs, a, lda, piv, b, ldb, info)
      character :: trans
      integer   :: info, lda, ldb, n, nrhs, piv(*)
      real(8)   :: a(lda,*), b(ldb,*)
    endsubroutine dgetrs

    subroutine cgetrs(trans, n, nrhs, a, lda, piv, b, ldb, info)
      character :: trans
      integer   :: info, lda, ldb, n, nrhs, piv(*)
      complex   :: a(lda,*), b(ldb,*)
    endsubroutine cgetrs

    subroutine zgetrs(trans, n, nrhs, a, lda, piv, b, ldb, info)
      character  :: trans
      integer    :: info, lda, ldb, n, nrhs, piv(*)
      complex(8) :: a(lda,*), b(ldb,*)
    endsubroutine zgetrs

  endinterface


!------------------------------------------------------------------------------!
! POTRF : Decomposition de Choleski d'une matrice symetrique definie positive
!   A est ecrasee par la matrice U ou L de la decomposition Ut.U ou L.Lt
!   si uplo='U', seul triang. sup. de A est utilisee en entree
!   si uplo='L', seul triang. inf. de A est utilisee en entree
!------------------------------------------------------------------------------!
  interface lapack_potrf

    subroutine spotrf(uplo, n, a, lda, info)
      character :: uplo
      integer   :: lda, n, info
      real      :: a(lda,*)
    endsubroutine spotrf

    subroutine dpotrf(uplo, n, a, lda, info)
      character :: uplo
      integer   :: lda, n, info
      real(8)   :: a(lda,*)
    endsubroutine dpotrf

    subroutine cpotrf(uplo, n, a, lda, info)
      character :: uplo
      integer   :: lda, n, info
      complex   :: a(lda,*)
    endsubroutine cpotrf

    subroutine zpotrf(uplo, n, a, lda, info)
      character  :: uplo
      integer    :: lda, n, info
      complex(8) :: a(lda,*)
    endsubroutine zpotrf

  endinterface


!------------------------------------------------------------------------------!
! POTRS : Resolution de A.X = B (A matrice symetrique ou hermitienne)
!   A est la decomposition de Choleski Ut.U ou L.Lt, calculee par POTRF
!   si uplo='U', seul triang. sup. de A est utilisee en entree
!   si uplo='L', seul triang. inf. de A est utilisee en entree
!------------------------------------------------------------------------------!
  interface lapack_potrs

    subroutine spotrs(uplo, n, nrhs, a, lda, b, ldb, info)
      character :: uplo
      integer   :: info, lda, ldb, n, nrhs
      real      :: a(lda,*), b(ldb,*)
    endsubroutine spotrs

    subroutine dpotrs(uplo, n, nrhs, a, lda, b, ldb, info)
      character :: uplo
      integer   :: info, lda, ldb, n, nrhs
      real(8)   :: a(lda,*), b(ldb,*)
    endsubroutine dpotrs

    subroutine cpotrs(uplo, n, nrhs, a, lda, b, ldb, info)
      character :: uplo
      integer   :: info, lda, ldb, n, nrhs
      complex   :: a(lda,*), b(ldb,*)
    endsubroutine cpotrs

    subroutine zpotrs(uplo, n, nrhs, a, lda, b, ldb, info)
      character  :: uplo
      integer    :: info, lda, ldb, n, nrhs
      complex(8) :: a(lda,*), b(ldb,*)
    endsubroutine zpotrs

  endinterface


!
!  interface lapack_gesvd   ! Calcul de valeurs singulieres
!  endinterface
!

endmodule LAPACK

!------------------------------------------------------------------------------!
! Change history
!
! Sep 2003 : creation du module, interfaces GExxx et POxxx
!------------------------------------------------------------------------------!
