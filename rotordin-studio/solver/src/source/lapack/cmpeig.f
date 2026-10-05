!     $Id$
!     ==================================================================
!
!>    @file cmpeig.f
!>    @brief solves eigenvalues for Campbell diagram using LAPACK routines,
!>    last changes:<br>
!>    removed subs to global module lapack.f - francisco - 03/12/2008.
!
!     ==================================================================
!
! =====================================================================
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
! =====================================================================
!
!
! =====================================================================
!> @brief DLASCL multiplies a general rectangular matrix by a real scalar defined as cto/cfrom.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          TYPE
!       INTEGER            INFO, KL, KU, LDA, M, N
!       DOUBLE PRECISION   CFROM, CTO
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DLASCL multiplies the M by N real matrix A by the real scalar
!> CTO/CFROM.  This is done without over/underflow as long as the final
!> result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
!> A may be full, upper triangular, lower triangular, upper Hessenberg,
!> or banded.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] TYPE
!> \verbatim
!>          TYPE is CHARACTER*1
!>          TYPE indices the storage type of the input matrix.
!>          = 'G':  A is a full matrix.
!>          = 'L':  A is a lower triangular matrix.
!>          = 'U':  A is an upper triangular matrix.
!>          = 'H':  A is an upper Hessenberg matrix.
!>          = 'B':  A is a symmetric band matrix with lower bandwidth KL
!>                  and upper bandwidth KU and with the only the lower
!>                  half stored.
!>          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
!>                  and upper bandwidth KU and with the only the upper
!>                  half stored.
!>          = 'Z':  A is a band matrix with lower bandwidth KL and upper
!>                  bandwidth KU. See DGBTRF for storage details.
!> \endverbatim
!>
!> @param[in] KL
!> \verbatim
!>          KL is INTEGER
!>          The lower bandwidth of A.  Referenced only if TYPE = 'B',
!>          'Q' or 'Z'.
!> \endverbatim
!>
!> @param[in] KU
!> \verbatim
!>          KU is INTEGER
!>          The upper bandwidth of A.  Referenced only if TYPE = 'B',
!>          'Q' or 'Z'.
!> \endverbatim
!>
!> @param[in] CFROM
!> \verbatim
!>          CFROM is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] CTO
!> \verbatim
!>          CTO is DOUBLE PRECISION
!>
!>          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
!>          without over/underflow if the final result CTO*A(I,J)/CFROM
!>          can be represented without over/underflow.  CFROM must be
!>          nonzero.
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix A.  M >= 0.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix A.  N >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is DOUBLE PRECISION array, dimension (LDA,N)
!>          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
!>          storage type.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          0  - successful exit
!>          <0 - if INFO = -i, the i-th argument had an illegal value.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
      SUBROUTINE DLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      DOUBLE PRECISION   CFROM, CTO
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DLASCL multiplies the M by N real matrix A by the real scalar
!  CTO/CFROM.  This is done without over/underflow as long as the final
!  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
!  A may be full, upper triangular, lower triangular, upper Hessenberg,
!  or banded.
!
!  Arguments
!  =========
!
!  TYPE    (input) CHARACTER*1
!          TYPE indices the storage type of the input matrix.
!          = 'G':  A is a full matrix.
!          = 'L':  A is a lower triangular matrix.
!          = 'U':  A is an upper triangular matrix.
!          = 'H':  A is an upper Hessenberg matrix.
!          = 'B':  A is a symmetric band matrix with lower bandwidth KL
!                  and upper bandwidth KU and with the only the lower
!                  half stored.
!          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
!                  and upper bandwidth KU and with the only the upper
!                  half stored.
!          = 'Z':  A is a band matrix with lower bandwidth KL and upper
!                  bandwidth KU.
!
!  KL      (input) INTEGER
!          The lower bandwidth of A.  Referenced only if TYPE = 'B',
!          'Q' or 'Z'.
!
!  KU      (input) INTEGER
!          The upper bandwidth of A.  Referenced only if TYPE = 'B',
!          'Q' or 'Z'.
!
!  CFROM   (input) DOUBLE PRECISION
!  CTO     (input) DOUBLE PRECISION
!          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
!          without over/underflow if the final result CTO*A(I,J)/CFROM
!          can be represented without over/underflow.  CFROM must be
!          nonzero.
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
!          storage type.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  INFO    (output) INTEGER
!          0  - successful exit
!          <0 - if INFO = -i, the i-th argument had an illegal value.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      DOUBLE PRECISION   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
!
      IF( LSAME( TYPE, 'G' ) ) THEN
         ITYPE = 0
      ELSE IF( LSAME( TYPE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LSAME( TYPE, 'U' ) ) THEN
         ITYPE = 2
      ELSE IF( LSAME( TYPE, 'H' ) ) THEN
         ITYPE = 3
      ELSE IF( LSAME( TYPE, 'B' ) ) THEN
         ITYPE = 4
      ELSE IF( LSAME( TYPE, 'Q' ) ) THEN
         ITYPE = 5
      ELSE IF( LSAME( TYPE, 'Z' ) ) THEN
         ITYPE = 6
      ELSE
         ITYPE = -1
      END IF
!
      IF( ITYPE.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( CFROM.EQ.ZERO ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR.
     &         ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR.
     &            ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) )
     &             THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR.
     &            ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR.
     &            ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         END IF
      END IF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASCL', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 .OR. M.EQ.0 )
     &   RETURN
!
!     Get machine parameters
!
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
!
      CFROMC = CFROM
      CTOC = CTO
!
   10 CONTINUE
      CFROM1 = CFROMC*SMLNUM
      CTO1 = CTOC / BIGNUM
      IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
         MUL = SMLNUM
         DONE = .FALSE.
         CFROMC = CFROM1
      ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
         MUL = BIGNUM
         DONE = .FALSE.
         CTOC = CTO1
      ELSE
         MUL = CTOC / CFROMC
         DONE = .TRUE.
      END IF
!
      IF( ITYPE.EQ.0 ) THEN
!
!        Full matrix
!
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
!
      ELSE IF( ITYPE.EQ.1 ) THEN
!
!        Lower triangular matrix
!
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
!
      ELSE IF( ITYPE.EQ.2 ) THEN
!
!        Upper triangular matrix
!
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
!
      ELSE IF( ITYPE.EQ.3 ) THEN
!
!        Upper Hessenberg matrix
!
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
!
      ELSE IF( ITYPE.EQ.4 ) THEN
!
!        Lower half of a symmetric band matrix
!
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
!
      ELSE IF( ITYPE.EQ.5 ) THEN
!
!        Upper half of a symmetric band matrix
!
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
!
      ELSE IF( ITYPE.EQ.6 ) THEN
!
!        Band matrix
!
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
!
      END IF
!
      IF( .NOT.DONE )
     &   GO TO 10
!
      RETURN
!
!     End of DLASCL
!
      END
!
!  =====================================================================
!> @brief ZTGEX2 swaps adjacent diagonal blocks in an upper (quasi)
!> triangular matrix pair by an unitary equivalence transformation.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZTGEX2( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
!                          LDZ, J1, INFO )
!
!       .. Scalar Arguments ..
!       LOGICAL            WANTQ, WANTZ
!       INTEGER            INFO, J1, LDA, LDB, LDQ, LDZ, N
!       ..
!       .. Array Arguments ..
!       !16         A( LDA, ! ), B( LDB, ! ), Q( LDQ, ! ),
!      $                   Z( LDZ, ! )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZTGEX2 swaps adjacent diagonalbyblocks (A11,B11) and (A22,B22)
!> in an upper triangular matrix pair (A, B) by an unitary equivalence
!> transformation.
!>
!> (A, B) must be in generalized Schur canonical form, that is, A and
!> B are both upper triangular.
!>
!> Optionally, the matrices Q and Z of generalized Schur vectors are
!> updated.
!>
!>        Q(in) ! A(in) ! !H = Q(out) ! A(out) ! !H
!>        Q(in) ! B(in) ! !H = Q(out) ! B(out) ! !H
!>
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] WANTQ
!> \verbatim
!>          WANTQ is LOGICAL
!>          .TRUE. : update the left transformation matrix Q;
!>          .FALSE.: do not update Q.
!> \endverbatim
!>
!> @param[in] WANTZ
!> \verbatim
!>          WANTZ is LOGICAL
!>          .TRUE. : update the right transformation matrix Z;
!>          .FALSE.: do not update Z.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrices A and B. N >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is !16 arrays, dimensions (LDA,N)
!>          On entry, the matrix A in the pair (A, B).
!>          On exit, the updated matrix A.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A. LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in,out] B
!> \verbatim
!>          B is !16 arrays, dimensions (LDB,N)
!>          On entry, the matrix B in the pair (A, B).
!>          On exit, the updated matrix B.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B. LDB >= max(1,N).
!> \endverbatim
!>
!> @param[in,out] Q
!> \verbatim
!>          Q is !16 array, dimension (LDZ,N)
!>          If WANTQ = .TRUE, on entry, the unitary matrix Q. On exit,
!>          the updated matrix Q.
!>          Not referenced if WANTQ = .FALSE..
!> \endverbatim
!>
!> @param[in] LDQ
!> \verbatim
!>          LDQ is INTEGER
!>          The leading dimension of the array Q. LDQ >= 1;
!>          If WANTQ = .TRUE., LDQ >= N.
!> \endverbatim
!>
!> @param[in,out] Z
!> \verbatim
!>          Z is !16 array, dimension (LDZ,N)
!>          If WANTZ = .TRUE, on entry, the unitary matrix Z. On exit,
!>          the updated matrix Z.
!>          Not referenced if WANTZ = .FALSE..
!> \endverbatim
!>
!> @param[in] LDZ
!> \verbatim
!>          LDZ is INTEGER
!>          The leading dimension of the array Z. LDZ >= 1;
!>          If WANTZ = .TRUE., LDZ >= N.
!> \endverbatim
!>
!> @param[in] J1
!> \verbatim
!>          J1 is INTEGER
!>          The index to the first block (A11, B11).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>           =0:  Successful exit.
!>           =1:  The transformed matrix pair (A, B) would be too far
!>                from generalized Schur form; the problem is ill-
!>                conditioned.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September
!
!  Further Details:
!  =====================
!>
!>  In the current code both weak and strong stability tests are
!>  performed. The user can omit the strong stability test by changing
!>  the internal logical parameter WANDS to .FALSE.. See ref. [2] for
!>  details.
!
!  Contributors:
!  ==================
!>
!>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!>     Umea University, S-901Umea, Sweden.
!
!  References:
!  ================
!>
!>  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
!>      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
!>      M.S. Moonen et al (eds), Linear Algebra for Large Scale and
!>      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.
!> \n
!>  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
!>      Eigenvalues of a Regular Matrix Pair (A, B) and Condition
!>      Estimation: Theory, Algorithms and Software, Report UMINF-94.04,
!>      Department of Computing Science, Umea University, S-901Umea,
!>      Sweden, 1994. Also as LAPACK Working Note 87. To appear in
!>      Numerical Algorithms, 1996.
!
      SUBROUTINE ZTGEX2( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
     &                   LDZ, J1, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      LOGICAL            WANTQ, WANTZ
      INTEGER            INFO, J1, LDA, LDB, LDQ, LDZ, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
     &                   Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  ZTGEX2 swaps adjacent diagonal 1 by 1 blocks (A11,B11) and (A22,B22)
!  in an upper triangular matrix pair (A, B) by an unitary equivalence
!  transformation.
!
!  (A, B) must be in generalized Schur canonical form, that is, A and
!  B are both upper triangular.
!
!  Optionally, the matrices Q and Z of generalized Schur vectors are
!  updated.
!
!         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)'
!         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)'
!
!
!  Arguments
!  =========
!
!  WANTQ   (input) LOGICAL
!          .TRUE. : update the left transformation matrix Q;
!          .FALSE.: do not update Q.
!
!  WANTZ   (input) LOGICAL
!          .TRUE. : update the right transformation matrix Z;
!          .FALSE.: do not update Z.
!
!  N       (input) INTEGER
!          The order of the matrices A and B. N >= 0.
!
!  A       (input/output) COMPLEX*16 arrays, dimensions (LDA,N)
!          On entry, the matrix A in the pair (A, B).
!          On exit, the updated matrix A.
!
!  LDA     (input)  INTEGER
!          The leading dimension of the array A. LDA >= max(1,N).
!
!  B       (input/output) COMPLEX*16 arrays, dimensions (LDB,N)
!          On entry, the matrix B in the pair (A, B).
!          On exit, the updated matrix B.
!
!  LDB     (input)  INTEGER
!          The leading dimension of the array B. LDB >= max(1,N).
!
!  Q       (input/output) COMPLEX*16 array, dimension (LDZ,N)
!          If WANTQ = .TRUE, on entry, the unitary matrix Q. On exit,
!          the updated matrix Q.
!          Not referenced if WANTQ = .FALSE..
!
!  LDQ     (input) INTEGER
!          The leading dimension of the array Q. LDQ >= 1;
!          If WANTQ = .TRUE., LDQ >= N.
!
!  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)
!          If WANTZ = .TRUE, on entry, the unitary matrix Z. On exit,
!          the updated matrix Z.
!          Not referenced if WANTZ = .FALSE..
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z. LDZ >= 1;
!          If WANTZ = .TRUE., LDZ >= N.
!
!  J1      (input) INTEGER
!          The index to the first block (A11, B11).
!
!  INFO    (output) INTEGER
!           =0:  Successful exit.
!           =1:  The transformed matrix pair (A, B) would be too far
!                from generalized Schur form; the problem is ill-
!                conditioned.
!
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!     Umea University, S-901 87 Umea, Sweden.
!
!  In the current code both weak and strong stability tests are
!  performed. The user can omit the strong stability test by changing
!  the internal logical parameter WANDS to .FALSE.. See ref. [2] for
!  details.
!
!  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
!      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
!      M.S. Moonen et al (eds), Linear Algebra for Large Scale and
!      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.
!
!  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
!      Eigenvalues of a Regular Matrix Pair (A, B) and Condition
!      Estimation: Theory, Algorithms and Software, Report UMINF-94.04,
!      Department of Computing Science, Umea University, S-901 87 Umea,
!      Sweden, 1994. Also as LAPACK Working Note 87. To appear in
!      Numerical Algorithms, 1996.
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     &                   CONE = ( 1.0D+0, 0.0D+0 ) )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 10.0D+0 )
      INTEGER            LDST
      PARAMETER          ( LDST = 2 )
      LOGICAL            WANDS
      PARAMETER          ( WANDS = .TRUE. )
!     ..
!     .. Local Scalars ..
      LOGICAL            DTRONG, WEAK
      INTEGER            I, M
      DOUBLE PRECISION   CQ, CZ, EPS, SA, SB, SCALE, SMLNUM, SS, SUM,
     &                   THRESH, WS
      COMPLEX*16         CDUM, F, G, SQ, SZ
!     ..
!     .. Local Arrays ..
      COMPLEX*16         S( LDST, LDST ), T( LDST, LDST ), WORK( 8 )
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZLACPY, ZLARTG, ZLASSQ, ZROT
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCONJG, MAX, SQRT
!     ..
!     .. Executable Statements ..
!
      INFO = 0
!
!     Quick return if possible
!
      IF( N.LE.1 )
     &   RETURN
!
      M = LDST
      WEAK = .FALSE.
      DTRONG = .FALSE.
!
!     Make a local copy of selected block in (A, B)
!
      CALL ZLACPY( 'Full', M, M, A( J1, J1 ), LDA, S, LDST )
      CALL ZLACPY( 'Full', M, M, B( J1, J1 ), LDB, T, LDST )
!
!     Compute the threshold for testing the acceptance of swapping.
!
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      SCALE = DBLE( CZERO )
      SUM = DBLE( CONE )
      CALL ZLACPY( 'Full', M, M, S, LDST, WORK, M )
      CALL ZLACPY( 'Full', M, M, T, LDST, WORK( M*M+1 ), M )
      CALL ZLASSQ( 2*M*M, WORK, 1, SCALE, SUM )
      SA = SCALE*SQRT( SUM )
      THRESH = MAX( TEN*EPS*SA, SMLNUM )
!
!     Compute unitary QL and RQ that swap 1-by-1 and 1-by-1 blocks
!     using Givens rotations and perform the swap tentatively.
!
      F = S( 2, 2 )*T( 1, 1 ) - T( 2, 2 )*S( 1, 1 )
      G = S( 2, 2 )*T( 1, 2 ) - T( 2, 2 )*S( 1, 2 )
      SA = ABS( S( 2, 2 ) )
      SB = ABS( T( 2, 2 ) )
      CALL ZLARTG( G, F, CZ, SZ, CDUM )
      SZ = -SZ
      CALL ZROT( 2, S( 1, 1 ), 1, S( 1, 2 ), 1, CZ, DCONJG( SZ ) )
      CALL ZROT( 2, T( 1, 1 ), 1, T( 1, 2 ), 1, CZ, DCONJG( SZ ) )
      IF( SA.GE.SB ) THEN
         CALL ZLARTG( S( 1, 1 ), S( 2, 1 ), CQ, SQ, CDUM )
      ELSE
         CALL ZLARTG( T( 1, 1 ), T( 2, 1 ), CQ, SQ, CDUM )
      END IF
      CALL ZROT( 2, S( 1, 1 ), LDST, S( 2, 1 ), LDST, CQ, SQ )
      CALL ZROT( 2, T( 1, 1 ), LDST, T( 2, 1 ), LDST, CQ, SQ )
!
!     Weak stability test: |S21| + |T21| <= O(EPS F-norm((S, T)))
!
      WS = ABS( S( 2, 1 ) ) + ABS( T( 2, 1 ) )
      WEAK = WS.LE.THRESH
      IF( .NOT.WEAK )
     &   GO TO 20
!
      IF( WANDS ) THEN
!
!        Strong stability test:
!           F-norm((A-QL'*S*QR, B-QL'*T*QR)) <= O(EPS*F-norm((A, B)))
!
         CALL ZLACPY( 'Full', M, M, S, LDST, WORK, M )
         CALL ZLACPY( 'Full', M, M, T, LDST, WORK( M*M+1 ), M )
         CALL ZROT( 2, WORK, 1, WORK( 3 ), 1, CZ, -DCONJG( SZ ) )
         CALL ZROT( 2, WORK( 5 ), 1, WORK( 7 ), 1, CZ, -DCONJG( SZ ) )
         CALL ZROT( 2, WORK, 2, WORK( 2 ), 2, CQ, -SQ )
         CALL ZROT( 2, WORK( 5 ), 2, WORK( 6 ), 2, CQ, -SQ )
         DO 10 I = 1, 2
            WORK( I ) = WORK( I ) - A( J1+I-1, J1 )
            WORK( I+2 ) = WORK( I+2 ) - A( J1+I-1, J1+1 )
            WORK( I+4 ) = WORK( I+4 ) - B( J1+I-1, J1 )
            WORK( I+6 ) = WORK( I+6 ) - B( J1+I-1, J1+1 )
   10    CONTINUE
         SCALE = DBLE( CZERO )
         SUM = DBLE( CONE )
         CALL ZLASSQ( 2*M*M, WORK, 1, SCALE, SUM )
         SS = SCALE*SQRT( SUM )
         DTRONG = SS.LE.THRESH
         IF( .NOT.DTRONG )
     &      GO TO 20
      END IF
!
!     If the swap is accepted ("weakly" and "strongly"), apply the
!     equivalence transformations to the original matrix pair (A,B)
!
      CALL ZROT( J1+1, A( 1, J1 ), 1, A( 1, J1+1 ), 1, CZ,
     &           DCONJG( SZ ) )
      CALL ZROT( J1+1, B( 1, J1 ), 1, B( 1, J1+1 ), 1, CZ,
     &           DCONJG( SZ ) )
      CALL ZROT( N-J1+1, A( J1, J1 ), LDA, A( J1+1, J1 ), LDA, CQ, SQ )
      CALL ZROT( N-J1+1, B( J1, J1 ), LDB, B( J1+1, J1 ), LDB, CQ, SQ )
!
!     Set  N1 by N2 (2,1) blocks to 0
!
      A( J1+1, J1 ) = CZERO
      B( J1+1, J1 ) = CZERO
!
!     Accumulate transformations into Q and Z if requested.
!
      IF( WANTZ )
     &   CALL ZROT( N, Z( 1, J1 ), 1, Z( 1, J1+1 ), 1, CZ,
     &              DCONJG( SZ ) )
      IF( WANTQ )
     &   CALL ZROT( N, Q( 1, J1 ), 1, Q( 1, J1+1 ), 1, CQ,
     &              DCONJG( SQ ) )
!
!     Exit with INFO = 0 if swap was successfully performed.
!
      RETURN
!
!     Exit with INFO = 1 if swap was rejected.
!
   20 CONTINUE
      INFO = 1
      RETURN
!
!     End of ZTGEX2
!
      END
!
!  =====================================================================
!> @brief ZTGEXC
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZTGEXC( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
!                          LDZ, IFST, ILST, INFO )
!
!       .. Scalar Arguments ..
!       LOGICAL            WANTQ, WANTZ
!       INTEGER            IFST, ILST, INFO, LDA, LDB, LDQ, LDZ, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
!      $                   Z( LDZ, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZTGEXC reorders the generalized Schur decomposition of a complex
!> matrix pair (A,B), using an unitary equivalence transformation
!> (A, B) := Q * (A, B) * Z**H, so that the diagonal block of (A, B) with
!> row index IFST is moved to row ILST.
!>
!> (A, B) must be in generalized Schur canonical form, that is, A and
!> B are both upper triangular.
!>
!> Optionally, the matrices Q and Z of generalized Schur vectors are
!> updated.
!>
!>        Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
!>        Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] WANTQ
!> \verbatim
!>          WANTQ is LOGICAL
!>          .TRUE. : update the left transformation matrix Q;
!>          .FALSE.: do not update Q.
!> \endverbatim
!>
!> @param[in] WANTZ
!> \verbatim
!>          WANTZ is LOGICAL
!>          .TRUE. : update the right transformation matrix Z;
!>          .FALSE.: do not update Z.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrices A and B. N >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the upper triangular matrix A in the pair (A, B).
!>          On exit, the updated matrix A.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A. LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in,out] B
!> \verbatim
!>          B is COMPLEX*16 array, dimension (LDB,N)
!>          On entry, the upper triangular matrix B in the pair (A, B).
!>          On exit, the updated matrix B.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B. LDB >= max(1,N).
!> \endverbatim
!>
!> @param[in,out] Q
!> \verbatim
!>          Q is COMPLEX*16 array, dimension (LDZ,N)
!>          On entry, if WANTQ = .TRUE., the unitary matrix Q.
!>          On exit, the updated matrix Q.
!>          If WANTQ = .FALSE., Q is not referenced.
!> \endverbatim
!>
!> @param[in] LDQ
!> \verbatim
!>          LDQ is INTEGER
!>          The leading dimension of the array Q. LDQ >= 1;
!>          If WANTQ = .TRUE., LDQ >= N.
!> \endverbatim
!>
!> @param[in,out] Z
!> \verbatim
!>          Z is COMPLEX*16 array, dimension (LDZ,N)
!>          On entry, if WANTZ = .TRUE., the unitary matrix Z.
!>          On exit, the updated matrix Z.
!>          If WANTZ = .FALSE., Z is not referenced.
!> \endverbatim
!>
!> @param[in] LDZ
!> \verbatim
!>          LDZ is INTEGER
!>          The leading dimension of the array Z. LDZ >= 1;
!>          If WANTZ = .TRUE., LDZ >= N.
!> \endverbatim
!>
!> @param[in] IFST
!> \verbatim
!>          IFST is INTEGER
!> \endverbatim
!>
!> @param[in,out] ILST
!> \verbatim
!>          ILST is INTEGER
!>          Specify the reordering of the diagonal blocks of (A, B).
!>          The block with row index IFST is moved to row ILST, by a
!>          sequence of swapping between adjacent blocks.
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>           =0:  Successful exit.
!>           <0:  if INFO = -i, the i-th argument had an illegal value.
!>           =1:  The transformed matrix pair (A, B) would be too far
!>                from generalized Schur form; the problem is ill-
!>                conditioned. (A, B) may have been partially reordered,
!>                and ILST points to the first row of the current
!>                position of the block being moved.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date November 2011
!
!  Contributors:
!  ==================
!>
!>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!>     Umea University, S-901 87 Umea, Sweden.
!
!  References:
!  ================
!>
!>  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
!>      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
!>      M.S. Moonen et al (eds), Linear Algebra for Large Scale and
!>      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.
!> \n
!>  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
!>      Eigenvalues of a Regular Matrix Pair (A, B) and Condition
!>      Estimation: Theory, Algorithms and Software, Report
!>      UMINF - 94.04, Department of Computing Science, Umea University,
!>      S-901 87 Umea, Sweden, 1994. Also as LAPACK Working Note 87.
!>      To appear in Numerical Algorithms, 1996.
!> \n
!>  [3] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
!>      for Solving the Generalized Sylvester Equation and Estimating the
!>      Separation between Regular Matrix Pairs, Report UMINF - 93.23,
!>      Department of Computing Science, Umea University, S-901 87 Umea,
!>      Sweden, December 1993, Revised April 1994, Also as LAPACK working
!>      Note 75. To appear in ACM Trans. on Math. Software, Vol 22, No 1,
!>      1996.
!
      SUBROUTINE ZTGEXC( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
     &                   LDZ, IFST, ILST, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      LOGICAL            WANTQ, WANTZ
      INTEGER            IFST, ILST, INFO, LDA, LDB, LDQ, LDZ, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
     &                   Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  ZTGEXC reorders the generalized Schur decomposition of a complex
!  matrix pair (A,B), using an unitary equivalence transformation
!  (A, B) := Q * (A, B) * Z', so that the diagonal block of (A, B) with
!  row index IFST is moved to row ILST.
!
!  (A, B) must be in generalized Schur canonical form, that is, A and
!  B are both upper triangular.
!
!  Optionally, the matrices Q and Z of generalized Schur vectors are
!  updated.
!
!         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)'
!         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)'
!
!  Arguments
!  =========
!
!  WANTQ   (input) LOGICAL
!          .TRUE. : update the left transformation matrix Q;
!          .FALSE.: do not update Q.
!
!  WANTZ   (input) LOGICAL
!          .TRUE. : update the right transformation matrix Z;
!          .FALSE.: do not update Z.
!
!  N       (input) INTEGER
!          The order of the matrices A and B. N >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the upper triangular matrix A in the pair (A, B).
!          On exit, the updated matrix A.
!
!  LDA     (input)  INTEGER
!          The leading dimension of the array A. LDA >= max(1,N).
!
!  B       (input/output) COMPLEX*16 array, dimension (LDB,N)
!          On entry, the upper triangular matrix B in the pair (A, B).
!          On exit, the updated matrix B.
!
!  LDB     (input)  INTEGER
!          The leading dimension of the array B. LDB >= max(1,N).
!
!  Q       (input/output) COMPLEX*16 array, dimension (LDZ,N)
!          On entry, if WANTQ = .TRUE., the unitary matrix Q.
!          On exit, the updated matrix Q.
!          If WANTQ = .FALSE., Q is not referenced.
!
!  LDQ     (input) INTEGER
!          The leading dimension of the array Q. LDQ >= 1;
!          If WANTQ = .TRUE., LDQ >= N.
!
!  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)
!          On entry, if WANTZ = .TRUE., the unitary matrix Z.
!          On exit, the updated matrix Z.
!          If WANTZ = .FALSE., Z is not referenced.
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z. LDZ >= 1;
!          If WANTZ = .TRUE., LDZ >= N.
!
!  IFST    (input) INTEGER
!  ILST    (input/output) INTEGER
!          Specify the reordering of the diagonal blocks of (A, B).
!          The block with row index IFST is moved to row ILST, by a
!          sequence of swapping between adjacent blocks.
!
!  INFO    (output) INTEGER
!           =0:  Successful exit.
!           <0:  if INFO = -i, the i-th argument had an illegal value.
!           =1:  The transformed matrix pair (A, B) would be too far
!                from generalized Schur form; the problem is ill-
!                conditioned. (A, B) may have been partially reordered,
!                and ILST points to the first row of the current
!                position of the block being moved.
!
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!     Umea University, S-901 87 Umea, Sweden.
!
!  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
!      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
!      M.S. Moonen et al (eds), Linear Algebra for Large Scale and
!      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.
!
!  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
!      Eigenvalues of a Regular Matrix Pair (A, B) and Condition
!      Estimation: Theory, Algorithms and Software, Report
!      UMINF - 94.04, Department of Computing Science, Umea University,
!      S-901 87 Umea, Sweden, 1994. Also as LAPACK Working Note 87.
!      To appear in Numerical Algorithms, 1996.
!
!  [3] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
!      for Solving the Generalized Sylvester Equation and Estimating the
!      Separation between Regular Matrix Pairs, Report UMINF - 93.23,
!      Department of Computing Science, Umea University, S-901 87 Umea,
!      Sweden, December 1993, Revised April 1994, Also as LAPACK working
!      Note 75. To appear in ACM Trans. on Math. Software, Vol 22, No 1,
!      1996.
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER            HERE
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZTGEX2
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Decode and test input arguments.
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDQ.LT.1 .OR. WANTQ .AND. ( LDQ.LT.MAX( 1, N ) ) ) THEN
         INFO = -9
      ELSE IF( LDZ.LT.1 .OR. WANTZ .AND. ( LDZ.LT.MAX( 1, N ) ) ) THEN
         INFO = -11
      ELSE IF( IFST.LT.1 .OR. IFST.GT.N ) THEN
         INFO = -12
      ELSE IF( ILST.LT.1 .OR. ILST.GT.N ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTGEXC', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.LE.1 )
     &   RETURN
      IF( IFST.EQ.ILST )
     &   RETURN
!
      IF( IFST.LT.ILST ) THEN
!
         HERE = IFST
!
   10    CONTINUE
!
!        Swap with next one below
!
         CALL ZTGEX2( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, LDZ,
     &                HERE, INFO )
         IF( INFO.NE.0 ) THEN
            ILST = HERE
            RETURN
         END IF
         HERE = HERE + 1
         IF( HERE.LT.ILST )
     &      GO TO 10
         HERE = HERE - 1
      ELSE
         HERE = IFST - 1
!
   20    CONTINUE
!
!        Swap with next one above
!
         CALL ZTGEX2( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, LDZ,
     &                HERE, INFO )
         IF( INFO.NE.0 ) THEN
            ILST = HERE
            RETURN
         END IF
         HERE = HERE - 1
         IF( HERE.GE.ILST )
     &      GO TO 20
         HERE = HERE + 1
      END IF
      ILST = HERE
      RETURN
!
!     End of ZTGEXC
!
      END
!
!  =====================================================================
!> @brief ZGETC2 computes the LU factorization with complete pivoting
!> of the general n-by-n matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGETC2( N, A, LDA, IPIV, JPIV, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, LDA, N
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * ), JPIV( * )
!       COMPLEX*16         A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGETC2 computes an LU factorization, using complete pivoting, of the
!> n-by-n matrix A. The factorization has the form A = P * L * U * Q,
!> where P and Q are permutation matrices, L is lower triangular with
!> unit diagonal elements and U is upper triangular.
!>
!> This is a level 1 BLAS version of the algorithm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A. N >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA, N)
!>          On entry, the n-by-n matrix to be factored.
!>          On exit, the factors L and U from the factorization
!>          A = P*L*U*Q; the unit diagonal elements of L are not stored.
!>          If U(k, k) appears to be less than SMIN, U(k, k) is given the
!>          value of SMIN, giving a nonsingular perturbed system.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1, N).
!> \endverbatim
!>
!> @param[out] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (N).
!>          The pivot indices; for 1 <= i <= N, row i of the
!>          matrix has been interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[out] JPIV
!> \verbatim
!>          JPIV is INTEGER array, dimension (N).
!>          The pivot indices; for 1 <= j <= N, column j of the
!>          matrix has been interchanged with column JPIV(j).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>           = 0: successful exit
!>           > 0: if INFO = k, U(k, k) is likely to produce overflow if
!>                one tries to solve for x in Ax = b. So U is perturbed
!>                to avoid the overflow.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date November 2013
!
!  Contributors:
!  ==================
!>
!>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!>     Umea University, S-901 87 Umea, Sweden.
!
      SUBROUTINE ZGETC2( N, A, LDA, IPIV, JPIV, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * ), JPIV( * )
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ZGETC2 computes an LU factorization, using complete pivoting, of the
!  n-by-n matrix A. The factorization has the form A = P * L * U * Q,
!  where P and Q are permutation matrices, L is lower triangular with
!  unit diagonal elements and U is upper triangular.
!
!  This is a level 1 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix A. N >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA, N)
!          On entry, the n-by-n matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U*Q; the unit diagonal elements of L are not stored.
!          If U(k, k) appears to be less than SMIN, U(k, k) is given the
!          value of SMIN, giving a nonsingular perturbed system.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1, N).
!
!  IPIV    (output) INTEGER array, dimension (N).
!          The pivot indices; for 1 <= i <= N, row i of the
!          matrix has been interchanged with row IPIV(i).
!
!  JPIV    (output) INTEGER array, dimension (N).
!          The pivot indices; for 1 <= j <= N, column j of the
!          matrix has been interchanged with column JPIV(j).
!
!  INFO    (output) INTEGER
!           = 0: successful exit
!           > 0: if INFO = k, U(k, k) is likely to produce overflow if
!                one tries to solve for x in Ax = b. So U is perturbed
!                to avoid the overflow.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!     Umea University, S-901 87 Umea, Sweden.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, IP, IPV, J, JP, JPV
      DOUBLE PRECISION   BIGNUM, EPS, SMIN, SMLNUM, XMAX
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZGERU, ZSWAP
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DCMPLX, MAX
!     ..
!     .. Executable Statements ..
!
!     Set constants to control overflow
!
      INFO = 0
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
!
!     Factorize A using complete pivoting.
!     Set pivots less than SMIN to SMIN
!
      DO 40 I = 1, N - 1
!
!        Find max element in matrix A
!
         XMAX = ZERO
         DO 20 IP = I, N
            DO 10 JP = I, N
               IF( ABS( A( IP, JP ) ).GE.XMAX ) THEN
                  XMAX = ABS( A( IP, JP ) )
                  IPV = IP
                  JPV = JP
               END IF
   10       CONTINUE
   20    CONTINUE
         IF( I.EQ.1 )
     &      SMIN = MAX( EPS*XMAX, SMLNUM )
!
!        Swap rows
!
         IF( IPV.NE.I )
     &      CALL ZSWAP( N, A( IPV, 1 ), LDA, A( I, 1 ), LDA )
         IPIV( I ) = IPV
!
!        Swap columns
!
         IF( JPV.NE.I )
     &      CALL ZSWAP( N, A( 1, JPV ), 1, A( 1, I ), 1 )
         JPIV( I ) = JPV
!
!        Check for singularity
!
         IF( ABS( A( I, I ) ).LT.SMIN ) THEN
            INFO = I
            A( I, I ) = DCMPLX( SMIN, ZERO )
         END IF
         DO 30 J = I + 1, N
            A( J, I ) = A( J, I ) / A( I, I )
   30    CONTINUE
         CALL ZGERU( N-I, N-I, -DCMPLX( ONE ), A( I+1, I ), 1,
     &               A( I, I+1 ), LDA, A( I+1, I+1 ), LDA )
   40 CONTINUE
!
      IF( ABS( A( N, N ) ).LT.SMIN ) THEN
         INFO = N
         A( N, N ) = DCMPLX( SMIN, ZERO )
      END IF
      RETURN
!
!     End of ZGETC2
!
      END
!
!  =====================================================================
!> @brief ZGESC2 solves a system of linear equations using the LU
!> factorization with complete pivoting computed by sgetc2.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGESC2( N, A, LDA, RHS, IPIV, JPIV, SCALE )
!
!       .. Scalar Arguments ..
!       INTEGER            LDA, N
!       DOUBLE PRECISION   SCALE
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * ), JPIV( * )
!       COMPLEX*16         A( LDA, * ), RHS( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGESC2 solves a system of linear equations
!>
!>           A * X = scale* RHS
!>
!> with a general N-by-N matrix A using the LU factorization with
!> complete pivoting computed by ZGETC2.
!>
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix A.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA, N)
!>          On entry, the  LU part of the factorization of the n-by-n
!>          matrix A computed by ZGETC2:  A = P * L * U * Q
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1, N).
!> \endverbatim
!>
!> @param[in,out] RHS
!> \verbatim
!>          RHS is COMPLEX*16 array, dimension N.
!>          On entry, the right hand side vector b.
!>          On exit, the solution vector X.
!> \endverbatim
!>
!> @param[in] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (N).
!>          The pivot indices; for 1 <= i <= N, row i of the
!>          matrix has been interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[in] JPIV
!> \verbatim
!>          JPIV is INTEGER array, dimension (N).
!>          The pivot indices; for 1 <= j <= N, column j of the
!>          matrix has been interchanged with column JPIV(j).
!> \endverbatim
!>
!> @param[out] SCALE
!> \verbatim
!>          SCALE is DOUBLE PRECISION
!>           On exit, SCALE contains the scale factor. SCALE is chosen
!>           0 <= SCALE <= 1 to prevent owerflow in the solution.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
!  Contributors:
!  ==================
!>
!>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!>     Umea University, S-901 87 Umea, Sweden.
!
      SUBROUTINE ZGESC2( N, A, LDA, RHS, IPIV, JPIV, SCALE )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            LDA, N
      DOUBLE PRECISION   SCALE
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * ), JPIV( * )
      COMPLEX*16         A( LDA, * ), RHS( * )
!     ..
!
!  Purpose
!  =======
!
!  ZGESC2 solves a system of linear equations
!
!            A * X = scale* RHS
!
!  with a general N-by-N matrix A using the LU factorization with
!  complete pivoting computed by ZGETC2.
!
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.
!
!  A       (input) COMPLEX*16 array, dimension (LDA, N)
!          On entry, the  LU part of the factorization of the n-by-n
!          matrix A computed by ZGETC2:  A = P * L * U * Q
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1, N).
!
!  RHS     (input/output) COMPLEX*16 array, dimension N.
!          On entry, the right hand side vector b.
!          On exit, the solution vector X.
!
!  IPIV    (input) INTEGER array, dimension (N).
!          The pivot indices; for 1 <= i <= N, row i of the
!          matrix has been interchanged with row IPIV(i).
!
!  JPIV    (input) INTEGER array, dimension (N).
!          The pivot indices; for 1 <= j <= N, column j of the
!          matrix has been interchanged with column JPIV(j).
!
!  SCALE    (output) DOUBLE PRECISION
!           On exit, SCALE contains the scale factor. SCALE is chosen
!           0 <= SCALE <= 1 to prevent owerflow in the solution.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!     Umea University, S-901 87 Umea, Sweden.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   BIGNUM, EPS, SMLNUM
      COMPLEX*16         TEMP
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZLASWP, ZSCAL
!     ..
!     .. External Functions ..
      INTEGER            IZAMAX
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           IZAMAX, DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX
!     ..
!     .. Executable Statements ..
!
!     Set constant to control overflow
!
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
!
!     Apply permutations IPIV to RHS
!
      CALL ZLASWP( 1, RHS, LDA, 1, N-1, IPIV, 1 )
!
!     Solve for L part
!
      DO 20 I = 1, N - 1
         DO 10 J = I + 1, N
            RHS( J ) = RHS( J ) - A( J, I )*RHS( I )
   10    CONTINUE
   20 CONTINUE
!
!     Solve for U part
!
      SCALE = ONE
!
!     Check for scaling
!
      I = IZAMAX( N, RHS, 1 )
      IF( TWO*SMLNUM*ABS( RHS( I ) ).GT.ABS( A( N, N ) ) ) THEN
         TEMP = DCMPLX( ONE / TWO, ZERO ) / ABS( RHS( I ) )
         CALL ZSCAL( N, TEMP, RHS( 1 ), 1 )
         SCALE = SCALE*DBLE( TEMP )
      END IF
      DO 40 I = N, 1, -1
         TEMP = DCMPLX( ONE, ZERO ) / A( I, I )
         RHS( I ) = RHS( I )*TEMP
         DO 30 J = I + 1, N
            RHS( I ) = RHS( I ) - RHS( J )*( A( I, J )*TEMP )
   30    CONTINUE
   40 CONTINUE
!
!     Apply permutations JPIV to the solution (RHS)
!
      CALL ZLASWP( 1, RHS, LDA, 1, N-1, JPIV, -1 )
      RETURN
!
!     End of ZGESC2
!
      END
!
!  =====================================================================
!> @brief DZASUM
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       DOUBLE PRECISION FUNCTION DZASUM(N,ZX,INCX)
!
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16 ZX(*)
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!>    DZASUM takes the sum of the absolute values.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date November 2011
!
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, 3/11/78.
!>     modified 3/93 to return if incx .le. 0.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!
      DOUBLE PRECISION FUNCTION DZASUM(N,ZX,INCX)
!     .. Scalar Arguments ..
      INTEGER INCX,N
!     ..
!     .. Array Arguments ..
      DOUBLE COMPLEX ZX(*)
!     ..
!
!  Purpose
!  =======
!
!     takes the sum of the absolute values.
!     jack dongarra, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
!     .. Local Scalars ..
      DOUBLE PRECISION STEMP
      INTEGER I,IX
!     ..
!     .. External Functions ..
      DOUBLE PRECISION DCABS1
      EXTERNAL DCABS1
!     ..
      DZASUM = 0.0d0
      STEMP = 0.0d0
      IF (N.LE.0 .OR. INCX.LE.0) RETURN
      IF (INCX.EQ.1) GO TO 20
!
!        code for increment not equal to 1
!
      IX = 1
      DO 10 I = 1,N
          STEMP = STEMP + DCABS1(ZX(IX))
          IX = IX + INCX
   10 CONTINUE
      DZASUM = STEMP
      RETURN
!
!        code for increment equal to 1
!
   20 DO 30 I = 1,N
          STEMP = STEMP + DCABS1(ZX(I))
   30 CONTINUE
      DZASUM = STEMP
      RETURN
      END
!
!  =====================================================================
!> @brief ZDOTU
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       COMPLEX*16 FUNCTION ZDOTU(N,ZX,INCX,ZY,INCY)
!
!       .. Scalar Arguments ..
!       INTEGER INCX,INCY,N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16 ZX(*),ZY(*)
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!>    ZDOTU forms the dot product of two vectors.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date November 2011
!
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, 3/11/78.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!
      DOUBLE COMPLEX FUNCTION ZDOTU(N,ZX,INCX,ZY,INCY)
!     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
!     ..
!     .. Array Arguments ..
      DOUBLE COMPLEX ZX(*),ZY(*)
!     ..
!
!  Purpose
!  =======
!
!     ZDOTU forms the dot product of two vectors.
!
!  Further Details
!  ===============
!
!     jack dongarra, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!     .. Local Scalars ..
      DOUBLE COMPLEX ZTEMP
      INTEGER I,IX,IY
!     ..
      ZTEMP = (0.0d0,0.0d0)
      ZDOTU = (0.0d0,0.0d0)
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          ZTEMP = ZTEMP + ZX(IX)*ZY(IY)
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      ZDOTU = ZTEMP
      RETURN
!
!        code for both increments equal to 1
!
   20 DO 30 I = 1,N
          ZTEMP = ZTEMP + ZX(I)*ZY(I)
   30 CONTINUE
      ZDOTU = ZTEMP
      RETURN
      END
!
!  =====================================================================
!> @brief IDAMAX
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       INTEGER FUNCTION IDAMAX(N,DX,INCX)
!
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION DX(*)
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!>    IDAMAX finds the index of element having max. absolute value.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date November 2011
!
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 3/93 to return if incx .le. 0.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!
      INTEGER FUNCTION IDAMAX(N,DX,INCX)
!     .. Scalar Arguments ..
      INTEGER INCX,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION DX(*)
!     ..
!
!  Purpose
!  =======
!
!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
!     .. Local Scalars ..
      DOUBLE PRECISION DMAX
      INTEGER I,IX
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DABS
!     ..
      IDAMAX = 0
      IF (N.LT.1 .OR. INCX.LE.0) RETURN
      IDAMAX = 1
      IF (N.EQ.1) RETURN
      IF (INCX.EQ.1) GO TO 20
!
!        code for increment not equal to 1
!
      IX = 1
      DMAX = DABS(DX(1))
      IX = IX + INCX
      DO 10 I = 2,N
          IF (DABS(DX(IX)).LE.DMAX) GO TO 5
          IDAMAX = I
          DMAX = DABS(DX(IX))
    5     IX = IX + INCX
   10 CONTINUE
      RETURN
!
!        code for increment equal to 1
!
   20 DMAX = DABS(DX(1))
      DO 30 I = 2,N
          IF (DABS(DX(I)).LE.DMAX) GO TO 30
          IDAMAX = I
          DMAX = DABS(DX(I))
   30 CONTINUE
      RETURN
      END
!
!  =====================================================================
!> @brief ZLATRS solves a triangular system of equations with the
!>scale factor set to prevent overflow.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLATRS( UPLO, TRANS, DIAG, NORMIN, N, A, LDA, X, SCALE,
!                          CNORM, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          DIAG, NORMIN, TRANS, UPLO
!       INTEGER            INFO, LDA, N
!       DOUBLE PRECISION   SCALE
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   CNORM( * )
!       COMPLEX*16         A( LDA, * ), X( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLATRS solves one of the triangular systems
!>
!>    A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
!>
!> with scaling to prevent overflow.  Here A is an upper or lower
!> triangular matrix, A**T denotes the transpose of A, A**H denotes the
!> conjugate transpose of A, x and b are n-element vectors, and s is a
!> scaling factor, usually less than or equal to 1, chosen so that the
!> components of x will be less than the overflow threshold.  If the
!> unscaled problem will not cause overflow, the Level 2 BLAS routine
!> ZTRSV is called. If the matrix A is singular (A(j,j) = 0 for some j),
!> then s is set to 0 and a non-trivial solution to A*x = 0 is returned.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>          Specifies whether the matrix A is upper or lower triangular.
!>          = 'U':  Upper triangular
!>          = 'L':  Lower triangular
!> \endverbatim
!>
!> @param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>          Specifies the operation applied to A.
!>          = 'N':  Solve A * x = s*b     (No transpose)
!>          = 'T':  Solve A**T * x = s*b  (Transpose)
!>          = 'C':  Solve A**H * x = s*b  (Conjugate transpose)
!> \endverbatim
!>
!> @param[in] DIAG
!> \verbatim
!>          DIAG is CHARACTER*1
!>          Specifies whether or not the matrix A is unit triangular.
!>          = 'N':  Non-unit triangular
!>          = 'U':  Unit triangular
!> \endverbatim
!>
!> @param[in] NORMIN
!> \verbatim
!>          NORMIN is CHARACTER*1
!>          Specifies whether CNORM has been set or not.
!>          = 'Y':  CNORM contains the column norms on entry
!>          = 'N':  CNORM is not set on entry.  On exit, the norms will
!>                  be computed and stored in CNORM.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          The triangular matrix A.  If UPLO = 'U', the leading n by n
!>          upper triangular part of the array A contains the upper
!>          triangular matrix, and the strictly lower triangular part of
!>          A is not referenced.  If UPLO = 'L', the leading n by n lower
!>          triangular part of the array A contains the lower triangular
!>          matrix, and the strictly upper triangular part of A is not
!>          referenced.  If DIAG = 'U', the diagonal elements of A are
!>          also not referenced and are assumed to be 1.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max (1,N).
!> \endverbatim
!>
!> @param[in,out] X
!> \verbatim
!>          X is COMPLEX*16 array, dimension (N)
!>          On entry, the right hand side b of the triangular system.
!>          On exit, X is overwritten by the solution vector x.
!> \endverbatim
!>
!> @param[out] SCALE
!> \verbatim
!>          SCALE is DOUBLE PRECISION
!>          The scaling factor s for the triangular system
!>             A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b.
!>          If SCALE = 0, the matrix A is singular or badly scaled, and
!>          the vector x is an exact or approximate solution to A*x = 0.
!> \endverbatim
!>
!> @param[in,out] CNORM
!> \verbatim
!>          CNORM is DOUBLE PRECISION array, dimension (N)
!>
!>          If NORMIN = 'Y', CNORM is an input argument and CNORM(j)
!>          contains the norm of the off-diagonal part of the j-th column
!>          of A.  If TRANS = 'N', CNORM(j) must be greater than or equal
!>          to the infinity-norm, and if TRANS = 'T' or 'C', CNORM(j)
!>          must be greater than or equal to the 1-norm.
!>
!>          If NORMIN = 'N', CNORM is an output argument and CNORM(j)
!>          returns the 1-norm of the offdiagonal part of the j-th column
!>          of A.
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit
!>          < 0:  if INFO = -k, the k-th argument had an illegal value
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>  A rough bound on x is computed; if that is less than overflow, ZTRSV
!>  is called, otherwise, specific code is used which checks for possible
!>  overflow or divide-by-zero at every operation.
!>
!>  A columnwise scheme is used for solving A*x = b.  The basic algorithm
!>  if A is lower triangular is
!>
!>       x[1:n] := b[1:n]
!>       for j = 1, ..., n
!>            x(j) := x(j) / A(j,j)
!>            x[j+1:n] := x[j+1:n] - x(j) * A[j+1:n,j]
!>       end
!>
!>  Define bounds on the components of x after j iterations of the loop:
!>     M(j) = bound on x[1:j]
!>     G(j) = bound on x[j+1:n]
!>  Initially, let M(0) = 0 and G(0) = max{x(i), i=1,...,n}.
!>
!>  Then for iteration j+1 we have
!>     M(j+1) <= G(j) / | A(j+1,j+1) |
!>     G(j+1) <= G(j) + M(j+1) * | A[j+2:n,j+1] |
!>            <= G(j) ( 1 + CNORM(j+1) / | A(j+1,j+1) | )
!>
!>  where CNORM(j+1) is greater than or equal to the infinity-norm of
!>  column j+1 of A, not counting the diagonal.  Hence
!>
!>     G(j) <= G(0) product ( 1 + CNORM(i) / | A(i,i) | )
!>                  1<=i<=j
!>  and
!>
!>     |x(j)| <= ( G(0) / |A(j,j)| ) product ( 1 + CNORM(i) / |A(i,i)| )
!>                                   1<=i< j
!>
!>  Since |x(j)| <= M(j), we use the Level 2 BLAS routine ZTRSV if the
!>  reciprocal of the largest M(j), j=1,..,n, is larger than
!>  max(underflow, 1/overflow).
!>
!>  The bound on x(j) is also used to determine when a step in the
!>  columnwise method can be performed without fear of overflow.  If
!>  the computed bound is greater than a large constant, x is scaled to
!>  prevent overflow, but if the bound overflows, x is set to 0, x(j) to
!>  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.
!>
!>  Similarly, a row-wise scheme is used to solve A**T *x = b  or
!>  A**H *x = b.  The basic algorithm for A upper triangular is
!>
!>       for j = 1, ..., n
!>            x(j) := ( b(j) - A[1:j-1,j]' * x[1:j-1] ) / A(j,j)
!>       end
!>
!>  We simultaneously compute two bounds
!>       G(j) = bound on ( b(i) - A[1:i-1,i]' * x[1:i-1] ), 1<=i<=j
!>       M(j) = bound on x(i), 1<=i<=j
!>
!>  The initial values are G(0) = 0, M(0) = max{b(i), i=1,..,n}, and we
!>  add the constraint G(j) >= G(j-1) and M(j) >= M(j-1) for j >= 1.
!>  Then the bound on x(j) is
!>
!>       M(j) <= M(j-1) * ( 1 + CNORM(j) ) / | A(j,j) |
!>
!>            <= M(0) * product ( ( 1 + CNORM(i) ) / |A(i,i)| )
!>                      1<=i<=j
!>
!>  and we can safely call ZTRSV if 1/M(n) and 1/G(n) are both greater
!>  than max(underflow, 1/overflow).
!> \endverbatim
!>
!
      SUBROUTINE ZLATRS( UPLO, TRANS, DIAG, NORMIN, N, A, LDA, X, SCALE,
     &                   CNORM, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          DIAG, NORMIN, TRANS, UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   SCALE
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   CNORM( * )
      COMPLEX*16         A( LDA, * ), X( * )
!     ..
!
!  Purpose
!  =======
!
!  ZLATRS solves one of the triangular systems
!
!     A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
!
!  with scaling to prevent overflow.  Here A is an upper or lower
!  triangular matrix, A**T denotes the transpose of A, A**H denotes the
!  conjugate transpose of A, x and b are n-element vectors, and s is a
!  scaling factor, usually less than or equal to 1, chosen so that the
!  components of x will be less than the overflow threshold.  If the
!  unscaled problem will not cause overflow, the Level 2 BLAS routine
!  ZTRSV is called. If the matrix A is singular (A(j,j) = 0 for some j),
!  then s is set to 0 and a non-trivial solution to A*x = 0 is returned.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies whether the matrix A is upper or lower triangular.
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular
!
!  TRANS   (input) CHARACTER*1
!          Specifies the operation applied to A.
!          = 'N':  Solve A * x = s*b     (No transpose)
!          = 'T':  Solve A**T * x = s*b  (Transpose)
!          = 'C':  Solve A**H * x = s*b  (Conjugate transpose)
!
!  DIAG    (input) CHARACTER*1
!          Specifies whether or not the matrix A is unit triangular.
!          = 'N':  Non-unit triangular
!          = 'U':  Unit triangular
!
!  NORMIN  (input) CHARACTER*1
!          Specifies whether CNORM has been set or not.
!          = 'Y':  CNORM contains the column norms on entry
!          = 'N':  CNORM is not set on entry.  On exit, the norms will
!                  be computed and stored in CNORM.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The triangular matrix A.  If UPLO = 'U', the leading n by n
!          upper triangular part of the array A contains the upper
!          triangular matrix, and the strictly lower triangular part of
!          A is not referenced.  If UPLO = 'L', the leading n by n lower
!          triangular part of the array A contains the lower triangular
!          matrix, and the strictly upper triangular part of A is not
!          referenced.  If DIAG = 'U', the diagonal elements of A are
!          also not referenced and are assumed to be 1.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max (1,N).
!
!  X       (input/output) COMPLEX*16 array, dimension (N)
!          On entry, the right hand side b of the triangular system.
!          On exit, X is overwritten by the solution vector x.
!
!  SCALE   (output) DOUBLE PRECISION
!          The scaling factor s for the triangular system
!             A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b.
!          If SCALE = 0, the matrix A is singular or badly scaled, and
!          the vector x is an exact or approximate solution to A*x = 0.
!
!  CNORM   (input or output) DOUBLE PRECISION array, dimension (N)
!
!          If NORMIN = 'Y', CNORM is an input argument and CNORM(j)
!          contains the norm of the off-diagonal part of the j-th column
!          of A.  If TRANS = 'N', CNORM(j) must be greater than or equal
!          to the infinity-norm, and if TRANS = 'T' or 'C', CNORM(j)
!          must be greater than or equal to the 1-norm.
!
!          If NORMIN = 'N', CNORM is an output argument and CNORM(j)
!          returns the 1-norm of the offdiagonal part of the j-th column
!          of A.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -k, the k-th argument had an illegal value
!
!  Further Details
!  ======= =======
!
!  A rough bound on x is computed; if that is less than overflow, ZTRSV
!  is called, otherwise, specific code is used which checks for possible
!  overflow or divide-by-zero at every operation.
!
!  A columnwise scheme is used for solving A*x = b.  The basic algorithm
!  if A is lower triangular is
!
!       x[1:n] := b[1:n]
!       for j = 1, ..., n
!            x(j) := x(j) / A(j,j)
!            x[j+1:n] := x[j+1:n] - x(j) * A[j+1:n,j]
!       end
!
!  Define bounds on the components of x after j iterations of the loop:
!     M(j) = bound on x[1:j]
!     G(j) = bound on x[j+1:n]
!  Initially, let M(0) = 0 and G(0) = max{x(i), i=1,...,n}.
!
!  Then for iteration j+1 we have
!     M(j+1) <= G(j) / | A(j+1,j+1) |
!     G(j+1) <= G(j) + M(j+1) * | A[j+2:n,j+1] |
!            <= G(j) ( 1 + CNORM(j+1) / | A(j+1,j+1) | )
!
!  where CNORM(j+1) is greater than or equal to the infinity-norm of
!  column j+1 of A, not counting the diagonal.  Hence
!
!     G(j) <= G(0) product ( 1 + CNORM(i) / | A(i,i) | )
!                  1<=i<=j
!  and
!
!     |x(j)| <= ( G(0) / |A(j,j)| ) product ( 1 + CNORM(i) / |A(i,i)| )
!                                   1<=i< j
!
!  Since |x(j)| <= M(j), we use the Level 2 BLAS routine ZTRSV if the
!  reciprocal of the largest M(j), j=1,..,n, is larger than
!  max(underflow, 1/overflow).
!
!  The bound on x(j) is also used to determine when a step in the
!  columnwise method can be performed without fear of overflow.  If
!  the computed bound is greater than a large constant, x is scaled to
!  prevent overflow, but if the bound overflows, x is set to 0, x(j) to
!  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.
!
!  Similarly, a row-wise scheme is used to solve A**T *x = b  or
!  A**H *x = b.  The basic algorithm for A upper triangular is
!
!       for j = 1, ..., n
!            x(j) := ( b(j) - A[1:j-1,j]' * x[1:j-1] ) / A(j,j)
!       end
!
!  We simultaneously compute two bounds
!       G(j) = bound on ( b(i) - A[1:i-1,i]' * x[1:i-1] ), 1<=i<=j
!       M(j) = bound on x(i), 1<=i<=j
!
!  The initial values are G(0) = 0, M(0) = max{b(i), i=1,..,n}, and we
!  add the constraint G(j) >= G(j-1) and M(j) >= M(j-1) for j >= 1.
!  Then the bound on x(j) is
!
!       M(j) <= M(j-1) * ( 1 + CNORM(j) ) / | A(j,j) |
!
!            <= M(0) * product ( ( 1 + CNORM(i) ) / |A(i,i)| )
!                      1<=i<=j
!
!  and we can safely call ZTRSV if 1/M(n) and 1/G(n) are both greater
!  than max(underflow, 1/overflow).
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0,
     &                   TWO = 2.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOTRAN, NOUNIT, UPPER
      INTEGER            I, IMAX, J, JFIRST, JINC, JLAST
      DOUBLE PRECISION   BIGNUM, GROW, REC, SMLNUM, TJJ, TMAX, TSCAL,
     &                   XBND, XJ, XMAX
      COMPLEX*16         CSUMJ, TJJS, USCAL, ZDUM
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX, IZAMAX
      DOUBLE PRECISION   DLAMCH, DZASUM
      COMPLEX*16         ZDOTC, ZDOTU, ZLADIV
      EXTERNAL           LSAME, IDAMAX, IZAMAX, DLAMCH, DZASUM, ZDOTC,
     &                   ZDOTU, ZLADIV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DSCAL, XERBLA, ZAXPY, ZDSCAL, ZTRSV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, MAX, MIN
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION   CABS1, CABS2
!     ..
!     .. Statement Function definitions ..
      CABS1( ZDUM ) = ABS( DBLE( ZDUM ) ) + ABS( DIMAG( ZDUM ) )
      CABS2( ZDUM ) = ABS( DBLE( ZDUM ) / 2.D0 ) +
     &                ABS( DIMAG( ZDUM ) / 2.D0 )
!     ..
!     .. Executable Statements ..
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOTRAN = LSAME( TRANS, 'N' )
      NOUNIT = LSAME( DIAG, 'N' )
!
!     Test the input parameters.
!
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     &         LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LSAME( NORMIN, 'Y' ) .AND. .NOT.
     &         LSAME( NORMIN, 'N' ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZLATRS', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 )
     &   RETURN
!
!     Determine machine dependent parameters to control overflow.
!
      SMLNUM = DLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SMLNUM / DLAMCH( 'Precision' )
      BIGNUM = ONE / SMLNUM
      SCALE = ONE
!
      IF( LSAME( NORMIN, 'N' ) ) THEN
!
!        Compute the 1-norm of each column, not including the diagonal.
!
         IF( UPPER ) THEN
!
!           A is upper triangular.
!
            DO 10 J = 1, N
               CNORM( J ) = DZASUM( J-1, A( 1, J ), 1 )
   10       CONTINUE
         ELSE
!
!           A is lower triangular.
!
            DO 20 J = 1, N - 1
               CNORM( J ) = DZASUM( N-J, A( J+1, J ), 1 )
   20       CONTINUE
            CNORM( N ) = ZERO
         END IF
      END IF
!
!     Scale the column norms by TSCAL if the maximum element in CNORM is
!     greater than BIGNUM/2.
!
      IMAX = IDAMAX( N, CNORM, 1 )
      TMAX = CNORM( IMAX )
      IF( TMAX.LE.BIGNUM*HALF ) THEN
         TSCAL = ONE
      ELSE
         TSCAL = HALF / ( SMLNUM*TMAX )
         CALL DSCAL( N, TSCAL, CNORM, 1 )
      END IF
!
!     Compute a bound on the computed solution vector to see if the
!     Level 2 BLAS routine ZTRSV can be used.
!
      XMAX = ZERO
      DO 30 J = 1, N
         XMAX = MAX( XMAX, CABS2( X( J ) ) )
   30 CONTINUE
      XBND = XMAX
!
      IF( NOTRAN ) THEN
!
!        Compute the growth in A * x = b.
!
         IF( UPPER ) THEN
            JFIRST = N
            JLAST = 1
            JINC = -1
         ELSE
            JFIRST = 1
            JLAST = N
            JINC = 1
         END IF
!
         IF( TSCAL.NE.ONE ) THEN
            GROW = ZERO
            GO TO 60
         END IF
!
         IF( NOUNIT ) THEN
!
!           A is non-unit triangular.
!
!           Compute GROW = 1/G(j) and XBND = 1/M(j).
!           Initially, G(0) = max{x(i), i=1,...,n}.
!
            GROW = HALF / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 40 J = JFIRST, JLAST, JINC
!
!              Exit the loop if the growth factor is too small.
!
               IF( GROW.LE.SMLNUM )
     &            GO TO 60
!
               TJJS = A( J, J )
               TJJ = CABS1( TJJS )
!
               IF( TJJ.GE.SMLNUM ) THEN
!
!                 M(j) = G(j-1) / abs(A(j,j))
!
                  XBND = MIN( XBND, MIN( ONE, TJJ )*GROW )
               ELSE
!
!                 M(j) could overflow, set XBND to 0.
!
                  XBND = ZERO
               END IF
!
               IF( TJJ+CNORM( J ).GE.SMLNUM ) THEN
!
!                 G(j) = G(j-1)*( 1 + CNORM(j) / abs(A(j,j)) )
!
                  GROW = GROW*( TJJ / ( TJJ+CNORM( J ) ) )
               ELSE
!
!                 G(j) could overflow, set GROW to 0.
!
                  GROW = ZERO
               END IF
   40       CONTINUE
            GROW = XBND
         ELSE
!
!           A is unit triangular.
!
!           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
!
            GROW = MIN( ONE, HALF / MAX( XBND, SMLNUM ) )
            DO 50 J = JFIRST, JLAST, JINC
!
!              Exit the loop if the growth factor is too small.
!
               IF( GROW.LE.SMLNUM )
     &            GO TO 60
!
!              G(j) = G(j-1)*( 1 + CNORM(j) )
!
               GROW = GROW*( ONE / ( ONE+CNORM( J ) ) )
   50       CONTINUE
         END IF
   60    CONTINUE
!
      ELSE
!
!        Compute the growth in A**T * x = b  or  A**H * x = b.
!
         IF( UPPER ) THEN
            JFIRST = 1
            JLAST = N
            JINC = 1
         ELSE
            JFIRST = N
            JLAST = 1
            JINC = -1
         END IF
!
         IF( TSCAL.NE.ONE ) THEN
            GROW = ZERO
            GO TO 90
         END IF
!
         IF( NOUNIT ) THEN
!
!           A is non-unit triangular.
!
!           Compute GROW = 1/G(j) and XBND = 1/M(j).
!           Initially, M(0) = max{x(i), i=1,...,n}.
!
            GROW = HALF / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 70 J = JFIRST, JLAST, JINC
!
!              Exit the loop if the growth factor is too small.
!
               IF( GROW.LE.SMLNUM )
     &            GO TO 90
!
!              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) )
!
               XJ = ONE + CNORM( J )
               GROW = MIN( GROW, XBND / XJ )
!
               TJJS = A( J, J )
               TJJ = CABS1( TJJS )
!
               IF( TJJ.GE.SMLNUM ) THEN
!
!                 M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j))
!
                  IF( XJ.GT.TJJ )
     &               XBND = XBND*( TJJ / XJ )
               ELSE
!
!                 M(j) could overflow, set XBND to 0.
!
                  XBND = ZERO
               END IF
   70       CONTINUE
            GROW = MIN( GROW, XBND )
         ELSE
!
!           A is unit triangular.
!
!           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
!
            GROW = MIN( ONE, HALF / MAX( XBND, SMLNUM ) )
            DO 80 J = JFIRST, JLAST, JINC
!
!              Exit the loop if the growth factor is too small.
!
               IF( GROW.LE.SMLNUM )
     &            GO TO 90
!
!              G(j) = ( 1 + CNORM(j) )*G(j-1)
!
               XJ = ONE + CNORM( J )
               GROW = GROW / XJ
   80       CONTINUE
         END IF
   90    CONTINUE
      END IF
!
      IF( ( GROW*TSCAL ).GT.SMLNUM ) THEN
!
!        Use the Level 2 BLAS solve if the reciprocal of the bound on
!        elements of X is not too small.
!
         CALL ZTRSV( UPLO, TRANS, DIAG, N, A, LDA, X, 1 )
      ELSE
!
!        Use a Level 1 BLAS solve, scaling intermediate results.
!
         IF( XMAX.GT.BIGNUM*HALF ) THEN
!
!           Scale X so that its components are less than or equal to
!           BIGNUM in absolute value.
!
            SCALE = ( BIGNUM*HALF ) / XMAX
            CALL ZDSCAL( N, SCALE, X, 1 )
            XMAX = BIGNUM
         ELSE
            XMAX = XMAX*TWO
         END IF
!
         IF( NOTRAN ) THEN
!
!           Solve A * x = b
!
            DO 120 J = JFIRST, JLAST, JINC
!
!              Compute x(j) = b(j) / A(j,j), scaling x if necessary.
!
               XJ = CABS1( X( J ) )
               IF( NOUNIT ) THEN
                  TJJS = A( J, J )*TSCAL
               ELSE
                  TJJS = TSCAL
                  IF( TSCAL.EQ.ONE )
     &               GO TO 110
               END IF
               TJJ = CABS1( TJJS )
               IF( TJJ.GT.SMLNUM ) THEN
!
!                    abs(A(j,j)) > SMLNUM:
!
                  IF( TJJ.LT.ONE ) THEN
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
!
!                          Scale x by 1/b(j).
!
                        REC = ONE / XJ
                        CALL ZDSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  X( J ) = ZLADIV( X( J ), TJJS )
                  XJ = CABS1( X( J ) )
               ELSE IF( TJJ.GT.ZERO ) THEN
!
!                    0 < abs(A(j,j)) <= SMLNUM:
!
                  IF( XJ.GT.TJJ*BIGNUM ) THEN
!
!                       Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM
!                       to avoid overflow when dividing by A(j,j).
!
                     REC = ( TJJ*BIGNUM ) / XJ
                     IF( CNORM( J ).GT.ONE ) THEN
!
!                          Scale by 1/CNORM(j) to avoid overflow when
!                          multiplying x(j) times column j.
!
                        REC = REC / CNORM( J )
                     END IF
                     CALL ZDSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
                  X( J ) = ZLADIV( X( J ), TJJS )
                  XJ = CABS1( X( J ) )
               ELSE
!
!                    A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
!                    scale = 0, and compute a solution to A*x = 0.
!
                  DO 100 I = 1, N
                     X( I ) = ZERO
  100             CONTINUE
                  X( J ) = ONE
                  XJ = ONE
                  SCALE = ZERO
                  XMAX = ZERO
               END IF
  110          CONTINUE
!
!              Scale x if necessary to avoid overflow when adding a
!              multiple of column j of A.
!
               IF( XJ.GT.ONE ) THEN
                  REC = ONE / XJ
                  IF( CNORM( J ).GT.( BIGNUM-XMAX )*REC ) THEN
!
!                    Scale x by 1/(2*abs(x(j))).
!
                     REC = REC*HALF
                     CALL ZDSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                  END IF
               ELSE IF( XJ*CNORM( J ).GT.( BIGNUM-XMAX ) ) THEN
!
!                 Scale x by 1/2.
!
                  CALL ZDSCAL( N, HALF, X, 1 )
                  SCALE = SCALE*HALF
               END IF
!
               IF( UPPER ) THEN
                  IF( J.GT.1 ) THEN
!
!                    Compute the update
!                       x(1:j-1) := x(1:j-1) - x(j) * A(1:j-1,j)
!
                     CALL ZAXPY( J-1, -X( J )*TSCAL, A( 1, J ), 1, X,
     &                           1 )
                     I = IZAMAX( J-1, X, 1 )
                     XMAX = CABS1( X( I ) )
                  END IF
               ELSE
                  IF( J.LT.N ) THEN
!
!                    Compute the update
!                       x(j+1:n) := x(j+1:n) - x(j) * A(j+1:n,j)
!
                     CALL ZAXPY( N-J, -X( J )*TSCAL, A( J+1, J ), 1,
     &                           X( J+1 ), 1 )
                     I = J + IZAMAX( N-J, X( J+1 ), 1 )
                     XMAX = CABS1( X( I ) )
                  END IF
               END IF
  120       CONTINUE
!
         ELSE IF( LSAME( TRANS, 'T' ) ) THEN
!
!           Solve A**T * x = b
!
            DO 170 J = JFIRST, JLAST, JINC
!
!              Compute x(j) = b(j) - sum A(k,j)*x(k).
!                                    k<>j
!
               XJ = CABS1( X( J ) )
               USCAL = TSCAL
               REC = ONE / MAX( XMAX, ONE )
               IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN
!
!                 If x(j) could overflow, scale x by 1/(2*XMAX).
!
                  REC = REC*HALF
                  IF( NOUNIT ) THEN
                     TJJS = A( J, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                  END IF
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.ONE ) THEN
!
!                       Divide by A(j,j) when scaling x if A(j,j) > 1.
!
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = ZLADIV( USCAL, TJJS )
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL ZDSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
!
               CSUMJ = ZERO
               IF( USCAL.EQ.DCMPLX( ONE ) ) THEN
!
!                 If the scaling needed for A in the dot product is 1,
!                 call ZDOTU to perform the dot product.
!
                  IF( UPPER ) THEN
                     CSUMJ = ZDOTU( J-1, A( 1, J ), 1, X, 1 )
                  ELSE IF( J.LT.N ) THEN
                     CSUMJ = ZDOTU( N-J, A( J+1, J ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
!
!                 Otherwise, use in-line code for the dot product.
!
                  IF( UPPER ) THEN
                     DO 130 I = 1, J - 1
                        CSUMJ = CSUMJ + ( A( I, J )*USCAL )*X( I )
  130                CONTINUE
                  ELSE IF( J.LT.N ) THEN
                     DO 140 I = J + 1, N
                        CSUMJ = CSUMJ + ( A( I, J )*USCAL )*X( I )
  140                CONTINUE
                  END IF
               END IF
!
               IF( USCAL.EQ.DCMPLX( TSCAL ) ) THEN
!
!                 Compute x(j) := ( x(j) - CSUMJ ) / A(j,j) if 1/A(j,j)
!                 was not used to scale the dotproduct.
!
                  X( J ) = X( J ) - CSUMJ
                  XJ = CABS1( X( J ) )
                  IF( NOUNIT ) THEN
                     TJJS = A( J, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     &                  GO TO 160
                  END IF
!
!                    Compute x(j) = x(j) / A(j,j), scaling if necessary.
!
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.SMLNUM ) THEN
!
!                       abs(A(j,j)) > SMLNUM:
!
                     IF( TJJ.LT.ONE ) THEN
                        IF( XJ.GT.TJJ*BIGNUM ) THEN
!
!                             Scale X by 1/abs(x(j)).
!
                           REC = ONE / XJ
                           CALL ZDSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = ZLADIV( X( J ), TJJS )
                  ELSE IF( TJJ.GT.ZERO ) THEN
!
!                       0 < abs(A(j,j)) <= SMLNUM:
!
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
!
!                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM.
!
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL ZDSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = ZLADIV( X( J ), TJJS )
                  ELSE
!
!                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
!                       scale = 0 and compute a solution to A**T *x = 0.
!
                     DO 150 I = 1, N
                        X( I ) = ZERO
  150                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  160             CONTINUE
               ELSE
!
!                 Compute x(j) := x(j) / A(j,j) - CSUMJ if the dot
!                 product has already been divided by 1/A(j,j).
!
                  X( J ) = ZLADIV( X( J ), TJJS ) - CSUMJ
               END IF
               XMAX = MAX( XMAX, CABS1( X( J ) ) )
  170       CONTINUE
!
         ELSE
!
!           Solve A**H * x = b
!
            DO 220 J = JFIRST, JLAST, JINC
!
!              Compute x(j) = b(j) - sum A(k,j)*x(k).
!                                    k<>j
!
               XJ = CABS1( X( J ) )
               USCAL = TSCAL
               REC = ONE / MAX( XMAX, ONE )
               IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN
!
!                 If x(j) could overflow, scale x by 1/(2*XMAX).
!
                  REC = REC*HALF
                  IF( NOUNIT ) THEN
                     TJJS = DCONJG( A( J, J ) )*TSCAL
                  ELSE
                     TJJS = TSCAL
                  END IF
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.ONE ) THEN
!
!                       Divide by A(j,j) when scaling x if A(j,j) > 1.
!
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = ZLADIV( USCAL, TJJS )
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL ZDSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
!
               CSUMJ = ZERO
               IF( USCAL.EQ.DCMPLX( ONE ) ) THEN
!
!                 If the scaling needed for A in the dot product is 1,
!                 call ZDOTC to perform the dot product.
!
                  IF( UPPER ) THEN
                     CSUMJ = ZDOTC( J-1, A( 1, J ), 1, X, 1 )
                  ELSE IF( J.LT.N ) THEN
                     CSUMJ = ZDOTC( N-J, A( J+1, J ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
!
!                 Otherwise, use in-line code for the dot product.
!
                  IF( UPPER ) THEN
                     DO 180 I = 1, J - 1
                        CSUMJ = CSUMJ + ( DCONJG( A( I, J ) )*USCAL )*
     &                          X( I )
  180                CONTINUE
                  ELSE IF( J.LT.N ) THEN
                     DO 190 I = J + 1, N
                        CSUMJ = CSUMJ + ( DCONJG( A( I, J ) )*USCAL )*
     &                          X( I )
  190                CONTINUE
                  END IF
               END IF
!
               IF( USCAL.EQ.DCMPLX( TSCAL ) ) THEN
!
!                 Compute x(j) := ( x(j) - CSUMJ ) / A(j,j) if 1/A(j,j)
!                 was not used to scale the dotproduct.
!
                  X( J ) = X( J ) - CSUMJ
                  XJ = CABS1( X( J ) )
                  IF( NOUNIT ) THEN
                     TJJS = DCONJG( A( J, J ) )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     &                  GO TO 210
                  END IF
!
!                    Compute x(j) = x(j) / A(j,j), scaling if necessary.
!
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.SMLNUM ) THEN
!
!                       abs(A(j,j)) > SMLNUM:
!
                     IF( TJJ.LT.ONE ) THEN
                        IF( XJ.GT.TJJ*BIGNUM ) THEN
!
!                             Scale X by 1/abs(x(j)).
!
                           REC = ONE / XJ
                           CALL ZDSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = ZLADIV( X( J ), TJJS )
                  ELSE IF( TJJ.GT.ZERO ) THEN
!
!                       0 < abs(A(j,j)) <= SMLNUM:
!
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
!
!                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM.
!
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL ZDSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = ZLADIV( X( J ), TJJS )
                  ELSE
!
!                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
!                       scale = 0 and compute a solution to A**H *x = 0.
!
                     DO 200 I = 1, N
                        X( I ) = ZERO
  200                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  210             CONTINUE
               ELSE
!
!                 Compute x(j) := x(j) / A(j,j) - CSUMJ if the dot
!                 product has already been divided by 1/A(j,j).
!
                  X( J ) = ZLADIV( X( J ), TJJS ) - CSUMJ
               END IF
               XMAX = MAX( XMAX, CABS1( X( J ) ) )
  220       CONTINUE
         END IF
         SCALE = SCALE / TSCAL
      END IF
!
!     Scale the column norms by 1/TSCAL for return.
!
      IF( TSCAL.NE.ONE ) THEN
         CALL DSCAL( N, ONE / TSCAL, CNORM, 1 )
      END IF
!
      RETURN
!
!     End of ZLATRS
!
      END
!
!  =====================================================================
!> @brief DZSUM1 forms the 1-norm of the complex vector using the true absolute value.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       DOUBLE PRECISION FUNCTION DZSUM1( N, CX, INCX )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         CX( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DZSUM1 takes the sum of the absolute values of a complex
!> vector and returns a double precision result.
!>
!> Based on DZASUM from the Level 1 BLAS.
!> The change is to use the 'genuine' absolute value.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of elements in the vector CX.
!> \endverbatim
!>
!> @param[in] CX
!> \verbatim
!>          CX is COMPLEX*16 array, dimension (N)
!>          The vector whose elements will be sum.
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The spacing between successive values of CX.  INCX > 0.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
!  Contributors:
!  ==================
!>
!> Nick Higham for use with ZLACON.
!
      DOUBLE PRECISION FUNCTION DZSUM1( N, CX, INCX )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         CX( * )
!     ..
!
!  Purpose
!  =======
!
!  DZSUM1 takes the sum of the absolute values of a complex
!  vector and returns a double precision result.
!
!  Based on DZASUM from the Level 1 BLAS.
!  The change is to use the 'genuine' absolute value.
!
!  Contributed by Nick Higham for use with ZLACON.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of elements in the vector CX.
!
!  CX      (input) COMPLEX*16 array, dimension (N)
!          The vector whose elements will be sum.
!
!  INCX    (input) INTEGER
!          The spacing between successive values of CX.  INCX > 0.
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, NINCX
      DOUBLE PRECISION   STEMP
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
      DZSUM1 = 0.0D0
      STEMP = 0.0D0
      IF( N.LE.0 )
     &   RETURN
      IF( INCX.EQ.1 )
     &   GO TO 20
!
!     CODE FOR INCREMENT NOT EQUAL TO 1
!
      NINCX = N*INCX
      DO 10 I = 1, NINCX, INCX
!
!        NEXT LINE MODIFIED.
!
         STEMP = STEMP + ABS( CX( I ) )
   10 CONTINUE
      DZSUM1 = STEMP
      RETURN
!
!     CODE FOR INCREMENT EQUAL TO 1
!
   20 CONTINUE
      DO 30 I = 1, N
!
!        NEXT LINE MODIFIED.
!
         STEMP = STEMP + ABS( CX( I ) )
   30 CONTINUE
      DZSUM1 = STEMP
      RETURN
!
!     End of DZSUM1
!
      END
!
!  =====================================================================
!> @brief IZMAX1 finds the index of the vector element whose real
!> part has maximum absolute value.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       INTEGER          FUNCTION IZMAX1( N, CX, INCX )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         CX( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> IZMAX1 finds the index of the element whose real part has maximum
!> absolute value.
!>
!> Based on IZAMAX from Level 1 BLAS.
!> The change is to use the 'genuine' absolute value.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of elements in the vector CX.
!> \endverbatim
!>
!> @param[in] CX
!> \verbatim
!>          CX is COMPLEX*16 array, dimension (N)
!>          The vector whose elements will be sum.
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The spacing between successive values of CX.  INCX >= 1.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
!  Contributors:
!  ==================
!>
!> Nick Higham for use with ZLACON.
!
      INTEGER          FUNCTION IZMAX1( N, CX, INCX )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         CX( * )
!     ..
!
!  Purpose
!  =======
!
!  IZMAX1 finds the index of the element whose real part has maximum
!  absolute value.
!
!  Based on IZAMAX from Level 1 BLAS.
!  The change is to use the 'genuine' absolute value.
!
!  Contributed by Nick Higham for use with ZLACON.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of elements in the vector CX.
!
!  CX      (input) COMPLEX*16 array, dimension (N)
!          The vector whose elements will be sum.
!
!  INCX    (input) INTEGER
!          The spacing between successive values of CX.  INCX >= 1.
!
! =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, IX
      DOUBLE PRECISION   SMAX
      COMPLEX*16         ZDUM
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
!     ..
!     .. Statement Function definitions ..
!
!     NEXT LINE IS THE ONLY MODIFICATION.
      CABS1( ZDUM ) = ABS( ZDUM )
!     ..
!     .. Executable Statements ..
!
      IZMAX1 = 0
      IF( N.LT.1 )
     &   RETURN
      IZMAX1 = 1
      IF( N.EQ.1 )
     &   RETURN
      IF( INCX.EQ.1 )
     &   GO TO 30
!
!     CODE FOR INCREMENT NOT EQUAL TO 1
!
      IX = 1
      SMAX = CABS1( CX( 1 ) )
      IX = IX + INCX
      DO 20 I = 2, N
         IF( CABS1( CX( IX ) ).LE.SMAX )
     &      GO TO 10
         IZMAX1 = I
         SMAX = CABS1( CX( IX ) )
   10    CONTINUE
         IX = IX + INCX
   20 CONTINUE
      RETURN
!
!     CODE FOR INCREMENT EQUAL TO 1
!
   30 CONTINUE
      SMAX = CABS1( CX( 1 ) )
      DO 40 I = 2, N
         IF( CABS1( CX( I ) ).LE.SMAX )
     &      GO TO 40
         IZMAX1 = I
         SMAX = CABS1( CX( I ) )
   40 CONTINUE
      RETURN
!
!     End of IZMAX1
!
      END
!
!  =====================================================================
!> @brief ZLACON estimates the 1-norm of a square matrix, using
!> reverse communication for evaluating matrix-vector products.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLACON( N, V, X, EST, KASE )
!
!       .. Scalar Arguments ..
!       INTEGER            KASE, N
!       DOUBLE PRECISION   EST
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         V( N ), X( N )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLACON estimates the 1-norm of a square, complex matrix A.
!> Reverse communication is used for evaluating matrix-vector products.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>         The order of the matrix.  N >= 1.
!> \endverbatim
!>
!> @param[out] V
!> \verbatim
!>          V is COMPLEX*16 array, dimension (N)
!>         On the final return, V = A*W,  where  EST = norm(V)/norm(W)
!>         (W is not returned).
!> \endverbatim
!>
!> @param[in,out] X
!> \verbatim
!>          X is COMPLEX*16 array, dimension (N)
!>         On an intermediate return, X should be overwritten by
!>               A * X,   if KASE=1,
!>               A**H * X,  if KASE=2,
!>         where A**H is the conjugate transpose of A, and ZLACON must be
!>         re-called with all the other parameters unchanged.
!> \endverbatim
!>
!> @param[in,out] EST
!> \verbatim
!>          EST is DOUBLE PRECISION
!>         On entry with KASE = 1 or 2 and JUMP = 3, EST should be
!>         unchanged from the previous call to ZLACON.
!>         On exit, EST is an estimate (a lower bound) for norm(A).
!> \endverbatim
!>
!> @param[in,out] KASE
!> \verbatim
!>          KASE is INTEGER
!>         On the initial call to ZLACON, KASE should be 0.
!>         On an intermediate return, KASE will be 1 or 2, indicating
!>         whether X should be overwritten by A * X  or A**H * X.
!>         On the final return from ZLACON, KASE will again be 0.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
!  Further Details:
!  =====================
!>
!>  Originally named CONEST, dated March 16, 1988. \n
!>  Last modified:  April, 1999
!
!  Contributors:
!  ==================
!>
!>     Nick Higham, University of Manchester
!
!  References:
!  ================
!>
!>  N.J. Higham, "FORTRAN codes for estimating the one-norm of
!>  a real or complex matrix, with applications to condition estimation",
!>  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.
!
      SUBROUTINE ZLACON( N, V, X, EST, KASE )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            KASE, N
      DOUBLE PRECISION   EST
!     ..
!     .. Array Arguments ..
      COMPLEX*16         V( N ), X( N )
!     ..
!
!  Purpose
!  =======
!
!  ZLACON estimates the 1-norm of a square, complex matrix A.
!  Reverse communication is used for evaluating matrix-vector products.
!
!  Arguments
!  =========
!
!  N      (input) INTEGER
!         The order of the matrix.  N >= 1.
!
!  V      (workspace) COMPLEX*16 array, dimension (N)
!         On the final return, V = A*W,  where  EST = norm(V)/norm(W)
!         (W is not returned).
!
!  X      (input/output) COMPLEX*16 array, dimension (N)
!         On an intermediate return, X should be overwritten by
!               A * X,   if KASE=1,
!               A' * X,  if KASE=2,
!         where A' is the conjugate transpose of A, and ZLACON must be
!         re-called with all the other parameters unchanged.
!
!  EST    (input/output) DOUBLE PRECISION
!         On entry with KASE = 1 or 2 and JUMP = 3, EST should be
!         unchanged from the previous call to ZLACON.
!         On exit, EST is an estimate (a lower bound) for norm(A).
!
!  KASE   (input/output) INTEGER
!         On the initial call to ZLACON, KASE should be 0.
!         On an intermediate return, KASE will be 1 or 2, indicating
!         whether X should be overwritten by A * X  or A' * X.
!         On the final return from ZLACON, KASE will again be 0.
!
!  Further Details
!  ======= =======
!
!  Contributed by Nick Higham, University of Manchester.
!  Originally named CONEST, dated March 16, 1988.
!
!  Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of
!  a real or complex matrix, with applications to condition estimation",
!  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.
!
!  Last modified:  April, 1999
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER            ITMAX
      PARAMETER          ( ITMAX = 5 )
      DOUBLE PRECISION   ONE, TWO
      PARAMETER          ( ONE = 1.0D0, TWO = 2.0D0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D0, 0.0D0 ),
     &                   CONE = ( 1.0D0, 0.0D0 ) )
!     ..
!     .. Local Scalars ..
      INTEGER            I, ITER, J, JLAST, JUMP
      DOUBLE PRECISION   ABSXI, ALTSGN, ESTOLD, SAFMIN, TEMP
!     ..
!     .. External Functions ..
      INTEGER            IZMAX1
      DOUBLE PRECISION   DLAMCH, DZSUM1
      EXTERNAL           IZMAX1, DLAMCH, DZSUM1
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZCOPY
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG
!     ..
!     .. Save statement ..
      SAVE
!     ..
!     .. Executable Statements ..
!
      SAFMIN = DLAMCH( 'Safe minimum' )
      IF( KASE.EQ.0 ) THEN
         DO 10 I = 1, N
            X( I ) = DCMPLX( ONE / DBLE( N ) )
   10    CONTINUE
         KASE = 1
         JUMP = 1
         RETURN
      END IF
!
      GO TO ( 20, 40, 70, 90, 120 )JUMP
!
!     ................ ENTRY   (JUMP = 1)
!     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
!
   20 CONTINUE
      IF( N.EQ.1 ) THEN
         V( 1 ) = X( 1 )
         EST = ABS( V( 1 ) )
!        ... QUIT
         GO TO 130
      END IF
      EST = DZSUM1( N, X, 1 )
!
      DO 30 I = 1, N
         ABSXI = ABS( X( I ) )
         IF( ABSXI.GT.SAFMIN ) THEN
            X( I ) = DCMPLX( DBLE( X( I ) ) / ABSXI,
     &               DIMAG( X( I ) ) / ABSXI )
         ELSE
            X( I ) = CONE
         END IF
   30 CONTINUE
      KASE = 2
      JUMP = 2
      RETURN
!
!     ................ ENTRY   (JUMP = 2)
!     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY CTRANS(A)*X.
!
   40 CONTINUE
      J = IZMAX1( N, X, 1 )
      ITER = 2
!
!     MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
!
   50 CONTINUE
      DO 60 I = 1, N
         X( I ) = CZERO
   60 CONTINUE
      X( J ) = CONE
      KASE = 1
      JUMP = 3
      RETURN
!
!     ................ ENTRY   (JUMP = 3)
!     X HAS BEEN OVERWRITTEN BY A*X.
!
   70 CONTINUE
      CALL ZCOPY( N, X, 1, V, 1 )
      ESTOLD = EST
      EST = DZSUM1( N, V, 1 )
!
!     TEST FOR CYCLING.
      IF( EST.LE.ESTOLD )
     &   GO TO 100
!
      DO 80 I = 1, N
         ABSXI = ABS( X( I ) )
         IF( ABSXI.GT.SAFMIN ) THEN
            X( I ) = DCMPLX( DBLE( X( I ) ) / ABSXI,
     &               DIMAG( X( I ) ) / ABSXI )
         ELSE
            X( I ) = CONE
         END IF
   80 CONTINUE
      KASE = 2
      JUMP = 4
      RETURN
!
!     ................ ENTRY   (JUMP = 4)
!     X HAS BEEN OVERWRITTEN BY CTRANS(A)*X.
!
   90 CONTINUE
      JLAST = J
      J = IZMAX1( N, X, 1 )
      IF( ( ABS( X( JLAST ) ).NE.ABS( X( J ) ) ) .AND.
     &    ( ITER.LT.ITMAX ) ) THEN
         ITER = ITER + 1
         GO TO 50
      END IF
!
!     ITERATION COMPLETE.  FINAL STAGE.
!
  100 CONTINUE
      ALTSGN = ONE
      DO 110 I = 1, N
         X( I ) = DCMPLX( ALTSGN*( ONE+DBLE( I-1 ) / DBLE( N-1 ) ) )
         ALTSGN = -ALTSGN
  110 CONTINUE
      KASE = 1
      JUMP = 5
      RETURN
!
!     ................ ENTRY   (JUMP = 5)
!     X HAS BEEN OVERWRITTEN BY A*X.
!
  120 CONTINUE
      TEMP = TWO*( DZSUM1( N, X, 1 ) / DBLE( 3*N ) )
      IF( TEMP.GT.EST ) THEN
         CALL ZCOPY( N, X, 1, V, 1 )
         EST = TEMP
      END IF
!
  130 CONTINUE
      KASE = 0
      RETURN
!
!     End of ZLACON
!
      END
!
!  =====================================================================
!> @brief ZDRSCL multiplies a vector by the reciprocal of a real scalar.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZDRSCL( N, SA, SX, INCX )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       DOUBLE PRECISION   SA
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         SX( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZDRSCL multiplies an n-element complex vector x by the real scalar
!> 1/a.  This is done without overflow or underflow as long as
!> the final result x/a does not overflow or underflow.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of components of the vector x.
!> \endverbatim
!>
!> @param[in] SA
!> \verbatim
!>          SA is DOUBLE PRECISION
!>          The scalar a which is used to divide each component of x.
!>          SA must be >= 0, or the subroutine will divide by zero.
!> \endverbatim
!>
!> @param[in,out] SX
!> \verbatim
!>          SX is COMPLEX*16 array, dimension
!>                         (1+(N-1)*abs(INCX))
!>          The n-element vector x.
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between successive values of the vector SX.
!>          > 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
      SUBROUTINE ZDRSCL( N, SA, SX, INCX )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SA
!     ..
!     .. Array Arguments ..
      COMPLEX*16         SX( * )
!     ..
!
!  Purpose
!  =======
!
!  ZDRSCL multiplies an n-element complex vector x by the real scalar
!  1/a.  This is done without overflow or underflow as long as
!  the final result x/a does not overflow or underflow.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of components of the vector x.
!
!  SA      (input) DOUBLE PRECISION
!          The scalar a which is used to divide each component of x.
!          SA must be >= 0, or the subroutine will divide by zero.
!
!  SX      (input/output) COMPLEX*16 array, dimension
!                         (1+(N-1)*abs(INCX))
!          The n-element vector x.
!
!  INCX    (input) INTEGER
!          The increment between successive values of the vector SX.
!          > 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            DONE
      DOUBLE PRECISION   BIGNUM, CDEN, CDEN1, CNUM, CNUM1, MUL, SMLNUM
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLABAD, ZDSCAL
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
!     Quick return if possible
!
      IF( N.LE.0 )
     &   RETURN
!
!     Get machine parameters
!
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
!
!     Initialize the denominator to SA and the numerator to 1.
!
      CDEN = SA
      CNUM = ONE
!
   10 CONTINUE
      CDEN1 = CDEN*SMLNUM
      CNUM1 = CNUM / BIGNUM
      IF( ABS( CDEN1 ).GT.ABS( CNUM ) .AND. CNUM.NE.ZERO ) THEN
!
!        Pre-multiply X by SMLNUM if CDEN is large compared to CNUM.
!
         MUL = SMLNUM
         DONE = .FALSE.
         CDEN = CDEN1
      ELSE IF( ABS( CNUM1 ).GT.ABS( CDEN ) ) THEN
!
!        Pre-multiply X by BIGNUM if CDEN is small compared to CNUM.
!
         MUL = BIGNUM
         DONE = .FALSE.
         CNUM = CNUM1
      ELSE
!
!        Multiply X by CNUM / CDEN and return.
!
         MUL = CNUM / CDEN
         DONE = .TRUE.
      END IF
!
!     Scale the vector X by MUL
!
      CALL ZDSCAL( N, MUL, SX, INCX )
!
      IF( .NOT.DONE )
     &   GO TO 10
!
      RETURN
!
!     End of ZDRSCL
!
      END
!
!  =====================================================================
!> @brief ZLACN2 estimates the 1-norm of a square matrix, using reverse communication for evaluating matrix-vector products.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLACN2( N, V, X, EST, KASE, ISAVE )
!
!       .. Scalar Arguments ..
!       INTEGER            KASE, N
!       DOUBLE PRECISION   EST
!       ..
!       .. Array Arguments ..
!       INTEGER            ISAVE( 3 )
!       COMPLEX*16         V( * ), X( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLACN2 estimates the 1-norm of a square, complex matrix A.
!> Reverse communication is used for evaluating matrix-vector products.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>         The order of the matrix.  N >= 1.
!> \endverbatim
!>
!> @param[out] V
!> \verbatim
!>          V is COMPLEX*16 array, dimension (N)
!>         On the final return, V = A*W,  where  EST = norm(V)/norm(W)
!>         (W is not returned).
!> \endverbatim
!>
!> @param[in,out] X
!> \verbatim
!>          X is COMPLEX*16 array, dimension (N)
!>         On an intermediate return, X should be overwritten by
!>               A * X,   if KASE=1,
!>               A**H * X,  if KASE=2,
!>         where A**H is the conjugate transpose of A, and ZLACN2 must be
!>         re-called with all the other parameters unchanged.
!> \endverbatim
!>
!> @param[in,out] EST
!> \verbatim
!>          EST is DOUBLE PRECISION
!>         On entry with KASE = 1 or 2 and ISAVE(1) = 3, EST should be
!>         unchanged from the previous call to ZLACN2.
!>         On exit, EST is an estimate (a lower bound) for norm(A).
!> \endverbatim
!>
!> @param[in,out] KASE
!> \verbatim
!>          KASE is INTEGER
!>         On the initial call to ZLACN2, KASE should be 0.
!>         On an intermediate return, KASE will be 1 or 2, indicating
!>         whether X should be overwritten by A * X  or A**H * X.
!>         On the final return from ZLACN2, KASE will again be 0.
!> \endverbatim
!>
!> @param[in,out] ISAVE
!> \verbatim
!>          ISAVE is INTEGER array, dimension (3)
!>         ISAVE is used to save variables between calls to ZLACN2
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Originally named CONEST, dated March 16, 1988.
!>
!>  Last modified:  April, 1999
!>
!>  This is a thread safe version of ZLACON, which uses the array ISAVE
!>  in place of a SAVE statement, as follows:
!>
!>     ZLACON     ZLACN2
!>      JUMP     ISAVE(1)
!>      J        ISAVE(2)
!>      ITER     ISAVE(3)
!> \endverbatim
!
!  Contributors:
!  ==================
!>
!>     Nick Higham, University of Manchester
!
!  References:
!  ================
!>
!>  N.J. Higham, "FORTRAN codes for estimating the one-norm of
!>  a real or complex matrix, with applications to condition estimation",
!>  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.
!
      SUBROUTINE ZLACN2( N, V, X, EST, KASE, ISAVE )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            KASE, N
      DOUBLE PRECISION   EST
!     ..
!     .. Array Arguments ..
      INTEGER            ISAVE( 3 )
      COMPLEX*16         V( * ), X( * )
!     ..
!
!  Purpose
!  =======
!
!  ZLACN2 estimates the 1-norm of a square, complex matrix A.
!  Reverse communication is used for evaluating matrix-vector products.
!
!  Arguments
!  =========
!
!  N      (input) INTEGER
!         The order of the matrix.  N >= 1.
!
!  V      (workspace) COMPLEX*16 array, dimension (N)
!         On the final return, V = A*W,  where  EST = norm(V)/norm(W)
!         (W is not returned).
!
!  X      (input/output) COMPLEX*16 array, dimension (N)
!         On an intermediate return, X should be overwritten by
!               A * X,   if KASE=1,
!               A' * X,  if KASE=2,
!         where A' is the conjugate transpose of A, and ZLACN2 must be
!         re-called with all the other parameters unchanged.
!
!  EST    (input/output) DOUBLE PRECISION
!         On entry with KASE = 1 or 2 and ISAVE(1) = 3, EST should be
!         unchanged from the previous call to ZLACN2.
!         On exit, EST is an estimate (a lower bound) for norm(A).
!
!  KASE   (input/output) INTEGER
!         On the initial call to ZLACN2, KASE should be 0.
!         On an intermediate return, KASE will be 1 or 2, indicating
!         whether X should be overwritten by A * X  or A' * X.
!         On the final return from ZLACN2, KASE will again be 0.
!
!  ISAVE  (input/output) INTEGER array, dimension (3)
!         ISAVE is used to save variables between calls to ZLACN2
!
!  Further Details
!  ======= =======
!
!  Contributed by Nick Higham, University of Manchester.
!  Originally named CONEST, dated March 16, 1988.
!
!  Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of
!  a real or complex matrix, with applications to condition estimation",
!  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.
!
!  Last modified:  April, 1999
!
!  This is a thread safe version of ZLACON, which uses the array ISAVE
!  in place of a SAVE statement, as follows:
!
!     ZLACON     ZLACN2
!      JUMP     ISAVE(1)
!      J        ISAVE(2)
!      ITER     ISAVE(3)
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER              ITMAX
      PARAMETER          ( ITMAX = 5 )
      DOUBLE PRECISION     ONE,         TWO
      PARAMETER          ( ONE = 1.0D0, TWO = 2.0D0 )
      COMPLEX*16           CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D0, 0.0D0 ),
     &                            CONE = ( 1.0D0, 0.0D0 ) )
!     ..
!     .. Local Scalars ..
      INTEGER            I, JLAST
      DOUBLE PRECISION   ABSXI, ALTSGN, ESTOLD, SAFMIN, TEMP
!     ..
!     .. External Functions ..
      INTEGER            IZMAX1
      DOUBLE PRECISION   DLAMCH, DZSUM1
      EXTERNAL           IZMAX1, DLAMCH, DZSUM1
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZCOPY
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG
!     ..
!     .. Executable Statements ..
!
      SAFMIN = DLAMCH( 'Safe minimum' )
      IF( KASE.EQ.0 ) THEN
         DO 10 I = 1, N
            X( I ) = DCMPLX( ONE / DBLE( N ) )
   10    CONTINUE
         KASE = 1
         ISAVE( 1 ) = 1
         RETURN
      END IF
!
      GO TO ( 20, 40, 70, 90, 120 )ISAVE( 1 )
!
!     ................ ENTRY   (ISAVE( 1 ) = 1)
!     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
!
   20 CONTINUE
      IF( N.EQ.1 ) THEN
         V( 1 ) = X( 1 )
         EST = ABS( V( 1 ) )
!        ... QUIT
         GO TO 130
      END IF
      EST = DZSUM1( N, X, 1 )
!
      DO 30 I = 1, N
         ABSXI = ABS( X( I ) )
         IF( ABSXI.GT.SAFMIN ) THEN
            X( I ) = DCMPLX( DBLE( X( I ) ) / ABSXI,
     &               DIMAG( X( I ) ) / ABSXI )
         ELSE
            X( I ) = CONE
         END IF
   30 CONTINUE
      KASE = 2
      ISAVE( 1 ) = 2
      RETURN
!
!     ................ ENTRY   (ISAVE( 1 ) = 2)
!     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY CTRANS(A)*X.
!
   40 CONTINUE
      ISAVE( 2 ) = IZMAX1( N, X, 1 )
      ISAVE( 3 ) = 2
!
!     MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
!
   50 CONTINUE
      DO 60 I = 1, N
         X( I ) = CZERO
   60 CONTINUE
      X( ISAVE( 2 ) ) = CONE
      KASE = 1
      ISAVE( 1 ) = 3
      RETURN
!
!     ................ ENTRY   (ISAVE( 1 ) = 3)
!     X HAS BEEN OVERWRITTEN BY A*X.
!
   70 CONTINUE
      CALL ZCOPY( N, X, 1, V, 1 )
      ESTOLD = EST
      EST = DZSUM1( N, V, 1 )
!
!     TEST FOR CYCLING.
      IF( EST.LE.ESTOLD )
     &   GO TO 100
!
      DO 80 I = 1, N
         ABSXI = ABS( X( I ) )
         IF( ABSXI.GT.SAFMIN ) THEN
            X( I ) = DCMPLX( DBLE( X( I ) ) / ABSXI,
     &               DIMAG( X( I ) ) / ABSXI )
         ELSE
            X( I ) = CONE
         END IF
   80 CONTINUE
      KASE = 2
      ISAVE( 1 ) = 4
      RETURN
!
!     ................ ENTRY   (ISAVE( 1 ) = 4)
!     X HAS BEEN OVERWRITTEN BY CTRANS(A)*X.
!
   90 CONTINUE
      JLAST = ISAVE( 2 )
      ISAVE( 2 ) = IZMAX1( N, X, 1 )
      IF( ( ABS( X( JLAST ) ).NE.ABS( X( ISAVE( 2 ) ) ) ) .AND.
     &    ( ISAVE( 3 ).LT.ITMAX ) ) THEN
         ISAVE( 3 ) = ISAVE( 3 ) + 1
         GO TO 50
      END IF
!
!     ITERATION COMPLETE.  FINAL STAGE.
!
  100 CONTINUE
      ALTSGN = ONE
      DO 110 I = 1, N
         X( I ) = DCMPLX( ALTSGN*( ONE+DBLE( I-1 ) / DBLE( N-1 ) ) )
         ALTSGN = -ALTSGN
  110 CONTINUE
      KASE = 1
      ISAVE( 1 ) = 5
      RETURN
!
!     ................ ENTRY   (ISAVE( 1 ) = 5)
!     X HAS BEEN OVERWRITTEN BY A*X.
!
  120 CONTINUE
      TEMP = TWO*( DZSUM1( N, X, 1 ) / DBLE( 3*N ) )
      IF( TEMP.GT.EST ) THEN
         CALL ZCOPY( N, X, 1, V, 1 )
         EST = TEMP
      END IF
!
  130 CONTINUE
      KASE = 0
      RETURN
!
!     End of ZLACN2
!
      END
!
!  =====================================================================
!> @brief ZGECON
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, RWORK,
!                          INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          NORM
!       INTEGER            INFO, LDA, N
!       DOUBLE PRECISION   ANORM, RCOND
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   RWORK( * )
!       COMPLEX*16         A( LDA, * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGECON estimates the reciprocal of the condition number of a general
!> complex matrix A, in either the 1-norm or the infinity-norm, using
!> the LU factorization computed by ZGETRF.
!>
!> An estimate is obtained for norm(inv(A)), and the reciprocal of the
!> condition number is computed as
!>    RCOND = 1 / ( norm(A) * norm(inv(A)) ).
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] NORM
!> \verbatim
!>          NORM is CHARACTER*1
!>          Specifies whether the 1-norm condition number or the
!>          infinity-norm condition number is required:
!>          = '1' or 'O':  1-norm;
!>          = 'I':         Infinity-norm.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          The factors L and U from the factorization A = P*L*U
!>          as computed by ZGETRF.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in] ANORM
!> \verbatim
!>          ANORM is DOUBLE PRECISION
!>          If NORM = '1' or 'O', the 1-norm of the original matrix A.
!>          If NORM = 'I', the infinity-norm of the original matrix A.
!> \endverbatim
!>
!> @param[out] RCOND
!> \verbatim
!>          RCOND is DOUBLE PRECISION
!>          The reciprocal of the condition number of the matrix A,
!>          computed as RCOND = 1/(norm(A) * norm(inv(A))).
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension (2*N)
!> \endverbatim
!>
!> @param[out] RWORK
!> \verbatim
!>          RWORK is DOUBLE PRECISION array, dimension (2*N)
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument had an illegal value
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date November 2011
!
      SUBROUTINE ZGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, RWORK,
     &                   INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     Modified to call ZLACN2 in place of ZLACON, 10 Feb 03, SJH.
!
!     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   ANORM, RCOND
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         A( LDA, * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZGECON estimates the reciprocal of the condition number of a general
!  complex matrix A, in either the 1-norm or the infinity-norm, using
!  the LU factorization computed by ZGETRF.
!
!  An estimate is obtained for norm(inv(A)), and the reciprocal of the
!  condition number is computed as
!     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
!
!  Arguments
!  =========
!
!  NORM    (input) CHARACTER*1
!          Specifies whether the 1-norm condition number or the
!          infinity-norm condition number is required:
!          = '1' or 'O':  1-norm;
!          = 'I':         Infinity-norm.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The factors L and U from the factorization A = P*L*U
!          as computed by ZGETRF.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  ANORM   (input) DOUBLE PRECISION
!          If NORM = '1' or 'O', the 1-norm of the original matrix A.
!          If NORM = 'I', the infinity-norm of the original matrix A.
!
!  RCOND   (output) DOUBLE PRECISION
!          The reciprocal of the condition number of the matrix A,
!          computed as RCOND = 1/(norm(A) * norm(inv(A))).
!
!  WORK    (workspace) COMPLEX*16 array, dimension (2*N)
!
!  RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            ONENRM
      CHARACTER          NORMIN
      INTEGER            IX, KASE, KASE1
      DOUBLE PRECISION   AINVNM, SCALE, SL, SMLNUM, SU
      COMPLEX*16         ZDUM
!     ..
!     .. Local Arrays ..
      INTEGER            ISAVE( 3 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IZAMAX
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, IZAMAX, DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZDRSCL, ZLACN2, ZLATRS
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG, MAX
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
!     ..
!     .. Statement Function definitions ..
      CABS1( ZDUM ) = ABS( DBLE( ZDUM ) ) + ABS( DIMAG( ZDUM ) )
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      ONENRM = NORM.EQ.'1' .OR. LSAME( NORM, 'O' )
      IF( .NOT.ONENRM .AND. .NOT.LSAME( NORM, 'I' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( ANORM.LT.ZERO ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGECON', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      RCOND = ZERO
      IF( N.EQ.0 ) THEN
         RCOND = ONE
         RETURN
      ELSE IF( ANORM.EQ.ZERO ) THEN
         RETURN
      END IF
!
      SMLNUM = DLAMCH( 'Safe minimum' )
!
!     Estimate the norm of inv(A).
!
      AINVNM = ZERO
      NORMIN = 'N'
      IF( ONENRM ) THEN
         KASE1 = 1
      ELSE
         KASE1 = 2
      END IF
      KASE = 0
   10 CONTINUE
      CALL ZLACN2( N, WORK( N+1 ), WORK, AINVNM, KASE, ISAVE )
      IF( KASE.NE.0 ) THEN
         IF( KASE.EQ.KASE1 ) THEN
!
!           Multiply by inv(L).
!
            CALL ZLATRS( 'Lower', 'No transpose', 'Unit', NORMIN, N, A,
     &                   LDA, WORK, SL, RWORK, INFO )
!
!           Multiply by inv(U).
!
            CALL ZLATRS( 'Upper', 'No transpose', 'Non-unit', NORMIN, N,
     &                   A, LDA, WORK, SU, RWORK( N+1 ), INFO )
         ELSE
!
!           Multiply by inv(U').
!
            CALL ZLATRS( 'Upper', 'Conjugate transpose', 'Non-unit',
     &                   NORMIN, N, A, LDA, WORK, SU, RWORK( N+1 ),
     &                   INFO )
!
!           Multiply by inv(L').
!
            CALL ZLATRS( 'Lower', 'Conjugate transpose', 'Unit', NORMIN,
     &                   N, A, LDA, WORK, SL, RWORK, INFO )
         END IF
!
!        Divide X by 1/(SL*SU) if doing so will not cause overflow.
!
         SCALE = SL*SU
         NORMIN = 'Y'
         IF( SCALE.NE.ONE ) THEN
            IX = IZAMAX( N, WORK, 1 )
            IF( SCALE.LT.CABS1( WORK( IX ) )*SMLNUM .OR. SCALE.EQ.ZERO )
     &         GO TO 20
            CALL ZDRSCL( N, SCALE, WORK, 1 )
         END IF
         GO TO 10
      END IF
!
!     Compute the estimate of the reciprocal condition number.
!
      IF( AINVNM.NE.ZERO )
     &   RCOND = ( ONE / AINVNM ) / ANORM
!
   20 CONTINUE
      RETURN
!
!     End of ZGECON
!
      END
!
!  =====================================================================
!> @brief ZLATDF uses the LU factorization of the n-by-n matrix computed by sgetc2 and computes a contribution to the reciprocal Dif-estimate.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLATDF( IJOB, N, Z, LDZ, RHS, RDSUM, RDSCAL, IPIV,
!                          JPIV )
!
!       .. Scalar Arguments ..
!       INTEGER            IJOB, LDZ, N
!       DOUBLE PRECISION   RDSCAL, RDSUM
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * ), JPIV( * )
!       COMPLEX*16         RHS( * ), Z( LDZ, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLATDF computes the contribution to the reciprocal Dif-estimate
!> by solving for x in Z * x = b, where b is chosen such that the norm
!> of x is as large as possible. It is assumed that LU decomposition
!> of Z has been computed by ZGETC2. On entry RHS = f holds the
!> contribution from earlier solved sub-systems, and on return RHS = x.
!>
!> The factorization of Z returned by ZGETC2 has the form
!> Z = P * L * U * Q, where P and Q are permutation matrices. L is lower
!> triangular with unit diagonal elements and U is upper triangular.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] IJOB
!> \verbatim
!>          IJOB is INTEGER
!>          IJOB = 2: First compute an approximative null-vector e
!>              of Z using ZGECON, e is normalized and solve for
!>              Zx = +-e - f with the sign giving the greater value of
!>              2-norm(x).  About 5 times as expensive as Default.
!>          IJOB .ne. 2: Local look ahead strategy where
!>              all entries of the r.h.s. b is choosen as either +1 or
!>              -1.  Default.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix Z.
!> \endverbatim
!>
!> @param[in] Z
!> \verbatim
!>          Z is DOUBLE PRECISION array, dimension (LDZ, N)
!>          On entry, the LU part of the factorization of the n-by-n
!>          matrix Z computed by ZGETC2:  Z = P * L * U * Q
!> \endverbatim
!>
!> @param[in] LDZ
!> \verbatim
!>          LDZ is INTEGER
!>          The leading dimension of the array Z.  LDA >= max(1, N).
!> \endverbatim
!>
!> @param[in,out] RHS
!> \verbatim
!>          RHS is DOUBLE PRECISION array, dimension (N).
!>          On entry, RHS contains contributions from other subsystems.
!>          On exit, RHS contains the solution of the subsystem with
!>          entries according to the value of IJOB (see above).
!> \endverbatim
!>
!> @param[in,out] RDSUM
!> \verbatim
!>          RDSUM is DOUBLE PRECISION
!>          On entry, the sum of squares of computed contributions to
!>          the Dif-estimate under computation by ZTGSYL, where the
!>          scaling factor RDSCAL (see below) has been factored out.
!>          On exit, the corresponding sum of squares updated with the
!>          contributions from the current sub-system.
!>          If TRANS = 'T' RDSUM is not touched.
!>          NOTE: RDSUM only makes sense when ZTGSY2 is called by CTGSYL.
!> \endverbatim
!>
!> @param[in,out] RDSCAL
!> \verbatim
!>          RDSCAL is DOUBLE PRECISION
!>          On entry, scaling factor used to prevent overflow in RDSUM.
!>          On exit, RDSCAL is updated w.r.t. the current contributions
!>          in RDSUM.
!>          If TRANS = 'T', RDSCAL is not touched.
!>          NOTE: RDSCAL only makes sense when ZTGSY2 is called by
!>          ZTGSYL.
!> \endverbatim
!>
!> @param[in] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (N).
!>          The pivot indices; for 1 <= i <= N, row i of the
!>          matrix has been interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[in] JPIV
!> \verbatim
!>          JPIV is INTEGER array, dimension (N).
!>          The pivot indices; for 1 <= j <= N, column j of the
!>          matrix has been interchanged with column JPIV(j).
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
!  Further Details:
!  =====================
!>
!>  This routine is a further developed implementation of algorithm
!>  BSOLVE in [1] using complete pivoting in the LU factorization.
!
!  Contributors:
!  ==================
!>
!>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!>     Umea University, S-901 87 Umea, Sweden.
!
!  References:
!  ================
!>
!>   [1]   Bo Kagstrom and Lars Westin,
!>         Generalized Schur Methods with Condition Estimators for
!>         Solving the Generalized Sylvester Equation, IEEE Transactions
!>         on Automatic Control, Vol. 34, No. 7, July 1989, pp 745-751.
!>\n
!>   [2]   Peter Poromaa,
!>         On Efficient and Robust Estimators for the Separation
!>         between two Regular Matrix Pairs with Applications in
!>         Condition Estimation. Report UMINF-95.05, Department of
!>         Computing Science, Umea University, S-901 87 Umea, Sweden,
!>         1995.
!
      SUBROUTINE ZLATDF( IJOB, N, Z, LDZ, RHS, RDSUM, RDSCAL, IPIV,
     &                   JPIV )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            IJOB, LDZ, N
      DOUBLE PRECISION   RDSCAL, RDSUM
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * ), JPIV( * )
      COMPLEX*16         RHS( * ), Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLATDF computes the contribution to the reciprocal Dif-estimate
!  by solving for x in Z * x = b, where b is chosen such that the norm
!  of x is as large as possible. It is assumed that LU decomposition
!  of Z has been computed by ZGETC2. On entry RHS = f holds the
!  contribution from earlier solved sub-systems, and on return RHS = x.
!
!  The factorization of Z returned by ZGETC2 has the form
!  Z = P * L * U * Q, where P and Q are permutation matrices. L is lower
!  triangular with unit diagonal elements and U is upper triangular.
!
!  Arguments
!  =========
!
!  IJOB    (input) INTEGER
!          IJOB = 2: First compute an approximative null-vector e
!              of Z using ZGECON, e is normalized and solve for
!              Zx = +-e - f with the sign giving the greater value of
!              2-norm(x).  About 5 times as expensive as Default.
!          IJOB .ne. 2: Local look ahead strategy where
!              all entries of the r.h.s. b is choosen as either +1 or
!              -1.  Default.
!
!  N       (input) INTEGER
!          The number of columns of the matrix Z.
!
!  Z       (input) DOUBLE PRECISION array, dimension (LDZ, N)
!          On entry, the LU part of the factorization of the n-by-n
!          matrix Z computed by ZGETC2:  Z = P * L * U * Q
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDA >= max(1, N).
!
!  RHS     (input/output) DOUBLE PRECISION array, dimension (N).
!          On entry, RHS contains contributions from other subsystems.
!          On exit, RHS contains the solution of the subsystem with
!          entries according to the value of IJOB (see above).
!
!  RDSUM   (input/output) DOUBLE PRECISION
!          On entry, the sum of squares of computed contributions to
!          the Dif-estimate under computation by ZTGSYL, where the
!          scaling factor RDSCAL (see below) has been factored out.
!          On exit, the corresponding sum of squares updated with the
!          contributions from the current sub-system.
!          If TRANS = 'T' RDSUM is not touched.
!          NOTE: RDSUM only makes sense when ZTGSY2 is called by CTGSYL.
!
!  RDSCAL  (input/output) DOUBLE PRECISION
!          On entry, scaling factor used to prevent overflow in RDSUM.
!          On exit, RDSCAL is updated w.r.t. the current contributions
!          in RDSUM.
!          If TRANS = 'T', RDSCAL is not touched.
!          NOTE: RDSCAL only makes sense when ZTGSY2 is called by
!          ZTGSYL.
!
!  IPIV    (input) INTEGER array, dimension (N).
!          The pivot indices; for 1 <= i <= N, row i of the
!          matrix has been interchanged with row IPIV(i).
!
!  JPIV    (input) INTEGER array, dimension (N).
!          The pivot indices; for 1 <= j <= N, column j of the
!          matrix has been interchanged with column JPIV(j).
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!     Umea University, S-901 87 Umea, Sweden.
!
!  This routine is a further developed implementation of algorithm
!  BSOLVE in [1] using complete pivoting in the LU factorization.
!
!   [1]   Bo Kagstrom and Lars Westin,
!         Generalized Schur Methods with Condition Estimators for
!         Solving the Generalized Sylvester Equation, IEEE Transactions
!         on Automatic Control, Vol. 34, No. 7, July 1989, pp 745-751.
!
!   [2]   Peter Poromaa,
!         On Efficient and Robust Estimators for the Separation
!         between two Regular Matrix Pairs with Applications in
!         Condition Estimation. Report UMINF-95.05, Department of
!         Computing Science, Umea University, S-901 87 Umea, Sweden,
!         1995.
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER            MAXDIM
      PARAMETER          ( MAXDIM = 2 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CONE
      PARAMETER          ( CONE = ( 1.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      INTEGER            I, INFO, J, K
      DOUBLE PRECISION   RTEMP, SCALE, SMINU, SPLUS
      COMPLEX*16         BM, BP, PMONE, TEMP
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION   RWORK( MAXDIM )
      COMPLEX*16         WORK( 4*MAXDIM ), XM( MAXDIM ), XP( MAXDIM )
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZAXPY, ZCOPY, ZGECON, ZGESC2, ZLASSQ, ZLASWP,
     &                   ZSCAL
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DZASUM
      COMPLEX*16         ZDOTC
      EXTERNAL           DZASUM, ZDOTC
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, SQRT
!     ..
!     .. Executable Statements ..
!
      IF( IJOB.NE.2 ) THEN
!
!        Apply permutations IPIV to RHS
!
         CALL ZLASWP( 1, RHS, LDZ, 1, N-1, IPIV, 1 )
!
!        Solve for L-part choosing RHS either to +1 or -1.
!
         PMONE = -CONE
         DO 10 J = 1, N - 1
            BP = RHS( J ) + CONE
            BM = RHS( J ) - CONE
            SPLUS = ONE
!
!           Lockahead for L- part RHS(1:N-1) = +-1
!           SPLUS and SMIN computed more efficiently than in BSOLVE[1].
!
            SPLUS = SPLUS + DBLE( ZDOTC( N-J, Z( J+1, J ), 1, Z( J+1,
     &              J ), 1 ) )
            SMINU = DBLE( ZDOTC( N-J, Z( J+1, J ), 1, RHS( J+1 ), 1 ) )
            SPLUS = SPLUS*DBLE( RHS( J ) )
            IF( SPLUS.GT.SMINU ) THEN
               RHS( J ) = BP
            ELSE IF( SMINU.GT.SPLUS ) THEN
               RHS( J ) = BM
            ELSE
!
!              In this case the updating sums are equal and we can
!              choose RHS(J) +1 or -1. The first time this happens we
!              choose -1, thereafter +1. This is a simple way to get
!              good estimates of matrices like Byers well-known example
!              (see [1]). (Not done in BSOLVE.)
!
               RHS( J ) = RHS( J ) + PMONE
               PMONE = CONE
            END IF
!
!           Compute the remaining r.h.s.
!
            TEMP = -RHS( J )
            CALL ZAXPY( N-J, TEMP, Z( J+1, J ), 1, RHS( J+1 ), 1 )
   10    CONTINUE
!
!        Solve for U- part, lockahead for RHS(N) = +-1. This is not done
!        In BSOLVE and will hopefully give us a better estimate because
!        any ill-conditioning of the original matrix is transfered to U
!        and not to L. U(N, N) is an approximation to sigma_min(LU).
!
         CALL ZCOPY( N-1, RHS, 1, WORK, 1 )
         WORK( N ) = RHS( N ) + CONE
         RHS( N ) = RHS( N ) - CONE
         SPLUS = ZERO
         SMINU = ZERO
         DO 30 I = N, 1, -1
            TEMP = CONE / Z( I, I )
            WORK( I ) = WORK( I )*TEMP
            RHS( I ) = RHS( I )*TEMP
            DO 20 K = I + 1, N
               WORK( I ) = WORK( I ) - WORK( K )*( Z( I, K )*TEMP )
               RHS( I ) = RHS( I ) - RHS( K )*( Z( I, K )*TEMP )
   20       CONTINUE
            SPLUS = SPLUS + ABS( WORK( I ) )
            SMINU = SMINU + ABS( RHS( I ) )
   30    CONTINUE
         IF( SPLUS.GT.SMINU )
     &      CALL ZCOPY( N, WORK, 1, RHS, 1 )
!
!        Apply the permutations JPIV to the computed solution (RHS)
!
         CALL ZLASWP( 1, RHS, LDZ, 1, N-1, JPIV, -1 )
!
!        Compute the sum of squares
!
         CALL ZLASSQ( N, RHS, 1, RDSCAL, RDSUM )
         RETURN
      END IF
!
!     ENTRY IJOB = 2
!
!     Compute approximate nullvector XM of Z
!
      CALL ZGECON( 'I', N, Z, LDZ, ONE, RTEMP, WORK, RWORK, INFO )
      CALL ZCOPY( N, WORK( N+1 ), 1, XM, 1 )
!
!     Compute RHS
!
      CALL ZLASWP( 1, XM, LDZ, 1, N-1, IPIV, -1 )
      TEMP = CONE / SQRT( ZDOTC( N, XM, 1, XM, 1 ) )
      CALL ZSCAL( N, TEMP, XM, 1 )
      CALL ZCOPY( N, XM, 1, XP, 1 )
      CALL ZAXPY( N, CONE, RHS, 1, XP, 1 )
      CALL ZAXPY( N, -CONE, XM, 1, RHS, 1 )
      CALL ZGESC2( N, Z, LDZ, RHS, IPIV, JPIV, SCALE )
      CALL ZGESC2( N, Z, LDZ, XP, IPIV, JPIV, SCALE )
      IF( DZASUM( N, XP, 1 ).GT.DZASUM( N, RHS, 1 ) )
     &   CALL ZCOPY( N, XP, 1, RHS, 1 )
!
!     Compute the sum of squares
!
      CALL ZLASSQ( N, RHS, 1, RDSCAL, RDSUM )
      RETURN
!
!     End of ZLATDF
!
      END
!
!  =====================================================================
!> @brief ZTGSY2 solves the generalized Sylvester equation
!> (unblocked algorithm).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZTGSY2( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,
!                          LDD, E, LDE, F, LDF, SCALE, RDSUM, RDSCAL,
!                          INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          TRANS
!       INTEGER            IJOB, INFO, LDA, LDB, LDC, LDD, LDE, LDF, M, N
!       DOUBLE PRECISION   RDSCAL, RDSUM, SCALE
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ),
!      $                   D( LDD, * ), E( LDE, * ), F( LDF, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZTGSY2 solves the generalized Sylvester equation
!>
!>             A * R - L * B = scale * C               (1)
!>             D * R - L * E = scale * F
!>
!> using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices,
!> (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
!> N-by-N and M-by-N, respectively. A, B, D and E are upper triangular
!> (i.e., (A,D) and (B,E) in generalized Schur form).
!>
!> The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
!> scaling factor chosen to avoid overflow.
!>
!> In matrix notation solving equation (1) corresponds to solve
!> Zx = scale * b, where Z is defined as
!>
!>        Z = [ kron(In, A)  -kron(B**H, Im) ]             (2)
!>            [ kron(In, D)  -kron(E**H, Im) ],
!>
!> Ik is the identity matrix of size k and X**H is the conjuguate transpose of X.
!> kron(X, Y) is the Kronecker product between the matrices X and Y.
!>
!> If TRANS = 'C', y in the conjugate transposed system Z**H*y = scale*b
!> is solved for, which is equivalent to solve for R and L in
!>
!>             A**H * R  + D**H * L   = scale * C           (3)
!>             R  * B**H + L  * E**H  = scale * -F
!>
!> This case is used to compute an estimate of Dif[(A, D), (B, E)] =
!> = sigma_min(Z) using reverse communicaton with ZLACON.
!>
!> ZTGSY2 also (IJOB >= 1) contributes to the computation in ZTGSYL
!> of an upper bound on the separation between to matrix pairs. Then
!> the input (A, D), (B, E) are sub-pencils of two matrix pairs in
!> ZTGSYL.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>          = 'N', solve the generalized Sylvester equation (1).
!>          = 'T': solve the 'transposed' system (3).
!> \endverbatim
!>
!> @param[in] IJOB
!> \verbatim
!>          IJOB is INTEGER
!>          Specifies what kind of functionality to be performed.
!>          =0: solve (1) only.
!>          =1: A contribution from this subsystem to a Frobenius
!>              norm-based estimate of the separation between two matrix
!>              pairs is computed. (look ahead strategy is used).
!>          =2: A contribution from this subsystem to a Frobenius
!>              norm-based estimate of the separation between two matrix
!>              pairs is computed. (DGECON on sub-systems is used.)
!>          Not referenced if TRANS = 'T'.
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          On entry, M specifies the order of A and D, and the row
!>          dimension of C, F, R and L.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          On entry, N specifies the order of B and E, and the column
!>          dimension of C, F, R and L.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA, M)
!>          On entry, A contains an upper triangular matrix.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the matrix A. LDA >= max(1, M).
!> \endverbatim
!>
!> @param[in] B
!> \verbatim
!>          B is COMPLEX*16 array, dimension (LDB, N)
!>          On entry, B contains an upper triangular matrix.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the matrix B. LDB >= max(1, N).
!> \endverbatim
!>
!> @param[in,out] C
!> \verbatim
!>          C is COMPLEX*16 array, dimension (LDC, N)
!>          On entry, C contains the right-hand-side of the first matrix
!>          equation in (1).
!>          On exit, if IJOB = 0, C has been overwritten by the solution
!>          R.
!> \endverbatim
!>
!> @param[in] LDC
!> \verbatim
!>          LDC is INTEGER
!>          The leading dimension of the matrix C. LDC >= max(1, M).
!> \endverbatim
!>
!> @param[in] D
!> \verbatim
!>          D is COMPLEX*16 array, dimension (LDD, M)
!>          On entry, D contains an upper triangular matrix.
!> \endverbatim
!>
!> @param[in] LDD
!> \verbatim
!>          LDD is INTEGER
!>          The leading dimension of the matrix D. LDD >= max(1, M).
!> \endverbatim
!>
!> @param[in] E
!> \verbatim
!>          E is COMPLEX*16 array, dimension (LDE, N)
!>          On entry, E contains an upper triangular matrix.
!> \endverbatim
!>
!> @param[in] LDE
!> \verbatim
!>          LDE is INTEGER
!>          The leading dimension of the matrix E. LDE >= max(1, N).
!> \endverbatim
!>
!> @param[in,out] F
!> \verbatim
!>          F is COMPLEX*16 array, dimension (LDF, N)
!>          On entry, F contains the right-hand-side of the second matrix
!>          equation in (1).
!>          On exit, if IJOB = 0, F has been overwritten by the solution
!>          L.
!> \endverbatim
!>
!> @param[in] LDF
!> \verbatim
!>          LDF is INTEGER
!>          The leading dimension of the matrix F. LDF >= max(1, M).
!> \endverbatim
!>
!> @param[out] SCALE
!> \verbatim
!>          SCALE is DOUBLE PRECISION
!>          On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions
!>          R and L (C and F on entry) will hold the solutions to a
!>          slightly perturbed system but the input matrices A, B, D and
!>          E have not been changed. If SCALE = 0, R and L will hold the
!>          solutions to the homogeneous system with C = F = 0.
!>          Normally, SCALE = 1.
!> \endverbatim
!>
!> @param[in,out] RDSUM
!> \verbatim
!>          RDSUM is DOUBLE PRECISION
!>          On entry, the sum of squares of computed contributions to
!>          the Dif-estimate under computation by ZTGSYL, where the
!>          scaling factor RDSCAL (see below) has been factored out.
!>          On exit, the corresponding sum of squares updated with the
!>          contributions from the current sub-system.
!>          If TRANS = 'T' RDSUM is not touched.
!>          NOTE: RDSUM only makes sense when ZTGSY2 is called by
!>          ZTGSYL.
!> \endverbatim
!>
!> @param[in,out] RDSCAL
!> \verbatim
!>          RDSCAL is DOUBLE PRECISION
!>          On entry, scaling factor used to prevent overflow in RDSUM.
!>          On exit, RDSCAL is updated w.r.t. the current contributions
!>          in RDSUM.
!>          If TRANS = 'T', RDSCAL is not touched.
!>          NOTE: RDSCAL only makes sense when ZTGSY2 is called by
!>          ZTGSYL.
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          On exit, if INFO is set to
!>            =0: Successful exit
!>            <0: If INFO = -i, input argument number i is illegal.
!>            >0: The matrix pairs (A, D) and (B, E) have common or very
!>                close eigenvalues.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
!  Contributors:
!  ==================
!>
!>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!>     Umea University, S-901 87 Umea, Sweden.
!
      SUBROUTINE ZTGSY2( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,
     &                   LDD, E, LDE, F, LDF, SCALE, RDSUM, RDSCAL,
     &                   INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            IJOB, INFO, LDA, LDB, LDC, LDD, LDE, LDF, M, N
      DOUBLE PRECISION   RDSCAL, RDSUM, SCALE
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ),
     &                   D( LDD, * ), E( LDE, * ), F( LDF, * )
!     ..
!
!  Purpose
!  =======
!
!  ZTGSY2 solves the generalized Sylvester equation
!
!              A * R - L * B = scale *   C               (1)
!              D * R - L * E = scale * F
!
!  using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices,
!  (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
!  N-by-N and M-by-N, respectively. A, B, D and E are upper triangular
!  (i.e., (A,D) and (B,E) in generalized Schur form).
!
!  The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
!  scaling factor chosen to avoid overflow.
!
!  In matrix notation solving equation (1) corresponds to solve
!  Zx = scale * b, where Z is defined as
!
!         Z = [ kron(In, A)  -kron(B', Im) ]             (2)
!             [ kron(In, D)  -kron(E', Im) ],
!
!  Ik is the identity matrix of size k and X' is the transpose of X.
!  kron(X, Y) is the Kronecker product between the matrices X and Y.
!
!  If TRANS = 'C', y in the conjugate transposed system Z'y = scale*b
!  is solved for, which is equivalent to solve for R and L in
!
!              A' * R  + D' * L   = scale *  C           (3)
!              R  * B' + L  * E'  = scale * -F
!
!  This case is used to compute an estimate of Dif[(A, D), (B, E)] =
!  = sigma_min(Z) using reverse communicaton with ZLACON.
!
!  ZTGSY2 also (IJOB >= 1) contributes to the computation in ZTGSYL
!  of an upper bound on the separation between to matrix pairs. Then
!  the input (A, D), (B, E) are sub-pencils of two matrix pairs in
!  ZTGSYL.
!
!  Arguments
!  =========
!
!  TRANS   (input) CHARACTER*1
!          = 'N', solve the generalized Sylvester equation (1).
!          = 'T': solve the 'transposed' system (3).
!
!  IJOB    (input) INTEGER
!          Specifies what kind of functionality to be performed.
!          =0: solve (1) only.
!          =1: A contribution from this subsystem to a Frobenius
!              norm-based estimate of the separation between two matrix
!              pairs is computed. (look ahead strategy is used).
!          =2: A contribution from this subsystem to a Frobenius
!              norm-based estimate of the separation between two matrix
!              pairs is computed. (DGECON on sub-systems is used.)
!          Not referenced if TRANS = 'T'.
!
!  M       (input) INTEGER
!          On entry, M specifies the order of A and D, and the row
!          dimension of C, F, R and L.
!
!  N       (input) INTEGER
!          On entry, N specifies the order of B and E, and the column
!          dimension of C, F, R and L.
!
!  A       (input) COMPLEX*16 array, dimension (LDA, M)
!          On entry, A contains an upper triangular matrix.
!
!  LDA     (input) INTEGER
!          The leading dimension of the matrix A. LDA >= max(1, M).
!
!  B       (input) COMPLEX*16 array, dimension (LDB, N)
!          On entry, B contains an upper triangular matrix.
!
!  LDB     (input) INTEGER
!          The leading dimension of the matrix B. LDB >= max(1, N).
!
!  C       (input/output) COMPLEX*16 array, dimension (LDC, N)
!          On entry, C contains the right-hand-side of the first matrix
!          equation in (1).
!          On exit, if IJOB = 0, C has been overwritten by the solution
!          R.
!
!  LDC     (input) INTEGER
!          The leading dimension of the matrix C. LDC >= max(1, M).
!
!  D       (input) COMPLEX*16 array, dimension (LDD, M)
!          On entry, D contains an upper triangular matrix.
!
!  LDD     (input) INTEGER
!          The leading dimension of the matrix D. LDD >= max(1, M).
!
!  E       (input) COMPLEX*16 array, dimension (LDE, N)
!          On entry, E contains an upper triangular matrix.
!
!  LDE     (input) INTEGER
!          The leading dimension of the matrix E. LDE >= max(1, N).
!
!  F       (input/output) COMPLEX*16 array, dimension (LDF, N)
!          On entry, F contains the right-hand-side of the second matrix
!          equation in (1).
!          On exit, if IJOB = 0, F has been overwritten by the solution
!          L.
!
!  LDF     (input) INTEGER
!          The leading dimension of the matrix F. LDF >= max(1, M).
!
!  SCALE   (output) DOUBLE PRECISION
!          On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions
!          R and L (C and F on entry) will hold the solutions to a
!          slightly perturbed system but the input matrices A, B, D and
!          E have not been changed. If SCALE = 0, R and L will hold the
!          solutions to the homogeneous system with C = F = 0.
!          Normally, SCALE = 1.
!
!  RDSUM   (input/output) DOUBLE PRECISION
!          On entry, the sum of squares of computed contributions to
!          the Dif-estimate under computation by ZTGSYL, where the
!          scaling factor RDSCAL (see below) has been factored out.
!          On exit, the corresponding sum of squares updated with the
!          contributions from the current sub-system.
!          If TRANS = 'T' RDSUM is not touched.
!          NOTE: RDSUM only makes sense when ZTGSY2 is called by
!          ZTGSYL.
!
!  RDSCAL  (input/output) DOUBLE PRECISION
!          On entry, scaling factor used to prevent overflow in RDSUM.
!          On exit, RDSCAL is updated w.r.t. the current contributions
!          in RDSUM.
!          If TRANS = 'T', RDSCAL is not touched.
!          NOTE: RDSCAL only makes sense when ZTGSY2 is called by
!          ZTGSYL.
!
!  INFO    (output) INTEGER
!          On exit, if INFO is set to
!            =0: Successful exit
!            <0: If INFO = -i, input argument number i is illegal.
!            >0: The matrix pairs (A, D) and (B, E) have common or very
!                close eigenvalues.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!     Umea University, S-901 87 Umea, Sweden.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      INTEGER            LDZ
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, LDZ = 2 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOTRAN
      INTEGER            I, IERR, J, K
      DOUBLE PRECISION   SCALOC
      COMPLEX*16         ALPHA
!     ..
!     .. Local Arrays ..
      INTEGER            IPIV( LDZ ), JPIV( LDZ )
      COMPLEX*16         RHS( LDZ ), Z( LDZ, LDZ )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZAXPY, ZGESC2, ZGETC2, ZLATDF, ZSCAL
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DCMPLX, DCONJG, MAX
!     ..
!     .. Executable Statements ..
!
!     Decode and test input parameters
!
      INFO = 0
      IERR = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( NOTRAN ) THEN
         IF( ( IJOB.LT.0 ) .OR. ( IJOB.GT.2 ) ) THEN
            INFO = -2
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( M.LE.0 ) THEN
            INFO = -3
         ELSE IF( N.LE.0 ) THEN
            INFO = -4
         ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
            INFO = -5
         ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
            INFO = -8
         ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
            INFO = -10
         ELSE IF( LDD.LT.MAX( 1, M ) ) THEN
            INFO = -12
         ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
            INFO = -14
         ELSE IF( LDF.LT.MAX( 1, M ) ) THEN
            INFO = -16
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTGSY2', -INFO )
         RETURN
      END IF
!
      IF( NOTRAN ) THEN
!
!        Solve (I, J) - system
!           A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J)
!           D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J)
!        for I = M, M - 1, ..., 1; J = 1, 2, ..., N
!
         SCALE = ONE
         SCALOC = ONE
         DO 30 J = 1, N
            DO 20 I = M, 1, -1
!
!              Build 2 by 2 system
!
               Z( 1, 1 ) = A( I, I )
               Z( 2, 1 ) = D( I, I )
               Z( 1, 2 ) = -B( J, J )
               Z( 2, 2 ) = -E( J, J )
!
!              Set up right hand side(s)
!
               RHS( 1 ) = C( I, J )
               RHS( 2 ) = F( I, J )
!
!              Solve Z * x = RHS
!
               CALL ZGETC2( LDZ, Z, LDZ, IPIV, JPIV, IERR )
               IF( IERR.GT.0 )
     &            INFO = IERR
               IF( IJOB.EQ.0 ) THEN
                  CALL ZGESC2( LDZ, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
                  IF( SCALOC.NE.ONE ) THEN
                     DO 10 K = 1, N
                        CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ),
     &                              C( 1, K ), 1 )
                        CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ),
     &                              F( 1, K ), 1 )
   10                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
               ELSE
                  CALL ZLATDF( IJOB, LDZ, Z, LDZ, RHS, RDSUM, RDSCAL,
     &                         IPIV, JPIV )
               END IF
!
!              Unpack solution vector(s)
!
               C( I, J ) = RHS( 1 )
               F( I, J ) = RHS( 2 )
!
!              Substitute R(I, J) and L(I, J) into remaining equation.
!
               IF( I.GT.1 ) THEN
                  ALPHA = -RHS( 1 )
                  CALL ZAXPY( I-1, ALPHA, A( 1, I ), 1, C( 1, J ), 1 )
                  CALL ZAXPY( I-1, ALPHA, D( 1, I ), 1, F( 1, J ), 1 )
               END IF
               IF( J.LT.N ) THEN
                  CALL ZAXPY( N-J, RHS( 2 ), B( J, J+1 ), LDB,
     &                        C( I, J+1 ), LDC )
                  CALL ZAXPY( N-J, RHS( 2 ), E( J, J+1 ), LDE,
     &                        F( I, J+1 ), LDF )
               END IF
!
   20       CONTINUE
   30    CONTINUE
      ELSE
!
!        Solve transposed (I, J) - system:
!           A(I, I)' * R(I, J) + D(I, I)' * L(J, J) = C(I, J)
!           R(I, I) * B(J, J) + L(I, J) * E(J, J)   = -F(I, J)
!        for I = 1, 2, ..., M, J = N, N - 1, ..., 1
!
         SCALE = ONE
         SCALOC = ONE
         DO 80 I = 1, M
            DO 70 J = N, 1, -1
!
!              Build 2 by 2 system Z'
!
               Z( 1, 1 ) = DCONJG( A( I, I ) )
               Z( 2, 1 ) = -DCONJG( B( J, J ) )
               Z( 1, 2 ) = DCONJG( D( I, I ) )
               Z( 2, 2 ) = -DCONJG( E( J, J ) )
!
!
!              Set up right hand side(s)
!
               RHS( 1 ) = C( I, J )
               RHS( 2 ) = F( I, J )
!
!              Solve Z' * x = RHS
!
               CALL ZGETC2( LDZ, Z, LDZ, IPIV, JPIV, IERR )
               IF( IERR.GT.0 )
     &            INFO = IERR
               CALL ZGESC2( LDZ, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
               IF( SCALOC.NE.ONE ) THEN
                  DO 40 K = 1, N
                     CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ), C( 1, K ),
     &                           1 )
                     CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ), F( 1, K ),
     &                           1 )
   40             CONTINUE
                  SCALE = SCALE*SCALOC
               END IF
!
!              Unpack solution vector(s)
!
               C( I, J ) = RHS( 1 )
               F( I, J ) = RHS( 2 )
!
!              Substitute R(I, J) and L(I, J) into remaining equation.
!
               DO 50 K = 1, J - 1
                  F( I, K ) = F( I, K ) + RHS( 1 )*DCONJG( B( K, J ) ) +
     &                        RHS( 2 )*DCONJG( E( K, J ) )
   50          CONTINUE
               DO 60 K = I + 1, M
                  C( K, J ) = C( K, J ) - DCONJG( A( I, K ) )*RHS( 1 ) -
     &                        DCONJG( D( I, K ) )*RHS( 2 )
   60          CONTINUE
!
   70       CONTINUE
   80    CONTINUE
      END IF
      RETURN
!
!     End of ZTGSY2
!
      END
!
!  =====================================================================
!> @brief ZLASET initializes the off-diagonal elements and the
!> diagonal elements of a matrix to given values.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
!
!       .. Scalar Arguments ..
!       CHARACTER          UPLO
!       INTEGER            LDA, M, N
!       COMPLEX*16         ALPHA, BETA
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLASET initializes a 2-D array A to BETA on the diagonal and
!> ALPHA on the offdiagonals.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>          Specifies the part of the matrix A to be set.
!>          = 'U':      Upper triangular part is set. The lower triangle
!>                      is unchanged.
!>          = 'L':      Lower triangular part is set. The upper triangle
!>                      is unchanged.
!>          Otherwise:  All of the matrix A is set.
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          On entry, M specifies the number of rows of A.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          On entry, N specifies the number of columns of A.
!> \endverbatim
!>
!> @param[in] ALPHA
!> \verbatim
!>          ALPHA is COMPLEX*16
!>          All the offdiagonal array elements are set to ALPHA.
!> \endverbatim
!>
!> @param[in] BETA
!> \verbatim
!>          BETA is COMPLEX*16
!>          All the diagonal array elements are set to BETA.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the m by n matrix A.
!>          On exit, A(i,j) = ALPHA, 1 <= i <= m, 1 <= j <= n, i.ne.j;
!>                   A(i,i) = BETA , 1 <= i <= min(m,n)
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date September 2012
!
      SUBROUTINE ZLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      COMPLEX*16         ALPHA, BETA
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLASET initializes a 2-D array A to BETA on the diagonal and
!  ALPHA on the offdiagonals.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies the part of the matrix A to be set.
!          = 'U':      Upper triangular part is set. The lower triangle
!                      is unchanged.
!          = 'L':      Lower triangular part is set. The upper triangle
!                      is unchanged.
!          Otherwise:  All of the matrix A is set.
!
!  M       (input) INTEGER
!          On entry, M specifies the number of rows of A.
!
!  N       (input) INTEGER
!          On entry, N specifies the number of columns of A.
!
!  ALPHA   (input) COMPLEX*16
!          All the offdiagonal array elements are set to ALPHA.
!
!  BETA    (input) COMPLEX*16
!          All the diagonal array elements are set to BETA.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the m by n matrix A.
!          On exit, A(i,j) = ALPHA, 1 <= i <= m, 1 <= j <= n, i.ne.j;
!                   A(i,i) = BETA , 1 <= i <= min(m,n)
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, J
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MIN
!     ..
!     .. Executable Statements ..
!
      IF( LSAME( UPLO, 'U' ) ) THEN
!
!        Set the diagonal to BETA and the strictly upper triangular
!        part of the array to ALPHA.
!
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
         DO 30 I = 1, MIN( N, M )
            A( I, I ) = BETA
   30    CONTINUE
!
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
!
!        Set the diagonal to BETA and the strictly lower triangular
!        part of the array to ALPHA.
!
         DO 50 J = 1, MIN( M, N )
            DO 40 I = J + 1, M
               A( I, J ) = ALPHA
   40       CONTINUE
   50    CONTINUE
         DO 60 I = 1, MIN( N, M )
            A( I, I ) = BETA
   60    CONTINUE
!
      ELSE
!
!        Set the array to BETA on the diagonal and ALPHA on the
!        offdiagonal.
!
         DO 80 J = 1, N
            DO 70 I = 1, M
               A( I, J ) = ALPHA
   70       CONTINUE
   80    CONTINUE
         DO 90 I = 1, MIN( M, N )
            A( I, I ) = BETA
   90    CONTINUE
      END IF
!
      RETURN
!
!     End of ZLASET
!
      END
!
!  =====================================================================
!> @brief ZTGSYL
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZTGSYL( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,
!                          LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,
!                          IWORK, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          TRANS
!       INTEGER            IJOB, INFO, LDA, LDB, LDC, LDD, LDE, LDF,
!      $                   LWORK, M, N
!       DOUBLE PRECISION   DIF, SCALE
!       ..
!       .. Array Arguments ..
!       INTEGER            IWORK( * )
!       COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ),
!      $                   D( LDD, * ), E( LDE, * ), F( LDF, * ),
!      $                   WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZTGSYL solves the generalized Sylvester equation:
!>
!>             A * R - L * B = scale * C            (1)
!>             D * R - L * E = scale * F
!>
!> where R and L are unknown m-by-n matrices, (A, D), (B, E) and
!> (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
!> respectively, with complex entries. A, B, D and E are upper
!> triangular (i.e., (A,D) and (B,E) in generalized Schur form).
!>
!> The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1
!> is an output scaling factor chosen to avoid overflow.
!>
!> In matrix notation (1) is equivalent to solve Zx = scale*b, where Z
!> is defined as
!>
!>        Z = [ kron(In, A)  -kron(B**H, Im) ]        (2)
!>            [ kron(In, D)  -kron(E**H, Im) ],
!>
!> Here Ix is the identity matrix of size x and X**H is the conjugate
!> transpose of X. Kron(X, Y) is the Kronecker product between the
!> matrices X and Y.
!>
!> If TRANS = 'C', y in the conjugate transposed system Z**H *y = scale*b
!> is solved for, which is equivalent to solve for R and L in
!>
!>             A**H * R + D**H * L = scale * C           (3)
!>             R * B**H + L * E**H = scale * -F
!>
!> This case (TRANS = 'C') is used to compute an one-norm-based estimate
!> of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
!> and (B,E), using ZLACON.
!>
!> If IJOB >= 1, ZTGSYL computes a Frobenius norm-based estimate of
!> Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
!> reciprocal of the smallest singular value of Z.
!>
!> This is a level-3 BLAS algorithm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>          = 'N': solve the generalized sylvester equation (1).
!>          = 'C': solve the "conjugate transposed" system (3).
!> \endverbatim
!>
!> @param[in] IJOB
!> \verbatim
!>          IJOB is INTEGER
!>          Specifies what kind of functionality to be performed.
!>          =0: solve (1) only.
!>          =1: The functionality of 0 and 3.
!>          =2: The functionality of 0 and 4.
!>          =3: Only an estimate of Dif[(A,D), (B,E)] is computed.
!>              (look ahead strategy is used).
!>          =4: Only an estimate of Dif[(A,D), (B,E)] is computed.
!>              (ZGECON on sub-systems is used).
!>          Not referenced if TRANS = 'C'.
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The order of the matrices A and D, and the row dimension of
!>          the matrices C, F, R and L.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrices B and E, and the column dimension
!>          of the matrices C, F, R and L.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA, M)
!>          The upper triangular matrix A.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A. LDA >= max(1, M).
!> \endverbatim
!>
!> @param[in] B
!> \verbatim
!>          B is COMPLEX*16 array, dimension (LDB, N)
!>          The upper triangular matrix B.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B. LDB >= max(1, N).
!> \endverbatim
!>
!> @param[in,out] C
!> \verbatim
!>          C is COMPLEX*16 array, dimension (LDC, N)
!>          On entry, C contains the right-hand-side of the first matrix
!>          equation in (1) or (3).
!>          On exit, if IJOB = 0, 1 or 2, C has been overwritten by
!>          the solution R. If IJOB = 3 or 4 and TRANS = 'N', C holds R,
!>          the solution achieved during the computation of the
!>          Dif-estimate.
!> \endverbatim
!>
!> @param[in] LDC
!> \verbatim
!>          LDC is INTEGER
!>          The leading dimension of the array C. LDC >= max(1, M).
!> \endverbatim
!>
!> @param[in] D
!> \verbatim
!>          D is COMPLEX*16 array, dimension (LDD, M)
!>          The upper triangular matrix D.
!> \endverbatim
!>
!> @param[in] LDD
!> \verbatim
!>          LDD is INTEGER
!>          The leading dimension of the array D. LDD >= max(1, M).
!> \endverbatim
!>
!> @param[in] E
!> \verbatim
!>          E is COMPLEX*16 array, dimension (LDE, N)
!>          The upper triangular matrix E.
!> \endverbatim
!>
!> @param[in] LDE
!> \verbatim
!>          LDE is INTEGER
!>          The leading dimension of the array E. LDE >= max(1, N).
!> \endverbatim
!>
!> @param[in,out] F
!> \verbatim
!>          F is COMPLEX*16 array, dimension (LDF, N)
!>          On entry, F contains the right-hand-side of the second matrix
!>          equation in (1) or (3).
!>          On exit, if IJOB = 0, 1 or 2, F has been overwritten by
!>          the solution L. If IJOB = 3 or 4 and TRANS = 'N', F holds L,
!>          the solution achieved during the computation of the
!>          Dif-estimate.
!> \endverbatim
!>
!> @param[in] LDF
!> \verbatim
!>          LDF is INTEGER
!>          The leading dimension of the array F. LDF >= max(1, M).
!> \endverbatim
!>
!> @param[out] DIF
!> \verbatim
!>          DIF is DOUBLE PRECISION
!>          On exit DIF is the reciprocal of a lower bound of the
!>          reciprocal of the Dif-function, i.e. DIF is an upper bound of
!>          Dif[(A,D), (B,E)] = sigma-min(Z), where Z as in (2).
!>          IF IJOB = 0 or TRANS = 'C', DIF is not referenced.
!> \endverbatim
!>
!> @param[out] SCALE
!> \verbatim
!>          SCALE is DOUBLE PRECISION
!>          On exit SCALE is the scaling factor in (1) or (3).
!>          If 0 < SCALE < 1, C and F hold the solutions R and L, resp.,
!>          to a slightly perturbed system but the input matrices A, B,
!>          D and E have not been changed. If SCALE = 0, R and L will
!>          hold the solutions to the homogenious system with C = F = 0.
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension (MAX(1,LWORK))
!>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!> \endverbatim
!>
!> @param[in] LWORK
!> \verbatim
!>          LWORK is INTEGER
!>          The dimension of the array WORK. LWORK > = 1.
!>          If IJOB = 1 or 2 and TRANS = 'N', LWORK >= max(1,2*M*N).
!>
!>          If LWORK = -1, then a workspace query is assumed; the routine
!>          only calculates the optimal size of the WORK array, returns
!>          this value as the first entry of the WORK array, and no error
!>          message related to LWORK is issued by XERBLA.
!> \endverbatim
!>
!> @param[out] IWORK
!> \verbatim
!>          IWORK is INTEGER array, dimension (M+N+2)
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>            =0: successful exit
!>            <0: If INFO = -i, the i-th argument had an illegal value.
!>            >0: (A, D) and (B, E) have common or very close
!>                eigenvalues.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date November 2011
!
!  Contributors:
!  ==================
!>
!>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!>     Umea University, S-901 87 Umea, Sweden.
!
!  References:
!  ================
!>
!>  [1] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
!>      for Solving the Generalized Sylvester Equation and Estimating the
!>      Separation between Regular Matrix Pairs, Report UMINF - 93.23,
!>      Department of Computing Science, Umea University, S-901 87 Umea,
!>      Sweden, December 1993, Revised April 1994, Also as LAPACK Working
!>      Note 75.  To appear in ACM Trans. on Math. Software, Vol 22,
!>      No 1, 1996.
!> \n
!>  [2] B. Kagstrom, A Perturbation Analysis of the Generalized Sylvester
!>      Equation (AR - LB, DR - LE ) = (C, F), SIAM J. Matrix Anal.
!>      Appl., 15(4):1045-1060, 1994.
!> \n
!>  [3] B. Kagstrom and L. Westin, Generalized Schur Methods with
!>      Condition Estimators for Solving the Generalized Sylvester
!>      Equation, IEEE Transactions on Automatic Control, Vol. 34, No. 7,
!>      July 1989, pp 745-751.
!
      SUBROUTINE ZTGSYL( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,
     &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,
     &                   IWORK, INFO )
!
!  -- LAPACK routine (version 3.1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     January 2007
!
!     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            IJOB, INFO, LDA, LDB, LDC, LDD, LDE, LDF,
     &                   LWORK, M, N
      DOUBLE PRECISION   DIF, SCALE
!     ..
!     .. Array Arguments ..
      INTEGER            IWORK( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ),
     &                   D( LDD, * ), E( LDE, * ), F( LDF, * ),
     &                   WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZTGSYL solves the generalized Sylvester equation:
!
!              A * R - L * B = scale * C            (1)
!              D * R - L * E = scale * F
!
!  where R and L are unknown m-by-n matrices, (A, D), (B, E) and
!  (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
!  respectively, with complex entries. A, B, D and E are upper
!  triangular (i.e., (A,D) and (B,E) in generalized Schur form).
!
!  The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1
!  is an output scaling factor chosen to avoid overflow.
!
!  In matrix notation (1) is equivalent to solve Zx = scale*b, where Z
!  is defined as
!
!         Z = [ kron(In, A)  -kron(B', Im) ]        (2)
!             [ kron(In, D)  -kron(E', Im) ],
!
!  Here Ix is the identity matrix of size x and X' is the conjugate
!  transpose of X. Kron(X, Y) is the Kronecker product between the
!  matrices X and Y.
!
!  If TRANS = 'C', y in the conjugate transposed system Z'*y = scale*b
!  is solved for, which is equivalent to solve for R and L in
!
!              A' * R + D' * L = scale * C           (3)
!              R * B' + L * E' = scale * -F
!
!  This case (TRANS = 'C') is used to compute an one-norm-based estimate
!  of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
!  and (B,E), using ZLACON.
!
!  If IJOB >= 1, ZTGSYL computes a Frobenius norm-based estimate of
!  Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
!  reciprocal of the smallest singular value of Z.
!
!  This is a level-3 BLAS algorithm.
!
!  Arguments
!  =========
!
!  TRANS   (input) CHARACTER*1
!          = 'N': solve the generalized sylvester equation (1).
!          = 'C': solve the "conjugate transposed" system (3).
!
!  IJOB    (input) INTEGER
!          Specifies what kind of functionality to be performed.
!          =0: solve (1) only.
!          =1: The functionality of 0 and 3.
!          =2: The functionality of 0 and 4.
!          =3: Only an estimate of Dif[(A,D), (B,E)] is computed.
!              (look ahead strategy is used).
!          =4: Only an estimate of Dif[(A,D), (B,E)] is computed.
!              (ZGECON on sub-systems is used).
!          Not referenced if TRANS = 'C'.
!
!  M       (input) INTEGER
!          The order of the matrices A and D, and the row dimension of
!          the matrices C, F, R and L.
!
!  N       (input) INTEGER
!          The order of the matrices B and E, and the column dimension
!          of the matrices C, F, R and L.
!
!  A       (input) COMPLEX*16 array, dimension (LDA, M)
!          The upper triangular matrix A.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A. LDA >= max(1, M).
!
!  B       (input) COMPLEX*16 array, dimension (LDB, N)
!          The upper triangular matrix B.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B. LDB >= max(1, N).
!
!  C       (input/output) COMPLEX*16 array, dimension (LDC, N)
!          On entry, C contains the right-hand-side of the first matrix
!          equation in (1) or (3).
!          On exit, if IJOB = 0, 1 or 2, C has been overwritten by
!          the solution R. If IJOB = 3 or 4 and TRANS = 'N', C holds R,
!          the solution achieved during the computation of the
!          Dif-estimate.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1, M).
!
!  D       (input) COMPLEX*16 array, dimension (LDD, M)
!          The upper triangular matrix D.
!
!  LDD     (input) INTEGER
!          The leading dimension of the array D. LDD >= max(1, M).
!
!  E       (input) COMPLEX*16 array, dimension (LDE, N)
!          The upper triangular matrix E.
!
!  LDE     (input) INTEGER
!          The leading dimension of the array E. LDE >= max(1, N).
!
!  F       (input/output) COMPLEX*16 array, dimension (LDF, N)
!          On entry, F contains the right-hand-side of the second matrix
!          equation in (1) or (3).
!          On exit, if IJOB = 0, 1 or 2, F has been overwritten by
!          the solution L. If IJOB = 3 or 4 and TRANS = 'N', F holds L,
!          the solution achieved during the computation of the
!          Dif-estimate.
!
!  LDF     (input) INTEGER
!          The leading dimension of the array F. LDF >= max(1, M).
!
!  DIF     (output) DOUBLE PRECISION
!          On exit DIF is the reciprocal of a lower bound of the
!          reciprocal of the Dif-function, i.e. DIF is an upper bound of
!          Dif[(A,D), (B,E)] = sigma-min(Z), where Z as in (2).
!          IF IJOB = 0 or TRANS = 'C', DIF is not referenced.
!
!  SCALE   (output) DOUBLE PRECISION
!          On exit SCALE is the scaling factor in (1) or (3).
!          If 0 < SCALE < 1, C and F hold the solutions R and L, resp.,
!          to a slightly perturbed system but the input matrices A, B,
!          D and E have not been changed. If SCALE = 0, R and L will
!          hold the solutions to the homogenious system with C = F = 0.
!
!  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK. LWORK > = 1.
!          If IJOB = 1 or 2 and TRANS = 'N', LWORK >= max(1,2*M*N).
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  IWORK   (workspace) INTEGER array, dimension (M+N+2)
!
!  INFO    (output) INTEGER
!            =0: successful exit
!            <0: If INFO = -i, the i-th argument had an illegal value.
!            >0: (A, D) and (B, E) have common or very close
!                eigenvalues.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!     Umea University, S-901 87 Umea, Sweden.
!
!  [1] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
!      for Solving the Generalized Sylvester Equation and Estimating the
!      Separation between Regular Matrix Pairs, Report UMINF - 93.23,
!      Department of Computing Science, Umea University, S-901 87 Umea,
!      Sweden, December 1993, Revised April 1994, Also as LAPACK Working
!      Note 75.  To appear in ACM Trans. on Math. Software, Vol 22,
!      No 1, 1996.
!
!  [2] B. Kagstrom, A Perturbation Analysis of the Generalized Sylvester
!      Equation (AR - LB, DR - LE ) = (C, F), SIAM J. Matrix Anal.
!      Appl., 15(4):1045-1060, 1994.
!
!  [3] B. Kagstrom and L. Westin, Generalized Schur Methods with
!      Condition Estimators for Solving the Generalized Sylvester
!      Equation, IEEE Transactions on Automatic Control, Vol. 34, No. 7,
!      July 1989, pp 745-751.
!
!  =====================================================================
!  Replaced various illegal calls to CCOPY by calls to CLASET.
!  Sven Hammarling, 1/5/02.
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CZERO
      PARAMETER          ( CZERO = (0.0D+0, 0.0D+0) )
!     ..
!     .. Local Scalars ..
      LOGICAL            LQUERY, NOTRAN
      INTEGER            I, IE, IFUNC, IROUND, IS, ISOLVE, J, JE, JS, K,
     &                   LINFO, LWMIN, MB, NB, P, PQ, Q
      DOUBLE PRECISION   DSCALE, DSUM, SCALE2, SCALOC
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEMM, ZLACPY, ZLASET, ZSCAL, ZTGSY2
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, MAX, SQRT
!     ..
!     .. Executable Statements ..
!
!     Decode and test input parameters
!
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
!
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( NOTRAN ) THEN
         IF( ( IJOB.LT.0 ) .OR. ( IJOB.GT.4 ) ) THEN
            INFO = -2
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( M.LE.0 ) THEN
            INFO = -3
         ELSE IF( N.LE.0 ) THEN
            INFO = -4
         ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
            INFO = -6
         ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
            INFO = -8
         ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
            INFO = -10
         ELSE IF( LDD.LT.MAX( 1, M ) ) THEN
            INFO = -12
         ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
            INFO = -14
         ELSE IF( LDF.LT.MAX( 1, M ) ) THEN
            INFO = -16
         END IF
      END IF
!
      IF( INFO.EQ.0 ) THEN
         IF( NOTRAN ) THEN
            IF( IJOB.EQ.1 .OR. IJOB.EQ.2 ) THEN
               LWMIN = MAX( 1, 2*M*N )
            ELSE
               LWMIN = 1
            END IF
         ELSE
            LWMIN = 1
         END IF
         WORK( 1 ) = LWMIN
!
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -20
         END IF
      END IF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTGSYL', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         SCALE = 1
         IF( NOTRAN ) THEN
            IF( IJOB.NE.0 ) THEN
               DIF = 0
            END IF
         END IF
         RETURN
      END IF
!
!     Determine  optimal block sizes MB and NB
!
      MB = ILAENV( 2, 'ZTGSYL', TRANS, M, N, -1, -1 )
      NB = ILAENV( 5, 'ZTGSYL', TRANS, M, N, -1, -1 )
!
      ISOLVE = 1
      IFUNC = 0
      IF( NOTRAN ) THEN
         IF( IJOB.GE.3 ) THEN
            IFUNC = IJOB - 2
            CALL ZLASET( 'F', M, N, CZERO, CZERO, C, LDC )
            CALL ZLASET( 'F', M, N, CZERO, CZERO, F, LDF )
         ELSE IF( IJOB.GE.1 .AND. NOTRAN ) THEN
            ISOLVE = 2
         END IF
      END IF
!
      IF( ( MB.LE.1 .AND. NB.LE.1 ) .OR. ( MB.GE.M .AND. NB.GE.N ) )
     &     THEN
!
!        Use unblocked Level 2 solver
!
         DO 30 IROUND = 1, ISOLVE
!
            SCALE = ONE
            DSCALE = ZERO
            DSUM = ONE
            PQ = M*N
            CALL ZTGSY2( TRANS, IFUNC, M, N, A, LDA, B, LDB, C, LDC, D,
     &                   LDD, E, LDE, F, LDF, SCALE, DSUM, DSCALE,
     &                   INFO )
            IF( DSCALE.NE.ZERO ) THEN
               IF( IJOB.EQ.1 .OR. IJOB.EQ.3 ) THEN
                  DIF = SQRT( DBLE( 2*M*N ) ) / ( DSCALE*SQRT( DSUM ) )
               ELSE
                  DIF = SQRT( DBLE( PQ ) ) / ( DSCALE*SQRT( DSUM ) )
               END IF
            END IF
            IF( ISOLVE.EQ.2 .AND. IROUND.EQ.1 ) THEN
               IF( NOTRAN ) THEN
                  IFUNC = IJOB
               END IF
               SCALE2 = SCALE
               CALL ZLACPY( 'F', M, N, C, LDC, WORK, M )
               CALL ZLACPY( 'F', M, N, F, LDF, WORK( M*N+1 ), M )
               CALL ZLASET( 'F', M, N, CZERO, CZERO, C, LDC )
               CALL ZLASET( 'F', M, N, CZERO, CZERO, F, LDF )
            ELSE IF( ISOLVE.EQ.2 .AND. IROUND.EQ.2 ) THEN
               CALL ZLACPY( 'F', M, N, WORK, M, C, LDC )
               CALL ZLACPY( 'F', M, N, WORK( M*N+1 ), M, F, LDF )
               SCALE = SCALE2
            END IF
   30    CONTINUE
!
         RETURN
!
      END IF
!
!     Determine block structure of A
!
      P = 0
      I = 1
   40 CONTINUE
      IF( I.GT.M )
     &   GO TO 50
      P = P + 1
      IWORK( P ) = I
      I = I + MB
      IF( I.GE.M )
     &   GO TO 50
      GO TO 40
   50 CONTINUE
      IWORK( P+1 ) = M + 1
      IF( IWORK( P ).EQ.IWORK( P+1 ) )
     &   P = P - 1
!
!     Determine block structure of B
!
      Q = P + 1
      J = 1
   60 CONTINUE
      IF( J.GT.N )
     &   GO TO 70
!
      Q = Q + 1
      IWORK( Q ) = J
      J = J + NB
      IF( J.GE.N )
     &   GO TO 70
      GO TO 60
!
   70 CONTINUE
      IWORK( Q+1 ) = N + 1
      IF( IWORK( Q ).EQ.IWORK( Q+1 ) )
     &   Q = Q - 1
!
      IF( NOTRAN ) THEN
         DO 150 IROUND = 1, ISOLVE
!
!           Solve (I, J) - subsystem
!               A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J)
!               D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J)
!           for I = P, P - 1, ..., 1; J = 1, 2, ..., Q
!
            PQ = 0
            SCALE = ONE
            DSCALE = ZERO
            DSUM = ONE
            DO 130 J = P + 2, Q
               JS = IWORK( J )
               JE = IWORK( J+1 ) - 1
               NB = JE - JS + 1
               DO 120 I = P, 1, -1
                  IS = IWORK( I )
                  IE = IWORK( I+1 ) - 1
                  MB = IE - IS + 1
                  CALL ZTGSY2( TRANS, IFUNC, MB, NB, A( IS, IS ), LDA,
     &                         B( JS, JS ), LDB, C( IS, JS ), LDC,
     &                         D( IS, IS ), LDD, E( JS, JS ), LDE,
     &                         F( IS, JS ), LDF, SCALOC, DSUM, DSCALE,
     &                         LINFO )
                  IF( LINFO.GT.0 )
     &               INFO = LINFO
                  PQ = PQ + MB*NB
                  IF( SCALOC.NE.ONE ) THEN
                     DO 80 K = 1, JS - 1
                        CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ),
     &                              C( 1, K ), 1 )
                        CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ),
     &                              F( 1, K ), 1 )
   80                CONTINUE
                     DO 90 K = JS, JE
                        CALL ZSCAL( IS-1, DCMPLX( SCALOC, ZERO ),
     &                              C( 1, K ), 1 )
                        CALL ZSCAL( IS-1, DCMPLX( SCALOC, ZERO ),
     &                              F( 1, K ), 1 )
   90                CONTINUE
                     DO 100 K = JS, JE
                        CALL ZSCAL( M-IE, DCMPLX( SCALOC, ZERO ),
     &                              C( IE+1, K ), 1 )
                        CALL ZSCAL( M-IE, DCMPLX( SCALOC, ZERO ),
     &                              F( IE+1, K ), 1 )
  100                CONTINUE
                     DO 110 K = JE + 1, N
                        CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ),
     &                              C( 1, K ), 1 )
                        CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ),
     &                              F( 1, K ), 1 )
  110                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
!
!                 Substitute R(I,J) and L(I,J) into remaining equation.
!
                  IF( I.GT.1 ) THEN
                     CALL ZGEMM( 'N', 'N', IS-1, NB, MB,
     &                           DCMPLX( -ONE, ZERO ), A( 1, IS ), LDA,
     &                           C( IS, JS ), LDC, DCMPLX( ONE, ZERO ),
     &                           C( 1, JS ), LDC )
                     CALL ZGEMM( 'N', 'N', IS-1, NB, MB,
     &                           DCMPLX( -ONE, ZERO ), D( 1, IS ), LDD,
     &                           C( IS, JS ), LDC, DCMPLX( ONE, ZERO ),
     &                           F( 1, JS ), LDF )
                  END IF
                  IF( J.LT.Q ) THEN
                     CALL ZGEMM( 'N', 'N', MB, N-JE, NB,
     &                           DCMPLX( ONE, ZERO ), F( IS, JS ), LDF,
     &                           B( JS, JE+1 ), LDB,
     &                           DCMPLX( ONE, ZERO ), C( IS, JE+1 ),
     &                           LDC )
                     CALL ZGEMM( 'N', 'N', MB, N-JE, NB,
     &                           DCMPLX( ONE, ZERO ), F( IS, JS ), LDF,
     &                           E( JS, JE+1 ), LDE,
     &                           DCMPLX( ONE, ZERO ), F( IS, JE+1 ),
     &                           LDF )
                  END IF
  120          CONTINUE
  130       CONTINUE
            IF( DSCALE.NE.ZERO ) THEN
               IF( IJOB.EQ.1 .OR. IJOB.EQ.3 ) THEN
                  DIF = SQRT( DBLE( 2*M*N ) ) / ( DSCALE*SQRT( DSUM ) )
               ELSE
                  DIF = SQRT( DBLE( PQ ) ) / ( DSCALE*SQRT( DSUM ) )
               END IF
            END IF
            IF( ISOLVE.EQ.2 .AND. IROUND.EQ.1 ) THEN
               IF( NOTRAN ) THEN
                  IFUNC = IJOB
               END IF
               SCALE2 = SCALE
               CALL ZLACPY( 'F', M, N, C, LDC, WORK, M )
               CALL ZLACPY( 'F', M, N, F, LDF, WORK( M*N+1 ), M )
               CALL ZLASET( 'F', M, N, CZERO, CZERO, C, LDC )
               CALL ZLASET( 'F', M, N, CZERO, CZERO, F, LDF )
            ELSE IF( ISOLVE.EQ.2 .AND. IROUND.EQ.2 ) THEN
               CALL ZLACPY( 'F', M, N, WORK, M, C, LDC )
               CALL ZLACPY( 'F', M, N, WORK( M*N+1 ), M, F, LDF )
               SCALE = SCALE2
            END IF
  150    CONTINUE
      ELSE
!
!        Solve transposed (I, J)-subsystem
!            A(I, I)' * R(I, J) + D(I, I)' * L(I, J) = C(I, J)
!            R(I, J) * B(J, J)  + L(I, J) * E(J, J) = -F(I, J)
!        for I = 1,2,..., P; J = Q, Q-1,..., 1
!
         SCALE = ONE
         DO 210 I = 1, P
            IS = IWORK( I )
            IE = IWORK( I+1 ) - 1
            MB = IE - IS + 1
            DO 200 J = Q, P + 2, -1
               JS = IWORK( J )
               JE = IWORK( J+1 ) - 1
               NB = JE - JS + 1
               CALL ZTGSY2( TRANS, IFUNC, MB, NB, A( IS, IS ), LDA,
     &                      B( JS, JS ), LDB, C( IS, JS ), LDC,
     &                      D( IS, IS ), LDD, E( JS, JS ), LDE,
     &                      F( IS, JS ), LDF, SCALOC, DSUM, DSCALE,
     &                      LINFO )
               IF( LINFO.GT.0 )
     &            INFO = LINFO
               IF( SCALOC.NE.ONE ) THEN
                  DO 160 K = 1, JS - 1
                     CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ), C( 1, K ),
     &                           1 )
                     CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ), F( 1, K ),
     &                           1 )
  160             CONTINUE
                  DO 170 K = JS, JE
                     CALL ZSCAL( IS-1, DCMPLX( SCALOC, ZERO ),
     &                           C( 1, K ), 1 )
                     CALL ZSCAL( IS-1, DCMPLX( SCALOC, ZERO ),
     &                           F( 1, K ), 1 )
  170             CONTINUE
                  DO 180 K = JS, JE
                     CALL ZSCAL( M-IE, DCMPLX( SCALOC, ZERO ),
     &                           C( IE+1, K ), 1 )
                     CALL ZSCAL( M-IE, DCMPLX( SCALOC, ZERO ),
     &                           F( IE+1, K ), 1 )
  180             CONTINUE
                  DO 190 K = JE + 1, N
                     CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ), C( 1, K ),
     &                           1 )
                     CALL ZSCAL( M, DCMPLX( SCALOC, ZERO ), F( 1, K ),
     &                           1 )
  190             CONTINUE
                  SCALE = SCALE*SCALOC
               END IF
!
!              Substitute R(I,J) and L(I,J) into remaining equation.
!
               IF( J.GT.P+2 ) THEN
                  CALL ZGEMM( 'N', 'C', MB, JS-1, NB,
     &                        DCMPLX( ONE, ZERO ), C( IS, JS ), LDC,
     &                        B( 1, JS ), LDB, DCMPLX( ONE, ZERO ),
     &                        F( IS, 1 ), LDF )
                  CALL ZGEMM( 'N', 'C', MB, JS-1, NB,
     &                        DCMPLX( ONE, ZERO ), F( IS, JS ), LDF,
     &                        E( 1, JS ), LDE, DCMPLX( ONE, ZERO ),
     &                        F( IS, 1 ), LDF )
               END IF
               IF( I.LT.P ) THEN
                  CALL ZGEMM( 'C', 'N', M-IE, NB, MB,
     &                        DCMPLX( -ONE, ZERO ), A( IS, IE+1 ), LDA,
     &                        C( IS, JS ), LDC, DCMPLX( ONE, ZERO ),
     &                        C( IE+1, JS ), LDC )
                  CALL ZGEMM( 'C', 'N', M-IE, NB, MB,
     &                        DCMPLX( -ONE, ZERO ), D( IS, IE+1 ), LDD,
     &                        F( IS, JS ), LDF, DCMPLX( ONE, ZERO ),
     &                        C( IE+1, JS ), LDC )
               END IF
  200       CONTINUE
  210    CONTINUE
      END IF
!
      WORK( 1 ) = LWMIN
!
      RETURN
!
!     End of ZTGSYL
!
      END
!
!  =====================================================================
!> @brief ZTGSNA
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZTGSNA( JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,
!                          LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,
!                          IWORK, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          HOWMNY, JOB
!       INTEGER            INFO, LDA, LDB, LDVL, LDVR, LWORK, M, MM, N
!       ..
!       .. Array Arguments ..
!       LOGICAL            SELECT( * )
!       INTEGER            IWORK( * )
!       DOUBLE PRECISION   DIF( * ), S( * )
!       COMPLEX*16         A( LDA, * ), B( LDB, * ), VL( LDVL, * ),
!      $                   VR( LDVR, * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZTGSNA estimates reciprocal condition numbers for specified
!> eigenvalues and/or eigenvectors of a matrix pair (A, B).
!>
!> (A, B) must be in generalized Schur canonical form, that is, A and
!> B are both upper triangular.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] JOB
!> \verbatim
!>          JOB is CHARACTER*1
!>          Specifies whether condition numbers are required for
!>          eigenvalues (S) or eigenvectors (DIF):
!>          = 'E': for eigenvalues only (S);
!>          = 'V': for eigenvectors only (DIF);
!>          = 'B': for both eigenvalues and eigenvectors (S and DIF).
!> \endverbatim
!>
!> @param[in] HOWMNY
!> \verbatim
!>          HOWMNY is CHARACTER*1
!>          = 'A': compute condition numbers for all eigenpairs;
!>          = 'S': compute condition numbers for selected eigenpairs
!>                 specified by the array SELECT.
!> \endverbatim
!>
!> @param[in] SELECT
!> \verbatim
!>          SELECT is LOGICAL array, dimension (N)
!>          If HOWMNY = 'S', SELECT specifies the eigenpairs for which
!>          condition numbers are required. To select condition numbers
!>          for the corresponding j-th eigenvalue and/or eigenvector,
!>          SELECT(j) must be set to .TRUE..
!>          If HOWMNY = 'A', SELECT is not referenced.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the square matrix pair (A, B). N >= 0.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          The upper triangular matrix A in the pair (A,B).
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A. LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in] B
!> \verbatim
!>          B is COMPLEX*16 array, dimension (LDB,N)
!>          The upper triangular matrix B in the pair (A, B).
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B. LDB >= max(1,N).
!> \endverbatim
!>
!> @param[in] VL
!> \verbatim
!>          VL is COMPLEX*16 array, dimension (LDVL,M)
!>          IF JOB = 'E' or 'B', VL must contain left eigenvectors of
!>          (A, B), corresponding to the eigenpairs specified by HOWMNY
!>          and SELECT.  The eigenvectors must be stored in consecutive
!>          columns of VL, as returned by ZTGEVC.
!>          If JOB = 'V', VL is not referenced.
!> \endverbatim
!>
!> @param[in] LDVL
!> \verbatim
!>          LDVL is INTEGER
!>          The leading dimension of the array VL. LDVL >= 1; and
!>          If JOB = 'E' or 'B', LDVL >= N.
!> \endverbatim
!>
!> @param[in] VR
!> \verbatim
!>          VR is COMPLEX*16 array, dimension (LDVR,M)
!>          IF JOB = 'E' or 'B', VR must contain right eigenvectors of
!>          (A, B), corresponding to the eigenpairs specified by HOWMNY
!>          and SELECT.  The eigenvectors must be stored in consecutive
!>          columns of VR, as returned by ZTGEVC.
!>          If JOB = 'V', VR is not referenced.
!> \endverbatim
!>
!> @param[in] LDVR
!> \verbatim
!>          LDVR is INTEGER
!>          The leading dimension of the array VR. LDVR >= 1;
!>          If JOB = 'E' or 'B', LDVR >= N.
!> \endverbatim
!>
!> @param[out] S
!> \verbatim
!>          S is DOUBLE PRECISION array, dimension (MM)
!>          If JOB = 'E' or 'B', the reciprocal condition numbers of the
!>          selected eigenvalues, stored in consecutive elements of the
!>          array.
!>          If JOB = 'V', S is not referenced.
!> \endverbatim
!>
!> @param[out] DIF
!> \verbatim
!>          DIF is DOUBLE PRECISION array, dimension (MM)
!>          If JOB = 'V' or 'B', the estimated reciprocal condition
!>          numbers of the selected eigenvectors, stored in consecutive
!>          elements of the array.
!>          If the eigenvalues cannot be reordered to compute DIF(j),
!>          DIF(j) is set to 0; this can only occur when the true value
!>          would be very small anyway.
!>          For each eigenvalue/vector specified by SELECT, DIF stores
!>          a Frobenius norm-based estimate of Difl.
!>          If JOB = 'E', DIF is not referenced.
!> \endverbatim
!>
!> @param[in] MM
!> \verbatim
!>          MM is INTEGER
!>          The number of elements in the arrays S and DIF. MM >= M.
!> \endverbatim
!>
!> @param[out] M
!> \verbatim
!>          M is INTEGER
!>          The number of elements of the arrays S and DIF used to store
!>          the specified condition numbers; for each selected eigenvalue
!>          one element is used. If HOWMNY = 'A', M is set to N.
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension (MAX(1,LWORK))
!>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!> \endverbatim
!>
!> @param[in] LWORK
!> \verbatim
!>          LWORK is INTEGER
!>          The dimension of the array WORK. LWORK >= max(1,N).
!>          If JOB = 'V' or 'B', LWORK >= max(1,2*N*N).
!> \endverbatim
!>
!> @param[out] IWORK
!> \verbatim
!>          IWORK is INTEGER array, dimension (N+2)
!>          If JOB = 'E', IWORK is not referenced.
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: Successful exit
!>          < 0: If INFO = -i, the i-th argument had an illegal value
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date November 2011
!
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>  The reciprocal of the condition number of the i-th generalized
!>  eigenvalue w = (a, b) is defined as
!>
!>          S(I) = (|v**HAu|**2 + |v**HBu|**2)**(1/2) / (norm(u)*norm(v))
!>
!>  where u and v are the right and left eigenvectors of (A, B)
!>  corresponding to w; |z| denotes the absolute value of the complex
!>  number, and norm(u) denotes the 2-norm of the vector u. The pair
!>  (a, b) corresponds to an eigenvalue w = a/b (= v**HAu/v**HBu) of the
!>  matrix pair (A, B). If both a and b equal zero, then (A,B) is
!>  singular and S(I) = -1 is returned.
!>
!>  An approximate error bound on the chordal distance between the i-th
!>  computed generalized eigenvalue w and the corresponding exact
!>  eigenvalue lambda is
!>
!>          chord(w, lambda) <=   EPS * norm(A, B) / S(I),
!>
!>  where EPS is the machine precision.
!>
!>  The reciprocal of the condition number of the right eigenvector u
!>  and left eigenvector v corresponding to the generalized eigenvalue w
!>  is defined as follows. Suppose
!>
!>                   (A, B) = ( a   *  ) ( b  *  )  1
!>                            ( 0  A22 ),( 0 B22 )  n-1
!>                              1  n-1     1 n-1
!>
!>  Then the reciprocal condition number DIF(I) is
!>
!>          Difl[(a, b), (A22, B22)]  = sigma-min( Zl )
!>
!>  where sigma-min(Zl) denotes the smallest singular value of
!>
!>         Zl = [ kron(a, In-1) -kron(1, A22) ]
!>              [ kron(b, In-1) -kron(1, B22) ].
!>
!>  Here In-1 is the identity matrix of size n-1 and X**H is the conjugate
!>  transpose of X. kron(X, Y) is the Kronecker product between the
!>  matrices X and Y.
!>
!>  We approximate the smallest singular value of Zl with an upper
!>  bound. This is done by ZLATDF.
!>
!>  An approximate error bound for a computed eigenvector VL(i) or
!>  VR(i) is given by
!>
!>                      EPS * norm(A, B) / DIF(i).
!>
!>  See ref. [2-3] for more details and further references.
!> \endverbatim
!
!  Contributors:
!  ==================
!>
!>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!>     Umea University, S-901 87 Umea, Sweden.
!
!  References:
!  ================
!>
!> \verbatim
!>
!>  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
!>      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
!>      M.S. Moonen et al (eds), Linear Algebra for Large Scale and
!>      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.
!>
!>  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
!>      Eigenvalues of a Regular Matrix Pair (A, B) and Condition
!>      Estimation: Theory, Algorithms and Software, Report
!>      UMINF - 94.04, Department of Computing Science, Umea University,
!>      S-901 87 Umea, Sweden, 1994. Also as LAPACK Working Note 87.
!>      To appear in Numerical Algorithms, 1996.
!>
!>  [3] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
!>      for Solving the Generalized Sylvester Equation and Estimating the
!>      Separation between Regular Matrix Pairs, Report UMINF - 93.23,
!>      Department of Computing Science, Umea University, S-901 87 Umea,
!>      Sweden, December 1993, Revised April 1994, Also as LAPACK Working
!>      Note 75.
!>      To appear in ACM Trans. on Math. Software, Vol 22, No 1, 1996.
!> \endverbatim
!
      SUBROUTINE ZTGSNA( JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,
     &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,
     &                   IWORK, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          HOWMNY, JOB
      INTEGER            INFO, LDA, LDB, LDVL, LDVR, LWORK, M, MM, N
!     ..
!     .. Array Arguments ..
      LOGICAL            SELECT( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   DIF( * ), S( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * ), VL( LDVL, * ),
     &                   VR( LDVR, * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZTGSNA estimates reciprocal condition numbers for specified
!  eigenvalues and/or eigenvectors of a matrix pair (A, B).
!
!  (A, B) must be in generalized Schur canonical form, that is, A and
!  B are both upper triangular.
!
!  Arguments
!  =========
!
!  JOB     (input) CHARACTER*1
!          Specifies whether condition numbers are required for
!          eigenvalues (S) or eigenvectors (DIF):
!          = 'E': for eigenvalues only (S);
!          = 'V': for eigenvectors only (DIF);
!          = 'B': for both eigenvalues and eigenvectors (S and DIF).
!
!  HOWMNY  (input) CHARACTER*1
!          = 'A': compute condition numbers for all eigenpairs;
!          = 'S': compute condition numbers for selected eigenpairs
!                 specified by the array SELECT.
!
!  SELECT  (input) LOGICAL array, dimension (N)
!          If HOWMNY = 'S', SELECT specifies the eigenpairs for which
!          condition numbers are required. To select condition numbers
!          for the corresponding j-th eigenvalue and/or eigenvector,
!          SELECT(j) must be set to .TRUE..
!          If HOWMNY = 'A', SELECT is not referenced.
!
!  N       (input) INTEGER
!          The order of the square matrix pair (A, B). N >= 0.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The upper triangular matrix A in the pair (A,B).
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A. LDA >= max(1,N).
!
!  B       (input) COMPLEX*16 array, dimension (LDB,N)
!          The upper triangular matrix B in the pair (A, B).
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B. LDB >= max(1,N).
!
!  VL      (input) COMPLEX*16 array, dimension (LDVL,M)
!          IF JOB = 'E' or 'B', VL must contain left eigenvectors of
!          (A, B), corresponding to the eigenpairs specified by HOWMNY
!          and SELECT.  The eigenvectors must be stored in consecutive
!          columns of VL, as returned by ZTGEVC.
!          If JOB = 'V', VL is not referenced.
!
!  LDVL    (input) INTEGER
!          The leading dimension of the array VL. LDVL >= 1; and
!          If JOB = 'E' or 'B', LDVL >= N.
!
!  VR      (input) COMPLEX*16 array, dimension (LDVR,M)
!          IF JOB = 'E' or 'B', VR must contain right eigenvectors of
!          (A, B), corresponding to the eigenpairs specified by HOWMNY
!          and SELECT.  The eigenvectors must be stored in consecutive
!          columns of VR, as returned by ZTGEVC.
!          If JOB = 'V', VR is not referenced.
!
!  LDVR    (input) INTEGER
!          The leading dimension of the array VR. LDVR >= 1;
!          If JOB = 'E' or 'B', LDVR >= N.
!
!  S       (output) DOUBLE PRECISION array, dimension (MM)
!          If JOB = 'E' or 'B', the reciprocal condition numbers of the
!          selected eigenvalues, stored in consecutive elements of the
!          array.
!          If JOB = 'V', S is not referenced.
!
!  DIF     (output) DOUBLE PRECISION array, dimension (MM)
!          If JOB = 'V' or 'B', the estimated reciprocal condition
!          numbers of the selected eigenvectors, stored in consecutive
!          elements of the array.
!          If the eigenvalues cannot be reordered to compute DIF(j),
!          DIF(j) is set to 0; this can only occur when the true value
!          would be very small anyway.
!          For each eigenvalue/vector specified by SELECT, DIF stores
!          a Frobenius norm-based estimate of Difl.
!          If JOB = 'E', DIF is not referenced.
!
!  MM      (input) INTEGER
!          The number of elements in the arrays S and DIF. MM >= M.
!
!  M       (output) INTEGER
!          The number of elements of the arrays S and DIF used to store
!          the specified condition numbers; for each selected eigenvalue
!          one element is used. If HOWMNY = 'A', M is set to N.
!
!  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK  (input) INTEGER
!          The dimension of the array WORK. LWORK >= max(1,N).
!          If JOB = 'V' or 'B', LWORK >= max(1,2*N*N).
!
!  IWORK   (workspace) INTEGER array, dimension (N+2)
!          If JOB = 'E', IWORK is not referenced.
!
!  INFO    (output) INTEGER
!          = 0: Successful exit
!          < 0: If INFO = -i, the i-th argument had an illegal value
!
!  Further Details
!  ===============
!
!  The reciprocal of the condition number of the i-th generalized
!  eigenvalue w = (a, b) is defined as
!
!          S(I) = (|v'Au|**2 + |v'Bu|**2)**(1/2) / (norm(u)*norm(v))
!
!  where u and v are the right and left eigenvectors of (A, B)
!  corresponding to w; |z| denotes the absolute value of the complex
!  number, and norm(u) denotes the 2-norm of the vector u. The pair
!  (a, b) corresponds to an eigenvalue w = a/b (= v'Au/v'Bu) of the
!  matrix pair (A, B). If both a and b equal zero, then (A,B) is
!  singular and S(I) = -1 is returned.
!
!  An approximate error bound on the chordal distance between the i-th
!  computed generalized eigenvalue w and the corresponding exact
!  eigenvalue lambda is
!
!          chord(w, lambda) <=   EPS * norm(A, B) / S(I),
!
!  where EPS is the machine precision.
!
!  The reciprocal of the condition number of the right eigenvector u
!  and left eigenvector v corresponding to the generalized eigenvalue w
!  is defined as follows. Suppose
!
!                   (A, B) = ( a   *  ) ( b  *  )  1
!                            ( 0  A22 ),( 0 B22 )  n-1
!                              1  n-1     1 n-1
!
!  Then the reciprocal condition number DIF(I) is
!
!          Difl[(a, b), (A22, B22)]  = sigma-min( Zl )
!
!  where sigma-min(Zl) denotes the smallest singular value of
!
!         Zl = [ kron(a, In-1) -kron(1, A22) ]
!              [ kron(b, In-1) -kron(1, B22) ].
!
!  Here In-1 is the identity matrix of size n-1 and X' is the conjugate
!  transpose of X. kron(X, Y) is the Kronecker product between the
!  matrices X and Y.
!
!  We approximate the smallest singular value of Zl with an upper
!  bound. This is done by ZLATDF.
!
!  An approximate error bound for a computed eigenvector VL(i) or
!  VR(i) is given by
!
!                      EPS * norm(A, B) / DIF(i).
!
!  See ref. [2-3] for more details and further references.
!
!  Based on contributions by
!     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
!     Umea University, S-901 87 Umea, Sweden.
!
!  References
!  ==========
!
!  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
!      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
!      M.S. Moonen et al (eds), Linear Algebra for Large Scale and
!      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.
!
!  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
!      Eigenvalues of a Regular Matrix Pair (A, B) and Condition
!      Estimation: Theory, Algorithms and Software, Report
!      UMINF - 94.04, Department of Computing Science, Umea University,
!      S-901 87 Umea, Sweden, 1994. Also as LAPACK Working Note 87.
!      To appear in Numerical Algorithms, 1996.
!
!  [3] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software
!      for Solving the Generalized Sylvester Equation and Estimating the
!      Separation between Regular Matrix Pairs, Report UMINF - 93.23,
!      Department of Computing Science, Umea University, S-901 87 Umea,
!      Sweden, December 1993, Revised April 1994, Also as LAPACK Working
!      Note 75.
!      To appear in ACM Trans. on Math. Software, Vol 22, No 1, 1996.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      INTEGER            IDIFJB
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, IDIFJB = 3 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LQUERY, SOMCON, WANTBH, WANTDF, WANTS
      INTEGER            I, IERR, IFST, ILST, K, KS, LWMIN, N1, N2
      DOUBLE PRECISION   BIGNUM, COND, EPS, LNRM, RNRM, SCALE, SMLNUM
      COMPLEX*16         YHAX, YHBX
!     ..
!     .. Local Arrays ..
      COMPLEX*16         DUMMY( 1 ), DUMMY1( 1 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLAPY2, DZNRM2
      COMPLEX*16         ZDOTC
      EXTERNAL           LSAME, DLAMCH, DLAPY2, DZNRM2, ZDOTC
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLABAD, XERBLA, ZGEMV, ZLACPY, ZTGEXC, ZTGSYL
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DCMPLX, MAX
!     ..
!     .. Executable Statements ..
!
!     Decode and test the input parameters
!
      WANTBH = LSAME( JOB, 'B' )
      WANTS = LSAME( JOB, 'E' ) .OR. WANTBH
      WANTDF = LSAME( JOB, 'V' ) .OR. WANTBH
!
      SOMCON = LSAME( HOWMNY, 'S' )
!
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
!
      IF( .NOT.WANTS .AND. .NOT.WANTDF ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( HOWMNY, 'A' ) .AND. .NOT.SOMCON ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( WANTS .AND. LDVL.LT.N ) THEN
         INFO = -10
      ELSE IF( WANTS .AND. LDVR.LT.N ) THEN
         INFO = -12
      ELSE
!
!        Set M to the number of eigenpairs for which condition numbers
!        are required, and test MM.
!
         IF( SOMCON ) THEN
            M = 0
            DO 10 K = 1, N
               IF( SELECT( K ) )
     &            M = M + 1
   10       CONTINUE
         ELSE
            M = N
         END IF
!
         IF( N.EQ.0 ) THEN
            LWMIN = 1
         ELSE IF( LSAME( JOB, 'V' ) .OR. LSAME( JOB, 'B' ) ) THEN
            LWMIN = 2*N*N
         ELSE
            LWMIN = N
         END IF
         WORK( 1 ) = LWMIN
!
         IF( MM.LT.M ) THEN
            INFO = -15
         ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -18
         END IF
      END IF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTGSNA', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 )
     &   RETURN
!
!     Get machine constants
!
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      KS = 0
      DO 20 K = 1, N
!
!        Determine whether condition numbers are required for the k-th
!        eigenpair.
!
         IF( SOMCON ) THEN
            IF( .NOT.SELECT( K ) )
     &         GO TO 20
         END IF
!
         KS = KS + 1
!
         IF( WANTS ) THEN
!
!           Compute the reciprocal condition number of the k-th
!           eigenvalue.
!
            RNRM = DZNRM2( N, VR( 1, KS ), 1 )
            LNRM = DZNRM2( N, VL( 1, KS ), 1 )
            CALL ZGEMV( 'N', N, N, DCMPLX( ONE, ZERO ), A, LDA,
     &                  VR( 1, KS ), 1, DCMPLX( ZERO, ZERO ), WORK, 1 )
            YHAX = ZDOTC( N, WORK, 1, VL( 1, KS ), 1 )
            CALL ZGEMV( 'N', N, N, DCMPLX( ONE, ZERO ), B, LDB,
     &                  VR( 1, KS ), 1, DCMPLX( ZERO, ZERO ), WORK, 1 )
            YHBX = ZDOTC( N, WORK, 1, VL( 1, KS ), 1 )
            COND = DLAPY2( ABS( YHAX ), ABS( YHBX ) )
            IF( COND.EQ.ZERO ) THEN
               S( KS ) = -ONE
            ELSE
               S( KS ) = COND / ( RNRM*LNRM )
            END IF
         END IF
!
         IF( WANTDF ) THEN
            IF( N.EQ.1 ) THEN
               DIF( KS ) = DLAPY2( ABS( A( 1, 1 ) ), ABS( B( 1, 1 ) ) )
            ELSE
!
!              Estimate the reciprocal condition number of the k-th
!              eigenvectors.
!
!              Copy the matrix (A, B) to the array WORK and move the
!              (k,k)th pair to the (1,1) position.
!
               CALL ZLACPY( 'Full', N, N, A, LDA, WORK, N )
               CALL ZLACPY( 'Full', N, N, B, LDB, WORK( N*N+1 ), N )
               IFST = K
               ILST = 1
!
               CALL ZTGEXC( .FALSE., .FALSE., N, WORK, N, WORK( N*N+1 ),
     &                      N, DUMMY, 1, DUMMY1, 1, IFST, ILST, IERR )
!
               IF( IERR.GT.0 ) THEN
!
!                 Ill-conditioned problem - swap rejected.
!
                  DIF( KS ) = ZERO
               ELSE
!
!                 Reordering successful, solve generalized Sylvester
!                 equation for R and L,
!                            A22 * R - L * A11 = A12
!                            B22 * R - L * B11 = B12,
!                 and compute estimate of Difl[(A11,B11), (A22, B22)].
!
                  N1 = 1
                  N2 = N - N1
                  I = N*N + 1
                  CALL ZTGSYL( 'N', IDIFJB, N2, N1, WORK( N*N1+N1+1 ),
     &                         N, WORK, N, WORK( N1+1 ), N,
     &                         WORK( N*N1+N1+I ), N, WORK( I ), N,
     &                         WORK( N1+I ), N, SCALE, DIF( KS ), DUMMY,
     &                         1, IWORK, IERR )
               END IF
            END IF
         END IF
!
   20 CONTINUE
      WORK( 1 ) = LWMIN
      RETURN
!
!     End of ZTGSNA
!
      END
!
!  =====================================================================
!> @brief <b> ZGGEVX computes the eigenvalues and, optionally, the left
!> and/or right eigenvectors for GE matrices</b>
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B, LDB,
!                          ALPHA, BETA, VL, LDVL, VR, LDVR, ILO, IHI,
!                          LSCALE, RSCALE, ABNRM, BBNRM, RCONDE, RCONDV,
!                          WORK, LWORK, RWORK, IWORK, BWORK, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          BALANC, JOBVL, JOBVR, SENSE
!       INTEGER            IHI, ILO, INFO, LDA, LDB, LDVL, LDVR, LWORK, N
!       DOUBLE PRECISION   ABNRM, BBNRM
!       ..
!       .. Array Arguments ..
!       LOGICAL            BWORK( * )
!       INTEGER            IWORK( * )
!       DOUBLE PRECISION   LSCALE( * ), RCONDE( * ), RCONDV( * ),
!      $                   RSCALE( * ), RWORK( * )
!       COMPLEX*16         A( LDA, * ), ALPHA( * ), B( LDB, * ),
!      $                   BETA( * ), VL( LDVL, * ), VR( LDVR, * ),
!      $                   WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
!> (A,B) the generalized eigenvalues, and optionally, the left and/or
!> right generalized eigenvectors.
!>
!> Optionally, it also computes a balancing transformation to improve
!> the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
!> LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
!> the eigenvalues (RCONDE), and reciprocal condition numbers for the
!> right eigenvectors (RCONDV).
!>
!> A generalized eigenvalue for a pair of matrices (A,B) is a scalar
!> lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
!> singular. It is usually represented as the pair (alpha,beta), as
!> there is a reasonable interpretation for beta=0, and even for both
!> being zero.
!>
!> The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
!> of (A,B) satisfies
!>                  A * v(j) = lambda(j) * B * v(j) .
!> The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
!> of (A,B) satisfies
!>                  u(j)**H * A  = lambda(j) * u(j)**H * B.
!> where u(j)**H is the conjugate-transpose of u(j).
!>
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] BALANC
!> \verbatim
!>          BALANC is CHARACTER*1
!>          Specifies the balance option to be performed:
!>          = 'N':  do not diagonally scale or permute;
!>          = 'P':  permute only;
!>          = 'S':  scale only;
!>          = 'B':  both permute and scale.
!>          Computed reciprocal condition numbers will be for the
!>          matrices after permuting and/or balancing. Permuting does
!>          not change condition numbers (in exact arithmetic), but
!>          balancing does.
!> \endverbatim
!>
!> @param[in] JOBVL
!> \verbatim
!>          JOBVL is CHARACTER*1
!>          = 'N':  do not compute the left generalized eigenvectors;
!>          = 'V':  compute the left generalized eigenvectors.
!> \endverbatim
!>
!> @param[in] JOBVR
!> \verbatim
!>          JOBVR is CHARACTER*1
!>          = 'N':  do not compute the right generalized eigenvectors;
!>          = 'V':  compute the right generalized eigenvectors.
!> \endverbatim
!>
!> @param[in] SENSE
!> \verbatim
!>          SENSE is CHARACTER*1
!>          Determines which reciprocal condition numbers are computed.
!>          = 'N': none are computed;
!>          = 'E': computed for eigenvalues only;
!>          = 'V': computed for eigenvectors only;
!>          = 'B': computed for eigenvalues and eigenvectors.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrices A, B, VL, and VR.  N >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA, N)
!>          On entry, the matrix A in the pair (A,B).
!>          On exit, A has been overwritten. If JOBVL='V' or JOBVR='V'
!>          or both, then A contains the first part of the complex Schur
!>          form of the "balanced" versions of the input A and B.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of A.  LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in,out] B
!> \verbatim
!>          B is COMPLEX*16 array, dimension (LDB, N)
!>          On entry, the matrix B in the pair (A,B).
!>          On exit, B has been overwritten. If JOBVL='V' or JOBVR='V'
!>          or both, then B contains the second part of the complex
!>          Schur form of the "balanced" versions of the input A and B.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of B.  LDB >= max(1,N).
!> \endverbatim
!>
!> @param[out] ALPHA
!> \verbatim
!>          ALPHA is COMPLEX*16 array, dimension (N)
!> \endverbatim
!>
!> @param[out] BETA
!> \verbatim
!>          BETA is COMPLEX*16 array, dimension (N)
!>          On exit, ALPHA(j)/BETA(j), j=1,...,N, will be the generalized
!>          eigenvalues.
!>
!>          Note: the quotient ALPHA(j)/BETA(j) ) may easily over- or
!>          underflow, and BETA(j) may even be zero.  Thus, the user
!>          should avoid naively computing the ratio ALPHA/BETA.
!>          However, ALPHA will be always less than and usually
!>          comparable with norm(A) in magnitude, and BETA always less
!>          than and usually comparable with norm(B).
!> \endverbatim
!>
!> @param[out] VL
!> \verbatim
!>          VL is COMPLEX*16 array, dimension (LDVL,N)
!>          If JOBVL = 'V', the left generalized eigenvectors u(j) are
!>          stored one after another in the columns of VL, in the same
!>          order as their eigenvalues.
!>          Each eigenvector will be scaled so the largest component
!>          will have abs(real part) + abs(imag. part) = 1.
!>          Not referenced if JOBVL = 'N'.
!> \endverbatim
!>
!> @param[in] LDVL
!> \verbatim
!>          LDVL is INTEGER
!>          The leading dimension of the matrix VL. LDVL >= 1, and
!>          if JOBVL = 'V', LDVL >= N.
!> \endverbatim
!>
!> @param[out] VR
!> \verbatim
!>          VR is COMPLEX*16 array, dimension (LDVR,N)
!>          If JOBVR = 'V', the right generalized eigenvectors v(j) are
!>          stored one after another in the columns of VR, in the same
!>          order as their eigenvalues.
!>          Each eigenvector will be scaled so the largest component
!>          will have abs(real part) + abs(imag. part) = 1.
!>          Not referenced if JOBVR = 'N'.
!> \endverbatim
!>
!> @param[in] LDVR
!> \verbatim
!>          LDVR is INTEGER
!>          The leading dimension of the matrix VR. LDVR >= 1, and
!>          if JOBVR = 'V', LDVR >= N.
!> \endverbatim
!>
!> @param[out] ILO
!> \verbatim
!>          ILO is INTEGER
!> \endverbatim
!>
!> @param[out] IHI
!> \verbatim
!>          IHI is INTEGER
!>          ILO and IHI are integer values such that on exit
!>          A(i,j) = 0 and B(i,j) = 0 if i > j and
!>          j = 1,...,ILO-1 or i = IHI+1,...,N.
!>          If BALANC = 'N' or 'S', ILO = 1 and IHI = N.
!> \endverbatim
!>
!> @param[out] LSCALE
!> \verbatim
!>          LSCALE is DOUBLE PRECISION array, dimension (N)
!>          Details of the permutations and scaling factors applied
!>          to the left side of A and B.  If PL(j) is the index of the
!>          row interchanged with row j, and DL(j) is the scaling
!>          factor applied to row j, then
!>            LSCALE(j) = PL(j)  for j = 1,...,ILO-1
!>                      = DL(j)  for j = ILO,...,IHI
!>                      = PL(j)  for j = IHI+1,...,N.
!>          The order in which the interchanges are made is N to IHI+1,
!>          then 1 to ILO-1.
!> \endverbatim
!>
!> @param[out] RSCALE
!> \verbatim
!>          RSCALE is DOUBLE PRECISION array, dimension (N)
!>          Details of the permutations and scaling factors applied
!>          to the right side of A and B.  If PR(j) is the index of the
!>          column interchanged with column j, and DR(j) is the scaling
!>          factor applied to column j, then
!>            RSCALE(j) = PR(j)  for j = 1,...,ILO-1
!>                      = DR(j)  for j = ILO,...,IHI
!>                      = PR(j)  for j = IHI+1,...,N
!>          The order in which the interchanges are made is N to IHI+1,
!>          then 1 to ILO-1.
!> \endverbatim
!>
!> @param[out] ABNRM
!> \verbatim
!>          ABNRM is DOUBLE PRECISION
!>          The one-norm of the balanced matrix A.
!> \endverbatim
!>
!> @param[out] BBNRM
!> \verbatim
!>          BBNRM is DOUBLE PRECISION
!>          The one-norm of the balanced matrix B.
!> \endverbatim
!>
!> @param[out] RCONDE
!> \verbatim
!>          RCONDE is DOUBLE PRECISION array, dimension (N)
!>          If SENSE = 'E' or 'B', the reciprocal condition numbers of
!>          the eigenvalues, stored in consecutive elements of the array.
!>          If SENSE = 'N' or 'V', RCONDE is not referenced.
!> \endverbatim
!>
!> @param[out] RCONDV
!> \verbatim
!>          RCONDV is DOUBLE PRECISION array, dimension (N)
!>          If JOB = 'V' or 'B', the estimated reciprocal condition
!>          numbers of the eigenvectors, stored in consecutive elements
!>          of the array. If the eigenvalues cannot be reordered to
!>          compute RCONDV(j), RCONDV(j) is set to 0; this can only occur
!>          when the true value would be very small anyway.
!>          If SENSE = 'N' or 'E', RCONDV is not referenced.
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension (MAX(1,LWORK))
!>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!> \endverbatim
!>
!> @param[in] LWORK
!> \verbatim
!>          LWORK is INTEGER
!>          The dimension of the array WORK. LWORK >= max(1,2*N).
!>          If SENSE = 'E', LWORK >= max(1,4*N).
!>          If SENSE = 'V' or 'B', LWORK >= max(1,2*N*N+2*N).
!>
!>          If LWORK = -1, then a workspace query is assumed; the routine
!>          only calculates the optimal size of the WORK array, returns
!>          this value as the first entry of the WORK array, and no error
!>          message related to LWORK is issued by XERBLA.
!> \endverbatim
!>
!> @param[out] RWORK
!> \verbatim
!>          RWORK is DOUBLE PRECISION array, dimension (lrwork)
!>          lrwork must be at least max(1,6*N) if BALANC = 'S' or 'B',
!>          and at least max(1,2*N) otherwise.
!>          Real workspace.
!> \endverbatim
!>
!> @param[out] IWORK
!> \verbatim
!>          IWORK is INTEGER array, dimension (N+2)
!>          If SENSE = 'E', IWORK is not referenced.
!> \endverbatim
!>
!> @param[out] BWORK
!> \verbatim
!>          BWORK is LOGICAL array, dimension (N)
!>          If SENSE = 'N', BWORK is not referenced.
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument had an illegal value.
!>          = 1,...,N:
!>                The QZ iteration failed.  No eigenvectors have been
!>                calculated, but ALPHA(j) and BETA(j) should be correct
!>                for j=INFO+1,...,N.
!>          > N:  =N+1: other than QZ iteration failed in ZHGEQZ.
!>                =N+2: error return from ZTGEVC.
!> \endverbatim
!
!  Authors:
!  ========
!
!> @author Univ. of Tennessee
!> @author Univ. of California Berkeley
!> @author Univ. of Colorado Denver
!> @author NAG Ltd.
!
!> @date April 2012
!
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Balancing a matrix pair (A,B) includes, first, permuting rows and
!>  columns to isolate eigenvalues, second, applying diagonal similarity
!>  transformation to the rows and columns to make the rows and columns
!>  as close in norm as possible. The computed reciprocal condition
!>  numbers correspond to the balanced matrix. Permuting rows and columns
!>  will not change the condition numbers (in exact arithmetic) but
!>  diagonal scaling will.  For further explanation of balancing, see
!>  section 4.11.1.2 of LAPACK Users' Guide.
!>
!>  An approximate error bound on the chordal distance between the i-th
!>  computed generalized eigenvalue w and the corresponding exact
!>  eigenvalue lambda is
!>
!>       chord(w, lambda) <= EPS * norm(ABNRM, BBNRM) / RCONDE(I)
!>
!>  An approximate error bound for the angle between the i-th computed
!>  eigenvector VL(i) or VR(i) is given by
!>
!>       EPS * norm(ABNRM, BBNRM) / DIF(i).
!>
!>  For further explanation of the reciprocal condition numbers RCONDE
!>  and RCONDV, see section 4.11 of LAPACK User's Guide.
!> \endverbatim
!
      SUBROUTINE ZGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B, LDB,
     &                   ALPHA, BETA, VL, LDVL, VR, LDVR, ILO, IHI,
     &                   LSCALE, RSCALE, ABNRM, BBNRM, RCONDE, RCONDV,
     &                   WORK, LWORK, RWORK, IWORK, BWORK, INFO )
!
!  -- LAPACK driver routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          BALANC, JOBVL, JOBVR, SENSE
      INTEGER            IHI, ILO, INFO, LDA, LDB, LDVL, LDVR, LWORK, N
      DOUBLE PRECISION   ABNRM, BBNRM
!     ..
!     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   LSCALE( * ), RCONDE( * ), RCONDV( * ),
     &                   RSCALE( * ), RWORK( * )
      COMPLEX*16         A( LDA, * ), ALPHA( * ), B( LDB, * ),
     &                   BETA( * ), VL( LDVL, * ), VR( LDVR, * ),
     &                   WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
!  (A,B) the generalized eigenvalues, and optionally, the left and/or
!  right generalized eigenvectors.
!
!  Optionally, it also computes a balancing transformation to improve
!  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
!  LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
!  the eigenvalues (RCONDE), and reciprocal condition numbers for the
!  right eigenvectors (RCONDV).
!
!  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
!  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
!  singular. It is usually represented as the pair (alpha,beta), as
!  there is a reasonable interpretation for beta=0, and even for both
!  being zero.
!
!  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
!  of (A,B) satisfies
!                   A * v(j) = lambda(j) * B * v(j) .
!  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
!  of (A,B) satisfies
!                   u(j)**H * A  = lambda(j) * u(j)**H * B.
!  where u(j)**H is the conjugate-transpose of u(j).
!
!
!  Arguments
!  =========
!
!  BALANC  (input) CHARACTER*1
!          Specifies the balance option to be performed:
!          = 'N':  do not diagonally scale or permute;
!          = 'P':  permute only;
!          = 'S':  scale only;
!          = 'B':  both permute and scale.
!          Computed reciprocal condition numbers will be for the
!          matrices after permuting and/or balancing. Permuting does
!          not change condition numbers (in exact arithmetic), but
!          balancing does.
!
!  JOBVL   (input) CHARACTER*1
!          = 'N':  do not compute the left generalized eigenvectors;
!          = 'V':  compute the left generalized eigenvectors.
!
!  JOBVR   (input) CHARACTER*1
!          = 'N':  do not compute the right generalized eigenvectors;
!          = 'V':  compute the right generalized eigenvectors.
!
!  SENSE   (input) CHARACTER*1
!          Determines which reciprocal condition numbers are computed.
!          = 'N': none are computed;
!          = 'E': computed for eigenvalues only;
!          = 'V': computed for eigenvectors only;
!          = 'B': computed for eigenvalues and eigenvectors.
!
!  N       (input) INTEGER
!          The order of the matrices A, B, VL, and VR.  N >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA, N)
!          On entry, the matrix A in the pair (A,B).
!          On exit, A has been overwritten. If JOBVL='V' or JOBVR='V'
!          or both, then A contains the first part of the complex Schur
!          form of the "balanced" versions of the input A and B.
!
!  LDA     (input) INTEGER
!          The leading dimension of A.  LDA >= max(1,N).
!
!  B       (input/output) COMPLEX*16 array, dimension (LDB, N)
!          On entry, the matrix B in the pair (A,B).
!          On exit, B has been overwritten. If JOBVL='V' or JOBVR='V'
!          or both, then B contains the second part of the complex
!          Schur form of the "balanced" versions of the input A and B.
!
!  LDB     (input) INTEGER
!          The leading dimension of B.  LDB >= max(1,N).
!
!  ALPHA   (output) COMPLEX*16 array, dimension (N)
!  BETA    (output) COMPLEX*16 array, dimension (N)
!          On exit, ALPHA(j)/BETA(j), j=1,...,N, will be the generalized
!          eigenvalues.
!
!          Note: the quotient ALPHA(j)/BETA(j) ) may easily over- or
!          underflow, and BETA(j) may even be zero.  Thus, the user
!          should avoid naively computing the ratio ALPHA/BETA.
!          However, ALPHA will be always less than and usually
!          comparable with norm(A) in magnitude, and BETA always less
!          than and usually comparable with norm(B).
!
!  VL      (output) COMPLEX*16 array, dimension (LDVL,N)
!          If JOBVL = 'V', the left generalized eigenvectors u(j) are
!          stored one after another in the columns of VL, in the same
!          order as their eigenvalues.
!          Each eigenvector will be scaled so the largest component
!          will have abs(real part) + abs(imag. part) = 1.
!          Not referenced if JOBVL = 'N'.
!
!  LDVL    (input) INTEGER
!          The leading dimension of the matrix VL. LDVL >= 1, and
!          if JOBVL = 'V', LDVL >= N.
!
!  VR      (output) COMPLEX*16 array, dimension (LDVR,N)
!          If JOBVR = 'V', the right generalized eigenvectors v(j) are
!          stored one after another in the columns of VR, in the same
!          order as their eigenvalues.
!          Each eigenvector will be scaled so the largest component
!          will have abs(real part) + abs(imag. part) = 1.
!          Not referenced if JOBVR = 'N'.
!
!  LDVR    (input) INTEGER
!          The leading dimension of the matrix VR. LDVR >= 1, and
!          if JOBVR = 'V', LDVR >= N.
!
!  ILO     (output) INTEGER
!  IHI     (output) INTEGER
!          ILO and IHI are integer values such that on exit
!          A(i,j) = 0 and B(i,j) = 0 if i > j and
!          j = 1,...,ILO-1 or i = IHI+1,...,N.
!          If BALANC = 'N' or 'S', ILO = 1 and IHI = N.
!
!  LSCALE  (output) DOUBLE PRECISION array, dimension (N)
!          Details of the permutations and scaling factors applied
!          to the left side of A and B.  If PL(j) is the index of the
!          row interchanged with row j, and DL(j) is the scaling
!          factor applied to row j, then
!            LSCALE(j) = PL(j)  for j = 1,...,ILO-1
!                      = DL(j)  for j = ILO,...,IHI
!                      = PL(j)  for j = IHI+1,...,N.
!          The order in which the interchanges are made is N to IHI+1,
!          then 1 to ILO-1.
!
!  RSCALE  (output) DOUBLE PRECISION array, dimension (N)
!          Details of the permutations and scaling factors applied
!          to the right side of A and B.  If PR(j) is the index of the
!          column interchanged with column j, and DR(j) is the scaling
!          factor applied to column j, then
!            RSCALE(j) = PR(j)  for j = 1,...,ILO-1
!                      = DR(j)  for j = ILO,...,IHI
!                      = PR(j)  for j = IHI+1,...,N
!          The order in which the interchanges are made is N to IHI+1,
!          then 1 to ILO-1.
!
!  ABNRM   (output) DOUBLE PRECISION
!          The one-norm of the balanced matrix A.
!
!  BBNRM   (output) DOUBLE PRECISION
!          The one-norm of the balanced matrix B.
!
!  RCONDE  (output) DOUBLE PRECISION array, dimension (N)
!          If SENSE = 'E' or 'B', the reciprocal condition numbers of
!          the eigenvalues, stored in consecutive elements of the array.
!          If SENSE = 'N' or 'V', RCONDE is not referenced.
!
!  RCONDV  (output) DOUBLE PRECISION array, dimension (N)
!          If JOB = 'V' or 'B', the estimated reciprocal condition
!          numbers of the eigenvectors, stored in consecutive elements
!          of the array. If the eigenvalues cannot be reordered to
!          compute RCONDV(j), RCONDV(j) is set to 0; this can only occur
!          when the true value would be very small anyway.
!          If SENSE = 'N' or 'E', RCONDV is not referenced.
!
!  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK. LWORK >= max(1,2*N).
!          If SENSE = 'E', LWORK >= max(1,4*N).
!          If SENSE = 'V' or 'B', LWORK >= max(1,2*N*N+2*N).
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  RWORK   (workspace) REAL array, dimension (lrwork)
!          lrwork must be at least max(1,6*N) if BALANC = 'S' or 'B',
!          and at least max(1,2*N) otherwise.
!          Real workspace.
!
!  IWORK   (workspace) INTEGER array, dimension (N+2)
!          If SENSE = 'E', IWORK is not referenced.
!
!  BWORK   (workspace) LOGICAL array, dimension (N)
!          If SENSE = 'N', BWORK is not referenced.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!          = 1,...,N:
!                The QZ iteration failed.  No eigenvectors have been
!                calculated, but ALPHA(j) and BETA(j) should be correct
!                for j=INFO+1,...,N.
!          > N:  =N+1: other than QZ iteration failed in ZHGEQZ.
!                =N+2: error return from ZTGEVC.
!
!  Further Details
!  ===============
!
!  Balancing a matrix pair (A,B) includes, first, permuting rows and
!  columns to isolate eigenvalues, second, applying diagonal similarity
!  transformation to the rows and columns to make the rows and columns
!  as close in norm as possible. The computed reciprocal condition
!  numbers correspond to the balanced matrix. Permuting rows and columns
!  will not change the condition numbers (in exact arithmetic) but
!  diagonal scaling will.  For further explanation of balancing, see
!  section 4.11.1.2 of LAPACK Users' Guide.
!
!  An approximate error bound on the chordal distance between the i-th
!  computed generalized eigenvalue w and the corresponding exact
!  eigenvalue lambda is
!
!       chord(w, lambda) <= EPS * norm(ABNRM, BBNRM) / RCONDE(I)
!
!  An approximate error bound for the angle between the i-th computed
!  eigenvector VL(i) or VR(i) is given by
!
!       EPS * norm(ABNRM, BBNRM) / DIF(i).
!
!  For further explanation of the reciprocal condition numbers RCONDE
!  and RCONDV, see section 4.11 of LAPACK User's Guide.
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     &                   CONE = ( 1.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            ILASCL, ILBSCL, ILV, ILVL, ILVR, LQUERY, NOSCL,
     &                   WANTSB, WANTSE, WANTSN, WANTSV
      CHARACTER          CHTEMP
      INTEGER            I, ICOLS, IERR, IJOBVL, IJOBVR, IN, IROWS,
     &                   ITAU, IWRK, IWRK1, J, JC, JR, M, MAXWRK, MINWRK
      DOUBLE PRECISION   ANRM, ANRMTO, BIGNUM, BNRM, BNRMTO, EPS,
     &                   SMLNUM, TEMP
      COMPLEX*16         X
!     ..
!     .. Local Arrays ..
      LOGICAL            LDUMMA( 1 )
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLABAD, DLASCL, XERBLA, ZGEQRF, ZGGBAK, ZGGBAL,
     &                   ZGGHRD, ZHGEQZ, ZLACPY, ZLASCL, ZLASET, ZTGEVC,
     &                   ZTGSNA, ZUNGQR, ZUNMQR
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, ZLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, ZLANGE
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG, MAX, SQRT
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION   ABS1
!     ..
!     .. Statement Function definitions ..
      ABS1( X ) = ABS( DBLE( X ) ) + ABS( DIMAG( X ) )
!     ..
!     .. Executable Statements ..
!
!     Decode the input arguments
!
      IF( LSAME( JOBVL, 'N' ) ) THEN
         IJOBVL = 1
         ILVL = .FALSE.
      ELSE IF( LSAME( JOBVL, 'V' ) ) THEN
         IJOBVL = 2
         ILVL = .TRUE.
      ELSE
         IJOBVL = -1
         ILVL = .FALSE.
      END IF
!
      IF( LSAME( JOBVR, 'N' ) ) THEN
         IJOBVR = 1
         ILVR = .FALSE.
      ELSE IF( LSAME( JOBVR, 'V' ) ) THEN
         IJOBVR = 2
         ILVR = .TRUE.
      ELSE
         IJOBVR = -1
         ILVR = .FALSE.
      END IF
      ILV = ILVL .OR. ILVR
!
      NOSCL  = LSAME( BALANC, 'N' ) .OR. LSAME( BALANC, 'P' )
      WANTSN = LSAME( SENSE, 'N' )
      WANTSE = LSAME( SENSE, 'E' )
      WANTSV = LSAME( SENSE, 'V' )
      WANTSB = LSAME( SENSE, 'B' )
!
!     Test the input arguments
!
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.( NOSCL .OR. LSAME( BALANC,'S' ) .OR.
     &    LSAME( BALANC, 'B' ) ) ) THEN
         INFO = -1
      ELSE IF( IJOBVL.LE.0 ) THEN
         INFO = -2
      ELSE IF( IJOBVR.LE.0 ) THEN
         INFO = -3
      ELSE IF( .NOT.( WANTSN .OR. WANTSE .OR. WANTSB .OR. WANTSV ) )
     &          THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDVL.LT.1 .OR. ( ILVL .AND. LDVL.LT.N ) ) THEN
         INFO = -13
      ELSE IF( LDVR.LT.1 .OR. ( ILVR .AND. LDVR.LT.N ) ) THEN
         INFO = -15
      END IF
!
!     Compute workspace
!      (Note: Comments in the code beginning "Workspace:" describe the
!       minimal amount of workspace needed at that point in the code,
!       as well as the preferred amount for good performance.
!       NB refers to the optimal block size for the immediately
!       following subroutine, as returned by ILAENV. The workspace is
!       computed assuming ILO = 1 and IHI = N, the worst case.)
!
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            MINWRK = 1
            MAXWRK = 1
         ELSE
            MINWRK = 2*N
            IF( WANTSE ) THEN
               MINWRK = 4*N
            ELSE IF( WANTSV .OR. WANTSB ) THEN
               MINWRK = 2*N*( N + 1)
            END IF
            MAXWRK = MINWRK
            MAXWRK = MAX( MAXWRK,
     &                    N + N*ILAENV( 1, 'ZGEQRF', ' ', N, 1, N, 0 ) )
            MAXWRK = MAX( MAXWRK,
     &                    N + N*ILAENV( 1, 'ZUNMQR', ' ', N, 1, N, 0 ) )
            IF( ILVL ) THEN
               MAXWRK = MAX( MAXWRK, N +
     &                       N*ILAENV( 1, 'ZUNGQR', ' ', N, 1, N, 0 ) )
            END IF
         END IF
         WORK( 1 ) = MAXWRK
!
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -25
         END IF
      END IF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGGEVX', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 )
     &   RETURN
!
!     Get machine constants
!
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SQRT( SMLNUM ) / EPS
      BIGNUM = ONE / SMLNUM
!
!     Scale A if max element outside range [SMLNUM,BIGNUM]
!
      ANRM = ZLANGE( 'M', N, N, A, LDA, RWORK )
      ILASCL = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ANRMTO = SMLNUM
         ILASCL = .TRUE.
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ANRMTO = BIGNUM
         ILASCL = .TRUE.
      END IF
      IF( ILASCL )
     &   CALL ZLASCL( 'G', 0, 0, ANRM, ANRMTO, N, N, A, LDA, IERR )
!
!     Scale B if max element outside range [SMLNUM,BIGNUM]
!
      BNRM = ZLANGE( 'M', N, N, B, LDB, RWORK )
      ILBSCL = .FALSE.
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
         BNRMTO = SMLNUM
         ILBSCL = .TRUE.
      ELSE IF( BNRM.GT.BIGNUM ) THEN
         BNRMTO = BIGNUM
         ILBSCL = .TRUE.
      END IF
      IF( ILBSCL )
     &   CALL ZLASCL( 'G', 0, 0, BNRM, BNRMTO, N, N, B, LDB, IERR )
!
!     Permute and/or balance the matrix pair (A,B)
!     (Real Workspace: need 6*N if BALANC = 'S' or 'B', 1 otherwise)
!
      CALL ZGGBAL( BALANC, N, A, LDA, B, LDB, ILO, IHI, LSCALE, RSCALE,
     &             RWORK, IERR )
!
!     Compute ABNRM and BBNRM
!
      ABNRM = ZLANGE( '1', N, N, A, LDA, RWORK( 1 ) )
      IF( ILASCL ) THEN
         RWORK( 1 ) = ABNRM
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, 1, 1, RWORK( 1 ), 1,
     &                IERR )
         ABNRM = RWORK( 1 )
      END IF
!
      BBNRM = ZLANGE( '1', N, N, B, LDB, RWORK( 1 ) )
      IF( ILBSCL ) THEN
         RWORK( 1 ) = BBNRM
         CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, 1, 1, RWORK( 1 ), 1,
     &                IERR )
         BBNRM = RWORK( 1 )
      END IF
!
!     Reduce B to triangular form (QR decomposition of B)
!     (Complex Workspace: need N, prefer N*NB )
!
      IROWS = IHI + 1 - ILO
      IF( ILV .OR. .NOT.WANTSN ) THEN
         ICOLS = N + 1 - ILO
      ELSE
         ICOLS = IROWS
      END IF
      ITAU = 1
      IWRK = ITAU + IROWS
      CALL ZGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK( ITAU ),
     &             WORK( IWRK ), LWORK+1-IWRK, IERR )
!
!     Apply the unitary transformation to A
!     (Complex Workspace: need N, prefer N*NB)
!
      CALL ZUNMQR( 'L', 'C', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     &             WORK( ITAU ), A( ILO, ILO ), LDA, WORK( IWRK ),
     &             LWORK+1-IWRK, IERR )
!
!     Initialize VL and/or VR
!     (Workspace: need N, prefer N*NB)
!
      IF( ILVL ) THEN
         CALL ZLASET( 'Full', N, N, CZERO, CONE, VL, LDVL )
         IF( IROWS.GT.1 ) THEN
            CALL ZLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     &                   VL( ILO+1, ILO ), LDVL )
         END IF
         CALL ZUNGQR( IROWS, IROWS, IROWS, VL( ILO, ILO ), LDVL,
     &                WORK( ITAU ), WORK( IWRK ), LWORK+1-IWRK, IERR )
      END IF
!
      IF( ILVR )
     &   CALL ZLASET( 'Full', N, N, CZERO, CONE, VR, LDVR )
!
!     Reduce to generalized Hessenberg form
!     (Workspace: none needed)
!
      IF( ILV .OR. .NOT.WANTSN ) THEN
!
!        Eigenvectors requested -- work on whole matrix.
!
         CALL ZGGHRD( JOBVL, JOBVR, N, ILO, IHI, A, LDA, B, LDB, VL,
     &                LDVL, VR, LDVR, IERR )
      ELSE
         CALL ZGGHRD( 'N', 'N', IROWS, 1, IROWS, A( ILO, ILO ), LDA,
     &                B( ILO, ILO ), LDB, VL, LDVL, VR, LDVR, IERR )
      END IF
!
!     Perform QZ algorithm (Compute eigenvalues, and optionally, the
!     Schur forms and Schur vectors)
!     (Complex Workspace: need N)
!     (Real Workspace: need N)
!
      IWRK = ITAU
      IF( ILV .OR. .NOT.WANTSN ) THEN
         CHTEMP = 'S'
      ELSE
         CHTEMP = 'E'
      END IF
!
      CALL ZHGEQZ( CHTEMP, JOBVL, JOBVR, N, ILO, IHI, A, LDA, B, LDB,
     &             ALPHA, BETA, VL, LDVL, VR, LDVR, WORK( IWRK ),
     &             LWORK+1-IWRK, RWORK, IERR )
      IF( IERR.NE.0 ) THEN
         IF( IERR.GT.0 .AND. IERR.LE.N ) THEN
            INFO = IERR
         ELSE IF( IERR.GT.N .AND. IERR.LE.2*N ) THEN
            INFO = IERR - N
         ELSE
            INFO = N + 1
         END IF
         GO TO 90
      END IF
!
!     Compute Eigenvectors and estimate condition numbers if desired
!     ZTGEVC: (Complex Workspace: need 2*N )
!             (Real Workspace:    need 2*N )
!     ZTGSNA: (Complex Workspace: need 2*N*N if SENSE='V' or 'B')
!             (Integer Workspace: need N+2 )
!
      IF( ILV .OR. .NOT.WANTSN ) THEN
         IF( ILV ) THEN
            IF( ILVL ) THEN
               IF( ILVR ) THEN
                  CHTEMP = 'B'
               ELSE
                  CHTEMP = 'L'
               END IF
            ELSE
               CHTEMP = 'R'
            END IF
!
            CALL ZTGEVC( CHTEMP, 'B', LDUMMA, N, A, LDA, B, LDB, VL,
     &                   LDVL, VR, LDVR, N, IN, WORK( IWRK ), RWORK,
     &                   IERR )
            IF( IERR.NE.0 ) THEN
               INFO = N + 2
               GO TO 90
            END IF
         END IF
!
         IF( .NOT.WANTSN ) THEN
!
!           compute eigenvectors (DTGEVC) and estimate condition
!           numbers (DTGSNA). Note that the definition of the condition
!           number is not invariant under transformation (u,v) to
!           (Q*u, Z*v), where (u,v) are eigenvectors of the generalized
!           Schur form (S,T), Q and Z are orthogonal matrices. In order
!           to avoid using extra 2*N*N workspace, we have to
!           re-calculate eigenvectors and estimate the condition numbers
!           one at a time.
!
            DO 20 I = 1, N
!
               DO 10 J = 1, N
                  BWORK( J ) = .FALSE.
   10          CONTINUE
               BWORK( I ) = .TRUE.
!
               IWRK = N + 1
               IWRK1 = IWRK + N
!
               IF( WANTSE .OR. WANTSB ) THEN
                  CALL ZTGEVC( 'B', 'S', BWORK, N, A, LDA, B, LDB,
     &                         WORK( 1 ), N, WORK( IWRK ), N, 1, M,
     &                         WORK( IWRK1 ), RWORK, IERR )
                  IF( IERR.NE.0 ) THEN
                     INFO = N + 2
                     GO TO 90
                  END IF
               END IF
!
               CALL ZTGSNA( SENSE, 'S', BWORK, N, A, LDA, B, LDB,
     &                      WORK( 1 ), N, WORK( IWRK ), N, RCONDE( I ),
     &                      RCONDV( I ), 1, M, WORK( IWRK1 ),
     &                      LWORK-IWRK1+1, IWORK, IERR )
!
   20       CONTINUE
         END IF
      END IF
!
!     Undo balancing on VL and VR and normalization
!     (Workspace: none needed)
!
      IF( ILVL ) THEN
         CALL ZGGBAK( BALANC, 'L', N, ILO, IHI, LSCALE, RSCALE, N, VL,
     &                LDVL, IERR )
!
         DO 50 JC = 1, N
            TEMP = ZERO
            DO 30 JR = 1, N
               TEMP = MAX( TEMP, ABS1( VL( JR, JC ) ) )
   30       CONTINUE
            IF( TEMP.LT.SMLNUM )
     &         GO TO 50
            TEMP = ONE / TEMP
            DO 40 JR = 1, N
               VL( JR, JC ) = VL( JR, JC )*TEMP
   40       CONTINUE
   50    CONTINUE
      END IF
!
      IF( ILVR ) THEN
         CALL ZGGBAK( BALANC, 'R', N, ILO, IHI, LSCALE, RSCALE, N, VR,
     &                LDVR, IERR )
         DO 80 JC = 1, N
            TEMP = ZERO
            DO 60 JR = 1, N
               TEMP = MAX( TEMP, ABS1( VR( JR, JC ) ) )
   60       CONTINUE
            IF( TEMP.LT.SMLNUM )
     &         GO TO 80
            TEMP = ONE / TEMP
            DO 70 JR = 1, N
               VR( JR, JC ) = VR( JR, JC )*TEMP
   70       CONTINUE
   80    CONTINUE
      END IF
!
!     Undo scaling if necessary
!
      IF( ILASCL )
     &   CALL ZLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHA, N, IERR )
!
      IF( ILBSCL )
     &   CALL ZLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR )
!
   90 CONTINUE
      WORK( 1 ) = MAXWRK
!
      RETURN
!
!     End of ZGGEVX
!
      END
!
