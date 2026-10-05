!     $Id$
!     ==================================================================
!
!>    @file lapack.f
!>    @brief LAPACK FORTRAN routines, last changes:<br>
!>    new global module lapack - francisco - 03/12/2008<br>
!>    updated ZGEQR2 new LAPACK 3_5 version - francisco - 16/04/2015<br>
!>    removed ZLARFP not needed anymore - francisco - 16/04/2015<br>
!>    moved BLAS routines to blas.f - francisco - 16/04/2015<br>
!>    Please remove also zgetri, dgetri if use optimized LAPAKC routines<br>
!>    added includes to perform legacy compilation - francisco - jul-15<br>
!>    removed some unused routines - francisco - may-20
!
!     ==================================================================
!
!
! =====================================================================
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
! =====================================================================
!
!     gfortran include
      include 'dlamch.inc'
!     g77 legacy include
!      include 'dlamch7.inc'
!
! =====================================================================
!
!> @brief  IEEECK
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!
!  Definition:
!  ===========
!
!       INTEGER          FUNCTION IEEECK( ISPEC, ZERO, ONE )
!
!       .. Scalar Arguments ..
!       INTEGER            ISPEC
!       REAL               ONE, ZERO
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> IEEECK is called from the ILAENV to verify that Infinity and
!> possibly NaN arithmetic is safe (i.e. will not trap).
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] ISPEC
!> \verbatim
!>          ISPEC is INTEGER
!>          Specifies whether to test just for inifinity arithmetic
!>          or whether to test for infinity and NaN arithmetic.
!>          = 0: Verify infinity arithmetic only.
!>          = 1: Verify infinity and NaN arithmetic.
!> \endverbatim
!>
!> @param[in] ZERO
!> \verbatim
!>          ZERO is REAL
!>          Must contain the value 0.0
!>          This is passed to prevent the compiler from optimizing
!>          away this code.
!> \endverbatim
!>
!> @param[in] ONE
!> \verbatim
!>          ONE is REAL
!>          Must contain the value 1.0
!>          This is passed to prevent the compiler from optimizing
!>          away this code.
!>
!>  RETURN VALUE:  INTEGER
!>          = 0:  Arithmetic failed to produce the correct answers
!>          = 1:  Arithmetic produced the correct answers
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
!  =====================================================================
      INTEGER          FUNCTION IEEECK( ISPEC, ZERO, ONE )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            ISPEC
      REAL               ONE, ZERO
!     ..
!
!  Purpose
!  =======
!
!  IEEECK is called from the ILAENV to verify that Infinity and
!  possibly NaN arithmetic is safe (i.e. will not trap).
!
!  Arguments
!  =========
!
!  ISPEC   (input) INTEGER
!          Specifies whether to test just for inifinity arithmetic
!          or whether to test for infinity and NaN arithmetic.
!          = 0: Verify infinity arithmetic only.
!          = 1: Verify infinity and NaN arithmetic.
!
!  ZERO    (input) REAL
!          Must contain the value 0.0
!          This is passed to prevent the compiler from optimizing
!          away this code.
!
!  ONE     (input) REAL
!          Must contain the value 1.0
!          This is passed to prevent the compiler from optimizing
!          away this code.
!
!  RETURN VALUE:  INTEGER
!          = 0:  Arithmetic failed to produce the correct answers
!          = 1:  Arithmetic produced the correct answers
!
!     .. Local Scalars ..
      REAL               NAN1, NAN2, NAN3, NAN4, NAN5, NAN6, NEGINF,
     $                   NEGZRO, NEWZRO, POSINF
!     ..
!     .. Executable Statements ..
      IEEECK = 1
!
      POSINF = ONE / ZERO
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      NEGINF = -ONE / ZERO
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      NEGZRO = ONE / ( NEGINF+ONE )
      IF( NEGZRO.NE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      NEGINF = ONE / NEGZRO
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      NEWZRO = NEGZRO + ZERO
      IF( NEWZRO.NE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      POSINF = ONE / NEWZRO
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      NEGINF = NEGINF*POSINF
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      POSINF = POSINF*POSINF
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
!
!
!
!
!     Return if we were only asked to check infinity arithmetic
!
      IF( ISPEC.EQ.0 )
     $   RETURN
!
      NAN1 = POSINF + NEGINF
!
      NAN2 = POSINF / NEGINF
!
      NAN3 = POSINF / POSINF
!
      NAN4 = POSINF*ZERO
!
      NAN5 = NEGINF*NEGZRO
!
      NAN6 = NAN5*0.0
!
      IF( NAN1.EQ.NAN1 ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      IF( NAN2.EQ.NAN2 ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      IF( NAN3.EQ.NAN3 ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      IF( NAN4.EQ.NAN4 ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      IF( NAN5.EQ.NAN5 ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      IF( NAN6.EQ.NAN6 ) THEN
         IEEECK = 0
         RETURN
      END IF
!
      RETURN
!
!     End of IEEECK
!
      END
!
! =====================================================================
!
!> @brief  ILAENV
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       INTEGER FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
!
!       .. Scalar Arguments ..
!       CHARACTER*( * )    NAME, OPTS
!       INTEGER            ISPEC, N1, N2, N3, N4
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ILAENV is called from the LAPACK routines to choose problem-dependent
!> parameters for the local environment.  See ISPEC for a description of
!> the parameters.
!>
!> ILAENV returns an INTEGER
!> if ILAENV >= 0: ILAENV returns the value of the parameter specified by ISPEC
!> if ILAENV < 0:  if ILAENV = -k, the k-th argument had an illegal value.
!>
!> This version provides a set of parameters which should give good,
!> but not optimal, performance on many of the currently available
!> computers.  Users are encouraged to modify this subroutine to set
!> the tuning parameters for their particular machine using the option
!> and problem size information in the arguments.
!>
!> This routine will not function correctly if it is converted to all
!> lower case.  Converting it to all upper case is allowed.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] ISPEC
!> \verbatim
!>          ISPEC is INTEGER
!>          Specifies the parameter to be returned as the value of
!>          ILAENV.
!>          = 1: the optimal blocksize; if this value is 1, an unblocked
!>               algorithm will give the best performance.
!>          = 2: the minimum block size for which the block routine
!>               should be used; if the usable block size is less than
!>               this value, an unblocked routine should be used.
!>          = 3: the crossover point (in a block routine, for N less
!>               than this value, an unblocked routine should be used)
!>          = 4: the number of shifts, used in the nonsymmetric
!>               eigenvalue routines (DEPRECATED)
!>          = 5: the minimum column dimension for blocking to be used;
!>               rectangular blocks must have dimension at least k by m,
!>               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
!>          = 6: the crossover point for the SVD (when reducing an m by n
!>               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
!>               this value, a QR factorization is used first to reduce
!>               the matrix to a triangular form.)
!>          = 7: the number of processors
!>          = 8: the crossover point for the multishift QR method
!>               for nonsymmetric eigenvalue problems (DEPRECATED)
!>          = 9: maximum size of the subproblems at the bottom of the
!>               computation tree in the divide-and-conquer algorithm
!>               (used by xGELSD and xGESDD)
!>          =10: ieee NaN arithmetic can be trusted not to trap
!>          =11: infinity arithmetic can be trusted not to trap
!>          12 <= ISPEC <= 16:
!>               xHSEQR or one of its subroutines,
!>               see IPARMQ for detailed explanation
!> \endverbatim
!>
!> @param[in] NAME
!> \verbatim
!>          NAME is CHARACTER*(*)
!>          The name of the calling subroutine, in either upper case or
!>          lower case.
!> \endverbatim
!>
!> @param[in] OPTS
!> \verbatim
!>          OPTS is CHARACTER*(*)
!>          The character options to the subroutine NAME, concatenated
!>          into a single character string.  For example, UPLO = 'U',
!>          TRANS = 'T', and DIAG = 'N' for a triangular routine would
!>          be specified as OPTS = 'UTN'.
!> \endverbatim
!>
!> @param[in] N1
!> \verbatim
!>          N1 is INTEGER
!> \endverbatim
!>
!> @param[in] N2
!> \verbatim
!>          N2 is INTEGER
!> \endverbatim
!>
!> @param[in] N3
!> \verbatim
!>          N3 is INTEGER
!> \endverbatim
!>
!> @param[in] N4
!> \verbatim
!>          N4 is INTEGER
!>          Problem dimensions for the subroutine NAME; these may not all
!>          be required.
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
!>  The following conventions have been used when calling ILAENV from the
!>  LAPACK routines:
!>  1)  OPTS is a concatenation of all of the character options to
!>      subroutine NAME, in the same order that they appear in the
!>      argument list for NAME, even if they are not used in determining
!>      the value of the parameter specified by ISPEC.
!>  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
!>      that they appear in the argument list for NAME.  N1 is used
!>      first, N2 second, and so on, and unused problem dimensions are
!>      passed a value of -1.
!>  3)  The parameter value returned by ILAENV is checked for validity in
!>      the calling subroutine.  For example, ILAENV is used to retrieve
!>      the optimal blocksize for STRTRI as follows:
!>
!>      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
!>      IF( NB.LE.1 ) NB = MAX( 1, N )
!> \endverbatim
!>
!  =====================================================================
      INTEGER FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
!
!  -- LAPACK auxiliary routine (version 3.2.1)                        --
!
!  -- April 2009                                                      --
!
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!
!     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
!     ..
!
!  Purpose
!  =======
!
!  ILAENV is called from the LAPACK routines to choose problem-dependent
!  parameters for the local environment.  See ISPEC for a description of
!  the parameters.
!
!  ILAENV returns an INTEGER
!  if ILAENV >= 0: ILAENV returns the value of the parameter specified by ISPEC
!  if ILAENV < 0:  if ILAENV = -k, the k-th argument had an illegal value.
!
!  This version provides a set of parameters which should give good,
!  but not optimal, performance on many of the currently available
!  computers.  Users are encouraged to modify this subroutine to set
!  the tuning parameters for their particular machine using the option
!  and problem size information in the arguments.
!
!  This routine will not function correctly if it is converted to all
!  lower case.  Converting it to all upper case is allowed.
!
!  Arguments
!  =========
!
!  ISPEC   (input) INTEGER
!          Specifies the parameter to be returned as the value of
!          ILAENV.
!          = 1: the optimal blocksize; if this value is 1, an unblocked
!               algorithm will give the best performance.
!          = 2: the minimum block size for which the block routine
!               should be used; if the usable block size is less than
!               this value, an unblocked routine should be used.
!          = 3: the crossover point (in a block routine, for N less
!               than this value, an unblocked routine should be used)
!          = 4: the number of shifts, used in the nonsymmetric
!               eigenvalue routines (DEPRECATED)
!          = 5: the minimum column dimension for blocking to be used;
!               rectangular blocks must have dimension at least k by m,
!               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
!          = 6: the crossover point for the SVD (when reducing an m by n
!               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
!               this value, a QR factorization is used first to reduce
!               the matrix to a triangular form.)
!          = 7: the number of processors
!          = 8: the crossover point for the multishift QR method
!               for nonsymmetric eigenvalue problems (DEPRECATED)
!          = 9: maximum size of the subproblems at the bottom of the
!               computation tree in the divide-and-conquer algorithm
!               (used by xGELSD and xGESDD)
!          =10: ieee NaN arithmetic can be trusted not to trap
!          =11: infinity arithmetic can be trusted not to trap
!          12 <= ISPEC <= 16:
!               xHSEQR or one of its subroutines,
!               see IPARMQ for detailed explanation
!
!  NAME    (input) CHARACTER*(*)
!          The name of the calling subroutine, in either upper case or
!          lower case.
!
!  OPTS    (input) CHARACTER*(*)
!          The character options to the subroutine NAME, concatenated
!          into a single character string.  For example, UPLO = 'U',
!          TRANS = 'T', and DIAG = 'N' for a triangular routine would
!          be specified as OPTS = 'UTN'.
!
!  N1      (input) INTEGER
!  N2      (input) INTEGER
!  N3      (input) INTEGER
!  N4      (input) INTEGER
!          Problem dimensions for the subroutine NAME; these may not all
!          be required.
!
!  Further Details
!  ===============
!
!  The following conventions have been used when calling ILAENV from the
!  LAPACK routines:
!  1)  OPTS is a concatenation of all of the character options to
!      subroutine NAME, in the same order that they appear in the
!      argument list for NAME, even if they are not used in determining
!      the value of the parameter specified by ISPEC.
!  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
!      that they appear in the argument list for NAME.  N1 is used
!      first, N2 second, and so on, and unused problem dimensions are
!      passed a value of -1.
!  3)  The parameter value returned by ILAENV is checked for validity in
!      the calling subroutine.  For example, ILAENV is used to retrieve
!      the optimal blocksize for STRTRI as follows:
!
!      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
!      IF( NB.LE.1 ) NB = MAX( 1, N )
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, IC, IZ, NB, NBMIN, NX
      LOGICAL            CNAME, SNAME
      CHARACTER          C1*1, C2*2, C4*2, C3*3, SUBNAM*6
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
!     ..
!     .. External Functions ..
      INTEGER            IEEECK, IPARMQ
      EXTERNAL           IEEECK, IPARMQ
!     ..
!     .. Executable Statements ..
!
      GO TO ( 10, 10, 10, 80, 90, 100, 110, 120,
     $        130, 140, 150, 160, 160, 160, 160, 160 )ISPEC
!
!     Invalid value for ISPEC
!
      ILAENV = -1
      RETURN
!
   10 CONTINUE
!
!     Convert NAME to upper case if the first character is lower case.
!
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1: 1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
!
!        ASCII character set
!
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC-32 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )
     $            SUBNAM( I: I ) = CHAR( IC-32 )
   20       CONTINUE
         END IF
!
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
!
!        EBCDIC character set
!
         IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $       ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $       ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC+64 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $             ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $             ( IC.GE.162 .AND. IC.LE.169 ) )SUBNAM( I:
     $             I ) = CHAR( IC+64 )
   30       CONTINUE
         END IF
!
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
!
!        Prime machines:  ASCII+128
!
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC-32 )
            DO 40 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )
     $            SUBNAM( I: I ) = CHAR( IC-32 )
   40       CONTINUE
         END IF
      END IF
!
      C1 = SUBNAM( 1: 1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )
     $   RETURN
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      C4 = C3( 2: 3 )
!
      GO TO ( 50, 60, 70 )ISPEC
!
   50 CONTINUE
!
!     ISPEC = 1:  block size
!
!     In these examples, separate code is provided for setting NB for
!     real and complex.  We assume that NB will take the same value in
!     single or double precision.
!
      NB = 1
!
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $            C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'PO' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NB = 32
         ELSE IF( SNAME .AND. C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            NB = 64
         ELSE IF( C3.EQ.'TRD' ) THEN
            NB = 32
         ELSE IF( C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         END IF
      ELSE IF( C2.EQ.'GB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'PB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'TR' ) THEN
         IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'LA' ) THEN
         IF( C3.EQ.'UUM' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN
         IF( C3.EQ.'EBZ' ) THEN
            NB = 1
         END IF
      END IF
      ILAENV = NB
      RETURN
!
   60 CONTINUE
!
!     ISPEC = 2:  minimum block size
!
      NBMIN = 2
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR. C3.EQ.
     $       'QLF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 8
            ELSE
               NBMIN = 8
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         END IF
      END IF
      ILAENV = NBMIN
      RETURN
!
   70 CONTINUE
!
!     ISPEC = 3:  crossover point
!
      NX = 0
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR. C3.EQ.
     $       'QLF' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NX = 32
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NX = 32
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NX = 128
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NX = 128
            END IF
         END IF
      END IF
      ILAENV = NX
      RETURN
!
   80 CONTINUE
!
!     ISPEC = 4:  number of shifts (used by xHSEQR)
!
      ILAENV = 6
      RETURN
!
   90 CONTINUE
!
!     ISPEC = 5:  minimum column dimension (not used)
!
      ILAENV = 2
      RETURN
!
  100 CONTINUE
!
!     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
!
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      RETURN
!
  110 CONTINUE
!
!     ISPEC = 7:  number of processors (not used)
!
      ILAENV = 1
      RETURN
!
  120 CONTINUE
!
!     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
!
      ILAENV = 50
      RETURN
!
  130 CONTINUE
!
!     ISPEC = 9:  maximum size of the subproblems at the bottom of the
!                 computation tree in the divide-and-conquer algorithm
!                 (used by xGELSD and xGESDD)
!
      ILAENV = 25
      RETURN
!
  140 CONTINUE
!
!     ISPEC = 10: ieee NaN arithmetic can be trusted not to trap
!
!     ILAENV = 0
      ILAENV = 1
      IF( ILAENV.EQ.1 ) THEN
         ILAENV = IEEECK( 1, 0.0, 1.0 )
      END IF
      RETURN
!
  150 CONTINUE
!
!     ISPEC = 11: infinity arithmetic can be trusted not to trap
!
!     ILAENV = 0
      ILAENV = 1
      IF( ILAENV.EQ.1 ) THEN
         ILAENV = IEEECK( 0, 0.0, 1.0 )
      END IF
      RETURN
!
  160 CONTINUE
!
!     12 <= ISPEC <= 16: xHSEQR or one of its subroutines.
!
      ILAENV = IPARMQ( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
      RETURN
!
!     End of ILAENV
!
      END
!
!  =====================================================================
!
!> @brief  ZLACPY copies all or part of one two-dimensional array to another.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLACPY( UPLO, M, N, A, LDA, B, LDB )
!
!       .. Scalar Arguments ..
!       CHARACTER          UPLO
!       INTEGER            LDA, LDB, M, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), B( LDB, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLACPY copies all or part of a two-dimensional matrix A to another
!> matrix B.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>          Specifies the part of the matrix A to be copied to B.
!>          = 'U':      Upper triangular part
!>          = 'L':      Lower triangular part
!>          Otherwise:  All of the matrix A
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
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          The m by n matrix A.  If UPLO = 'U', only the upper trapezium
!>          is accessed; if UPLO = 'L', only the lower trapezium is
!>          accessed.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> @param[out] B
!> \verbatim
!>          B is COMPLEX*16 array, dimension (LDB,N)
!>          On exit, B = A in the locations specified by UPLO.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B.  LDB >= max(1,M).
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
!  =====================================================================
      SUBROUTINE ZLACPY( UPLO, M, N, A, LDA, B, LDB )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDB, M, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLACPY copies all or part of a two-dimensional matrix A to another
!  matrix B.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies the part of the matrix A to be copied to B.
!          = 'U':      Upper triangular part
!          = 'L':      Lower triangular part
!          Otherwise:  All of the matrix A
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The m by n matrix A.  If UPLO = 'U', only the upper trapezium
!          is accessed; if UPLO = 'L', only the lower trapezium is
!          accessed.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  B       (output) COMPLEX*16 array, dimension (LDB,N)
!          On exit, B = A in the locations specified by UPLO.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,M).
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
         DO 20 J = 1, N
            DO 10 I = 1, MIN( J, M )
               B( I, J ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
!
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
         DO 40 J = 1, N
            DO 30 I = J, M
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
!
      ELSE
         DO 60 J = 1, N
            DO 50 I = 1, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
!
      RETURN
!
!     End of ZLACPY
!
      END
!
!  =====================================================================
!
!> @brief  ZLASWP performs a series of row interchanges on a general rectangular matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLASWP( N, A, LDA, K1, K2, IPIV, INCX )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, K1, K2, LDA, N
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * )
!       COMPLEX*16         A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLASWP performs a series of row interchanges on the matrix A.
!> One row interchange is initiated for each of rows K1 through K2 of A.
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
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the matrix of column dimension N to which the row
!>          interchanges will be applied.
!>          On exit, the permuted matrix.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.
!> \endverbatim
!>
!> @param[in] K1
!> \verbatim
!>          K1 is INTEGER
!>          The first element of IPIV for which a row interchange will
!>          be done.
!> \endverbatim
!>
!> @param[in] K2
!> \verbatim
!>          K2 is INTEGER
!>          The last element of IPIV for which a row interchange will
!>          be done.
!> \endverbatim
!>
!> @param[in] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (K2*abs(INCX))
!>          The vector of pivot indices.  Only the elements in positions
!>          K1 through K2 of IPIV are accessed.
!>          IPIV(K) = L implies rows K and L are to be interchanged.
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between successive values of IPIV.  If IPIV
!>          is negative, the pivots are applied in reverse order.
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
!>  Modified by
!>   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZLASWP( N, A, LDA, K1, K2, IPIV, INCX )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, K1, K2, LDA, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLASWP performs a series of row interchanges on the matrix A.
!  One row interchange is initiated for each of rows K1 through K2 of A.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the matrix of column dimension N to which the row
!          interchanges will be applied.
!          On exit, the permuted matrix.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!
!  K1      (input) INTEGER
!          The first element of IPIV for which a row interchange will
!          be done.
!
!  K2      (input) INTEGER
!          The last element of IPIV for which a row interchange will
!          be done.
!
!  IPIV    (input) INTEGER array, dimension (K2*abs(INCX))
!          The vector of pivot indices.  Only the elements in positions
!          K1 through K2 of IPIV are accessed.
!          IPIV(K) = L implies rows K and L are to be interchanged.
!
!  INCX    (input) INTEGER
!          The increment between successive values of IPIV.  If IPIV
!          is negative, the pivots are applied in reverse order.
!
!  Further Details
!  ===============
!
!  Modified by
!   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA
!
! =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, I1, I2, INC, IP, IX, IX0, J, K, N32
      COMPLEX*16         TEMP
!     ..
!     .. Executable Statements ..
!
!     Interchange row I with row IPIV(I) for each of rows K1 through K2.
!
      IF( INCX.GT.0 ) THEN
         IX0 = K1
         I1 = K1
         I2 = K2
         INC = 1
      ELSE IF( INCX.LT.0 ) THEN
         IX0 = 1 + ( 1-K2 )*INCX
         I1 = K2
         I2 = K1
         INC = -1
      ELSE
         RETURN
      END IF
!
      N32 = ( N / 32 )*32
      IF( N32.NE.0 ) THEN
         DO 30 J = 1, N32, 32
            IX = IX0
            DO 20 I = I1, I2, INC
               IP = IPIV( IX )
               IF( IP.NE.I ) THEN
                  DO 10 K = J, J + 31
                     TEMP = A( I, K )
                     A( I, K ) = A( IP, K )
                     A( IP, K ) = TEMP
   10             CONTINUE
               END IF
               IX = IX + INCX
   20       CONTINUE
   30    CONTINUE
      END IF
      IF( N32.NE.N ) THEN
         N32 = N32 + 1
         IX = IX0
         DO 50 I = I1, I2, INC
            IP = IPIV( IX )
            IF( IP.NE.I ) THEN
               DO 40 K = N32, N
                  TEMP = A( I, K )
                  A( I, K ) = A( IP, K )
                  A( IP, K ) = TEMP
   40          CONTINUE
            END IF
            IX = IX + INCX
   50    CONTINUE
      END IF
!
      RETURN
!
!     End of ZLASWP
!
      END
!
!  =====================================================================
!
!> @brief  ZLARTG generates a plane rotation with real cosine and complex sine.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLARTG( F, G, CS, SN, R )
!
!       .. Scalar Arguments ..
!       DOUBLE PRECISION   CS
!       COMPLEX*16         F, G, R, SN
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLARTG generates a plane rotation so that
!>
!>    [  CS  SN  ]     [ F ]     [ R ]
!>    [  __      ]  .  [   ]  =  [   ]   where CS**2 + |SN|**2 = 1.
!>    [ -SN  CS  ]     [ G ]     [ 0 ]
!>
!> This is a faster version of the BLAS1 routine ZROTG, except for
!> the following differences:
!>    F and G are unchanged on return.
!>    If G=0, then CS=1 and SN=0.
!>    If F=0, then CS=0 and SN is chosen so that R is real.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] F
!> \verbatim
!>          F is COMPLEX*16
!>          The first component of vector to be rotated.
!> \endverbatim
!>
!> @param[in] G
!> \verbatim
!>          G is COMPLEX*16
!>          The second component of vector to be rotated.
!> \endverbatim
!>
!> @param[out] CS
!> \verbatim
!>          CS is DOUBLE PRECISION
!>          The cosine of the rotation.
!> \endverbatim
!>
!> @param[out] SN
!> \verbatim
!>          SN is COMPLEX*16
!>          The sine of the rotation.
!> \endverbatim
!>
!> @param[out] R
!> \verbatim
!>          R is COMPLEX*16
!>          The nonzero component of the rotated vector.
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
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>  3-5-96 - Modified with a new algorithm by W. Kahan and J. Demmel
!>
!>  This version has a few statements commented out for thread safety
!>  (machine parameters are computed on each entry). 10 feb 03, SJH.
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZLARTG( F, G, CS, SN, R )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   CS
      COMPLEX*16         F, G, R, SN
!     ..
!
!  Purpose
!  =======
!
!  ZLARTG generates a plane rotation so that
!
!     [  CS  SN  ]     [ F ]     [ R ]
!     [  __      ]  .  [   ]  =  [   ]   where CS**2 + |SN|**2 = 1.
!     [ -SN  CS  ]     [ G ]     [ 0 ]
!
!  This is a faster version of the BLAS1 routine ZROTG, except for
!  the following differences:
!     F and G are unchanged on return.
!     If G=0, then CS=1 and SN=0.
!     If F=0, then CS=0 and SN is chosen so that R is real.
!
!  Arguments
!  =========
!
!  F       (input) COMPLEX*16
!          The first component of vector to be rotated.
!
!  G       (input) COMPLEX*16
!          The second component of vector to be rotated.
!
!  CS      (output) DOUBLE PRECISION
!          The cosine of the rotation.
!
!  SN      (output) COMPLEX*16
!          The sine of the rotation.
!
!  R       (output) COMPLEX*16
!          The nonzero component of the rotated vector.
!
!  Further Details
!  ======= =======
!
!  3-5-96 - Modified with a new algorithm by W. Kahan and J. Demmel
!
!  This version has a few statements commented out for thread safety
!  (machine parameters are computed on each entry). 10 feb 03, SJH.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   TWO, ONE, ZERO
      PARAMETER          ( TWO = 2.0D+0, ONE = 1.0D+0, ZERO = 0.0D+0 )
      COMPLEX*16         CZERO
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
!     LOGICAL            FIRST
      INTEGER            COUNT, I
      DOUBLE PRECISION   D, DI, DR, EPS, F2, F2S, G2, G2S, SAFMIN,
     $                   SAFMN2, SAFMX2, SCALE
      COMPLEX*16         FF, FS, GS
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, INT, LOG,
     $                   MAX, SQRT
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION   ABS1, ABSSQ
!     ..
!     .. Save statement ..
!     SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2
!     ..
!     .. Data statements ..
!     DATA               FIRST / .TRUE. /
!     ..
!     .. Statement Function definitions ..
      ABS1( FF ) = MAX( ABS( DBLE( FF ) ), ABS( DIMAG( FF ) ) )
      ABSSQ( FF ) = DBLE( FF )**2 + DIMAG( FF )**2
!     ..
!     .. Executable Statements ..
!
!     IF( FIRST ) THEN
         SAFMIN = DLAMCH( 'S' )
         EPS = DLAMCH( 'E' )
         SAFMN2 = DLAMCH( 'B' )**INT( LOG( SAFMIN / EPS ) /
     $            LOG( DLAMCH( 'B' ) ) / TWO )
         SAFMX2 = ONE / SAFMN2
!        FIRST = .FALSE.
!     END IF
      SCALE = MAX( ABS1( F ), ABS1( G ) )
      FS = F
      GS = G
      COUNT = 0
      IF( SCALE.GE.SAFMX2 ) THEN
   10    CONTINUE
         COUNT = COUNT + 1
         FS = FS*SAFMN2
         GS = GS*SAFMN2
         SCALE = SCALE*SAFMN2
         IF( SCALE.GE.SAFMX2 )
     $      GO TO 10
      ELSE IF( SCALE.LE.SAFMN2 ) THEN
         IF( G.EQ.CZERO ) THEN
            CS = ONE
            SN = CZERO
            R = F
            RETURN
         END IF
   20    CONTINUE
         COUNT = COUNT - 1
         FS = FS*SAFMX2
         GS = GS*SAFMX2
         SCALE = SCALE*SAFMX2
         IF( SCALE.LE.SAFMN2 )
     $      GO TO 20
      END IF
      F2 = ABSSQ( FS )
      G2 = ABSSQ( GS )
      IF( F2.LE.MAX( G2, ONE )*SAFMIN ) THEN
!
!        This is a rare case: F is very small.
!
         IF( F.EQ.CZERO ) THEN
            CS = ZERO
            R = DLAPY2( DBLE( G ), DIMAG( G ) )
!           Do complex/real division explicitly with two real divisions
            D = DLAPY2( DBLE( GS ), DIMAG( GS ) )
            SN = DCMPLX( DBLE( GS ) / D, -DIMAG( GS ) / D )
            RETURN
         END IF
         F2S = DLAPY2( DBLE( FS ), DIMAG( FS ) )
!        G2 and G2S are accurate
!        G2 is at least SAFMIN, and G2S is at least SAFMN2
         G2S = SQRT( G2 )
!        Error in CS from underflow in F2S is at most
!        UNFL / SAFMN2 .lt. sqrt(UNFL*EPS) .lt. EPS
!        If MAX(G2,ONE)=G2, then F2 .lt. G2*SAFMIN,
!        and so CS .lt. sqrt(SAFMIN)
!        If MAX(G2,ONE)=ONE, then F2 .lt. SAFMIN
!        and so CS .lt. sqrt(SAFMIN)/SAFMN2 = sqrt(EPS)
!        Therefore, CS = F2S/G2S / sqrt( 1 + (F2S/G2S)**2 ) = F2S/G2S
         CS = F2S / G2S
!        Make sure abs(FF) = 1
!        Do complex/real division explicitly with 2 real divisions
         IF( ABS1( F ).GT.ONE ) THEN
            D = DLAPY2( DBLE( F ), DIMAG( F ) )
            FF = DCMPLX( DBLE( F ) / D, DIMAG( F ) / D )
         ELSE
            DR = SAFMX2*DBLE( F )
            DI = SAFMX2*DIMAG( F )
            D = DLAPY2( DR, DI )
            FF = DCMPLX( DR / D, DI / D )
         END IF
         SN = FF*DCMPLX( DBLE( GS ) / G2S, -DIMAG( GS ) / G2S )
         R = CS*F + SN*G
      ELSE
!
!        This is the most common case.
!        Neither F2 nor F2/G2 are less than SAFMIN
!        F2S cannot overflow, and it is accurate
!
         F2S = SQRT( ONE+G2 / F2 )
!        Do the F2S(real)*FS(complex) multiply with two real multiplies
         R = DCMPLX( F2S*DBLE( FS ), F2S*DIMAG( FS ) )
         CS = ONE / F2S
         D = F2 + G2
!        Do complex/real division explicitly with two real divisions
         SN = DCMPLX( DBLE( R ) / D, DIMAG( R ) / D )
         SN = SN*DCONJG( GS )
         IF( COUNT.NE.0 ) THEN
            IF( COUNT.GT.0 ) THEN
               DO 30 I = 1, COUNT
                  R = R*SAFMX2
   30          CONTINUE
            ELSE
               DO 40 I = 1, -COUNT
                  R = R*SAFMN2
   40          CONTINUE
            END IF
         END IF
      END IF
      RETURN
!
!     End of ZLARTG
!
      END
!
!  =====================================================================
!
!> @brief  ZROT applies a plane rotation with real cosine and complex sine to a pair of complex vectors.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZROT( N, CX, INCX, CY, INCY, C, S )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, INCY, N
!       DOUBLE PRECISION   C
!       COMPLEX*16         S
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         CX( * ), CY( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZROT   applies a plane rotation, where the cos (C) is real and the
!> sin (S) is complex, and the vectors CX and CY are complex.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of elements in the vectors CX and CY.
!> \endverbatim
!>
!> @param[in,out] CX
!> \verbatim
!>          CX is COMPLEX*16 array, dimension (N)
!>          On input, the vector X.
!>          On output, CX is overwritten with C*X + S*Y.
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between successive values of CY.  INCX <> 0.
!> \endverbatim
!>
!> @param[in,out] CY
!> \verbatim
!>          CY is COMPLEX*16 array, dimension (N)
!>          On input, the vector Y.
!>          On output, CY is overwritten with -CONJG(S)*X + C*Y.
!> \endverbatim
!>
!> @param[in] INCY
!> \verbatim
!>          INCY is INTEGER
!>          The increment between successive values of CY.  INCX <> 0.
!> \endverbatim
!>
!> @param[in] C
!> \verbatim
!>          C is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] S
!> \verbatim
!>          S is COMPLEX*16
!>          C and S define a rotation
!>             [  C          S  ]
!>             [ -conjg(S)   C  ]
!>          where C*C + S*CONJG(S) = 1.0.
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
!  =====================================================================
      SUBROUTINE ZROT( N, CX, INCX, CY, INCY, C, S )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, INCY, N
      DOUBLE PRECISION   C
      COMPLEX*16         S
!     ..
!     .. Array Arguments ..
      COMPLEX*16         CX( * ), CY( * )
!     ..
!
!  Purpose
!  =======
!
!  ZROT   applies a plane rotation, where the cos (C) is real and the
!  sin (S) is complex, and the vectors CX and CY are complex.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of elements in the vectors CX and CY.
!
!  CX      (input/output) COMPLEX*16 array, dimension (N)
!          On input, the vector X.
!          On output, CX is overwritten with C*X + S*Y.
!
!  INCX    (input) INTEGER
!          The increment between successive values of CY.  INCX <> 0.
!
!  CY      (input/output) COMPLEX*16 array, dimension (N)
!          On input, the vector Y.
!          On output, CY is overwritten with -CONJG(S)*X + C*Y.
!
!  INCY    (input) INTEGER
!          The increment between successive values of CY.  INCX <> 0.
!
!  C       (input) DOUBLE PRECISION
!  S       (input) COMPLEX*16
!          C and S define a rotation
!             [  C          S  ]
!             [ -conjg(S)   C  ]
!          where C*C + S*CONJG(S) = 1.0.
!
! =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, IX, IY
      COMPLEX*16         STEMP
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
!     ..
!     .. Executable Statements ..
!
      IF( N.LE.0 )
     $   RETURN
      IF( INCX.EQ.1 .AND. INCY.EQ.1 )
     $   GO TO 20
!
!     Code for unequal increments or equal increments not equal to 1
!
      IX = 1
      IY = 1
      IF( INCX.LT.0 )
     $   IX = ( -N+1 )*INCX + 1
      IF( INCY.LT.0 )
     $   IY = ( -N+1 )*INCY + 1
      DO 10 I = 1, N
         STEMP = C*CX( IX ) + S*CY( IY )
         CY( IY ) = C*CY( IY ) - DCONJG( S )*CX( IX )
         CX( IX ) = STEMP
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
!
!     Code for both increments equal to 1
!
   20 CONTINUE
      DO 30 I = 1, N
         STEMP = C*CX( I ) + S*CY( I )
         CY( I ) = C*CY( I ) - DCONJG( S )*CX( I )
         CX( I ) = STEMP
   30 CONTINUE
      RETURN
!
!     End of ZROT
!
      END
!
!  =====================================================================
!
!> @brief  ZGETRF
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGETRF( M, N, A, LDA, IPIV, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, LDA, M, N
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * )
!       COMPLEX*16         A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGETRF computes an LU factorization of a general M-by-N matrix A
!> using partial pivoting with row interchanges.
!>
!> The factorization has the form
!>    A = P * L * U
!> where P is a permutation matrix, L is lower triangular with unit
!> diagonal elements (lower trapezoidal if m > n), and U is upper
!> triangular (upper trapezoidal if m < n).
!>
!> This is the right-looking Level 3 BLAS version of the algorithm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
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
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the M-by-N matrix to be factored.
!>          On exit, the factors L and U from the factorization
!>          A = P*L*U; the unit diagonal elements of L are not stored.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> @param[out] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (min(M,N))
!>          The pivot indices; for 1 <= i <= min(M,N), row i of the
!>          matrix was interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument had an illegal value
!>          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!>                has been completed, but the factor U is exactly
!>                singular, and division by zero will occur if it is used
!>                to solve a system of equations.
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
!  =====================================================================
      SUBROUTINE ZGETRF( M, N, A, LDA, IPIV, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ZGETRF computes an LU factorization of a general M-by-N matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the M-by-N matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!                has been completed, but the factor U is exactly
!                singular, and division by zero will occur if it is used
!                to solve a system of equations.
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      INTEGER            I, IINFO, J, JB, NB
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEMM, ZGETF2, ZLASWP, ZTRSM
!     ..
!     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGETRF', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
!
!     Determine the block size for this environment.
!
      NB = ILAENV( 1, 'ZGETRF', ' ', M, N, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.MIN( M, N ) ) THEN
!
!        Use unblocked code.
!
         CALL ZGETF2( M, N, A, LDA, IPIV, INFO )
      ELSE
!
!        Use blocked code.
!
         DO 20 J = 1, MIN( M, N ), NB
            JB = MIN( MIN( M, N )-J+1, NB )
!
!           Factor diagonal and subdiagonal blocks and test for exact
!           singularity.
!
            CALL ZGETF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
!
!           Adjust INFO and the pivot indices.
!
            IF( INFO.EQ.0 .AND. IINFO.GT.0 )
     $         INFO = IINFO + J - 1
            DO 10 I = J, MIN( M, J+JB-1 )
               IPIV( I ) = J - 1 + IPIV( I )
   10       CONTINUE
!
!           Apply interchanges to columns 1:J-1.
!
            CALL ZLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
!
            IF( J+JB.LE.N ) THEN
!
!              Apply interchanges to columns J+JB:N.
!
               CALL ZLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,
     $                      IPIV, 1 )
!
!              Compute block row of U.
!
               CALL ZTRSM( 'Left', 'Lower', 'No transpose', 'Unit', JB,
     $                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ),
     $                     LDA )
               IF( J+JB.LE.M ) THEN
!
!                 Update trailing submatrix.
!
                  CALL ZGEMM( 'No transpose', 'No transpose', M-J-JB+1,
     $                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,
     $                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),
     $                        LDA )
               END IF
            END IF
   20    CONTINUE
      END IF
      RETURN
!
!     End of ZGETRF
!
      END
!
!  =====================================================================
!
!> @brief  ZGETRS
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          TRANS
!       INTEGER            INFO, LDA, LDB, N, NRHS
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * )
!       COMPLEX*16         A( LDA, * ), B( LDB, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGETRS solves a system of linear equations
!>    A * X = B,  A**T * X = B,  or  A**H * X = B
!> with a general N-by-N matrix A using the LU factorization computed
!> by ZGETRF.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>          Specifies the form of the system of equations:
!>          = 'N':  A * X = B     (No transpose)
!>          = 'T':  A**T * X = B  (Transpose)
!>          = 'C':  A**H * X = B  (Conjugate transpose)
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.
!> \endverbatim
!>
!> @param[in] NRHS
!> \verbatim
!>          NRHS is INTEGER
!>          The number of right hand sides, i.e., the number of columns
!>          of the matrix B.  NRHS >= 0.
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
!> @param[in] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (N)
!>          The pivot indices from ZGETRF; for 1<=i<=N, row i of the
!>          matrix was interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[in,out] B
!> \verbatim
!>          B is COMPLEX*16 array, dimension (LDB,NRHS)
!>          On entry, the right hand side matrix B.
!>          On exit, the solution matrix X.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B.  LDB >= max(1,N).
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
!  =====================================================================
      SUBROUTINE ZGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            INFO, LDA, LDB, N, NRHS
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * )
!     ..
!
!  Purpose
!  =======
!
!  ZGETRS solves a system of linear equations
!     A * X = B,  A**T * X = B,  or  A**H * X = B
!  with a general N-by-N matrix A using the LU factorization computed
!  by ZGETRF.
!
!  Arguments
!  =========
!
!  TRANS   (input) CHARACTER*1
!          Specifies the form of the system of equations:
!          = 'N':  A * X = B     (No transpose)
!          = 'T':  A**T * X = B  (Transpose)
!          = 'C':  A**H * X = B  (Conjugate transpose)
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrix B.  NRHS >= 0.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The factors L and U from the factorization A = P*L*U
!          as computed by ZGETRF.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  IPIV    (input) INTEGER array, dimension (N)
!          The pivot indices from ZGETRF; for 1<=i<=N, row i of the
!          matrix was interchanged with row IPIV(i).
!
!  B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
!          On entry, the right hand side matrix B.
!          On exit, the solution matrix X.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOTRAN
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLASWP, ZTRSM
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGETRS', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
!
      IF( NOTRAN ) THEN
!
!        Solve A * X = B.
!
!        Apply row interchanges to the right hand sides.
!
         CALL ZLASWP( NRHS, B, LDB, 1, N, IPIV, 1 )
!
!        Solve L*X = B, overwriting B with X.
!
         CALL ZTRSM( 'Left', 'Lower', 'No transpose', 'Unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
!
!        Solve U*X = B, overwriting B with X.
!
         CALL ZTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
      ELSE
!
!        Solve A**T * X = B  or A**H * X = B.
!
!        Solve U'*X = B, overwriting B with X.
!
         CALL ZTRSM( 'Left', 'Upper', TRANS, 'Non-unit', N, NRHS, ONE,
     $               A, LDA, B, LDB )
!
!        Solve L'*X = B, overwriting B with X.
!
         CALL ZTRSM( 'Left', 'Lower', TRANS, 'Unit', N, NRHS, ONE, A,
     $               LDA, B, LDB )
!
!        Apply row interchanges to the solution vectors.
!
         CALL ZLASWP( NRHS, B, LDB, 1, N, IPIV, -1 )
      END IF
!
      RETURN
!
!     End of ZGETRS
!
      END
!
! =====================================================================
!
!> @brief  ZLANGE returns the value of the 1-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a general rectangular matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       DOUBLE PRECISION FUNCTION ZLANGE( NORM, M, N, A, LDA, WORK )
!
!       .. Scalar Arguments ..
!       CHARACTER          NORM
!       INTEGER            LDA, M, N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   WORK( * )
!       COMPLEX*16         A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLANGE  returns the value of the one norm,  or the Frobenius norm, or
!> the  infinity norm,  or the  element of  largest absolute value  of a
!> complex matrix A.
!> \endverbatim
!>
!> \return ZLANGE
!> \verbatim
!>
!>    ZLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!>             (
!>             ( norm1(A),         NORM = '1', 'O' or 'o'
!>             (
!>             ( normI(A),         NORM = 'I' or 'i'
!>             (
!>             ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!>
!> where  norm1  denotes the  one norm of a matrix (maximum column sum),
!> normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!> normF  denotes the  Frobenius norm of a matrix (square root of sum of
!> squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] NORM
!> \verbatim
!>          NORM is CHARACTER*1
!>          Specifies the value to be returned in ZLANGE as described
!>          above.
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix A.  M >= 0.  When M = 0,
!>          ZLANGE is set to zero.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix A.  N >= 0.  When N = 0,
!>          ZLANGE is set to zero.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          The m by n matrix A.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(M,1).
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
!>          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
!>          referenced.
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
!  =====================================================================
      DOUBLE PRECISION FUNCTION ZLANGE( NORM, M, N, A, LDA, WORK )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   WORK( * )
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLANGE  returns the value of the one norm,  or the Frobenius norm, or
!  the  infinity norm,  or the  element of  largest absolute value  of a
!  complex matrix A.
!
!  Description
!  ===========
!
!  ZLANGE returns the value
!
!     ZLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!              (
!              ( norm1(A),         NORM = '1', 'O' or 'o'
!              (
!              ( normI(A),         NORM = 'I' or 'i'
!              (
!              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!
!  where  norm1  denotes the  one norm of a matrix (maximum column sum),
!  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!  normF  denotes the  Frobenius norm of a matrix (square root of sum of
!  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!
!  Arguments
!  =========
!
!  NORM    (input) CHARACTER*1
!          Specifies the value to be returned in ZLANGE as described
!          above.
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.  When M = 0,
!          ZLANGE is set to zero.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.  When N = 0,
!          ZLANGE is set to zero.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The m by n matrix A.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(M,1).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
!          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
!          referenced.
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   SCALE, SUM, VALUE
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZLASSQ
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
      IF( MIN( M, N ).EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
!
!        Find max(abs(A(i,j))).
!
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, M
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
!
!        Find norm1(A).
!
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, M
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
!
!        Find normI(A).
!
         DO 50 I = 1, M
            WORK( I ) = ZERO
   50    CONTINUE
         DO 70 J = 1, N
            DO 60 I = 1, M
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, M
            VALUE = MAX( VALUE, WORK( I ) )
   80    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
!
!        Find normF(A).
!
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL ZLASSQ( M, A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
!
      ZLANGE = VALUE
      RETURN
!
!     End of ZLANGE
!
      END
!
! =====================================================================
!
!> @brief  DLAISNAN tests input for NaN by comparing two arguments for inequality.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       LOGICAL FUNCTION DLAISNAN( DIN1, DIN2 )
!
!       .. Scalar Arguments ..
!       DOUBLE PRECISION   DIN1, DIN2
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> This routine is not for general use.  It exists solely to avoid
!> over-optimization in DISNAN.
!>
!> DLAISNAN checks for NaNs by comparing its two arguments for
!> inequality.  NaN is the only floating-point value where NaN != NaN
!> returns .TRUE.  To check for NaNs, pass the same variable as both
!> arguments.
!>
!> A compiler must assume that the two arguments are
!> not the same variable, and the test will not be optimized away.
!> Interprocedural or whole-program optimization may delete this
!> test.  The ISNAN functions will be replaced by the correct
!> Fortran 03 intrinsic once the intrinsic is widely available.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] DIN1
!> \verbatim
!>          DIN1 is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] DIN2
!> \verbatim
!>          DIN2 is DOUBLE PRECISION
!>          Two numbers to compare for inequality.
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
!  =====================================================================
      LOGICAL FUNCTION DLAISNAN(DIN1,DIN2)
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION DIN1,DIN2
!     ..
!
!  Purpose
!  =======
!
!  This routine is not for general use.  It exists solely to avoid
!  over-optimization in DISNAN.
!
!  DLAISNAN checks for NaNs by comparing its two arguments for
!  inequality.  NaN is the only floating-point value where NaN != NaN
!  returns .TRUE.  To check for NaNs, pass the same variable as both
!  arguments.
!
!  A compiler must assume that the two arguments are
!  not the same variable, and the test will not be optimized away.
!  Interprocedural or whole-program optimization may delete this
!  test.  The ISNAN functions will be replaced by the correct
!  Fortran 03 intrinsic once the intrinsic is widely available.
!
!  Arguments
!  =========
!
!  DIN1     (input) DOUBLE PRECISION
!  DIN2     (input) DOUBLE PRECISION
!          Two numbers to compare for inequality.
!
!  =====================================================================
!
!  .. Executable Statements ..
      DLAISNAN = (DIN1.NE.DIN2)
      RETURN
!
!     End of DLAISNAN
!
      END
!
! =====================================================================
!
!> @brief  DISNAN tests input for NaN.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       LOGICAL FUNCTION DISNAN( DIN )
!
!       .. Scalar Arguments ..
!       DOUBLE PRECISION   DIN
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
!> otherwise.  To be replaced by the Fortran 2003 intrinsic in the
!> future.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] DIN
!> \verbatim
!>          DIN is DOUBLE PRECISION
!>          Input to test for NaN.
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
!  =====================================================================
      LOGICAL FUNCTION DISNAN(DIN)
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION DIN
!     ..
!
!  Purpose
!  =======
!
!  DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
!  otherwise.  To be replaced by the Fortran 2003 intrinsic in the
!  future.
!
!  Arguments
!  =========
!
!  DIN      (input) DOUBLE PRECISION
!          Input to test for NaN.
!
!  =====================================================================
!
!  .. External Functions ..
      LOGICAL DLAISNAN
      EXTERNAL DLAISNAN
!  ..
!  .. Executable Statements ..
      DISNAN = DLAISNAN(DIN,DIN)
      RETURN
!
!     End of DISNAN
!
      END
!
!  =====================================================================
!
!> @brief  ZLASCL multiplies a general rectangular matrix by a real scalar defined as cto/cfrom.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          TYPE
!       INTEGER            INFO, KL, KU, LDA, M, N
!       DOUBLE PRECISION   CFROM, CTO
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
!> ZLASCL multiplies the M by N complex matrix A by the real scalar
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
!>                  bandwidth KU. See ZGBTRF for storage details.
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
!>          A is COMPLEX*16 array, dimension (LDA,N)
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
!  =====================================================================
      SUBROUTINE ZLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      DOUBLE PRECISION   CFROM, CTO
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLASCL multiplies the M by N complex matrix A by the real scalar
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
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
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
      LOGICAL            LSAME, DISNAN
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH, DISNAN
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
      ELSE IF( CFROM.EQ.ZERO .OR. DISNAN(CFROM) ) THEN
         INFO = -4
      ELSE IF( DISNAN(CTO) ) THEN
         INFO = -5
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR.
     $         ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR.
     $            ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) )
     $             THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR.
     $            ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR.
     $            ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         END IF
      END IF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZLASCL', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   RETURN
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
      IF( CFROM1.EQ.CFROMC ) THEN
!        CFROMC is an inf.  Multiply by a correctly signed zero for
!        finite CTOC, or a NaN if CTOC is infinite.
         MUL = CTOC / CFROMC
         DONE = .TRUE.
         CTO1 = CTOC
      ELSE
         CTO1 = CTOC / BIGNUM
         IF( CTO1.EQ.CTOC ) THEN
!           CTOC is either 0 or an inf.  In both cases, CTOC itself
!           serves as the correct multiplication factor.
            MUL = CTOC
            DONE = .TRUE.
            CFROMC = ONE
         ELSE IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
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
     $   GO TO 10
!
      RETURN
!
!     End of ZLASCL
!
      END
!
!  =====================================================================
!
!> @brief  ZGGBAL
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGGBAL( JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,
!                          RSCALE, WORK, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          JOB
!       INTEGER            IHI, ILO, INFO, LDA, LDB, N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   LSCALE( * ), RSCALE( * ), WORK( * )
!       COMPLEX*16         A( LDA, * ), B( LDB, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGGBAL balances a pair of general complex matrices (A,B).  This
!> involves, first, permuting A and B by similarity transformations to
!> isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
!> elements on the diagonal; and second, applying a diagonal similarity
!> transformation to rows and columns ILO to IHI to make the rows
!> and columns as close in norm as possible. Both steps are optional.
!>
!> Balancing may reduce the 1-norm of the matrices, and improve the
!> accuracy of the computed eigenvalues and/or eigenvectors in the
!> generalized eigenvalue problem A*x = lambda*B*x.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] JOB
!> \verbatim
!>          JOB is CHARACTER*1
!>          Specifies the operations to be performed on A and B:
!>          = 'N':  none:  simply set ILO = 1, IHI = N, LSCALE(I) = 1.0
!>                  and RSCALE(I) = 1.0 for i=1,...,N;
!>          = 'P':  permute only;
!>          = 'S':  scale only;
!>          = 'B':  both permute and scale.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrices A and B.  N >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the input matrix A.
!>          On exit, A is overwritten by the balanced matrix.
!>          If JOB = 'N', A is not referenced.
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
!>          On entry, the input matrix B.
!>          On exit, B is overwritten by the balanced matrix.
!>          If JOB = 'N', B is not referenced.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B. LDB >= max(1,N).
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
!>          ILO and IHI are set to integers such that on exit
!>          A(i,j) = 0 and B(i,j) = 0 if i > j and
!>          j = 1,...,ILO-1 or i = IHI+1,...,N.
!>          If JOB = 'N' or 'S', ILO = 1 and IHI = N.
!> \endverbatim
!>
!> @param[out] LSCALE
!> \verbatim
!>          LSCALE is DOUBLE PRECISION array, dimension (N)
!>          Details of the permutations and scaling factors applied
!>          to the left side of A and B.  If P(j) is the index of the
!>          row interchanged with row j, and D(j) is the scaling factor
!>          applied to row j, then
!>            LSCALE(j) = P(j)    for J = 1,...,ILO-1
!>                      = D(j)    for J = ILO,...,IHI
!>                      = P(j)    for J = IHI+1,...,N.
!>          The order in which the interchanges are made is N to IHI+1,
!>          then 1 to ILO-1.
!> \endverbatim
!>
!> @param[out] RSCALE
!> \verbatim
!>          RSCALE is DOUBLE PRECISION array, dimension (N)
!>          Details of the permutations and scaling factors applied
!>          to the right side of A and B.  If P(j) is the index of the
!>          column interchanged with column j, and D(j) is the scaling
!>          factor applied to column j, then
!>            RSCALE(j) = P(j)    for J = 1,...,ILO-1
!>                      = D(j)    for J = ILO,...,IHI
!>                      = P(j)    for J = IHI+1,...,N.
!>          The order in which the interchanges are made is N to IHI+1,
!>          then 1 to ILO-1.
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is REAL array, dimension (lwork)
!>          lwork must be at least max(1,6*N) when JOB = 'S' or 'B', and
!>          at least 1 when JOB = 'N' or 'P'.
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument had an illegal value.
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
!>  See R.C. WARD, Balancing the generalized eigenvalue problem,
!>                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZGGBAL( JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,
     $                   RSCALE, WORK, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          JOB
      INTEGER            IHI, ILO, INFO, LDA, LDB, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   LSCALE( * ), RSCALE( * ), WORK( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * )
!     ..
!
!  Purpose
!  =======
!
!  ZGGBAL balances a pair of general complex matrices (A,B).  This
!  involves, first, permuting A and B by similarity transformations to
!  isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
!  elements on the diagonal; and second, applying a diagonal similarity
!  transformation to rows and columns ILO to IHI to make the rows
!  and columns as close in norm as possible. Both steps are optional.
!
!  Balancing may reduce the 1-norm of the matrices, and improve the
!  accuracy of the computed eigenvalues and/or eigenvectors in the
!  generalized eigenvalue problem A*x = lambda*B*x.
!
!  Arguments
!  =========
!
!  JOB     (input) CHARACTER*1
!          Specifies the operations to be performed on A and B:
!          = 'N':  none:  simply set ILO = 1, IHI = N, LSCALE(I) = 1.0
!                  and RSCALE(I) = 1.0 for i=1,...,N;
!          = 'P':  permute only;
!          = 'S':  scale only;
!          = 'B':  both permute and scale.
!
!  N       (input) INTEGER
!          The order of the matrices A and B.  N >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the input matrix A.
!          On exit, A is overwritten by the balanced matrix.
!          If JOB = 'N', A is not referenced.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A. LDA >= max(1,N).
!
!  B       (input/output) COMPLEX*16 array, dimension (LDB,N)
!          On entry, the input matrix B.
!          On exit, B is overwritten by the balanced matrix.
!          If JOB = 'N', B is not referenced.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B. LDB >= max(1,N).
!
!  ILO     (output) INTEGER
!  IHI     (output) INTEGER
!          ILO and IHI are set to integers such that on exit
!          A(i,j) = 0 and B(i,j) = 0 if i > j and
!          j = 1,...,ILO-1 or i = IHI+1,...,N.
!          If JOB = 'N' or 'S', ILO = 1 and IHI = N.
!
!  LSCALE  (output) DOUBLE PRECISION array, dimension (N)
!          Details of the permutations and scaling factors applied
!          to the left side of A and B.  If P(j) is the index of the
!          row interchanged with row j, and D(j) is the scaling factor
!          applied to row j, then
!            LSCALE(j) = P(j)    for J = 1,...,ILO-1
!                      = D(j)    for J = ILO,...,IHI
!                      = P(j)    for J = IHI+1,...,N.
!          The order in which the interchanges are made is N to IHI+1,
!          then 1 to ILO-1.
!
!  RSCALE  (output) DOUBLE PRECISION array, dimension (N)
!          Details of the permutations and scaling factors applied
!          to the right side of A and B.  If P(j) is the index of the
!          column interchanged with column j, and D(j) is the scaling
!          factor applied to column j, then
!            RSCALE(j) = P(j)    for J = 1,...,ILO-1
!                      = D(j)    for J = ILO,...,IHI
!                      = P(j)    for J = IHI+1,...,N.
!          The order in which the interchanges are made is N to IHI+1,
!          then 1 to ILO-1.
!
!  WORK    (workspace) REAL array, dimension (lwork)
!          lwork must be at least max(1,6*N) when JOB = 'S' or 'B', and
!          at least 1 when JOB = 'N' or 'P'.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!
!  Further Details
!  ===============
!
!  See R.C. WARD, Balancing the generalized eigenvalue problem,
!                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   THREE, SCLFAC
      PARAMETER          ( THREE = 3.0D+0, SCLFAC = 1.0D+1 )
      COMPLEX*16         CZERO
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      INTEGER            I, ICAB, IFLOW, IP1, IR, IRAB, IT, J, JC, JP1,
     $                   K, KOUNT, L, LCAB, LM1, LRAB, LSFMAX, LSFMIN,
     $                   M, NR, NRP2
      DOUBLE PRECISION   ALPHA, BASL, BETA, CAB, CMAX, COEF, COEF2,
     $                   COEF5, COR, EW, EWC, GAMMA, PGAMMA, RAB, SFMAX,
     $                   SFMIN, SUM, T, TA, TB, TC
      COMPLEX*16         CDUM
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IZAMAX
      DOUBLE PRECISION   DDOT, DLAMCH
      EXTERNAL           LSAME, IZAMAX, DDOT, DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DAXPY, DSCAL, XERBLA, ZDSCAL, ZSWAP
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG, INT, LOG10, MAX, MIN, SIGN
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
!     ..
!     .. Statement Function definitions ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters
!
      INFO = 0
      IF( .NOT.LSAME( JOB, 'N' ) .AND. .NOT.LSAME( JOB, 'P' ) .AND.
     $    .NOT.LSAME( JOB, 'S' ) .AND. .NOT.LSAME( JOB, 'B' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGGBAL', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 ) THEN
         ILO = 1
         IHI = N
         RETURN
      END IF
!
      IF( N.EQ.1 ) THEN
         ILO = 1
         IHI = N
         LSCALE( 1 ) = ONE
         RSCALE( 1 ) = ONE
         RETURN
      END IF
!
      IF( LSAME( JOB, 'N' ) ) THEN
         ILO = 1
         IHI = N
         DO 10 I = 1, N
            LSCALE( I ) = ONE
            RSCALE( I ) = ONE
   10    CONTINUE
         RETURN
      END IF
!
      K = 1
      L = N
      IF( LSAME( JOB, 'S' ) )
     $   GO TO 190
!
      GO TO 30
!
!     Permute the matrices A and B to isolate the eigenvalues.
!
!     Find row with one nonzero in columns 1 through L
!
   20 CONTINUE
      L = LM1
      IF( L.NE.1 )
     $   GO TO 30
!
      RSCALE( 1 ) = 1
      LSCALE( 1 ) = 1
      GO TO 190
!
   30 CONTINUE
      LM1 = L - 1
      DO 80 I = L, 1, -1
         DO 40 J = 1, LM1
            JP1 = J + 1
            IF( A( I, J ).NE.CZERO .OR. B( I, J ).NE.CZERO )
     $         GO TO 50
   40    CONTINUE
         J = L
         GO TO 70
!
   50    CONTINUE
         DO 60 J = JP1, L
            IF( A( I, J ).NE.CZERO .OR. B( I, J ).NE.CZERO )
     $         GO TO 80
   60    CONTINUE
         J = JP1 - 1
!
   70    CONTINUE
         M = L
         IFLOW = 1
         GO TO 160
   80 CONTINUE
      GO TO 100
!
!     Find column with one nonzero in rows K through N
!
   90 CONTINUE
      K = K + 1
!
  100 CONTINUE
      DO 150 J = K, L
         DO 110 I = K, LM1
            IP1 = I + 1
            IF( A( I, J ).NE.CZERO .OR. B( I, J ).NE.CZERO )
     $         GO TO 120
  110    CONTINUE
         I = L
         GO TO 140
  120    CONTINUE
         DO 130 I = IP1, L
            IF( A( I, J ).NE.CZERO .OR. B( I, J ).NE.CZERO )
     $         GO TO 150
  130    CONTINUE
         I = IP1 - 1
  140    CONTINUE
         M = K
         IFLOW = 2
         GO TO 160
  150 CONTINUE
      GO TO 190
!
!     Permute rows M and I
!
  160 CONTINUE
      LSCALE( M ) = I
      IF( I.EQ.M )
     $   GO TO 170
      CALL ZSWAP( N-K+1, A( I, K ), LDA, A( M, K ), LDA )
      CALL ZSWAP( N-K+1, B( I, K ), LDB, B( M, K ), LDB )
!
!     Permute columns M and J
!
  170 CONTINUE
      RSCALE( M ) = J
      IF( J.EQ.M )
     $   GO TO 180
      CALL ZSWAP( L, A( 1, J ), 1, A( 1, M ), 1 )
      CALL ZSWAP( L, B( 1, J ), 1, B( 1, M ), 1 )
!
  180 CONTINUE
      GO TO ( 20, 90 )IFLOW
!
  190 CONTINUE
      ILO = K
      IHI = L
!
      IF( LSAME( JOB, 'P' ) ) THEN
         DO 195 I = ILO, IHI
            LSCALE( I ) = ONE
            RSCALE( I ) = ONE
  195    CONTINUE
         RETURN
      END IF
!
      IF( ILO.EQ.IHI )
     $   RETURN
!
!     Balance the submatrix in rows ILO to IHI.
!
      NR = IHI - ILO + 1
      DO 200 I = ILO, IHI
         RSCALE( I ) = ZERO
         LSCALE( I ) = ZERO
!
         WORK( I ) = ZERO
         WORK( I+N ) = ZERO
         WORK( I+2*N ) = ZERO
         WORK( I+3*N ) = ZERO
         WORK( I+4*N ) = ZERO
         WORK( I+5*N ) = ZERO
  200 CONTINUE
!
!     Compute right side vector in resulting linear equations
!
      BASL = LOG10( SCLFAC )
      DO 240 I = ILO, IHI
         DO 230 J = ILO, IHI
            IF( A( I, J ).EQ.CZERO ) THEN
               TA = ZERO
               GO TO 210
            END IF
            TA = LOG10( CABS1( A( I, J ) ) ) / BASL
!
  210       CONTINUE
            IF( B( I, J ).EQ.CZERO ) THEN
               TB = ZERO
               GO TO 220
            END IF
            TB = LOG10( CABS1( B( I, J ) ) ) / BASL
!
  220       CONTINUE
            WORK( I+4*N ) = WORK( I+4*N ) - TA - TB
            WORK( J+5*N ) = WORK( J+5*N ) - TA - TB
  230    CONTINUE
  240 CONTINUE
!
      COEF = ONE / DBLE( 2*NR )
      COEF2 = COEF*COEF
      COEF5 = HALF*COEF2
      NRP2 = NR + 2
      BETA = ZERO
      IT = 1
!
!     Start generalized conjugate gradient iteration
!
  250 CONTINUE
!
      GAMMA = DDOT( NR, WORK( ILO+4*N ), 1, WORK( ILO+4*N ), 1 ) +
     $        DDOT( NR, WORK( ILO+5*N ), 1, WORK( ILO+5*N ), 1 )
!
      EW = ZERO
      EWC = ZERO
      DO 260 I = ILO, IHI
         EW = EW + WORK( I+4*N )
         EWC = EWC + WORK( I+5*N )
  260 CONTINUE
!
      GAMMA = COEF*GAMMA - COEF2*( EW**2+EWC**2 ) - COEF5*( EW-EWC )**2
      IF( GAMMA.EQ.ZERO )
     $   GO TO 350
      IF( IT.NE.1 )
     $   BETA = GAMMA / PGAMMA
      T = COEF5*( EWC-THREE*EW )
      TC = COEF5*( EW-THREE*EWC )
!
      CALL DSCAL( NR, BETA, WORK( ILO ), 1 )
      CALL DSCAL( NR, BETA, WORK( ILO+N ), 1 )
!
      CALL DAXPY( NR, COEF, WORK( ILO+4*N ), 1, WORK( ILO+N ), 1 )
      CALL DAXPY( NR, COEF, WORK( ILO+5*N ), 1, WORK( ILO ), 1 )
!
      DO 270 I = ILO, IHI
         WORK( I ) = WORK( I ) + TC
         WORK( I+N ) = WORK( I+N ) + T
  270 CONTINUE
!
!     Apply matrix to vector
!
      DO 300 I = ILO, IHI
         KOUNT = 0
         SUM = ZERO
         DO 290 J = ILO, IHI
            IF( A( I, J ).EQ.CZERO )
     $         GO TO 280
            KOUNT = KOUNT + 1
            SUM = SUM + WORK( J )
  280       CONTINUE
            IF( B( I, J ).EQ.CZERO )
     $         GO TO 290
            KOUNT = KOUNT + 1
            SUM = SUM + WORK( J )
  290    CONTINUE
         WORK( I+2*N ) = DBLE( KOUNT )*WORK( I+N ) + SUM
  300 CONTINUE
!
      DO 330 J = ILO, IHI
         KOUNT = 0
         SUM = ZERO
         DO 320 I = ILO, IHI
            IF( A( I, J ).EQ.CZERO )
     $         GO TO 310
            KOUNT = KOUNT + 1
            SUM = SUM + WORK( I+N )
  310       CONTINUE
            IF( B( I, J ).EQ.CZERO )
     $         GO TO 320
            KOUNT = KOUNT + 1
            SUM = SUM + WORK( I+N )
  320    CONTINUE
         WORK( J+3*N ) = DBLE( KOUNT )*WORK( J ) + SUM
  330 CONTINUE
!
      SUM = DDOT( NR, WORK( ILO+N ), 1, WORK( ILO+2*N ), 1 ) +
     $      DDOT( NR, WORK( ILO ), 1, WORK( ILO+3*N ), 1 )
      ALPHA = GAMMA / SUM
!
!     Determine correction to current iteration
!
      CMAX = ZERO
      DO 340 I = ILO, IHI
         COR = ALPHA*WORK( I+N )
         IF( ABS( COR ).GT.CMAX )
     $      CMAX = ABS( COR )
         LSCALE( I ) = LSCALE( I ) + COR
         COR = ALPHA*WORK( I )
         IF( ABS( COR ).GT.CMAX )
     $      CMAX = ABS( COR )
         RSCALE( I ) = RSCALE( I ) + COR
  340 CONTINUE
      IF( CMAX.LT.HALF )
     $   GO TO 350
!
      CALL DAXPY( NR, -ALPHA, WORK( ILO+2*N ), 1, WORK( ILO+4*N ), 1 )
      CALL DAXPY( NR, -ALPHA, WORK( ILO+3*N ), 1, WORK( ILO+5*N ), 1 )
!
      PGAMMA = GAMMA
      IT = IT + 1
      IF( IT.LE.NRP2 )
     $   GO TO 250
!
!     End generalized conjugate gradient iteration
!
  350 CONTINUE
      SFMIN = DLAMCH( 'S' )
      SFMAX = ONE / SFMIN
      LSFMIN = INT( LOG10( SFMIN ) / BASL+ONE )
      LSFMAX = INT( LOG10( SFMAX ) / BASL )
      DO 360 I = ILO, IHI
         IRAB = IZAMAX( N-ILO+1, A( I, ILO ), LDA )
         RAB = ABS( A( I, IRAB+ILO-1 ) )
         IRAB = IZAMAX( N-ILO+1, B( I, ILO ), LDB )
         RAB = MAX( RAB, ABS( B( I, IRAB+ILO-1 ) ) )
         LRAB = INT( LOG10( RAB+SFMIN ) / BASL+ONE )
         IR = LSCALE( I ) + SIGN( HALF, LSCALE( I ) )
         IR = MIN( MAX( IR, LSFMIN ), LSFMAX, LSFMAX-LRAB )
         LSCALE( I ) = SCLFAC**IR
         ICAB = IZAMAX( IHI, A( 1, I ), 1 )
         CAB = ABS( A( ICAB, I ) )
         ICAB = IZAMAX( IHI, B( 1, I ), 1 )
         CAB = MAX( CAB, ABS( B( ICAB, I ) ) )
         LCAB = INT( LOG10( CAB+SFMIN ) / BASL+ONE )
         JC = RSCALE( I ) + SIGN( HALF, RSCALE( I ) )
         JC = MIN( MAX( JC, LSFMIN ), LSFMAX, LSFMAX-LCAB )
         RSCALE( I ) = SCLFAC**JC
  360 CONTINUE
!
!     Row scaling of matrices A and B
!
      DO 370 I = ILO, IHI
         CALL ZDSCAL( N-ILO+1, LSCALE( I ), A( I, ILO ), LDA )
         CALL ZDSCAL( N-ILO+1, LSCALE( I ), B( I, ILO ), LDB )
  370 CONTINUE
!
!     Column scaling of matrices A and B
!
      DO 380 J = ILO, IHI
         CALL ZDSCAL( IHI, RSCALE( J ), A( 1, J ), 1 )
         CALL ZDSCAL( IHI, RSCALE( J ), B( 1, J ), 1 )
  380 CONTINUE
!
      RETURN
!
!     End of ZGGBAL
!
      END
!
!  =====================================================================
!
!> @brief  ZGEQRF
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, LDA, LWORK, M, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGEQRF computes a QR factorization of a complex M-by-N matrix A:
!> A = Q * R.
!> \endverbatim
!
!  Arguments:
!  ==========
!
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
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the M-by-N matrix A.
!>          On exit, the elements on and above the diagonal of the array
!>          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
!>          upper triangular if m >= n); the elements below the diagonal,
!>          with the array TAU, represent the unitary matrix Q as a
!>          product of min(m,n) elementary reflectors (see Further
!>          Details).
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> @param[out] TAU
!> \verbatim
!>          TAU is COMPLEX*16 array, dimension (min(M,N))
!>          The scalar factors of the elementary reflectors (see Further
!>          Details).
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
!>          The dimension of the array WORK.  LWORK >= max(1,N).
!>          For optimum performance LWORK >= N*NB, where NB is
!>          the optimal blocksize.
!>
!>          If LWORK = -1, then a workspace query is assumed; the routine
!>          only calculates the optimal size of the WORK array, returns
!>          this value as the first entry of the WORK array, and no error
!>          message related to LWORK is issued by XERBLA.
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
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>  The matrix Q is represented as a product of elementary reflectors
!>
!>     Q = H(1) H(2) . . . H(k), where k = min(m,n).
!>
!>  Each H(i) has the form
!>
!>     H(i) = I - tau * v * v**H
!>
!>  where tau is a complex scalar, and v is a complex vector with
!>  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
!>  and tau in TAU(i).
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZGEQRF computes a QR factorization of a complex M-by-N matrix A:
!  A = Q * R.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the M-by-N matrix A.
!          On exit, the elements on and above the diagonal of the array
!          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
!          upper triangular if m >= n); the elements below the diagonal,
!          with the array TAU, represent the unitary matrix Q as a
!          product of min(m,n) elementary reflectors (see Further
!          Details).
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  TAU     (output) COMPLEX*16 array, dimension (min(M,N))
!          The scalar factors of the elementary reflectors (see Further
!          Details).
!
!  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.  LWORK >= max(1,N).
!          For optimum performance LWORK >= N*NB, where NB is
!          the optimal blocksize.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  Further Details
!  ===============
!
!  The matrix Q is represented as a product of elementary reflectors
!
!     Q = H(1) H(2) . . . H(k), where k = min(m,n).
!
!  Each H(i) has the form
!
!     H(i) = I - tau * v * v'
!
!  where tau is a complex scalar, and v is a complex vector with
!  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
!  and tau in TAU(i).
!
!  =====================================================================
!
!     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, LWKOPT, NB,
     $                   NBMIN, NX
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEQR2, ZLARFB, ZLARFT
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      NB = ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
      LWKOPT = N*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEQRF', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
!
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
!
!        Determine when to cross over from blocked to unblocked code.
!
         NX = MAX( 0, ILAENV( 3, 'ZGEQRF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
!
!           Determine if workspace is large enough for blocked code.
!
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
!
!              Not enough workspace to use optimal NB:  reduce NB and
!              determine the minimum value of NB.
!
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'ZGEQRF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
!
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
!
!        Use blocked code initially
!
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
!
!           Compute the QR factorization of the current block
!           A(i:m,i:i+ib-1)
!
            CALL ZGEQR2( M-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.N ) THEN
!
!              Form the triangular factor of the block reflector
!              H = H(i) H(i+1) . . . H(i+ib-1)
!
               CALL ZLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
!
!              Apply H' to A(i:m,i+ib:n) from the left
!
               CALL ZLARFB( 'Left', 'Conjugate transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
!
!     Use unblocked code to factor the last or only block.
!
      IF( I.LE.K )
     $   CALL ZGEQR2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
!
      WORK( 1 ) = IWS
      RETURN
!
!     End of ZGEQRF
!
      END
!
!  =====================================================================
!
!> @brief  ZUNMQR
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZUNMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
!                          WORK, LWORK, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          SIDE, TRANS
!       INTEGER            INFO, K, LDA, LDC, LWORK, M, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZUNMQR overwrites the general complex M-by-N matrix C with
!>
!>                 SIDE = 'L'     SIDE = 'R'
!> TRANS = 'N':      Q * C          C * Q
!> TRANS = 'C':      Q**H * C       C * Q**H
!>
!> where Q is a complex unitary matrix defined as the product of k
!> elementary reflectors
!>
!>       Q = H(1) H(2) . . . H(k)
!>
!> as returned by ZGEQRF. Q is of order M if SIDE = 'L' and of order N
!> if SIDE = 'R'.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] SIDE
!> \verbatim
!>          SIDE is CHARACTER*1
!>          = 'L': apply Q or Q**H from the Left;
!>          = 'R': apply Q or Q**H from the Right.
!> \endverbatim
!>
!> @param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>          = 'N':  No transpose, apply Q;
!>          = 'C':  Conjugate transpose, apply Q**H.
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix C. M >= 0.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix C. N >= 0.
!> \endverbatim
!>
!> @param[in] K
!> \verbatim
!>          K is INTEGER
!>          The number of elementary reflectors whose product defines
!>          the matrix Q.
!>          If SIDE = 'L', M >= K >= 0;
!>          if SIDE = 'R', N >= K >= 0.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,K)
!>          The i-th column must contain the vector which defines the
!>          elementary reflector H(i), for i = 1,2,...,k, as returned by
!>          ZGEQRF in the first k columns of its array argument A.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.
!>          If SIDE = 'L', LDA >= max(1,M);
!>          if SIDE = 'R', LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in] TAU
!> \verbatim
!>          TAU is COMPLEX*16 array, dimension (K)
!>          TAU(i) must contain the scalar factor of the elementary
!>          reflector H(i), as returned by ZGEQRF.
!> \endverbatim
!>
!> @param[in,out] C
!> \verbatim
!>          C is COMPLEX*16 array, dimension (LDC,N)
!>          On entry, the M-by-N matrix C.
!>          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
!> \endverbatim
!>
!> @param[in] LDC
!> \verbatim
!>          LDC is INTEGER
!>          The leading dimension of the array C. LDC >= max(1,M).
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
!>          The dimension of the array WORK.
!>          If SIDE = 'L', LWORK >= max(1,N);
!>          if SIDE = 'R', LWORK >= max(1,M).
!>          For optimum performance LWORK >= N*NB if SIDE = 'L', and
!>          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
!>          blocksize.
!>
!>          If LWORK = -1, then a workspace query is assumed; the routine
!>          only calculates the optimal size of the WORK array, returns
!>          this value as the first entry of the WORK array, and no error
!>          message related to LWORK is issued by XERBLA.
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
!  =====================================================================
      SUBROUTINE ZUNMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZUNMQR overwrites the general complex M-by-N matrix C with
!
!                  SIDE = 'L'     SIDE = 'R'
!  TRANS = 'N':      Q * C          C * Q
!  TRANS = 'C':      Q**H * C       C * Q**H
!
!  where Q is a complex unitary matrix defined as the product of k
!  elementary reflectors
!
!        Q = H(1) H(2) . . . H(k)
!
!  as returned by ZGEQRF. Q is of order M if SIDE = 'L' and of order N
!  if SIDE = 'R'.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply Q or Q**H from the Left;
!          = 'R': apply Q or Q**H from the Right.
!
!  TRANS   (input) CHARACTER*1
!          = 'N':  No transpose, apply Q;
!          = 'C':  Conjugate transpose, apply Q**H.
!
!  M       (input) INTEGER
!          The number of rows of the matrix C. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C. N >= 0.
!
!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines
!          the matrix Q.
!          If SIDE = 'L', M >= K >= 0;
!          if SIDE = 'R', N >= K >= 0.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,K)
!          The i-th column must contain the vector which defines the
!          elementary reflector H(i), for i = 1,2,...,k, as returned by
!          ZGEQRF in the first k columns of its array argument A.
!          A is modified by the routine but restored on exit.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!          If SIDE = 'L', LDA >= max(1,M);
!          if SIDE = 'R', LDA >= max(1,N).
!
!  TAU     (input) COMPLEX*16 array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by ZGEQRF.
!
!  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
!          On entry, the M-by-N matrix C.
!          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.
!          If SIDE = 'L', LWORK >= max(1,N);
!          if SIDE = 'R', LWORK >= max(1,M).
!          For optimum performance LWORK >= N*NB if SIDE = 'L', and
!          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
!          blocksize.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   LWKOPT, MI, NB, NBMIN, NI, NQ, NW
!     ..
!     .. Local Arrays ..
      COMPLEX*16         T( LDT, NBMAX )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNM2R
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
!
!     NQ is the order of Q and NW is the minimum dimension of WORK
!
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
!
      IF( INFO.EQ.0 ) THEN
!
!        Determine the block size.  NB may be at most NBMAX, where NBMAX
!        is used to define the local array T.
!
         NB = MIN( NBMAX, ILAENV( 1, 'ZUNMQR', SIDE // TRANS, M, N, K,
     $        -1 ) )
         LWKOPT = MAX( 1, NW )*NB
         WORK( 1 ) = LWKOPT
      END IF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNMQR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
!
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'ZUNMQR', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      ELSE
         IWS = NW
      END IF
!
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
!
!        Use unblocked code
!
         CALL ZUNM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
!
!        Use blocked code
!
         IF( ( LEFT .AND. .NOT.NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
!
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
!
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
!
!           Form the triangular factor of the block reflector
!           H = H(i) H(i+1) . . . H(i+ib-1)
!
            CALL ZLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
!
!              H or H' is applied to C(i:m,1:n)
!
               MI = M - I + 1
               IC = I
            ELSE
!
!              H or H' is applied to C(1:m,i:n)
!
               NI = N - I + 1
               JC = I
            END IF
!
!           Apply H or H'
!
            CALL ZLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NI,
     $                   IB, A( I, I ), LDA, T, LDT, C( IC, JC ), LDC,
     $                   WORK, LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
!
!     End of ZUNMQR
!
      END
!
!  =====================================================================
!
!> @brief  ZUNGQR
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, K, LDA, LWORK, M, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
!> which is defined as the first N columns of a product of K elementary
!> reflectors of order M
!>
!>       Q  =  H(1) H(2) . . . H(k)
!>
!> as returned by ZGEQRF.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix Q. M >= 0.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix Q. M >= N >= 0.
!> \endverbatim
!>
!> @param[in] K
!> \verbatim
!>          K is INTEGER
!>          The number of elementary reflectors whose product defines the
!>          matrix Q. N >= K >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the i-th column must contain the vector which
!>          defines the elementary reflector H(i), for i = 1,2,...,k, as
!>          returned by ZGEQRF in the first k columns of its array
!>          argument A.
!>          On exit, the M-by-N matrix Q.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The first dimension of the array A. LDA >= max(1,M).
!> \endverbatim
!>
!> @param[in] TAU
!> \verbatim
!>          TAU is COMPLEX*16 array, dimension (K)
!>          TAU(i) must contain the scalar factor of the elementary
!>          reflector H(i), as returned by ZGEQRF.
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
!>          For optimum performance LWORK >= N*NB, where NB is the
!>          optimal blocksize.
!>
!>          If LWORK = -1, then a workspace query is assumed; the routine
!>          only calculates the optimal size of the WORK array, returns
!>          this value as the first entry of the WORK array, and no error
!>          message related to LWORK is issued by XERBLA.
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument has an illegal value
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
!  =====================================================================
      SUBROUTINE ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
!  which is defined as the first N columns of a product of K elementary
!  reflectors of order M
!
!        Q  =  H(1) H(2) . . . H(k)
!
!  as returned by ZGEQRF.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix Q. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix Q. M >= N >= 0.
!
!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines the
!          matrix Q. N >= K >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the i-th column must contain the vector which
!          defines the elementary reflector H(i), for i = 1,2,...,k, as
!          returned by ZGEQRF in the first k columns of its array
!          argument A.
!          On exit, the M-by-N matrix Q.
!
!  LDA     (input) INTEGER
!          The first dimension of the array A. LDA >= max(1,M).
!
!  TAU     (input) COMPLEX*16 array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by ZGEQRF.
!
!  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK. LWORK >= max(1,N).
!          For optimum performance LWORK >= N*NB, where NB is the
!          optimal blocksize.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument has an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK,
     $                   LWKOPT, NB, NBMIN, NX
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNG2R
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      NB = ILAENV( 1, 'ZUNGQR', ' ', M, N, K, -1 )
      LWKOPT = MAX( 1, N )*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGQR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
!
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
!
!        Determine when to cross over from blocked to unblocked code.
!
         NX = MAX( 0, ILAENV( 3, 'ZUNGQR', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
!
!           Determine if workspace is large enough for blocked code.
!
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
!
!              Not enough workspace to use optimal NB:  reduce NB and
!              determine the minimum value of NB.
!
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'ZUNGQR', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
!
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
!
!        Use blocked code after the last block.
!        The first kk columns are handled by the block method.
!
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
!
!        Set A(1:kk,kk+1:n) to zero.
!
         DO 20 J = KK + 1, N
            DO 10 I = 1, KK
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
!
!     Use unblocked code for the last or only block.
!
      IF( KK.LT.N )
     $   CALL ZUNG2R( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
!
      IF( KK.GT.0 ) THEN
!
!        Use blocked code
!
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.N ) THEN
!
!              Form the triangular factor of the block reflector
!              H = H(i) H(i+1) . . . H(i+ib-1)
!
               CALL ZLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
!
!              Apply H to A(i:m,i+ib:n) from the left
!
               CALL ZLARFB( 'Left', 'No transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
!
!           Apply H to rows i:m of current block
!
            CALL ZUNG2R( M-I+1, IB, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
!
!           Set rows 1:i-1 of current block to zero
!
            DO 40 J = I, I + IB - 1
               DO 30 L = 1, I - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
!
      WORK( 1 ) = IWS
      RETURN
!
!     End of ZUNGQR
!
      END
!
!  =====================================================================
!
!> @brief  ZGGHRD
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,
!                          LDQ, Z, LDZ, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          COMPQ, COMPZ
!       INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, N
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
!> ZGGHRD reduces a pair of complex matrices (A,B) to generalized upper
!> Hessenberg form using unitary transformations, where A is a
!> general matrix and B is upper triangular.  The form of the
!> generalized eigenvalue problem is
!>    A*x = lambda*B*x,
!> and B is typically made upper triangular by computing its QR
!> factorization and moving the unitary matrix Q to the left side
!> of the equation.
!>
!> This subroutine simultaneously reduces A to a Hessenberg matrix H:
!>    Q**H*A*Z = H
!> and transforms B to another upper triangular matrix T:
!>    Q**H*B*Z = T
!> in order to reduce the problem to its standard form
!>    H*y = lambda*T*y
!> where y = Z**H*x.
!>
!> The unitary matrices Q and Z are determined as products of Givens
!> rotations.  They may either be formed explicitly, or they may be
!> postmultiplied into input matrices Q1 and Z1, so that
!>      Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
!>      Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
!> If Q1 is the unitary matrix from the QR factorization of B in the
!> original equation A*x = lambda*B*x, then ZGGHRD reduces the original
!> problem to generalized Hessenberg form.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] COMPQ
!> \verbatim
!>          COMPQ is CHARACTER*1
!>          = 'N': do not compute Q;
!>          = 'I': Q is initialized to the unit matrix, and the
!>                 unitary matrix Q is returned;
!>          = 'V': Q must contain a unitary matrix Q1 on entry,
!>                 and the product Q1*Q is returned.
!> \endverbatim
!>
!> @param[in] COMPZ
!> \verbatim
!>          COMPZ is CHARACTER*1
!>          = 'N': do not compute Q;
!>          = 'I': Q is initialized to the unit matrix, and the
!>                 unitary matrix Q is returned;
!>          = 'V': Q must contain a unitary matrix Q1 on entry,
!>                 and the product Q1*Q is returned.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrices A and B.  N >= 0.
!> \endverbatim
!>
!> @param[in] ILO
!> \verbatim
!>          ILO is INTEGER
!> \endverbatim
!>
!> @param[in] IHI
!> \verbatim
!>          IHI is INTEGER
!>
!>          ILO and IHI mark the rows and columns of A which are to be
!>          reduced.  It is assumed that A is already upper triangular
!>          in rows and columns 1:ILO-1 and IHI+1:N.  ILO and IHI are
!>          normally set by a previous call to ZGGBAL; otherwise they
!>          should be set to 1 and N respectively.
!>          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA, N)
!>          On entry, the N-by-N general matrix to be reduced.
!>          On exit, the upper triangle and the first subdiagonal of A
!>          are overwritten with the upper Hessenberg matrix H, and the
!>          rest is set to zero.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in,out] B
!> \verbatim
!>          B is COMPLEX*16 array, dimension (LDB, N)
!>          On entry, the N-by-N upper triangular matrix B.
!>          On exit, the upper triangular matrix T = Q**H B Z.  The
!>          elements below the diagonal are set to zero.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B.  LDB >= max(1,N).
!> \endverbatim
!>
!> @param[in,out] Q
!> \verbatim
!>          Q is COMPLEX*16 array, dimension (LDQ, N)
!>          On entry, if COMPQ = 'V', the unitary matrix Q1, typically
!>          from the QR factorization of B.
!>          On exit, if COMPQ='I', the unitary matrix Q, and if
!>          COMPQ = 'V', the product Q1*Q.
!>          Not referenced if COMPQ='N'.
!> \endverbatim
!>
!> @param[in] LDQ
!> \verbatim
!>          LDQ is INTEGER
!>          The leading dimension of the array Q.
!>          LDQ >= N if COMPQ='V' or 'I'; LDQ >= 1 otherwise.
!> \endverbatim
!>
!> @param[in,out] Z
!> \verbatim
!>          Z is COMPLEX*16 array, dimension (LDZ, N)
!>          On entry, if COMPZ = 'V', the unitary matrix Z1.
!>          On exit, if COMPZ='I', the unitary matrix Z, and if
!>          COMPZ = 'V', the product Z1*Z.
!>          Not referenced if COMPZ='N'.
!> \endverbatim
!>
!> @param[in] LDZ
!> \verbatim
!>          LDZ is INTEGER
!>          The leading dimension of the array Z.
!>          LDZ >= N if COMPZ='V' or 'I'; LDZ >= 1 otherwise.
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit.
!>          < 0:  if INFO = -i, the i-th argument had an illegal value.
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
!>  This routine reduces A to Hessenberg and B to triangular form by
!>  an unblocked reduction, as described in _Matrix_Computations_,
!>  by Golub and van Loan (Johns Hopkins Press).
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,
     $                   LDQ, Z, LDZ, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPZ
      INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
     $                   Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  ZGGHRD reduces a pair of complex matrices (A,B) to generalized upper
!  Hessenberg form using unitary transformations, where A is a
!  general matrix and B is upper triangular.  The form of the
!  generalized eigenvalue problem is
!     A*x = lambda*B*x,
!  and B is typically made upper triangular by computing its QR
!  factorization and moving the unitary matrix Q to the left side
!  of the equation.
!
!  This subroutine simultaneously reduces A to a Hessenberg matrix H:
!     Q**H*A*Z = H
!  and transforms B to another upper triangular matrix T:
!     Q**H*B*Z = T
!  in order to reduce the problem to its standard form
!     H*y = lambda*T*y
!  where y = Z**H*x.
!
!  The unitary matrices Q and Z are determined as products of Givens
!  rotations.  They may either be formed explicitly, or they may be
!  postmultiplied into input matrices Q1 and Z1, so that
!       Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
!       Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
!  If Q1 is the unitary matrix from the QR factorization of B in the
!  original equation A*x = lambda*B*x, then ZGGHRD reduces the original
!  problem to generalized Hessenberg form.
!
!  Arguments
!  =========
!
!  COMPQ   (input) CHARACTER*1
!          = 'N': do not compute Q;
!          = 'I': Q is initialized to the unit matrix, and the
!                 unitary matrix Q is returned;
!          = 'V': Q must contain a unitary matrix Q1 on entry,
!                 and the product Q1*Q is returned.
!
!  COMPZ   (input) CHARACTER*1
!          = 'N': do not compute Q;
!          = 'I': Q is initialized to the unit matrix, and the
!                 unitary matrix Q is returned;
!          = 'V': Q must contain a unitary matrix Q1 on entry,
!                 and the product Q1*Q is returned.
!
!  N       (input) INTEGER
!          The order of the matrices A and B.  N >= 0.
!
!  ILO     (input) INTEGER
!  IHI     (input) INTEGER
!          ILO and IHI mark the rows and columns of A which are to be
!          reduced.  It is assumed that A is already upper triangular
!          in rows and columns 1:ILO-1 and IHI+1:N.  ILO and IHI are
!          normally set by a previous call to ZGGBAL; otherwise they
!          should be set to 1 and N respectively.
!          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA, N)
!          On entry, the N-by-N general matrix to be reduced.
!          On exit, the upper triangle and the first subdiagonal of A
!          are overwritten with the upper Hessenberg matrix H, and the
!          rest is set to zero.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  B       (input/output) COMPLEX*16 array, dimension (LDB, N)
!          On entry, the N-by-N upper triangular matrix B.
!          On exit, the upper triangular matrix T = Q**H B Z.  The
!          elements below the diagonal are set to zero.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).
!
!  Q       (input/output) COMPLEX*16 array, dimension (LDQ, N)
!          On entry, if COMPQ = 'V', the unitary matrix Q1, typically
!          from the QR factorization of B.
!          On exit, if COMPQ='I', the unitary matrix Q, and if
!          COMPQ = 'V', the product Q1*Q.
!          Not referenced if COMPQ='N'.
!
!  LDQ     (input) INTEGER
!          The leading dimension of the array Q.
!          LDQ >= N if COMPQ='V' or 'I'; LDQ >= 1 otherwise.
!
!  Z       (input/output) COMPLEX*16 array, dimension (LDZ, N)
!          On entry, if COMPZ = 'V', the unitary matrix Z1.
!          On exit, if COMPZ='I', the unitary matrix Z, and if
!          COMPZ = 'V', the product Z1*Z.
!          Not referenced if COMPZ='N'.
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.
!          LDZ >= N if COMPZ='V' or 'I'; LDZ >= 1 otherwise.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!
!  Further Details
!  ===============
!
!  This routine reduces A to Hessenberg and B to triangular form by
!  an unblocked reduction, as described in _Matrix_Computations_,
!  by Golub and van Loan (Johns Hopkins Press).
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         CONE, CZERO
      PARAMETER          ( CONE = ( 1.0D+0, 0.0D+0 ),
     $                   CZERO = ( 0.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            ILQ, ILZ
      INTEGER            ICOMPQ, ICOMPZ, JCOL, JROW
      DOUBLE PRECISION   C
      COMPLEX*16         CTEMP, S
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARTG, ZLASET, ZROT
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
!     ..
!     .. Executable Statements ..
!
!     Decode COMPQ
!
      IF( LSAME( COMPQ, 'N' ) ) THEN
         ILQ = .FALSE.
         ICOMPQ = 1
      ELSE IF( LSAME( COMPQ, 'V' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 2
      ELSE IF( LSAME( COMPQ, 'I' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 3
      ELSE
         ICOMPQ = 0
      END IF
!
!     Decode COMPZ
!
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ILZ = .FALSE.
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 2
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 3
      ELSE
         ICOMPZ = 0
      END IF
!
!     Test the input parameters.
!
      INFO = 0
      IF( ICOMPQ.LE.0 ) THEN
         INFO = -1
      ELSE IF( ICOMPZ.LE.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ILO.LT.1 ) THEN
         INFO = -4
      ELSE IF( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( ( ILQ .AND. LDQ.LT.N ) .OR. LDQ.LT.1 ) THEN
         INFO = -11
      ELSE IF( ( ILZ .AND. LDZ.LT.N ) .OR. LDZ.LT.1 ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGGHRD', -INFO )
         RETURN
      END IF
!
!     Initialize Q and Z if desired.
!
      IF( ICOMPQ.EQ.3 )
     $   CALL ZLASET( 'Full', N, N, CZERO, CONE, Q, LDQ )
      IF( ICOMPZ.EQ.3 )
     $   CALL ZLASET( 'Full', N, N, CZERO, CONE, Z, LDZ )
!
!     Quick return if possible
!
      IF( N.LE.1 )
     $   RETURN
!
!     Zero out lower triangle of B
!
      DO 20 JCOL = 1, N - 1
         DO 10 JROW = JCOL + 1, N
            B( JROW, JCOL ) = CZERO
   10    CONTINUE
   20 CONTINUE
!
!     Reduce A and B
!
      DO 40 JCOL = ILO, IHI - 2
!
         DO 30 JROW = IHI, JCOL + 2, -1
!
!           Step 1: rotate rows JROW-1, JROW to kill A(JROW,JCOL)
!
            CTEMP = A( JROW-1, JCOL )
            CALL ZLARTG( CTEMP, A( JROW, JCOL ), C, S,
     $                   A( JROW-1, JCOL ) )
            A( JROW, JCOL ) = CZERO
            CALL ZROT( N-JCOL, A( JROW-1, JCOL+1 ), LDA,
     $                 A( JROW, JCOL+1 ), LDA, C, S )
            CALL ZROT( N+2-JROW, B( JROW-1, JROW-1 ), LDB,
     $                 B( JROW, JROW-1 ), LDB, C, S )
            IF( ILQ )
     $         CALL ZROT( N, Q( 1, JROW-1 ), 1, Q( 1, JROW ), 1, C,
     $                    DCONJG( S ) )
!
!           Step 2: rotate columns JROW, JROW-1 to kill B(JROW,JROW-1)
!
            CTEMP = B( JROW, JROW )
            CALL ZLARTG( CTEMP, B( JROW, JROW-1 ), C, S,
     $                   B( JROW, JROW ) )
            B( JROW, JROW-1 ) = CZERO
            CALL ZROT( IHI, A( 1, JROW ), 1, A( 1, JROW-1 ), 1, C, S )
            CALL ZROT( JROW-1, B( 1, JROW ), 1, B( 1, JROW-1 ), 1, C,
     $                 S )
            IF( ILZ )
     $         CALL ZROT( N, Z( 1, JROW ), 1, Z( 1, JROW-1 ), 1, C, S )
   30    CONTINUE
   40 CONTINUE
!
      RETURN
!
!     End of ZGGHRD
!
      END
!
!  =====================================================================
!
!> @brief  ZHGEQZ
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, H, LDH, T, LDT,
!                          ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,
!                          RWORK, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          COMPQ, COMPZ, JOB
!       INTEGER            IHI, ILO, INFO, LDH, LDQ, LDT, LDZ, LWORK, N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   RWORK( * )
!       COMPLEX*16         ALPHA( * ), BETA( * ), H( LDH, * ),
!      $                   Q( LDQ, * ), T( LDT, * ), WORK( * ),
!      $                   Z( LDZ, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZHGEQZ computes the eigenvalues of a complex matrix pair (H,T),
!> where H is an upper Hessenberg matrix and T is upper triangular,
!> using the single-shift QZ method.
!> Matrix pairs of this type are produced by the reduction to
!> generalized upper Hessenberg form of a complex matrix pair (A,B):
!>
!>    A = Q1*H*Z1**H,  B = Q1*T*Z1**H,
!>
!> as computed by ZGGHRD.
!>
!> If JOB='S', then the Hessenberg-triangular pair (H,T) is
!> also reduced to generalized Schur form,
!>
!>    H = Q*S*Z**H,  T = Q*P*Z**H,
!>
!> where Q and Z are unitary matrices and S and P are upper triangular.
!>
!> Optionally, the unitary matrix Q from the generalized Schur
!> factorization may be postmultiplied into an input matrix Q1, and the
!> unitary matrix Z may be postmultiplied into an input matrix Z1.
!> If Q1 and Z1 are the unitary matrices from ZGGHRD that reduced
!> the matrix pair (A,B) to generalized Hessenberg form, then the output
!> matrices Q1*Q and Z1*Z are the unitary factors from the generalized
!> Schur factorization of (A,B):
!>
!>    A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H.
!>
!> To avoid overflow, eigenvalues of the matrix pair (H,T)
!> (equivalently, of (A,B)) are computed as a pair of complex values
!> (alpha,beta).  If beta is nonzero, lambda = alpha / beta is an
!> eigenvalue of the generalized nonsymmetric eigenvalue problem (GNEP)
!>    A*x = lambda*B*x
!> and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
!> alternate form of the GNEP
!>    mu*A*y = B*y.
!> The values of alpha and beta for the i-th eigenvalue can be read
!> directly from the generalized Schur form:  alpha = S(i,i),
!> beta = P(i,i).
!>
!> Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix
!>      Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
!>      pp. 241--256.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] JOB
!> \verbatim
!>          JOB is CHARACTER*1
!>          = 'E': Compute eigenvalues only;
!>          = 'S': Computer eigenvalues and the Schur form.
!> \endverbatim
!>
!> @param[in] COMPQ
!> \verbatim
!>          COMPQ is CHARACTER*1
!>          = 'N': Left Schur vectors (Q) are not computed;
!>          = 'I': Q is initialized to the unit matrix and the matrix Q
!>                 of left Schur vectors of (H,T) is returned;
!>          = 'V': Q must contain a unitary matrix Q1 on entry and
!>                 the product Q1*Q is returned.
!> \endverbatim
!>
!> @param[in] COMPZ
!> \verbatim
!>          COMPZ is CHARACTER*1
!>          = 'N': Right Schur vectors (Z) are not computed;
!>          = 'I': Q is initialized to the unit matrix and the matrix Z
!>                 of right Schur vectors of (H,T) is returned;
!>          = 'V': Z must contain a unitary matrix Z1 on entry and
!>                 the product Z1*Z is returned.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrices H, T, Q, and Z.  N >= 0.
!> \endverbatim
!>
!> @param[in] ILO
!> \verbatim
!>          ILO is INTEGER
!> \endverbatim
!>
!> @param[in] IHI
!> \verbatim
!>          IHI is INTEGER
!>          ILO and IHI mark the rows and columns of H which are in
!>          Hessenberg form.  It is assumed that A is already upper
!>          triangular in rows and columns 1:ILO-1 and IHI+1:N.
!>          If N > 0, 1 <= ILO <= IHI <= N; if N = 0, ILO=1 and IHI=0.
!> \endverbatim
!>
!> @param[in,out] H
!> \verbatim
!>          H is COMPLEX*16 array, dimension (LDH, N)
!>          On entry, the N-by-N upper Hessenberg matrix H.
!>          On exit, if JOB = 'S', H contains the upper triangular
!>          matrix S from the generalized Schur factorization.
!>          If JOB = 'E', the diagonal of H matches that of S, but
!>          the rest of H is unspecified.
!> \endverbatim
!>
!> @param[in] LDH
!> \verbatim
!>          LDH is INTEGER
!>          The leading dimension of the array H.  LDH >= max( 1, N ).
!> \endverbatim
!>
!> @param[in,out] T
!> \verbatim
!>          T is COMPLEX*16 array, dimension (LDT, N)
!>          On entry, the N-by-N upper triangular matrix T.
!>          On exit, if JOB = 'S', T contains the upper triangular
!>          matrix P from the generalized Schur factorization.
!>          If JOB = 'E', the diagonal of T matches that of P, but
!>          the rest of T is unspecified.
!> \endverbatim
!>
!> @param[in] LDT
!> \verbatim
!>          LDT is INTEGER
!>          The leading dimension of the array T.  LDT >= max( 1, N ).
!> \endverbatim
!>
!> @param[out] ALPHA
!> \verbatim
!>          ALPHA is COMPLEX*16 array, dimension (N)
!>          The complex scalars alpha that define the eigenvalues of
!>          GNEP.  ALPHA(i) = S(i,i) in the generalized Schur
!>          factorization.
!> \endverbatim
!>
!> @param[out] BETA
!> \verbatim
!>          BETA is COMPLEX*16 array, dimension (N)
!>          The real non-negative scalars beta that define the
!>          eigenvalues of GNEP.  BETA(i) = P(i,i) in the generalized
!>          Schur factorization.
!>
!>          Together, the quantities alpha = ALPHA(j) and beta = BETA(j)
!>          represent the j-th eigenvalue of the matrix pair (A,B), in
!>          one of the forms lambda = alpha/beta or mu = beta/alpha.
!>          Since either lambda or mu may overflow, they should not,
!>          in general, be computed.
!> \endverbatim
!>
!> @param[in,out] Q
!> \verbatim
!>          Q is COMPLEX*16 array, dimension (LDQ, N)
!>          On entry, if COMPZ = 'V', the unitary matrix Q1 used in the
!>          reduction of (A,B) to generalized Hessenberg form.
!>          On exit, if COMPZ = 'I', the unitary matrix of left Schur
!>          vectors of (H,T), and if COMPZ = 'V', the unitary matrix of
!>          left Schur vectors of (A,B).
!>          Not referenced if COMPZ = 'N'.
!> \endverbatim
!>
!> @param[in] LDQ
!> \verbatim
!>          LDQ is INTEGER
!>          The leading dimension of the array Q.  LDQ >= 1.
!>          If COMPQ='V' or 'I', then LDQ >= N.
!> \endverbatim
!>
!> @param[in,out] Z
!> \verbatim
!>          Z is COMPLEX*16 array, dimension (LDZ, N)
!>          On entry, if COMPZ = 'V', the unitary matrix Z1 used in the
!>          reduction of (A,B) to generalized Hessenberg form.
!>          On exit, if COMPZ = 'I', the unitary matrix of right Schur
!>          vectors of (H,T), and if COMPZ = 'V', the unitary matrix of
!>          right Schur vectors of (A,B).
!>          Not referenced if COMPZ = 'N'.
!> \endverbatim
!>
!> @param[in] LDZ
!> \verbatim
!>          LDZ is INTEGER
!>          The leading dimension of the array Z.  LDZ >= 1.
!>          If COMPZ='V' or 'I', then LDZ >= N.
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension (MAX(1,LWORK))
!>          On exit, if INFO >= 0, WORK(1) returns the optimal LWORK.
!> \endverbatim
!>
!> @param[in] LWORK
!> \verbatim
!>          LWORK is INTEGER
!>          The dimension of the array WORK.  LWORK >= max(1,N).
!>
!>          If LWORK = -1, then a workspace query is assumed; the routine
!>          only calculates the optimal size of the WORK array, returns
!>          this value as the first entry of the WORK array, and no error
!>          message related to LWORK is issued by XERBLA.
!> \endverbatim
!>
!> @param[out] RWORK
!> \verbatim
!>          RWORK is DOUBLE PRECISION array, dimension (N)
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = 1,...,N: the QZ iteration did not converge.  (H,T) is not
!>                     in Schur form, but ALPHA(i) and BETA(i),
!>                     i=INFO+1,...,N should be correct.
!>          = N+1,...,2*N: the shift calculation failed.  (H,T) is not
!>                     in Schur form, but ALPHA(i) and BETA(i),
!>                     i=INFO-N+1,...,N should be correct.
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
!>  We assume that complex ABS works as long as its value is less than
!>  overflow.
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, H, LDH, T, LDT,
     $                   ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,
     $                   RWORK, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPZ, JOB
      INTEGER            IHI, ILO, INFO, LDH, LDQ, LDT, LDZ, LWORK, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         ALPHA( * ), BETA( * ), H( LDH, * ),
     $                   Q( LDQ, * ), T( LDT, * ), WORK( * ),
     $                   Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  ZHGEQZ computes the eigenvalues of a complex matrix pair (H,T),
!  where H is an upper Hessenberg matrix and T is upper triangular,
!  using the single-shift QZ method.
!  Matrix pairs of this type are produced by the reduction to
!  generalized upper Hessenberg form of a complex matrix pair (A,B):
!
!     A = Q1*H*Z1**H,  B = Q1*T*Z1**H,
!
!  as computed by ZGGHRD.
!
!  If JOB='S', then the Hessenberg-triangular pair (H,T) is
!  also reduced to generalized Schur form,
!
!     H = Q*S*Z**H,  T = Q*P*Z**H,
!
!  where Q and Z are unitary matrices and S and P are upper triangular.
!
!  Optionally, the unitary matrix Q from the generalized Schur
!  factorization may be postmultiplied into an input matrix Q1, and the
!  unitary matrix Z may be postmultiplied into an input matrix Z1.
!  If Q1 and Z1 are the unitary matrices from ZGGHRD that reduced
!  the matrix pair (A,B) to generalized Hessenberg form, then the output
!  matrices Q1*Q and Z1*Z are the unitary factors from the generalized
!  Schur factorization of (A,B):
!
!     A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H.
!
!  To avoid overflow, eigenvalues of the matrix pair (H,T)
!  (equivalently, of (A,B)) are computed as a pair of complex values
!  (alpha,beta).  If beta is nonzero, lambda = alpha / beta is an
!  eigenvalue of the generalized nonsymmetric eigenvalue problem (GNEP)
!     A*x = lambda*B*x
!  and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
!  alternate form of the GNEP
!     mu*A*y = B*y.
!  The values of alpha and beta for the i-th eigenvalue can be read
!  directly from the generalized Schur form:  alpha = S(i,i),
!  beta = P(i,i).
!
!  Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix
!       Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
!       pp. 241--256.
!
!  Arguments
!  =========
!
!  JOB     (input) CHARACTER*1
!          = 'E': Compute eigenvalues only;
!          = 'S': Computer eigenvalues and the Schur form.
!
!  COMPQ   (input) CHARACTER*1
!          = 'N': Left Schur vectors (Q) are not computed;
!          = 'I': Q is initialized to the unit matrix and the matrix Q
!                 of left Schur vectors of (H,T) is returned;
!          = 'V': Q must contain a unitary matrix Q1 on entry and
!                 the product Q1*Q is returned.
!
!  COMPZ   (input) CHARACTER*1
!          = 'N': Right Schur vectors (Z) are not computed;
!          = 'I': Q is initialized to the unit matrix and the matrix Z
!                 of right Schur vectors of (H,T) is returned;
!          = 'V': Z must contain a unitary matrix Z1 on entry and
!                 the product Z1*Z is returned.
!
!  N       (input) INTEGER
!          The order of the matrices H, T, Q, and Z.  N >= 0.
!
!  ILO     (input) INTEGER
!  IHI     (input) INTEGER
!          ILO and IHI mark the rows and columns of H which are in
!          Hessenberg form.  It is assumed that A is already upper
!          triangular in rows and columns 1:ILO-1 and IHI+1:N.
!          If N > 0, 1 <= ILO <= IHI <= N; if N = 0, ILO=1 and IHI=0.
!
!  H       (input/output) COMPLEX*16 array, dimension (LDH, N)
!          On entry, the N-by-N upper Hessenberg matrix H.
!          On exit, if JOB = 'S', H contains the upper triangular
!          matrix S from the generalized Schur factorization.
!          If JOB = 'E', the diagonal of H matches that of S, but
!          the rest of H is unspecified.
!
!  LDH     (input) INTEGER
!          The leading dimension of the array H.  LDH >= max( 1, N ).
!
!  T       (input/output) COMPLEX*16 array, dimension (LDT, N)
!          On entry, the N-by-N upper triangular matrix T.
!          On exit, if JOB = 'S', T contains the upper triangular
!          matrix P from the generalized Schur factorization.
!          If JOB = 'E', the diagonal of T matches that of P, but
!          the rest of T is unspecified.
!
!  LDT     (input) INTEGER
!          The leading dimension of the array T.  LDT >= max( 1, N ).
!
!  ALPHA   (output) COMPLEX*16 array, dimension (N)
!          The complex scalars alpha that define the eigenvalues of
!          GNEP.  ALPHA(i) = S(i,i) in the generalized Schur
!          factorization.
!
!  BETA    (output) COMPLEX*16 array, dimension (N)
!          The real non-negative scalars beta that define the
!          eigenvalues of GNEP.  BETA(i) = P(i,i) in the generalized
!          Schur factorization.
!
!          Together, the quantities alpha = ALPHA(j) and beta = BETA(j)
!          represent the j-th eigenvalue of the matrix pair (A,B), in
!          one of the forms lambda = alpha/beta or mu = beta/alpha.
!          Since either lambda or mu may overflow, they should not,
!          in general, be computed.
!
!  Q       (input/output) COMPLEX*16 array, dimension (LDQ, N)
!          On entry, if COMPZ = 'V', the unitary matrix Q1 used in the
!          reduction of (A,B) to generalized Hessenberg form.
!          On exit, if COMPZ = 'I', the unitary matrix of left Schur
!          vectors of (H,T), and if COMPZ = 'V', the unitary matrix of
!          left Schur vectors of (A,B).
!          Not referenced if COMPZ = 'N'.
!
!  LDQ     (input) INTEGER
!          The leading dimension of the array Q.  LDQ >= 1.
!          If COMPQ='V' or 'I', then LDQ >= N.
!
!  Z       (input/output) COMPLEX*16 array, dimension (LDZ, N)
!          On entry, if COMPZ = 'V', the unitary matrix Z1 used in the
!          reduction of (A,B) to generalized Hessenberg form.
!          On exit, if COMPZ = 'I', the unitary matrix of right Schur
!          vectors of (H,T), and if COMPZ = 'V', the unitary matrix of
!          right Schur vectors of (A,B).
!          Not referenced if COMPZ = 'N'.
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDZ >= 1.
!          If COMPZ='V' or 'I', then LDZ >= N.
!
!  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK))
!          On exit, if INFO >= 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.  LWORK >= max(1,N).
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          = 1,...,N: the QZ iteration did not converge.  (H,T) is not
!                     in Schur form, but ALPHA(i) and BETA(i),
!                     i=INFO+1,...,N should be correct.
!          = N+1,...,2*N: the shift calculation failed.  (H,T) is not
!                     in Schur form, but ALPHA(i) and BETA(i),
!                     i=INFO-N+1,...,N should be correct.
!
!  Further Details
!  ===============
!
!  We assume that complex ABS works as long as its value is less than
!  overflow.
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                   CONE = ( 1.0D+0, 0.0D+0 ) )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            ILAZR2, ILAZRO, ILQ, ILSCHR, ILZ, LQUERY
      INTEGER            ICOMPQ, ICOMPZ, IFIRST, IFRSTM, IITER, ILAST,
     $                   ILASTM, IN, ISCHUR, ISTART, J, JC, JCH, JITER,
     $                   JR, MAXIT
      DOUBLE PRECISION   ABSB, ANORM, ASCALE, ATOL, BNORM, BSCALE, BTOL,
     $                   C, SAFMIN, TEMP, TEMP2, TEMPR, ULP
      COMPLEX*16         ABI22, AD11, AD12, AD21, AD22, CTEMP, CTEMP2,
     $                   CTEMP3, ESHIFT, RTDISC, S, SHIFT, SIGNBC, T1,
     $                   U12, X
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, ZLANHS
      EXTERNAL           LSAME, DLAMCH, ZLANHS
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARTG, ZLASET, ZROT, ZSCAL
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, MAX, MIN,
     $                   SQRT
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION   ABS1
!     ..
!     .. Statement Function definitions ..
      ABS1( X ) = ABS( DBLE( X ) ) + ABS( DIMAG( X ) )
!     ..
!     .. Executable Statements ..
!
!     Decode JOB, COMPQ, COMPZ
!
      IF( LSAME( JOB, 'E' ) ) THEN
         ILSCHR = .FALSE.
         ISCHUR = 1
      ELSE IF( LSAME( JOB, 'S' ) ) THEN
         ILSCHR = .TRUE.
         ISCHUR = 2
      ELSE
         ISCHUR = 0
      END IF
!
      IF( LSAME( COMPQ, 'N' ) ) THEN
         ILQ = .FALSE.
         ICOMPQ = 1
      ELSE IF( LSAME( COMPQ, 'V' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 2
      ELSE IF( LSAME( COMPQ, 'I' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 3
      ELSE
         ICOMPQ = 0
      END IF
!
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ILZ = .FALSE.
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 2
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 3
      ELSE
         ICOMPZ = 0
      END IF
!
!     Check Argument Values
!
      INFO = 0
      WORK( 1 ) = MAX( 1, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( ISCHUR.EQ.0 ) THEN
         INFO = -1
      ELSE IF( ICOMPQ.EQ.0 ) THEN
         INFO = -2
      ELSE IF( ICOMPZ.EQ.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( ILO.LT.1 ) THEN
         INFO = -5
      ELSE IF( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN
         INFO = -6
      ELSE IF( LDH.LT.N ) THEN
         INFO = -8
      ELSE IF( LDT.LT.N ) THEN
         INFO = -10
      ELSE IF( LDQ.LT.1 .OR. ( ILQ .AND. LDQ.LT.N ) ) THEN
         INFO = -14
      ELSE IF( LDZ.LT.1 .OR. ( ILZ .AND. LDZ.LT.N ) ) THEN
         INFO = -16
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -18
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZHGEQZ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
!     WORK( 1 ) = CMPLX( 1 )
      IF( N.LE.0 ) THEN
         WORK( 1 ) = DCMPLX( 1 )
         RETURN
      END IF
!
!     Initialize Q and Z
!
      IF( ICOMPQ.EQ.3 )
     $   CALL ZLASET( 'Full', N, N, CZERO, CONE, Q, LDQ )
      IF( ICOMPZ.EQ.3 )
     $   CALL ZLASET( 'Full', N, N, CZERO, CONE, Z, LDZ )
!
!     Machine Constants
!
      IN = IHI + 1 - ILO
      SAFMIN = DLAMCH( 'S' )
      ULP = DLAMCH( 'E' )*DLAMCH( 'B' )
      ANORM = ZLANHS( 'F', IN, H( ILO, ILO ), LDH, RWORK )
      BNORM = ZLANHS( 'F', IN, T( ILO, ILO ), LDT, RWORK )
      ATOL = MAX( SAFMIN, ULP*ANORM )
      BTOL = MAX( SAFMIN, ULP*BNORM )
      ASCALE = ONE / MAX( SAFMIN, ANORM )
      BSCALE = ONE / MAX( SAFMIN, BNORM )
!
!
!     Set Eigenvalues IHI+1:N
!
      DO 10 J = IHI + 1, N
         ABSB = ABS( T( J, J ) )
         IF( ABSB.GT.SAFMIN ) THEN
            SIGNBC = DCONJG( T( J, J ) / ABSB )
            T( J, J ) = ABSB
            IF( ILSCHR ) THEN
               CALL ZSCAL( J-1, SIGNBC, T( 1, J ), 1 )
               CALL ZSCAL( J, SIGNBC, H( 1, J ), 1 )
            ELSE
               H( J, J ) = H( J, J )*SIGNBC
            END IF
            IF( ILZ )
     $         CALL ZSCAL( N, SIGNBC, Z( 1, J ), 1 )
         ELSE
            T( J, J ) = CZERO
         END IF
         ALPHA( J ) = H( J, J )
         BETA( J ) = T( J, J )
   10 CONTINUE
!
!     If IHI < ILO, skip QZ steps
!
      IF( IHI.LT.ILO )
     $   GO TO 190
!
!     MAIN QZ ITERATION LOOP
!
!     Initialize dynamic indices
!
!     Eigenvalues ILAST+1:N have been found.
!        Column operations modify rows IFRSTM:whatever
!        Row operations modify columns whatever:ILASTM
!
!     If only eigenvalues are being computed, then
!        IFRSTM is the row of the last splitting row above row ILAST;
!        this is always at least ILO.
!     IITER counts iterations since the last eigenvalue was found,
!        to tell when to use an extraordinary shift.
!     MAXIT is the maximum number of QZ sweeps allowed.
!
      ILAST = IHI
      IF( ILSCHR ) THEN
         IFRSTM = 1
         ILASTM = N
      ELSE
         IFRSTM = ILO
         ILASTM = IHI
      END IF
      IITER = 0
      ESHIFT = CZERO
      MAXIT = 30*( IHI-ILO+1 )
!
      DO 170 JITER = 1, MAXIT
!
!        Check for too many iterations.
!
         IF( JITER.GT.MAXIT )
     $      GO TO 180
!
!        Split the matrix if possible.
!
!        Two tests:
!           1: H(j,j-1)=0  or  j=ILO
!           2: T(j,j)=0
!
!        Special case: j=ILAST
!
         IF( ILAST.EQ.ILO ) THEN
            GO TO 60
         ELSE
            IF( ABS1( H( ILAST, ILAST-1 ) ).LE.ATOL ) THEN
               H( ILAST, ILAST-1 ) = CZERO
               GO TO 60
            END IF
         END IF
!
         IF( ABS( T( ILAST, ILAST ) ).LE.BTOL ) THEN
            T( ILAST, ILAST ) = CZERO
            GO TO 50
         END IF
!
!        General case: j<ILAST
!
         DO 40 J = ILAST - 1, ILO, -1
!
!           Test 1: for H(j,j-1)=0 or j=ILO
!
            IF( J.EQ.ILO ) THEN
               ILAZRO = .TRUE.
            ELSE
               IF( ABS1( H( J, J-1 ) ).LE.ATOL ) THEN
                  H( J, J-1 ) = CZERO
                  ILAZRO = .TRUE.
               ELSE
                  ILAZRO = .FALSE.
               END IF
            END IF
!
!           Test 2: for T(j,j)=0
!
            IF( ABS( T( J, J ) ).LT.BTOL ) THEN
               T( J, J ) = CZERO
!
!              Test 1a: Check for 2 consecutive small subdiagonals in A
!
               ILAZR2 = .FALSE.
               IF( .NOT.ILAZRO ) THEN
                  IF( ABS1( H( J, J-1 ) )*( ASCALE*ABS1( H( J+1,
     $                J ) ) ).LE.ABS1( H( J, J ) )*( ASCALE*ATOL ) )
     $                ILAZR2 = .TRUE.
               END IF
!
!              If both tests pass (1 & 2), i.e., the leading diagonal
!              element of B in the block is zero, split a 1x1 block off
!              at the top. (I.e., at the J-th row/column) The leading
!              diagonal element of the remainder can also be zero, so
!              this may have to be done repeatedly.
!
               IF( ILAZRO .OR. ILAZR2 ) THEN
                  DO 20 JCH = J, ILAST - 1
                     CTEMP = H( JCH, JCH )
                     CALL ZLARTG( CTEMP, H( JCH+1, JCH ), C, S,
     $                            H( JCH, JCH ) )
                     H( JCH+1, JCH ) = CZERO
                     CALL ZROT( ILASTM-JCH, H( JCH, JCH+1 ), LDH,
     $                          H( JCH+1, JCH+1 ), LDH, C, S )
                     CALL ZROT( ILASTM-JCH, T( JCH, JCH+1 ), LDT,
     $                          T( JCH+1, JCH+1 ), LDT, C, S )
                     IF( ILQ )
     $                  CALL ZROT( N, Q( 1, JCH ), 1, Q( 1, JCH+1 ), 1,
     $                             C, DCONJG( S ) )
                     IF( ILAZR2 )
     $                  H( JCH, JCH-1 ) = H( JCH, JCH-1 )*C
                     ILAZR2 = .FALSE.
                     IF( ABS1( T( JCH+1, JCH+1 ) ).GE.BTOL ) THEN
                        IF( JCH+1.GE.ILAST ) THEN
                           GO TO 60
                        ELSE
                           IFIRST = JCH + 1
                           GO TO 70
                        END IF
                     END IF
                     T( JCH+1, JCH+1 ) = CZERO
   20             CONTINUE
                  GO TO 50
               ELSE
!
!                 Only test 2 passed -- chase the zero to T(ILAST,ILAST)
!                 Then process as in the case T(ILAST,ILAST)=0
!
                  DO 30 JCH = J, ILAST - 1
                     CTEMP = T( JCH, JCH+1 )
                     CALL ZLARTG( CTEMP, T( JCH+1, JCH+1 ), C, S,
     $                            T( JCH, JCH+1 ) )
                     T( JCH+1, JCH+1 ) = CZERO
                     IF( JCH.LT.ILASTM-1 )
     $                  CALL ZROT( ILASTM-JCH-1, T( JCH, JCH+2 ), LDT,
     $                             T( JCH+1, JCH+2 ), LDT, C, S )
                     CALL ZROT( ILASTM-JCH+2, H( JCH, JCH-1 ), LDH,
     $                          H( JCH+1, JCH-1 ), LDH, C, S )
                     IF( ILQ )
     $                  CALL ZROT( N, Q( 1, JCH ), 1, Q( 1, JCH+1 ), 1,
     $                             C, DCONJG( S ) )
                     CTEMP = H( JCH+1, JCH )
                     CALL ZLARTG( CTEMP, H( JCH+1, JCH-1 ), C, S,
     $                            H( JCH+1, JCH ) )
                     H( JCH+1, JCH-1 ) = CZERO
                     CALL ZROT( JCH+1-IFRSTM, H( IFRSTM, JCH ), 1,
     $                          H( IFRSTM, JCH-1 ), 1, C, S )
                     CALL ZROT( JCH-IFRSTM, T( IFRSTM, JCH ), 1,
     $                          T( IFRSTM, JCH-1 ), 1, C, S )
                     IF( ILZ )
     $                  CALL ZROT( N, Z( 1, JCH ), 1, Z( 1, JCH-1 ), 1,
     $                             C, S )
   30             CONTINUE
                  GO TO 50
               END IF
            ELSE IF( ILAZRO ) THEN
!
!              Only test 1 passed -- work on J:ILAST
!
               IFIRST = J
               GO TO 70
            END IF
!
!           Neither test passed -- try next J
!
   40    CONTINUE
!
!        (Drop-through is "impossible")
!
         INFO = 2*N + 1
         GO TO 210
!
!        T(ILAST,ILAST)=0 -- clear H(ILAST,ILAST-1) to split off a
!        1x1 block.
!
   50    CONTINUE
         CTEMP = H( ILAST, ILAST )
         CALL ZLARTG( CTEMP, H( ILAST, ILAST-1 ), C, S,
     $                H( ILAST, ILAST ) )
         H( ILAST, ILAST-1 ) = CZERO
         CALL ZROT( ILAST-IFRSTM, H( IFRSTM, ILAST ), 1,
     $              H( IFRSTM, ILAST-1 ), 1, C, S )
         CALL ZROT( ILAST-IFRSTM, T( IFRSTM, ILAST ), 1,
     $              T( IFRSTM, ILAST-1 ), 1, C, S )
         IF( ILZ )
     $      CALL ZROT( N, Z( 1, ILAST ), 1, Z( 1, ILAST-1 ), 1, C, S )
!
!        H(ILAST,ILAST-1)=0 -- Standardize B, set ALPHA and BETA
!
   60    CONTINUE
         ABSB = ABS( T( ILAST, ILAST ) )
         IF( ABSB.GT.SAFMIN ) THEN
            SIGNBC = DCONJG( T( ILAST, ILAST ) / ABSB )
            T( ILAST, ILAST ) = ABSB
            IF( ILSCHR ) THEN
               CALL ZSCAL( ILAST-IFRSTM, SIGNBC, T( IFRSTM, ILAST ), 1 )
               CALL ZSCAL( ILAST+1-IFRSTM, SIGNBC, H( IFRSTM, ILAST ),
     $                     1 )
            ELSE
               H( ILAST, ILAST ) = H( ILAST, ILAST )*SIGNBC
            END IF
            IF( ILZ )
     $         CALL ZSCAL( N, SIGNBC, Z( 1, ILAST ), 1 )
         ELSE
            T( ILAST, ILAST ) = CZERO
         END IF
         ALPHA( ILAST ) = H( ILAST, ILAST )
         BETA( ILAST ) = T( ILAST, ILAST )
!
!        Go to next block -- exit if finished.
!
         ILAST = ILAST - 1
         IF( ILAST.LT.ILO )
     $      GO TO 190
!
!        Reset counters
!
         IITER = 0
         ESHIFT = CZERO
         IF( .NOT.ILSCHR ) THEN
            ILASTM = ILAST
            IF( IFRSTM.GT.ILAST )
     $         IFRSTM = ILO
         END IF
         GO TO 160
!
!        QZ step
!
!        This iteration only involves rows/columns IFIRST:ILAST.  We
!        assume IFIRST < ILAST, and that the diagonal of B is non-zero.
!
   70    CONTINUE
         IITER = IITER + 1
         IF( .NOT.ILSCHR ) THEN
            IFRSTM = IFIRST
         END IF
!
!        Compute the Shift.
!
!        At this point, IFIRST < ILAST, and the diagonal elements of
!        T(IFIRST:ILAST,IFIRST,ILAST) are larger than BTOL (in
!        magnitude)
!
         IF( ( IITER / 10 )*10.NE.IITER ) THEN
!
!           The Wilkinson shift (AEP p.512), i.e., the eigenvalue of
!           the bottom-right 2x2 block of A inv(B) which is nearest to
!           the bottom-right element.
!
!           We factor B as U*D, where U has unit diagonals, and
!           compute (A*inv(D))*inv(U).
!
            U12 = ( BSCALE*T( ILAST-1, ILAST ) ) /
     $            ( BSCALE*T( ILAST, ILAST ) )
            AD11 = ( ASCALE*H( ILAST-1, ILAST-1 ) ) /
     $             ( BSCALE*T( ILAST-1, ILAST-1 ) )
            AD21 = ( ASCALE*H( ILAST, ILAST-1 ) ) /
     $             ( BSCALE*T( ILAST-1, ILAST-1 ) )
            AD12 = ( ASCALE*H( ILAST-1, ILAST ) ) /
     $             ( BSCALE*T( ILAST, ILAST ) )
            AD22 = ( ASCALE*H( ILAST, ILAST ) ) /
     $             ( BSCALE*T( ILAST, ILAST ) )
            ABI22 = AD22 - U12*AD21
!
            T1 = HALF*( AD11+ABI22 )
            RTDISC = SQRT( T1**2+AD12*AD21-AD11*AD22 )
            TEMP = DBLE( T1-ABI22 )*DBLE( RTDISC ) +
     $             DIMAG( T1-ABI22 )*DIMAG( RTDISC )
            IF( TEMP.LE.ZERO ) THEN
               SHIFT = T1 + RTDISC
            ELSE
               SHIFT = T1 - RTDISC
            END IF
         ELSE
!
!           Exceptional shift.  Chosen for no particularly good reason.
!
            ESHIFT = ESHIFT + DCONJG( ( ASCALE*H( ILAST-1, ILAST ) ) /
     $               ( BSCALE*T( ILAST-1, ILAST-1 ) ) )
            SHIFT = ESHIFT
         END IF
!
!        Now check for two consecutive small subdiagonals.
!
         DO 80 J = ILAST - 1, IFIRST + 1, -1
            ISTART = J
            CTEMP = ASCALE*H( J, J ) - SHIFT*( BSCALE*T( J, J ) )
            TEMP = ABS1( CTEMP )
            TEMP2 = ASCALE*ABS1( H( J+1, J ) )
            TEMPR = MAX( TEMP, TEMP2 )
            IF( TEMPR.LT.ONE .AND. TEMPR.NE.ZERO ) THEN
               TEMP = TEMP / TEMPR
               TEMP2 = TEMP2 / TEMPR
            END IF
            IF( ABS1( H( J, J-1 ) )*TEMP2.LE.TEMP*ATOL )
     $         GO TO 90
   80    CONTINUE
!
         ISTART = IFIRST
         CTEMP = ASCALE*H( IFIRST, IFIRST ) -
     $           SHIFT*( BSCALE*T( IFIRST, IFIRST ) )
   90    CONTINUE
!
!        Do an implicit-shift QZ sweep.
!
!        Initial Q
!
         CTEMP2 = ASCALE*H( ISTART+1, ISTART )
         CALL ZLARTG( CTEMP, CTEMP2, C, S, CTEMP3 )
!
!        Sweep
!
         DO 150 J = ISTART, ILAST - 1
            IF( J.GT.ISTART ) THEN
               CTEMP = H( J, J-1 )
               CALL ZLARTG( CTEMP, H( J+1, J-1 ), C, S, H( J, J-1 ) )
               H( J+1, J-1 ) = CZERO
            END IF
!
            DO 100 JC = J, ILASTM
               CTEMP = C*H( J, JC ) + S*H( J+1, JC )
               H( J+1, JC ) = -DCONJG( S )*H( J, JC ) + C*H( J+1, JC )
               H( J, JC ) = CTEMP
               CTEMP2 = C*T( J, JC ) + S*T( J+1, JC )
               T( J+1, JC ) = -DCONJG( S )*T( J, JC ) + C*T( J+1, JC )
               T( J, JC ) = CTEMP2
  100       CONTINUE
            IF( ILQ ) THEN
               DO 110 JR = 1, N
                  CTEMP = C*Q( JR, J ) + DCONJG( S )*Q( JR, J+1 )
                  Q( JR, J+1 ) = -S*Q( JR, J ) + C*Q( JR, J+1 )
                  Q( JR, J ) = CTEMP
  110          CONTINUE
            END IF
!
            CTEMP = T( J+1, J+1 )
            CALL ZLARTG( CTEMP, T( J+1, J ), C, S, T( J+1, J+1 ) )
            T( J+1, J ) = CZERO
!
            DO 120 JR = IFRSTM, MIN( J+2, ILAST )
               CTEMP = C*H( JR, J+1 ) + S*H( JR, J )
               H( JR, J ) = -DCONJG( S )*H( JR, J+1 ) + C*H( JR, J )
               H( JR, J+1 ) = CTEMP
  120       CONTINUE
            DO 130 JR = IFRSTM, J
               CTEMP = C*T( JR, J+1 ) + S*T( JR, J )
               T( JR, J ) = -DCONJG( S )*T( JR, J+1 ) + C*T( JR, J )
               T( JR, J+1 ) = CTEMP
  130       CONTINUE
            IF( ILZ ) THEN
               DO 140 JR = 1, N
                  CTEMP = C*Z( JR, J+1 ) + S*Z( JR, J )
                  Z( JR, J ) = -DCONJG( S )*Z( JR, J+1 ) + C*Z( JR, J )
                  Z( JR, J+1 ) = CTEMP
  140          CONTINUE
            END IF
  150    CONTINUE
!
  160    CONTINUE
!
  170 CONTINUE
!
!     Drop-through = non-convergence
!
  180 CONTINUE
      INFO = ILAST
      GO TO 210
!
!     Successful completion of all QZ steps
!
  190 CONTINUE
!
!     Set Eigenvalues 1:ILO-1
!
      DO 200 J = 1, ILO - 1
         ABSB = ABS( T( J, J ) )
         IF( ABSB.GT.SAFMIN ) THEN
            SIGNBC = DCONJG( T( J, J ) / ABSB )
            T( J, J ) = ABSB
            IF( ILSCHR ) THEN
               CALL ZSCAL( J-1, SIGNBC, T( 1, J ), 1 )
               CALL ZSCAL( J, SIGNBC, H( 1, J ), 1 )
            ELSE
               H( J, J ) = H( J, J )*SIGNBC
            END IF
            IF( ILZ )
     $         CALL ZSCAL( N, SIGNBC, Z( 1, J ), 1 )
         ELSE
            T( J, J ) = CZERO
         END IF
         ALPHA( J ) = H( J, J )
         BETA( J ) = T( J, J )
  200 CONTINUE
!
!     Normal Termination
!
      INFO = 0
!
!     Exit (other than argument error) -- return optimal workspace size
!
  210 CONTINUE
      WORK( 1 ) = DCMPLX( N )
      RETURN
!
!     End of ZHGEQZ
!
      END
!
!  =====================================================================
!
!> @brief  ZGGBAK
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGGBAK( JOB, SIDE, N, ILO, IHI, LSCALE, RSCALE, M, V,
!                          LDV, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          JOB, SIDE
!       INTEGER            IHI, ILO, INFO, LDV, M, N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   LSCALE( * ), RSCALE( * )
!       COMPLEX*16         V( LDV, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGGBAK forms the right or left eigenvectors of a complex generalized
!> eigenvalue problem A*x = lambda*B*x, by backward transformation on
!> the computed eigenvectors of the balanced pair of matrices output by
!> ZGGBAL.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] JOB
!> \verbatim
!>          JOB is CHARACTER*1
!>          Specifies the type of backward transformation required:
!>          = 'N':  do nothing, return immediately;
!>          = 'P':  do backward transformation for permutation only;
!>          = 'S':  do backward transformation for scaling only;
!>          = 'B':  do backward transformations for both permutation and
!>                  scaling.
!>          JOB must be the same as the argument JOB supplied to ZGGBAL.
!> \endverbatim
!>
!> @param[in] SIDE
!> \verbatim
!>          SIDE is CHARACTER*1
!>          = 'R':  V contains right eigenvectors;
!>          = 'L':  V contains left eigenvectors.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of rows of the matrix V.  N >= 0.
!> \endverbatim
!>
!> @param[in] ILO
!> \verbatim
!>          ILO is INTEGER
!> \endverbatim
!>
!> @param[in] IHI
!> \verbatim
!>          IHI is INTEGER
!>          The integers ILO and IHI determined by ZGGBAL.
!>          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
!> \endverbatim
!>
!> @param[in] LSCALE
!> \verbatim
!>          LSCALE is DOUBLE PRECISION array, dimension (N)
!>          Details of the permutations and/or scaling factors applied
!>          to the left side of A and B, as returned by ZGGBAL.
!> \endverbatim
!>
!> @param[in] RSCALE
!> \verbatim
!>          RSCALE is DOUBLE PRECISION array, dimension (N)
!>          Details of the permutations and/or scaling factors applied
!>          to the right side of A and B, as returned by ZGGBAL.
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of columns of the matrix V.  M >= 0.
!> \endverbatim
!>
!> @param[in,out] V
!> \verbatim
!>          V is COMPLEX*16 array, dimension (LDV,M)
!>          On entry, the matrix of right or left eigenvectors to be
!>          transformed, as returned by ZTGEVC.
!>          On exit, V is overwritten by the transformed eigenvectors.
!> \endverbatim
!>
!> @param[in] LDV
!> \verbatim
!>          LDV is INTEGER
!>          The leading dimension of the matrix V. LDV >= max(1,N).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit.
!>          < 0:  if INFO = -i, the i-th argument had an illegal value.
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
!>  See R.C. Ward, Balancing the generalized eigenvalue problem,
!>                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZGGBAK( JOB, SIDE, N, ILO, IHI, LSCALE, RSCALE, M, V,
     $                   LDV, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          JOB, SIDE
      INTEGER            IHI, ILO, INFO, LDV, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   LSCALE( * ), RSCALE( * )
      COMPLEX*16         V( LDV, * )
!     ..
!
!  Purpose
!  =======
!
!  ZGGBAK forms the right or left eigenvectors of a complex generalized
!  eigenvalue problem A*x = lambda*B*x, by backward transformation on
!  the computed eigenvectors of the balanced pair of matrices output by
!  ZGGBAL.
!
!  Arguments
!  =========
!
!  JOB     (input) CHARACTER*1
!          Specifies the type of backward transformation required:
!          = 'N':  do nothing, return immediately;
!          = 'P':  do backward transformation for permutation only;
!          = 'S':  do backward transformation for scaling only;
!          = 'B':  do backward transformations for both permutation and
!                  scaling.
!          JOB must be the same as the argument JOB supplied to ZGGBAL.
!
!  SIDE    (input) CHARACTER*1
!          = 'R':  V contains right eigenvectors;
!          = 'L':  V contains left eigenvectors.
!
!  N       (input) INTEGER
!          The number of rows of the matrix V.  N >= 0.
!
!  ILO     (input) INTEGER
!  IHI     (input) INTEGER
!          The integers ILO and IHI determined by ZGGBAL.
!          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
!
!  LSCALE  (input) DOUBLE PRECISION array, dimension (N)
!          Details of the permutations and/or scaling factors applied
!          to the left side of A and B, as returned by ZGGBAL.
!
!  RSCALE  (input) DOUBLE PRECISION array, dimension (N)
!          Details of the permutations and/or scaling factors applied
!          to the right side of A and B, as returned by ZGGBAL.
!
!  M       (input) INTEGER
!          The number of columns of the matrix V.  M >= 0.
!
!  V       (input/output) COMPLEX*16 array, dimension (LDV,M)
!          On entry, the matrix of right or left eigenvectors to be
!          transformed, as returned by ZTGEVC.
!          On exit, V is overwritten by the transformed eigenvectors.
!
!  LDV     (input) INTEGER
!          The leading dimension of the matrix V. LDV >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!
!  Further Details
!  ===============
!
!  See R.C. Ward, Balancing the generalized eigenvalue problem,
!                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.
!
!  =====================================================================
!
!     .. Local Scalars ..
      LOGICAL            LEFTV, RIGHTV
      INTEGER            I, K
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZDSCAL, ZSWAP
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters
!
      RIGHTV = LSAME( SIDE, 'R' )
      LEFTV = LSAME( SIDE, 'L' )
!
      INFO = 0
      IF( .NOT.LSAME( JOB, 'N' ) .AND. .NOT.LSAME( JOB, 'P' ) .AND.
     $    .NOT.LSAME( JOB, 'S' ) .AND. .NOT.LSAME( JOB, 'B' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.RIGHTV .AND. .NOT.LEFTV ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ILO.LT.1 ) THEN
         INFO = -4
      ELSE IF( N.EQ.0 .AND. IHI.EQ.0 .AND. ILO.NE.1 ) THEN
         INFO = -4
      ELSE IF( N.GT.0 .AND. ( IHI.LT.ILO .OR. IHI.GT.MAX( 1, N ) ) )
     $   THEN
         INFO = -5
      ELSE IF( N.EQ.0 .AND. ILO.EQ.1 .AND. IHI.NE.0 ) THEN
         INFO = -5
      ELSE IF( M.LT.0 ) THEN
         INFO = -8
      ELSE IF( LDV.LT.MAX( 1, N ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGGBAK', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 )
     $   RETURN
      IF( M.EQ.0 )
     $   RETURN
      IF( LSAME( JOB, 'N' ) )
     $   RETURN
!
      IF( ILO.EQ.IHI )
     $   GO TO 30
!
!     Backward balance
!
      IF( LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'B' ) ) THEN
!
!        Backward transformation on right eigenvectors
!
         IF( RIGHTV ) THEN
            DO 10 I = ILO, IHI
               CALL ZDSCAL( M, RSCALE( I ), V( I, 1 ), LDV )
   10       CONTINUE
         END IF
!
!        Backward transformation on left eigenvectors
!
         IF( LEFTV ) THEN
            DO 20 I = ILO, IHI
               CALL ZDSCAL( M, LSCALE( I ), V( I, 1 ), LDV )
   20       CONTINUE
         END IF
      END IF
!
!     Backward permutation
!
   30 CONTINUE
      IF( LSAME( JOB, 'P' ) .OR. LSAME( JOB, 'B' ) ) THEN
!
!        Backward permutation on right eigenvectors
!
         IF( RIGHTV ) THEN
            IF( ILO.EQ.1 )
     $         GO TO 50
            DO 40 I = ILO - 1, 1, -1
               K = RSCALE( I )
               IF( K.EQ.I )
     $            GO TO 40
               CALL ZSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV )
   40       CONTINUE
!
   50       CONTINUE
            IF( IHI.EQ.N )
     $         GO TO 70
            DO 60 I = IHI + 1, N
               K = RSCALE( I )
               IF( K.EQ.I )
     $            GO TO 60
               CALL ZSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV )
   60       CONTINUE
         END IF
!
!        Backward permutation on left eigenvectors
!
   70    CONTINUE
         IF( LEFTV ) THEN
            IF( ILO.EQ.1 )
     $         GO TO 90
            DO 80 I = ILO - 1, 1, -1
               K = LSCALE( I )
               IF( K.EQ.I )
     $            GO TO 80
               CALL ZSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV )
   80       CONTINUE
!
   90       CONTINUE
            IF( IHI.EQ.N )
     $         GO TO 110
            DO 100 I = IHI + 1, N
               K = LSCALE( I )
               IF( K.EQ.I )
     $            GO TO 100
               CALL ZSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV )
  100       CONTINUE
         END IF
      END IF
!
  110 CONTINUE
!
      RETURN
!
!     End of ZGGBAK
!
      END
!
! =====================================================================
!
!> @brief  IPARMQ
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       INTEGER FUNCTION IPARMQ( ISPEC, NAME, OPTS, N, ILO, IHI, LWORK )
!
!       .. Scalar Arguments ..
!       INTEGER            IHI, ILO, ISPEC, LWORK, N
!       CHARACTER          NAME*( * ), OPTS*( * )
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!>      This program sets problem and machine dependent parameters
!>      useful for xHSEQR and its subroutines. It is called whenever
!>      ILAENV is called with 12 <= ISPEC <= 16
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] ISPEC
!> \verbatim
!>          ISPEC is integer scalar
!>              ISPEC specifies which tunable parameter IPARMQ should
!>              return.
!>
!>              ISPEC=12: (INMIN)  Matrices of order nmin or less
!>                        are sent directly to xLAHQR, the implicit
!>                        double shift QR algorithm.  NMIN must be
!>                        at least 11.
!>
!>              ISPEC=13: (INWIN)  Size of the deflation window.
!>                        This is best set greater than or equal to
!>                        the number of simultaneous shifts NS.
!>                        Larger matrices benefit from larger deflation
!>                        windows.
!>
!>              ISPEC=14: (INIBL) Determines when to stop nibbling and
!>                        invest in an (expensive) multi-shift QR sweep.
!>                        If the aggressive early deflation subroutine
!>                        finds LD converged eigenvalues from an order
!>                        NW deflation window and LD.GT.(NW*NIBBLE)/100,
!>                        then the next QR sweep is skipped and early
!>                        deflation is applied immediately to the
!>                        remaining active diagonal block.  Setting
!>                        IPARMQ(ISPEC=14) = 0 causes TTQRE to skip a
!>                        multi-shift QR sweep whenever early deflation
!>                        finds a converged eigenvalue.  Setting
!>                        IPARMQ(ISPEC=14) greater than or equal to 100
!>                        prevents TTQRE from skipping a multi-shift
!>                        QR sweep.
!>
!>              ISPEC=15: (NSHFTS) The number of simultaneous shifts in
!>                        a multi-shift QR iteration.
!>
!>              ISPEC=16: (IACC22) IPARMQ is set to 0, 1 or 2 with the
!>                        following meanings.
!>                        0:  During the multi-shift QR sweep,
!>                            xLAQR5 does not accumulate reflections and
!>                            does not use matrix-matrix multiply to
!>                            update the far-from-diagonal matrix
!>                            entries.
!>                        1:  During the multi-shift QR sweep,
!>                            xLAQR5 and/or xLAQRaccumulates reflections and uses
!>                            matrix-matrix multiply to update the
!>                            far-from-diagonal matrix entries.
!>                        2:  During the multi-shift QR sweep.
!>                            xLAQR5 accumulates reflections and takes
!>                            advantage of 2-by-2 block structure during
!>                            matrix-matrix multiplies.
!>                        (If xTRMM is slower than xGEMM, then
!>                        IPARMQ(ISPEC=16)=1 may be more efficient than
!>                        IPARMQ(ISPEC=16)=2 despite the greater level of
!>                        arithmetic work implied by the latter choice.)
!> \endverbatim
!>
!> @param[in] NAME
!> \verbatim
!>          NAME is character string
!>               Name of the calling subroutine
!> \endverbatim
!>
!> @param[in] OPTS
!> \verbatim
!>          OPTS is character string
!>               This is a concatenation of the string arguments to
!>               TTQRE.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is integer scalar
!>               N is the order of the Hessenberg matrix H.
!> \endverbatim
!>
!> @param[in] ILO
!> \verbatim
!>          ILO is INTEGER
!> \endverbatim
!>
!> @param[in] IHI
!> \verbatim
!>          IHI is INTEGER
!>               It is assumed that H is already upper triangular
!>               in rows and columns 1:ILO-1 and IHI+1:N.
!> \endverbatim
!>
!> @param[in] LWORK
!> \verbatim
!>          LWORK is integer scalar
!>               The amount of workspace available.
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
!>       Little is known about how best to choose these parameters.
!>       It is possible to use different values of the parameters
!>       for each of CHSEQR, DHSEQR, SHSEQR and ZHSEQR.
!>
!>       It is probably best to choose different parameters for
!>       different matrices and different parameters at different
!>       times during the iteration, but this has not been
!>       implemented --- yet.
!>
!>
!>       The best choices of most of the parameters depend
!>       in an ill-understood way on the relative execution
!>       rate of xLAQR3 and xLAQR5 and on the nature of each
!>       particular eigenvalue problem.  Experiment may be the
!>       only practical way to determine which choices are most
!>       effective.
!>
!>       Following is a list of default values supplied by IPARMQ.
!>       These defaults may be adjusted in order to attain better
!>       performance in any particular computational environment.
!>
!>       IPARMQ(ISPEC=12) The xLAHQR vs xLAQR0 crossover point.
!>                        Default: 75. (Must be at least 11.)
!>
!>       IPARMQ(ISPEC=13) Recommended deflation window size.
!>                        This depends on ILO, IHI and NS, the
!>                        number of simultaneous shifts returned
!>                        by IPARMQ(ISPEC=15).  The default for
!>                        (IHI-ILO+1).LE.500 is NS.  The default
!>                        for (IHI-ILO+1).GT.500 is 3*NS/2.
!>
!>       IPARMQ(ISPEC=14) Nibble crossover point.  Default: 14.
!>
!>       IPARMQ(ISPEC=15) Number of simultaneous shifts, NS.
!>                        a multi-shift QR iteration.
!>
!>                        If IHI-ILO+1 is ...
!>
!>                        greater than      ...but less    ... the
!>                        or equal to ...      than        default is
!>
!>                                0               30       NS =   2+
!>                               30               60       NS =   4+
!>                               60              150       NS =  10
!>                              150              590       NS =  **
!>                              590             3000       NS =  64
!>                             3000             6000       NS = 128
!>                             6000             infinity   NS = 256
!>
!>                    (+)  By default matrices of this order are
!>                         passed to the implicit double shift routine
!>                         xLAHQR.  See IPARMQ(ISPEC=12) above.   These
!>                         values of NS are used only in case of a rare
!>                         xLAHQR failure.
!>
!>                    (**) The asterisks (**) indicate an ad-hoc
!>                         function increasing from 10 to 64.
!>
!>       IPARMQ(ISPEC=16) Select structured matrix multiply.
!>                        (See ISPEC=16 above for details.)
!>                        Default: 3.
!> \endverbatim
!>
!  =====================================================================
      INTEGER FUNCTION IPARMQ( ISPEC, NAME, OPTS, N, ILO, IHI, LWORK )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            IHI, ILO, ISPEC, LWORK, N
      CHARACTER          NAME*( * ), OPTS*( * )
!
!  Purpose
!  =======
!
!       This program sets problem and machine dependent parameters
!       useful for xHSEQR and its subroutines. It is called whenever
!       ILAENV is called with 12 <= ISPEC <= 16
!
!  Arguments
!  =========
!
!       ISPEC  (input) integer scalar
!              ISPEC specifies which tunable parameter IPARMQ should
!              return.
!
!              ISPEC=12: (INMIN)  Matrices of order nmin or less
!                        are sent directly to xLAHQR, the implicit
!                        double shift QR algorithm.  NMIN must be
!                        at least 11.
!
!              ISPEC=13: (INWIN)  Size of the deflation window.
!                        This is best set greater than or equal to
!                        the number of simultaneous shifts NS.
!                        Larger matrices benefit from larger deflation
!                        windows.
!
!              ISPEC=14: (INIBL) Determines when to stop nibbling and
!                        invest in an (expensive) multi-shift QR sweep.
!                        If the aggressive early deflation subroutine
!                        finds LD converged eigenvalues from an order
!                        NW deflation window and LD.GT.(NW*NIBBLE)/100,
!                        then the next QR sweep is skipped and early
!                        deflation is applied immediately to the
!                        remaining active diagonal block.  Setting
!                        IPARMQ(ISPEC=14) = 0 causes TTQRE to skip a
!                        multi-shift QR sweep whenever early deflation
!                        finds a converged eigenvalue.  Setting
!                        IPARMQ(ISPEC=14) greater than or equal to 100
!                        prevents TTQRE from skipping a multi-shift
!                        QR sweep.
!
!              ISPEC=15: (NSHFTS) The number of simultaneous shifts in
!                        a multi-shift QR iteration.
!
!              ISPEC=16: (IACC22) IPARMQ is set to 0, 1 or 2 with the
!                        following meanings.
!                        0:  During the multi-shift QR sweep,
!                            xLAQR5 does not accumulate reflections and
!                            does not use matrix-matrix multiply to
!                            update the far-from-diagonal matrix
!                            entries.
!                        1:  During the multi-shift QR sweep,
!                            xLAQR5 and/or xLAQRaccumulates reflections and uses
!                            matrix-matrix multiply to update the
!                            far-from-diagonal matrix entries.
!                        2:  During the multi-shift QR sweep.
!                            xLAQR5 accumulates reflections and takes
!                            advantage of 2-by-2 block structure during
!                            matrix-matrix multiplies.
!                        (If xTRMM is slower than xGEMM, then
!                        IPARMQ(ISPEC=16)=1 may be more efficient than
!                        IPARMQ(ISPEC=16)=2 despite the greater level of
!                        arithmetic work implied by the latter choice.)
!
!       NAME    (input) character string
!               Name of the calling subroutine
!
!       OPTS    (input) character string
!               This is a concatenation of the string arguments to
!               TTQRE.
!
!       N       (input) integer scalar
!               N is the order of the Hessenberg matrix H.
!
!       ILO     (input) INTEGER
!       IHI     (input) INTEGER
!               It is assumed that H is already upper triangular
!               in rows and columns 1:ILO-1 and IHI+1:N.
!
!       LWORK   (input) integer scalar
!               The amount of workspace available.
!
!  Further Details
!  ===============
!
!       Little is known about how best to choose these parameters.
!       It is possible to use different values of the parameters
!       for each of CHSEQR, DHSEQR, SHSEQR and ZHSEQR.
!
!       It is probably best to choose different parameters for
!       different matrices and different parameters at different
!       times during the iteration, but this has not been
!       implemented --- yet.
!
!
!       The best choices of most of the parameters depend
!       in an ill-understood way on the relative execution
!       rate of xLAQR3 and xLAQR5 and on the nature of each
!       particular eigenvalue problem.  Experiment may be the
!       only practical way to determine which choices are most
!       effective.
!
!       Following is a list of default values supplied by IPARMQ.
!       These defaults may be adjusted in order to attain better
!       performance in any particular computational environment.
!
!       IPARMQ(ISPEC=12) The xLAHQR vs xLAQR0 crossover point.
!                        Default: 75. (Must be at least 11.)
!
!       IPARMQ(ISPEC=13) Recommended deflation window size.
!                        This depends on ILO, IHI and NS, the
!                        number of simultaneous shifts returned
!                        by IPARMQ(ISPEC=15).  The default for
!                        (IHI-ILO+1).LE.500 is NS.  The default
!                        for (IHI-ILO+1).GT.500 is 3*NS/2.
!
!       IPARMQ(ISPEC=14) Nibble crossover point.  Default: 14.
!
!       IPARMQ(ISPEC=15) Number of simultaneous shifts, NS.
!                        a multi-shift QR iteration.
!
!                        If IHI-ILO+1 is ...
!
!                        greater than      ...but less    ... the
!                        or equal to ...      than        default is
!
!                                0               30       NS =   2+
!                               30               60       NS =   4+
!                               60              150       NS =  10
!                              150              590       NS =  **
!                              590             3000       NS =  64
!                             3000             6000       NS = 128
!                             6000             infinity   NS = 256
!
!                    (+)  By default matrices of this order are
!                         passed to the implicit double shift routine
!                         xLAHQR.  See IPARMQ(ISPEC=12) above.   These
!                         values of NS are used only in case of a rare
!                         xLAHQR failure.
!
!                    (**) The asterisks (**) indicate an ad-hoc
!                         function increasing from 10 to 64.
!
!       IPARMQ(ISPEC=16) Select structured matrix multiply.
!                        (See ISPEC=16 above for details.)
!                        Default: 3.
!
!     ================================================================
!     .. Parameters ..
      INTEGER            INMIN, INWIN, INIBL, ISHFTS, IACC22
      PARAMETER          ( INMIN = 12, INWIN = 13, INIBL = 14,
     $                   ISHFTS = 15, IACC22 = 16 )
      INTEGER            NMIN, K22MIN, KACMIN, NIBBLE, KNWSWP
      PARAMETER          ( NMIN = 75, K22MIN = 14, KACMIN = 14,
     $                   NIBBLE = 14, KNWSWP = 500 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0 )
!     ..
!     .. Local Scalars ..
      INTEGER            NH, NS
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          LOG, MAX, MOD, NINT, REAL
!     ..
!     .. Executable Statements ..
      IF( ( ISPEC.EQ.ISHFTS ) .OR. ( ISPEC.EQ.INWIN ) .OR.
     $    ( ISPEC.EQ.IACC22 ) ) THEN
!
!        ==== Set the number simultaneous shifts ====
!
         NH = IHI - ILO + 1
         NS = 2
         IF( NH.GE.30 )
     $      NS = 4
         IF( NH.GE.60 )
     $      NS = 10
         IF( NH.GE.150 )
     $      NS = MAX( 10, NH / NINT( LOG( REAL( NH ) ) / LOG( TWO ) ) )
         IF( NH.GE.590 )
     $      NS = 64
         IF( NH.GE.3000 )
     $      NS = 128
         IF( NH.GE.6000 )
     $      NS = 256
         NS = MAX( 2, NS-MOD( NS, 2 ) )
      END IF
!
      IF( ISPEC.EQ.INMIN ) THEN
!
!
!        ===== Matrices of order smaller than NMIN get sent
!        .     to xLAHQR, the classic double shift algorithm.
!        .     This must be at least 11. ====
!
         IPARMQ = NMIN
!
      ELSE IF( ISPEC.EQ.INIBL ) THEN
!
!        ==== INIBL: skip a multi-shift qr iteration and
!        .    whenever aggressive early deflation finds
!        .    at least (NIBBLE*(window size)/100) deflations. ====
!
         IPARMQ = NIBBLE
!
      ELSE IF( ISPEC.EQ.ISHFTS ) THEN
!
!        ==== NSHFTS: The number of simultaneous shifts =====
!
         IPARMQ = NS
!
      ELSE IF( ISPEC.EQ.INWIN ) THEN
!
!        ==== NW: deflation window size.  ====
!
         IF( NH.LE.KNWSWP ) THEN
            IPARMQ = NS
         ELSE
            IPARMQ = 3*NS / 2
         END IF
!
      ELSE IF( ISPEC.EQ.IACC22 ) THEN
!
!        ==== IACC22: Whether to accumulate reflections
!        .     before updating the far-from-diagonal elements
!        .     and whether to use 2-by-2 block structure while
!        .     doing it.  A small amount of work could be saved
!        .     by making this choice dependent also upon the
!        .     NH=IHI-ILO+1.
!
         IPARMQ = 0
         IF( NS.GE.KACMIN )
     $      IPARMQ = 1
         IF( NS.GE.K22MIN )
     $      IPARMQ = 2
!
      ELSE
!        ===== invalid value of ispec =====
         IPARMQ = -1
!
      END IF
!
!     ==== End of IPARMQ ====
!
      END
!
!  =====================================================================
!
!> @brief  ZTGEVC
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZTGEVC( SIDE, HOWMNY, SELECT, N, S, LDS, P, LDP, VL,
!                          LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          HOWMNY, SIDE
!       INTEGER            INFO, LDP, LDS, LDVL, LDVR, M, MM, N
!       ..
!       .. Array Arguments ..
!       LOGICAL            SELECT( * )
!       DOUBLE PRECISION   RWORK( * )
!       COMPLEX*16         P( LDP, * ), S( LDS, * ), VL( LDVL, * ),
!      $                   VR( LDVR, * ), WORK( * )
!       ..
!
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZTGEVC computes some or all of the right and/or left eigenvectors of
!> a pair of complex matrices (S,P), where S and P are upper triangular.
!> Matrix pairs of this type are produced by the generalized Schur
!> factorization of a complex matrix pair (A,B):
!>
!>    A = Q*S*Z**H,  B = Q*P*Z**H
!>
!> as computed by ZGGHRD + ZHGEQZ.
!>
!> The right eigenvector x and the left eigenvector y of (S,P)
!> corresponding to an eigenvalue w are defined by:
!>
!>    S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
!>
!> where y**H denotes the conjugate tranpose of y.
!> The eigenvalues are not input to this routine, but are computed
!> directly from the diagonal elements of S and P.
!>
!> This routine returns the matrices X and/or Y of right and left
!> eigenvectors of (S,P), or the products Z*X and/or Q*Y,
!> where Z and Q are input matrices.
!> If Q and Z are the unitary factors from the generalized Schur
!> factorization of a matrix pair (A,B), then Z*X and Q*Y
!> are the matrices of right and left eigenvectors of (A,B).
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] SIDE
!> \verbatim
!>          SIDE is CHARACTER*1
!>          = 'R': compute right eigenvectors only;
!>          = 'L': compute left eigenvectors only;
!>          = 'B': compute both right and left eigenvectors.
!> \endverbatim
!>
!> @param[in] HOWMNY
!> \verbatim
!>          HOWMNY is CHARACTER*1
!>          = 'A': compute all right and/or left eigenvectors;
!>          = 'B': compute all right and/or left eigenvectors,
!>                 backtransformed by the matrices in VR and/or VL;
!>          = 'S': compute selected right and/or left eigenvectors,
!>                 specified by the logical array SELECT.
!> \endverbatim
!>
!> @param[in] SELECT
!> \verbatim
!>          SELECT is LOGICAL array, dimension (N)
!>          If HOWMNY='S', SELECT specifies the eigenvectors to be
!>          computed.  The eigenvector corresponding to the j-th
!>          eigenvalue is computed if SELECT(j) = .TRUE..
!>          Not referenced if HOWMNY = 'A' or 'B'.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrices S and P.  N >= 0.
!> \endverbatim
!>
!> @param[in] S
!> \verbatim
!>          S is COMPLEX*16 array, dimension (LDS,N)
!>          The upper triangular matrix S from a generalized Schur
!>          factorization, as computed by ZHGEQZ.
!> \endverbatim
!>
!> @param[in] LDS
!> \verbatim
!>          LDS is INTEGER
!>          The leading dimension of array S.  LDS >= max(1,N).
!> \endverbatim
!>
!> @param[in] P
!> \verbatim
!>          P is COMPLEX*16 array, dimension (LDP,N)
!>          The upper triangular matrix P from a generalized Schur
!>          factorization, as computed by ZHGEQZ.  P must have real
!>          diagonal elements.
!> \endverbatim
!>
!> @param[in] LDP
!> \verbatim
!>          LDP is INTEGER
!>          The leading dimension of array P.  LDP >= max(1,N).
!> \endverbatim
!>
!> @param[in,out] VL
!> \verbatim
!>          VL is COMPLEX*16 array, dimension (LDVL,MM)
!>          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
!>          contain an N-by-N matrix Q (usually the unitary matrix Q
!>          of left Schur vectors returned by ZHGEQZ).
!>          On exit, if SIDE = 'L' or 'B', VL contains:
!>          if HOWMNY = 'A', the matrix Y of left eigenvectors of (S,P);
!>          if HOWMNY = 'B', the matrix Q*Y;
!>          if HOWMNY = 'S', the left eigenvectors of (S,P) specified by
!>                      SELECT, stored consecutively in the columns of
!>                      VL, in the same order as their eigenvalues.
!>          Not referenced if SIDE = 'R'.
!> \endverbatim
!>
!> @param[in] LDVL
!> \verbatim
!>          LDVL is INTEGER
!>          The leading dimension of array VL.  LDVL >= 1, and if
!>          SIDE = 'L' or 'l' or 'B' or 'b', LDVL >= N.
!> \endverbatim
!>
!> @param[in,out] VR
!> \verbatim
!>          VR is COMPLEX*16 array, dimension (LDVR,MM)
!>          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
!>          contain an N-by-N matrix Q (usually the unitary matrix Z
!>          of right Schur vectors returned by ZHGEQZ).
!>          On exit, if SIDE = 'R' or 'B', VR contains:
!>          if HOWMNY = 'A', the matrix X of right eigenvectors of (S,P);
!>          if HOWMNY = 'B', the matrix Z*X;
!>          if HOWMNY = 'S', the right eigenvectors of (S,P) specified by
!>                      SELECT, stored consecutively in the columns of
!>                      VR, in the same order as their eigenvalues.
!>          Not referenced if SIDE = 'L'.
!> \endverbatim
!>
!> @param[in] LDVR
!> \verbatim
!>          LDVR is INTEGER
!>          The leading dimension of the array VR.  LDVR >= 1, and if
!>          SIDE = 'R' or 'B', LDVR >= N.
!> \endverbatim
!>
!> @param[in] MM
!> \verbatim
!>          MM is INTEGER
!>          The number of columns in the arrays VL and/or VR. MM >= M.
!> \endverbatim
!>
!> @param[out] M
!> \verbatim
!>          M is INTEGER
!>          The number of columns in the arrays VL and/or VR actually
!>          used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M
!>          is set to N.  Each selected eigenvector occupies one column.
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
!>          = 0:  successful exit.
!>          < 0:  if INFO = -i, the i-th argument had an illegal value.
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
!  =====================================================================
      SUBROUTINE ZTGEVC( SIDE, HOWMNY, SELECT, N, S, LDS, P, LDP, VL,
     $                   LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          HOWMNY, SIDE
      INTEGER            INFO, LDP, LDS, LDVL, LDVR, M, MM, N
!     ..
!     .. Array Arguments ..
      LOGICAL            SELECT( * )
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         P( LDP, * ), S( LDS, * ), VL( LDVL, * ),
     $                   VR( LDVR, * ), WORK( * )
!     ..
!
!
!  Purpose
!  =======
!
!  ZTGEVC computes some or all of the right and/or left eigenvectors of
!  a pair of complex matrices (S,P), where S and P are upper triangular.
!  Matrix pairs of this type are produced by the generalized Schur
!  factorization of a complex matrix pair (A,B):
!
!     A = Q*S*Z**H,  B = Q*P*Z**H
!
!  as computed by ZGGHRD + ZHGEQZ.
!
!  The right eigenvector x and the left eigenvector y of (S,P)
!  corresponding to an eigenvalue w are defined by:
!
!     S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
!
!  where y**H denotes the conjugate tranpose of y.
!  The eigenvalues are not input to this routine, but are computed
!  directly from the diagonal elements of S and P.
!
!  This routine returns the matrices X and/or Y of right and left
!  eigenvectors of (S,P), or the products Z*X and/or Q*Y,
!  where Z and Q are input matrices.
!  If Q and Z are the unitary factors from the generalized Schur
!  factorization of a matrix pair (A,B), then Z*X and Q*Y
!  are the matrices of right and left eigenvectors of (A,B).
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'R': compute right eigenvectors only;
!          = 'L': compute left eigenvectors only;
!          = 'B': compute both right and left eigenvectors.
!
!  HOWMNY  (input) CHARACTER*1
!          = 'A': compute all right and/or left eigenvectors;
!          = 'B': compute all right and/or left eigenvectors,
!                 backtransformed by the matrices in VR and/or VL;
!          = 'S': compute selected right and/or left eigenvectors,
!                 specified by the logical array SELECT.
!
!  SELECT  (input) LOGICAL array, dimension (N)
!          If HOWMNY='S', SELECT specifies the eigenvectors to be
!          computed.  The eigenvector corresponding to the j-th
!          eigenvalue is computed if SELECT(j) = .TRUE..
!          Not referenced if HOWMNY = 'A' or 'B'.
!
!  N       (input) INTEGER
!          The order of the matrices S and P.  N >= 0.
!
!  S       (input) COMPLEX*16 array, dimension (LDS,N)
!          The upper triangular matrix S from a generalized Schur
!          factorization, as computed by ZHGEQZ.
!
!  LDS     (input) INTEGER
!          The leading dimension of array S.  LDS >= max(1,N).
!
!  P       (input) COMPLEX*16 array, dimension (LDP,N)
!          The upper triangular matrix P from a generalized Schur
!          factorization, as computed by ZHGEQZ.  P must have real
!          diagonal elements.
!
!  LDP     (input) INTEGER
!          The leading dimension of array P.  LDP >= max(1,N).
!
!  VL      (input/output) COMPLEX*16 array, dimension (LDVL,MM)
!          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
!          contain an N-by-N matrix Q (usually the unitary matrix Q
!          of left Schur vectors returned by ZHGEQZ).
!          On exit, if SIDE = 'L' or 'B', VL contains:
!          if HOWMNY = 'A', the matrix Y of left eigenvectors of (S,P);
!          if HOWMNY = 'B', the matrix Q*Y;
!          if HOWMNY = 'S', the left eigenvectors of (S,P) specified by
!                      SELECT, stored consecutively in the columns of
!                      VL, in the same order as their eigenvalues.
!          Not referenced if SIDE = 'R'.
!
!  LDVL    (input) INTEGER
!          The leading dimension of array VL.  LDVL >= 1, and if
!          SIDE = 'L' or 'l' or 'B' or 'b', LDVL >= N.
!
!  VR      (input/output) COMPLEX*16 array, dimension (LDVR,MM)
!          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
!          contain an N-by-N matrix Q (usually the unitary matrix Z
!          of right Schur vectors returned by ZHGEQZ).
!          On exit, if SIDE = 'R' or 'B', VR contains:
!          if HOWMNY = 'A', the matrix X of right eigenvectors of (S,P);
!          if HOWMNY = 'B', the matrix Z*X;
!          if HOWMNY = 'S', the right eigenvectors of (S,P) specified by
!                      SELECT, stored consecutively in the columns of
!                      VR, in the same order as their eigenvalues.
!          Not referenced if SIDE = 'L'.
!
!  LDVR    (input) INTEGER
!          The leading dimension of the array VR.  LDVR >= 1, and if
!          SIDE = 'R' or 'B', LDVR >= N.
!
!  MM      (input) INTEGER
!          The number of columns in the arrays VL and/or VR. MM >= M.
!
!  M       (output) INTEGER
!          The number of columns in the arrays VL and/or VR actually
!          used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M
!          is set to N.  Each selected eigenvector occupies one column.
!
!  WORK    (workspace) COMPLEX*16 array, dimension (2*N)
!
!  RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                   CONE = ( 1.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            COMPL, COMPR, ILALL, ILBACK, ILBBAD, ILCOMP,
     $                   LSA, LSB
      INTEGER            I, IBEG, IEIG, IEND, IHWMNY, IM, ISIDE, ISRC,
     $                   J, JE, JR
      DOUBLE PRECISION   ACOEFA, ACOEFF, ANORM, ASCALE, BCOEFA, BIG,
     $                   BIGNUM, BNORM, BSCALE, DMIN, SAFMIN, SBETA,
     $                   SCALE, SMALL, TEMP, ULP, XMAX
      COMPLEX*16         BCOEFF, CA, CB, D, SALPHA, SUM, SUMA, SUMB, X
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      COMPLEX*16         ZLADIV
      EXTERNAL           LSAME, DLAMCH, ZLADIV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLABAD, XERBLA, ZGEMV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, MAX, MIN
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION   ABS1
!     ..
!     .. Statement Function definitions ..
      ABS1( X ) = ABS( DBLE( X ) ) + ABS( DIMAG( X ) )
!     ..
!     .. Executable Statements ..
!
!     Decode and Test the input parameters
!
      IF( LSAME( HOWMNY, 'A' ) ) THEN
         IHWMNY = 1
         ILALL = .TRUE.
         ILBACK = .FALSE.
      ELSE IF( LSAME( HOWMNY, 'S' ) ) THEN
         IHWMNY = 2
         ILALL = .FALSE.
         ILBACK = .FALSE.
      ELSE IF( LSAME( HOWMNY, 'B' ) ) THEN
         IHWMNY = 3
         ILALL = .TRUE.
         ILBACK = .TRUE.
      ELSE
         IHWMNY = -1
      END IF
!
      IF( LSAME( SIDE, 'R' ) ) THEN
         ISIDE = 1
         COMPL = .FALSE.
         COMPR = .TRUE.
      ELSE IF( LSAME( SIDE, 'L' ) ) THEN
         ISIDE = 2
         COMPL = .TRUE.
         COMPR = .FALSE.
      ELSE IF( LSAME( SIDE, 'B' ) ) THEN
         ISIDE = 3
         COMPL = .TRUE.
         COMPR = .TRUE.
      ELSE
         ISIDE = -1
      END IF
!
      INFO = 0
      IF( ISIDE.LT.0 ) THEN
         INFO = -1
      ELSE IF( IHWMNY.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDS.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDP.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTGEVC', -INFO )
         RETURN
      END IF
!
!     Count the number of eigenvectors
!
      IF( .NOT.ILALL ) THEN
         IM = 0
         DO 10 J = 1, N
            IF( SELECT( J ) )
     $         IM = IM + 1
   10    CONTINUE
      ELSE
         IM = N
      END IF
!
!     Check diagonal of B
!
      ILBBAD = .FALSE.
      DO 20 J = 1, N
         IF( DIMAG( P( J, J ) ).NE.ZERO )
     $      ILBBAD = .TRUE.
   20 CONTINUE
!
      IF( ILBBAD ) THEN
         INFO = -7
      ELSE IF( COMPL .AND. LDVL.LT.N .OR. LDVL.LT.1 ) THEN
         INFO = -10
      ELSE IF( COMPR .AND. LDVR.LT.N .OR. LDVR.LT.1 ) THEN
         INFO = -12
      ELSE IF( MM.LT.IM ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTGEVC', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      M = IM
      IF( N.EQ.0 )
     $   RETURN
!
!     Machine Constants
!
      SAFMIN = DLAMCH( 'Safe minimum' )
      BIG = ONE / SAFMIN
      CALL DLABAD( SAFMIN, BIG )
      ULP = DLAMCH( 'Epsilon' )*DLAMCH( 'Base' )
      SMALL = SAFMIN*N / ULP
      BIG = ONE / SMALL
      BIGNUM = ONE / ( SAFMIN*N )
!
!     Compute the 1-norm of each column of the strictly upper triangular
!     part of A and B to check for possible overflow in the triangular
!     solver.
!
      ANORM = ABS1( S( 1, 1 ) )
      BNORM = ABS1( P( 1, 1 ) )
      RWORK( 1 ) = ZERO
      RWORK( N+1 ) = ZERO
      DO 40 J = 2, N
         RWORK( J ) = ZERO
         RWORK( N+J ) = ZERO
         DO 30 I = 1, J - 1
            RWORK( J ) = RWORK( J ) + ABS1( S( I, J ) )
            RWORK( N+J ) = RWORK( N+J ) + ABS1( P( I, J ) )
   30    CONTINUE
         ANORM = MAX( ANORM, RWORK( J )+ABS1( S( J, J ) ) )
         BNORM = MAX( BNORM, RWORK( N+J )+ABS1( P( J, J ) ) )
   40 CONTINUE
!
      ASCALE = ONE / MAX( ANORM, SAFMIN )
      BSCALE = ONE / MAX( BNORM, SAFMIN )
!
!     Left eigenvectors
!
      IF( COMPL ) THEN
         IEIG = 0
!
!        Main loop over eigenvalues
!
         DO 140 JE = 1, N
            IF( ILALL ) THEN
               ILCOMP = .TRUE.
            ELSE
               ILCOMP = SELECT( JE )
            END IF
            IF( ILCOMP ) THEN
               IEIG = IEIG + 1
!
               IF( ABS1( S( JE, JE ) ).LE.SAFMIN .AND.
     $             ABS( DBLE( P( JE, JE ) ) ).LE.SAFMIN ) THEN
!
!                 Singular matrix pencil -- return unit eigenvector
!
                  DO 50 JR = 1, N
                     VL( JR, IEIG ) = CZERO
   50             CONTINUE
                  VL( IEIG, IEIG ) = CONE
                  GO TO 140
               END IF
!
!              Non-singular eigenvalue:
!              Compute coefficients  a  and  b  in
!                   H
!                 y  ( a A - b B ) = 0
!
               TEMP = ONE / MAX( ABS1( S( JE, JE ) )*ASCALE,
     $                ABS( DBLE( P( JE, JE ) ) )*BSCALE, SAFMIN )
               SALPHA = ( TEMP*S( JE, JE ) )*ASCALE
               SBETA = ( TEMP*DBLE( P( JE, JE ) ) )*BSCALE
               ACOEFF = SBETA*ASCALE
               BCOEFF = SALPHA*BSCALE
!
!              Scale to avoid underflow
!
               LSA = ABS( SBETA ).GE.SAFMIN .AND. ABS( ACOEFF ).LT.SMALL
               LSB = ABS1( SALPHA ).GE.SAFMIN .AND. ABS1( BCOEFF ).LT.
     $               SMALL
!
               SCALE = ONE
               IF( LSA )
     $            SCALE = ( SMALL / ABS( SBETA ) )*MIN( ANORM, BIG )
               IF( LSB )
     $            SCALE = MAX( SCALE, ( SMALL / ABS1( SALPHA ) )*
     $                    MIN( BNORM, BIG ) )
               IF( LSA .OR. LSB ) THEN
                  SCALE = MIN( SCALE, ONE /
     $                    ( SAFMIN*MAX( ONE, ABS( ACOEFF ),
     $                    ABS1( BCOEFF ) ) ) )
                  IF( LSA ) THEN
                     ACOEFF = ASCALE*( SCALE*SBETA )
                  ELSE
                     ACOEFF = SCALE*ACOEFF
                  END IF
                  IF( LSB ) THEN
                     BCOEFF = BSCALE*( SCALE*SALPHA )
                  ELSE
                     BCOEFF = SCALE*BCOEFF
                  END IF
               END IF
!
               ACOEFA = ABS( ACOEFF )
               BCOEFA = ABS1( BCOEFF )
               XMAX = ONE
               DO 60 JR = 1, N
                  WORK( JR ) = CZERO
   60          CONTINUE
               WORK( JE ) = CONE
               DMIN = MAX( ULP*ACOEFA*ANORM, ULP*BCOEFA*BNORM, SAFMIN )
!
!                                              H
!              Triangular solve of  (a A - b B)  y = 0
!
!                                      H
!              (rowwise in  (a A - b B) , or columnwise in a A - b B)
!
               DO 100 J = JE + 1, N
!
!                 Compute
!                       j-1
!                 SUM = sum  conjg( a*S(k,j) - b*P(k,j) )*x(k)
!                       k=je
!                 (Scale if necessary)
!
                  TEMP = ONE / XMAX
                  IF( ACOEFA*RWORK( J )+BCOEFA*RWORK( N+J ).GT.BIGNUM*
     $                TEMP ) THEN
                     DO 70 JR = JE, J - 1
                        WORK( JR ) = TEMP*WORK( JR )
   70                CONTINUE
                     XMAX = ONE
                  END IF
                  SUMA = CZERO
                  SUMB = CZERO
!
                  DO 80 JR = JE, J - 1
                     SUMA = SUMA + DCONJG( S( JR, J ) )*WORK( JR )
                     SUMB = SUMB + DCONJG( P( JR, J ) )*WORK( JR )
   80             CONTINUE
                  SUM = ACOEFF*SUMA - DCONJG( BCOEFF )*SUMB
!
!                 Form x(j) = - SUM / conjg( a*S(j,j) - b*P(j,j) )
!
!                 with scaling and perturbation of the denominator
!
                  D = DCONJG( ACOEFF*S( J, J )-BCOEFF*P( J, J ) )
                  IF( ABS1( D ).LE.DMIN )
     $               D = DCMPLX( DMIN )
!
                  IF( ABS1( D ).LT.ONE ) THEN
                     IF( ABS1( SUM ).GE.BIGNUM*ABS1( D ) ) THEN
                        TEMP = ONE / ABS1( SUM )
                        DO 90 JR = JE, J - 1
                           WORK( JR ) = TEMP*WORK( JR )
   90                   CONTINUE
                        XMAX = TEMP*XMAX
                        SUM = TEMP*SUM
                     END IF
                  END IF
                  WORK( J ) = ZLADIV( -SUM, D )
                  XMAX = MAX( XMAX, ABS1( WORK( J ) ) )
  100          CONTINUE
!
!              Back transform eigenvector if HOWMNY='B'.
!
               IF( ILBACK ) THEN
                  CALL ZGEMV( 'N', N, N+1-JE, CONE, VL( 1, JE ), LDVL,
     $                        WORK( JE ), 1, CZERO, WORK( N+1 ), 1 )
                  ISRC = 2
                  IBEG = 1
               ELSE
                  ISRC = 1
                  IBEG = JE
               END IF
!
!              Copy and scale eigenvector into column of VL
!
               XMAX = ZERO
               DO 110 JR = IBEG, N
                  XMAX = MAX( XMAX, ABS1( WORK( ( ISRC-1 )*N+JR ) ) )
  110          CONTINUE
!
               IF( XMAX.GT.SAFMIN ) THEN
                  TEMP = ONE / XMAX
                  DO 120 JR = IBEG, N
                     VL( JR, IEIG ) = TEMP*WORK( ( ISRC-1 )*N+JR )
  120             CONTINUE
               ELSE
                  IBEG = N + 1
               END IF
!
               DO 130 JR = 1, IBEG - 1
                  VL( JR, IEIG ) = CZERO
  130          CONTINUE
!
            END IF
  140    CONTINUE
      END IF
!
!     Right eigenvectors
!
      IF( COMPR ) THEN
         IEIG = IM + 1
!
!        Main loop over eigenvalues
!
         DO 250 JE = N, 1, -1
            IF( ILALL ) THEN
               ILCOMP = .TRUE.
            ELSE
               ILCOMP = SELECT( JE )
            END IF
            IF( ILCOMP ) THEN
               IEIG = IEIG - 1
!
               IF( ABS1( S( JE, JE ) ).LE.SAFMIN .AND.
     $             ABS( DBLE( P( JE, JE ) ) ).LE.SAFMIN ) THEN
!
!                 Singular matrix pencil -- return unit eigenvector
!
                  DO 150 JR = 1, N
                     VR( JR, IEIG ) = CZERO
  150             CONTINUE
                  VR( IEIG, IEIG ) = CONE
                  GO TO 250
               END IF
!
!              Non-singular eigenvalue:
!              Compute coefficients  a  and  b  in
!
!              ( a A - b B ) x  = 0
!
               TEMP = ONE / MAX( ABS1( S( JE, JE ) )*ASCALE,
     $                ABS( DBLE( P( JE, JE ) ) )*BSCALE, SAFMIN )
               SALPHA = ( TEMP*S( JE, JE ) )*ASCALE
               SBETA = ( TEMP*DBLE( P( JE, JE ) ) )*BSCALE
               ACOEFF = SBETA*ASCALE
               BCOEFF = SALPHA*BSCALE
!
!              Scale to avoid underflow
!
               LSA = ABS( SBETA ).GE.SAFMIN .AND. ABS( ACOEFF ).LT.SMALL
               LSB = ABS1( SALPHA ).GE.SAFMIN .AND. ABS1( BCOEFF ).LT.
     $               SMALL
!
               SCALE = ONE
               IF( LSA )
     $            SCALE = ( SMALL / ABS( SBETA ) )*MIN( ANORM, BIG )
               IF( LSB )
     $            SCALE = MAX( SCALE, ( SMALL / ABS1( SALPHA ) )*
     $                    MIN( BNORM, BIG ) )
               IF( LSA .OR. LSB ) THEN
                  SCALE = MIN( SCALE, ONE /
     $                    ( SAFMIN*MAX( ONE, ABS( ACOEFF ),
     $                    ABS1( BCOEFF ) ) ) )
                  IF( LSA ) THEN
                     ACOEFF = ASCALE*( SCALE*SBETA )
                  ELSE
                     ACOEFF = SCALE*ACOEFF
                  END IF
                  IF( LSB ) THEN
                     BCOEFF = BSCALE*( SCALE*SALPHA )
                  ELSE
                     BCOEFF = SCALE*BCOEFF
                  END IF
               END IF
!
               ACOEFA = ABS( ACOEFF )
               BCOEFA = ABS1( BCOEFF )
               XMAX = ONE
               DO 160 JR = 1, N
                  WORK( JR ) = CZERO
  160          CONTINUE
               WORK( JE ) = CONE
               DMIN = MAX( ULP*ACOEFA*ANORM, ULP*BCOEFA*BNORM, SAFMIN )
!
!              Triangular solve of  (a A - b B) x = 0  (columnwise)
!
!              WORK(1:j-1) contains sums w,
!              WORK(j+1:JE) contains x
!
               DO 170 JR = 1, JE - 1
                  WORK( JR ) = ACOEFF*S( JR, JE ) - BCOEFF*P( JR, JE )
  170          CONTINUE
               WORK( JE ) = CONE
!
               DO 210 J = JE - 1, 1, -1
!
!                 Form x(j) := - w(j) / d
!                 with scaling and perturbation of the denominator
!
                  D = ACOEFF*S( J, J ) - BCOEFF*P( J, J )
                  IF( ABS1( D ).LE.DMIN )
     $               D = DCMPLX( DMIN )
!
                  IF( ABS1( D ).LT.ONE ) THEN
                     IF( ABS1( WORK( J ) ).GE.BIGNUM*ABS1( D ) ) THEN
                        TEMP = ONE / ABS1( WORK( J ) )
                        DO 180 JR = 1, JE
                           WORK( JR ) = TEMP*WORK( JR )
  180                   CONTINUE
                     END IF
                  END IF
!
                  WORK( J ) = ZLADIV( -WORK( J ), D )
!
                  IF( J.GT.1 ) THEN
!
!                    w = w + x(j)*(a S(*,j) - b P(*,j) ) with scaling
!
                     IF( ABS1( WORK( J ) ).GT.ONE ) THEN
                        TEMP = ONE / ABS1( WORK( J ) )
                        IF( ACOEFA*RWORK( J )+BCOEFA*RWORK( N+J ).GE.
     $                      BIGNUM*TEMP ) THEN
                           DO 190 JR = 1, JE
                              WORK( JR ) = TEMP*WORK( JR )
  190                      CONTINUE
                        END IF
                     END IF
!
                     CA = ACOEFF*WORK( J )
                     CB = BCOEFF*WORK( J )
                     DO 200 JR = 1, J - 1
                        WORK( JR ) = WORK( JR ) + CA*S( JR, J ) -
     $                               CB*P( JR, J )
  200                CONTINUE
                  END IF
  210          CONTINUE
!
!              Back transform eigenvector if HOWMNY='B'.
!
               IF( ILBACK ) THEN
                  CALL ZGEMV( 'N', N, JE, CONE, VR, LDVR, WORK, 1,
     $                        CZERO, WORK( N+1 ), 1 )
                  ISRC = 2
                  IEND = N
               ELSE
                  ISRC = 1
                  IEND = JE
               END IF
!
!              Copy and scale eigenvector into column of VR
!
               XMAX = ZERO
               DO 220 JR = 1, IEND
                  XMAX = MAX( XMAX, ABS1( WORK( ( ISRC-1 )*N+JR ) ) )
  220          CONTINUE
!
               IF( XMAX.GT.SAFMIN ) THEN
                  TEMP = ONE / XMAX
                  DO 230 JR = 1, IEND
                     VR( JR, IEIG ) = TEMP*WORK( ( ISRC-1 )*N+JR )
  230             CONTINUE
               ELSE
                  IEND = 0
               END IF
!
               DO 240 JR = IEND + 1, N
                  VR( JR, IEIG ) = CZERO
  240          CONTINUE
!
            END IF
  250    CONTINUE
      END IF
!
      RETURN
!
!     End of ZTGEVC
!
      END
!
!  =====================================================================
!
!> @brief  DLAPY2 returns sqrt(x2+y2).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       DOUBLE PRECISION FUNCTION DLAPY2( X, Y )
!
!       .. Scalar Arguments ..
!       DOUBLE PRECISION   X, Y
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
!> overflow.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] X
!> \verbatim
!>          X is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] Y
!> \verbatim
!>          Y is DOUBLE PRECISION
!>          X and Y specify the values x and y.
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
!  =====================================================================
      DOUBLE PRECISION FUNCTION DLAPY2( X, Y )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   X, Y
!     ..
!
!  Purpose
!  =======
!
!  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
!  overflow.
!
!  Arguments
!  =========
!
!  X       (input) DOUBLE PRECISION
!  Y       (input) DOUBLE PRECISION
!          X and Y specify the values x and y.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION   W, XABS, YABS, Z
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      IF( Z.EQ.ZERO ) THEN
         DLAPY2 = W
      ELSE
         DLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      END IF
      RETURN
!
!     End of DLAPY2
!
      END
!
!=====================================================================
!
!> @brief  ZGETF2 computes the LU factorization of a general m-by-n matrix using partial pivoting with row interchanges (unblocked algorithm).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGETF2( M, N, A, LDA, IPIV, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, LDA, M, N
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * )
!       COMPLEX*16         A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGETF2 computes an LU factorization of a general m-by-n matrix A
!> using partial pivoting with row interchanges.
!>
!> The factorization has the form
!>    A = P * L * U
!> where P is a permutation matrix, L is lower triangular with unit
!> diagonal elements (lower trapezoidal if m > n), and U is upper
!> triangular (upper trapezoidal if m < n).
!>
!> This is the right-looking Level 2 BLAS version of the algorithm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
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
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the m by n matrix to be factored.
!>          On exit, the factors L and U from the factorization
!>          A = P*L*U; the unit diagonal elements of L are not stored.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> @param[out] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (min(M,N))
!>          The pivot indices; for 1 <= i <= min(M,N), row i of the
!>          matrix was interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -k, the k-th argument had an illegal value
!>          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!>               has been completed, but the factor U is exactly
!>               singular, and division by zero will occur if it is used
!>               to solve a system of equations.
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
!  =====================================================================
      SUBROUTINE ZGETF2( M, N, A, LDA, IPIV, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ZGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the m by n matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION   SFMIN
      INTEGER            I, J, JP
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      INTEGER            IZAMAX
      EXTERNAL           DLAMCH, IZAMAX
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGERU, ZSCAL, ZSWAP
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGETF2', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
!
!     Compute machine safe minimum
!
      SFMIN = DLAMCH('S')
!
      DO 10 J = 1, MIN( M, N )
!
!        Find pivot and test for singularity.
!
         JP = J - 1 + IZAMAX( M-J+1, A( J, J ), 1 )
         IPIV( J ) = JP
         IF( A( JP, J ).NE.ZERO ) THEN
!
!           Apply the interchange to columns 1:N.
!
            IF( JP.NE.J )
     $         CALL ZSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
!
!           Compute elements J+1:M of J-th column.
!
            IF( J.LT.M ) THEN
               IF( ABS(A( J, J )) .GE. SFMIN ) THEN
                  CALL ZSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
               ELSE
                  DO 20 I = 1, M-J
                     A( J+I, J ) = A( J+I, J ) / A( J, J )
   20             CONTINUE
               END IF
            END IF
!
         ELSE IF( INFO.EQ.0 ) THEN
!
            INFO = J
         END IF
!
         IF( J.LT.MIN( M, N ) ) THEN
!
!           Update trailing submatrix.
!
            CALL ZGERU( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ),
     $                  LDA, A( J+1, J+1 ), LDA )
         END IF
   10 CONTINUE
      RETURN
!
!     End of ZGETF2
!
      END
!
! =====================================================================
!
!> @brief  ZLASSQ updates a sum of squares represented in scaled form.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLASSQ( N, X, INCX, SCALE, SUMSQ )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       DOUBLE PRECISION   SCALE, SUMSQ
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         X( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLASSQ returns the values scl and ssq such that
!>
!>    ( scl**2 )*ssq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
!>
!> where x( i ) = abs( X( 1 + ( i - 1 )*INCX ) ). The value of sumsq is
!> assumed to be at least unity and the value of ssq will then satisfy
!>
!>    1.0 .le. ssq .le. ( sumsq + 2*n ).
!>
!> scale is assumed to be non-negative and scl returns the value
!>
!>    scl = max( scale, abs( real( x( i ) ) ), abs( aimag( x( i ) ) ) ),
!>           i
!>
!> scale and sumsq must be supplied in SCALE and SUMSQ respectively.
!> SCALE and SUMSQ are overwritten by scl and ssq respectively.
!>
!> The routine makes only one pass through the vector X.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of elements to be used from the vector X.
!> \endverbatim
!>
!> @param[in] X
!> \verbatim
!>          X is COMPLEX*16 array, dimension (N)
!>          The vector x as described above.
!>             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between successive values of the vector X.
!>          INCX > 0.
!> \endverbatim
!>
!> @param[in,out] SCALE
!> \verbatim
!>          SCALE is DOUBLE PRECISION
!>          On entry, the value  scale  in the equation above.
!>          On exit, SCALE is overwritten with the value  scl .
!> \endverbatim
!>
!> @param[in,out] SUMSQ
!> \verbatim
!>          SUMSQ is DOUBLE PRECISION
!>          On entry, the value  sumsq  in the equation above.
!>          On exit, SUMSQ is overwritten with the value  ssq .
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
!  =====================================================================
      SUBROUTINE ZLASSQ( N, X, INCX, SCALE, SUMSQ )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SCALE, SUMSQ
!     ..
!     .. Array Arguments ..
      COMPLEX*16         X( * )
!     ..
!
!  Purpose
!  =======
!
!  ZLASSQ returns the values scl and ssq such that
!
!     ( scl**2 )*ssq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
!
!  where x( i ) = abs( X( 1 + ( i - 1 )*INCX ) ). The value of sumsq is
!  assumed to be at least unity and the value of ssq will then satisfy
!
!     1.0 .le. ssq .le. ( sumsq + 2*n ).
!
!  scale is assumed to be non-negative and scl returns the value
!
!     scl = max( scale, abs( real( x( i ) ) ), abs( aimag( x( i ) ) ) ),
!            i
!
!  scale and sumsq must be supplied in SCALE and SUMSQ respectively.
!  SCALE and SUMSQ are overwritten by scl and ssq respectively.
!
!  The routine makes only one pass through the vector X.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of elements to be used from the vector X.
!
!  X       (input) COMPLEX*16 array, dimension (N)
!          The vector x as described above.
!             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
!
!  INCX    (input) INTEGER
!          The increment between successive values of the vector X.
!          INCX > 0.
!
!  SCALE   (input/output) DOUBLE PRECISION
!          On entry, the value  scale  in the equation above.
!          On exit, SCALE is overwritten with the value  scl .
!
!  SUMSQ   (input/output) DOUBLE PRECISION
!          On entry, the value  sumsq  in the equation above.
!          On exit, SUMSQ is overwritten with the value  ssq .
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            IX
      DOUBLE PRECISION   TEMP1
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG
!     ..
!     .. Executable Statements ..
!
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            IF( DBLE( X( IX ) ).NE.ZERO ) THEN
               TEMP1 = ABS( DBLE( X( IX ) ) )
               IF( SCALE.LT.TEMP1 ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2
                  SCALE = TEMP1
               ELSE
                  SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2
               END IF
            END IF
            IF( DIMAG( X( IX ) ).NE.ZERO ) THEN
               TEMP1 = ABS( DIMAG( X( IX ) ) )
               IF( SCALE.LT.TEMP1 ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2
                  SCALE = TEMP1
               ELSE
                  SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
!
      RETURN
!
!     End of ZLASSQ
!
      END
!
!  =====================================================================
!
!> @brief  ZLARFG generates an elementary reflector (Householder matrix).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLARFG( N, ALPHA, X, INCX, TAU )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       COMPLEX*16         ALPHA, TAU
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         X( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLARFG generates a complex elementary reflector H of order n, such
!> that
!>
!>       H**H * ( alpha ) = ( beta ),   H**H * H = I.
!>              (   x   )   (   0  )
!>
!> where alpha and beta are scalars, with beta real, and x is an
!> (n-1)-element complex vector. H is represented in the form
!>
!>       H = I - tau * ( 1 ) * ( 1 v**H ) ,
!>                     ( v )
!>
!> where tau is a complex scalar and v is a complex (n-1)-element
!> vector. Note that H is not hermitian.
!>
!> If the elements of x are all zero and alpha is real, then tau = 0
!> and H is taken to be the unit matrix.
!>
!> Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 .
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the elementary reflector.
!> \endverbatim
!>
!> @param[in,out] ALPHA
!> \verbatim
!>          ALPHA is COMPLEX*16
!>          On entry, the value alpha.
!>          On exit, it is overwritten with the value beta.
!> \endverbatim
!>
!> @param[in,out] X
!> \verbatim
!>          X is COMPLEX*16 array, dimension
!>                         (1+(N-2)*abs(INCX))
!>          On entry, the vector x.
!>          On exit, it is overwritten with the vector v.
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between elements of X. INCX > 0.
!> \endverbatim
!>
!> @param[out] TAU
!> \verbatim
!>          TAU is COMPLEX*16
!>          The value tau.
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
!  =====================================================================
      SUBROUTINE ZLARFG( N, ALPHA, X, INCX, TAU )
!
!  -- LAPACK auxiliary routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
      COMPLEX*16         ALPHA, TAU
!     ..
!     .. Array Arguments ..
      COMPLEX*16         X( * )
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            J, KNT
      DOUBLE PRECISION   ALPHI, ALPHR, BETA, RSAFMN, SAFMIN, XNORM
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY3, DZNRM2
      COMPLEX*16         ZLADIV
      EXTERNAL           DLAMCH, DLAPY3, DZNRM2, ZLADIV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, SIGN
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZDSCAL, ZSCAL
!     ..
!     .. Executable Statements ..
!
      IF( N.LE.0 ) THEN
         TAU = ZERO
         RETURN
      END IF
!
      XNORM = DZNRM2( N-1, X, INCX )
      ALPHR = DBLE( ALPHA )
      ALPHI = DIMAG( ALPHA )
!
      IF( XNORM.EQ.ZERO .AND. ALPHI.EQ.ZERO ) THEN
!
!        H  =  I
!
         TAU = ZERO
      ELSE
!
!        general case
!
         BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
         RSAFMN = ONE / SAFMIN
!
         KNT = 0
         IF( ABS( BETA ).LT.SAFMIN ) THEN
!
!           XNORM, BETA may be inaccurate; scale X and recompute them
!
   10       CONTINUE
            KNT = KNT + 1
            CALL ZDSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHI = ALPHI*RSAFMN
            ALPHR = ALPHR*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     $         GO TO 10
!
!           New BETA is at most 1, at least SAFMIN
!
            XNORM = DZNRM2( N-1, X, INCX )
            ALPHA = DCMPLX( ALPHR, ALPHI )
            BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
         END IF
         TAU = DCMPLX( ( BETA-ALPHR ) / BETA, -ALPHI / BETA )
         ALPHA = ZLADIV( DCMPLX( ONE ), ALPHA-BETA )
         CALL ZSCAL( N-1, ALPHA, X, INCX )
!
!        If ALPHA is subnormal, it may lose relative accuracy
!
         DO 20 J = 1, KNT
            BETA = BETA*SAFMIN
 20      CONTINUE
         ALPHA = BETA
      END IF
!
      RETURN
!
!     End of ZLARFG
!
      END
!
!  =====================================================================
!
!> @brief  ZGEQR2 computes the QR factorization of a general rectangular matrix using an unblocked algorithm.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZGEQR2( M, N, A, LDA, TAU, WORK, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, LDA, M, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZGEQR2 computes a QR factorization of a complex m by n matrix A:
!> A = Q * R.
!> \endverbatim
!
!  Arguments:
!  ==========
!
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
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the m by n matrix A.
!>          On exit, the elements on and above the diagonal of the array
!>          contain the min(m,n) by n upper trapezoidal matrix R (R is
!>          upper triangular if m >= n); the elements below the diagonal,
!>          with the array TAU, represent the unitary matrix Q as a
!>          product of elementary reflectors (see Further Details).
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> @param[out] TAU
!> \verbatim
!>          TAU is COMPLEX*16 array, dimension (min(M,N))
!>          The scalar factors of the elementary reflectors (see Further
!>          Details).
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension (N)
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
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
!>  The matrix Q is represented as a product of elementary reflectors
!>
!>     Q = H(1) H(2) . . . H(k), where k = min(m,n).
!>
!>  Each H(i) has the form
!>
!>     H(i) = I - tau * v * v**H
!>
!>  where tau is a complex scalar, and v is a complex vector with
!>  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
!>  and tau in TAU(i).
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZGEQR2( M, N, A, LDA, TAU, WORK, INFO )
!
!  -- LAPACK computational routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      INTEGER            I, K
      COMPLEX*16         ALPHA
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARF, ZLARFG
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEQR2', -INFO )
         RETURN
      END IF
!
      K = MIN( M, N )
!
      DO 10 I = 1, K
!
!        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
!
         CALL ZLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                TAU( I ) )
         IF( I.LT.N ) THEN
!
!           Apply H(i)**H to A(i:m,i+1:n) from the left
!
            ALPHA = A( I, I )
            A( I, I ) = ONE
            CALL ZLARF( 'Left', M-I+1, N-I, A( I, I ), 1,
     $                  DCONJG( TAU( I ) ), A( I, I+1 ), LDA, WORK )
            A( I, I ) = ALPHA
         END IF
   10 CONTINUE
      RETURN
!
!     End of ZGEQR2
!
      END
!
!  =====================================================================
!
!> @brief  ZLARFT forms the triangular factor T of a block reflector H = I - vtvH
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
!
!       .. Scalar Arguments ..
!       CHARACTER          DIRECT, STOREV
!       INTEGER            K, LDT, LDV, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         T( LDT, * ), TAU( * ), V( LDV, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLARFT forms the triangular factor T of a complex block reflector H
!> of order n, which is defined as a product of k elementary reflectors.
!>
!> If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
!>
!> If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
!>
!> If STOREV = 'C', the vector which defines the elementary reflector
!> H(i) is stored in the i-th column of the array V, and
!>
!>    H  =  I - V * T * V**H
!>
!> If STOREV = 'R', the vector which defines the elementary reflector
!> H(i) is stored in the i-th row of the array V, and
!>
!>    H  =  I - V**H * T * V
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] DIRECT
!> \verbatim
!>          DIRECT is CHARACTER*1
!>          Specifies the order in which the elementary reflectors are
!>          multiplied to form the block reflector:
!>          = 'F': H = H(1) H(2) . . . H(k) (Forward)
!>          = 'B': H = H(k) . . . H(2) H(1) (Backward)
!> \endverbatim
!>
!> @param[in] STOREV
!> \verbatim
!>          STOREV is CHARACTER*1
!>          Specifies how the vectors which define the elementary
!>          reflectors are stored (see also Further Details):
!>          = 'C': columnwise
!>          = 'R': rowwise
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the block reflector H. N >= 0.
!> \endverbatim
!>
!> @param[in] K
!> \verbatim
!>          K is INTEGER
!>          The order of the triangular factor T (= the number of
!>          elementary reflectors). K >= 1.
!> \endverbatim
!>
!> @param[in] V
!> \verbatim
!>          V is COMPLEX*16 array, dimension
!>                               (LDV,K) if STOREV = 'C'
!>                               (LDV,N) if STOREV = 'R'
!>          The matrix V. See further details.
!> \endverbatim
!>
!> @param[in] LDV
!> \verbatim
!>          LDV is INTEGER
!>          The leading dimension of the array V.
!>          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
!> \endverbatim
!>
!> @param[in] TAU
!> \verbatim
!>          TAU is COMPLEX*16 array, dimension (K)
!>          TAU(i) must contain the scalar factor of the elementary
!>          reflector H(i).
!> \endverbatim
!>
!> @param[out] T
!> \verbatim
!>          T is COMPLEX*16 array, dimension (LDT,K)
!>          The k by k triangular factor T of the block reflector.
!>          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
!>          lower triangular. The rest of the array is not used.
!> \endverbatim
!>
!> @param[in] LDT
!> \verbatim
!>          LDT is INTEGER
!>          The leading dimension of the array T. LDT >= K.
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
!>  The shape of the matrix V and the storage of the vectors which define
!>  the H(i) is best illustrated by the following example with n = 5 and
!>  k = 3. The elements equal to 1 are not stored.
!>
!>  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
!>
!>               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
!>                   ( v1  1    )                     (     1 v2 v2 v2 )
!>                   ( v1 v2  1 )                     (        1 v3 v3 )
!>                   ( v1 v2 v3 )
!>                   ( v1 v2 v3 )
!>
!>  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
!>
!>               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
!>                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
!>                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
!>                   (     1 v3 )
!>                   (        1 )
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         T( LDT, * ), TAU( * ), V( LDV, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLARFT forms the triangular factor T of a complex block reflector H
!  of order n, which is defined as a product of k elementary reflectors.
!
!  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
!
!  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
!
!  If STOREV = 'C', the vector which defines the elementary reflector
!  H(i) is stored in the i-th column of the array V, and
!
!     H  =  I - V * T * V'
!
!  If STOREV = 'R', the vector which defines the elementary reflector
!  H(i) is stored in the i-th row of the array V, and
!
!     H  =  I - V' * T * V
!
!  Arguments
!  =========
!
!  DIRECT  (input) CHARACTER*1
!          Specifies the order in which the elementary reflectors are
!          multiplied to form the block reflector:
!          = 'F': H = H(1) H(2) . . . H(k) (Forward)
!          = 'B': H = H(k) . . . H(2) H(1) (Backward)
!
!  STOREV  (input) CHARACTER*1
!          Specifies how the vectors which define the elementary
!          reflectors are stored (see also Further Details):
!          = 'C': columnwise
!          = 'R': rowwise
!
!  N       (input) INTEGER
!          The order of the block reflector H. N >= 0.
!
!  K       (input) INTEGER
!          The order of the triangular factor T (= the number of
!          elementary reflectors). K >= 1.
!
!  V       (input/output) COMPLEX*16 array, dimension
!                               (LDV,K) if STOREV = 'C'
!                               (LDV,N) if STOREV = 'R'
!          The matrix V. See further details.
!
!  LDV     (input) INTEGER
!          The leading dimension of the array V.
!          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
!
!  TAU     (input) COMPLEX*16 array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i).
!
!  T       (output) COMPLEX*16 array, dimension (LDT,K)
!          The k by k triangular factor T of the block reflector.
!          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
!          lower triangular. The rest of the array is not used.
!
!  LDT     (input) INTEGER
!          The leading dimension of the array T. LDT >= K.
!
!  Further Details
!  ===============
!
!  The shape of the matrix V and the storage of the vectors which define
!  the H(i) is best illustrated by the following example with n = 5 and
!  k = 3. The elements equal to 1 are not stored; the corresponding
!  array elements are modified but restored on exit. The rest of the
!  array is not used.
!
!  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
!
!               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
!                   ( v1  1    )                     (     1 v2 v2 v2 )
!                   ( v1 v2  1 )                     (        1 v3 v3 )
!                   ( v1 v2 v3 )
!                   ( v1 v2 v3 )
!
!  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
!
!               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
!                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
!                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
!                   (     1 v3 )
!                   (        1 )
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J, PREVLASTV, LASTV
      COMPLEX*16         VII
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZGEMV, ZLACGV, ZTRMV
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. Executable Statements ..
!
!     Quick return if possible
!
      IF( N.EQ.0 )
     $   RETURN
!
      IF( LSAME( DIRECT, 'F' ) ) THEN
         PREVLASTV = N
         DO 20 I = 1, K
            PREVLASTV = MAX( PREVLASTV, I )
            IF( TAU( I ).EQ.ZERO ) THEN
!
!              H(i)  =  I
!
               DO 10 J = 1, I
                  T( J, I ) = ZERO
   10          CONTINUE
            ELSE
!
!              general case
!
               VII = V( I, I )
               V( I, I ) = ONE
               IF( LSAME( STOREV, 'C' ) ) THEN
!                 Skip any trailing zeros.
                  DO LASTV = N, I+1, -1
                     IF( V( LASTV, I ).NE.ZERO ) EXIT
                  END DO
                  J = MIN( LASTV, PREVLASTV )
!
!                 T(1:i-1,i) := - tau(i) * V(i:j,1:i-1)' * V(i:j,i)
!
                  CALL ZGEMV( 'Conjugate transpose', J-I+1, I-1,
     $                        -TAU( I ), V( I, 1 ), LDV, V( I, I ), 1,
     $                        ZERO, T( 1, I ), 1 )
               ELSE
!                 Skip any trailing zeros.
                  DO LASTV = N, I+1, -1
                     IF( V( I, LASTV ).NE.ZERO ) EXIT
                  END DO
                  J = MIN( LASTV, PREVLASTV )
!
!                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:j) * V(i,i:j)'
!
                  IF( I.LT.J )
     $               CALL ZLACGV( J-I, V( I, I+1 ), LDV )
                  CALL ZGEMV( 'No transpose', I-1, J-I+1, -TAU( I ),
     $                        V( 1, I ), LDV, V( I, I ), LDV, ZERO,
     $                        T( 1, I ), 1 )
                  IF( I.LT.J )
     $               CALL ZLACGV( J-I, V( I, I+1 ), LDV )
               END IF
               V( I, I ) = VII
!
!              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
!
               CALL ZTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T,
     $                     LDT, T( 1, I ), 1 )
               T( I, I ) = TAU( I )
               IF( I.GT.1 ) THEN
                  PREVLASTV = MAX( PREVLASTV, LASTV )
               ELSE
                  PREVLASTV = LASTV
               END IF
             END IF
   20    CONTINUE
      ELSE
         PREVLASTV = 1
         DO 40 I = K, 1, -1
            IF( TAU( I ).EQ.ZERO ) THEN
!
!              H(i)  =  I
!
               DO 30 J = I, K
                  T( J, I ) = ZERO
   30          CONTINUE
            ELSE
!
!              general case
!
               IF( I.LT.K ) THEN
                  IF( LSAME( STOREV, 'C' ) ) THEN
                     VII = V( N-K+I, I )
                     V( N-K+I, I ) = ONE
!                    Skip any leading zeros.
                     DO LASTV = 1, I-1
                        IF( V( LASTV, I ).NE.ZERO ) EXIT
                     END DO
                     J = MAX( LASTV, PREVLASTV )
!
!                    T(i+1:k,i) :=
!                            - tau(i) * V(j:n-k+i,i+1:k)' * V(j:n-k+i,i)
!
                     CALL ZGEMV( 'Conjugate transpose', N-K+I-J+1, K-I,
     $                           -TAU( I ), V( J, I+1 ), LDV, V( J, I ),
     $                           1, ZERO, T( I+1, I ), 1 )
                     V( N-K+I, I ) = VII
                  ELSE
                     VII = V( I, N-K+I )
                     V( I, N-K+I ) = ONE
!                    Skip any leading zeros.
                     DO LASTV = 1, I-1
                        IF( V( I, LASTV ).NE.ZERO ) EXIT
                     END DO
                     J = MAX( LASTV, PREVLASTV )
!
!                    T(i+1:k,i) :=
!                            - tau(i) * V(i+1:k,j:n-k+i) * V(i,j:n-k+i)'
!
                     CALL ZLACGV( N-K+I-1-J+1, V( I, J ), LDV )
                     CALL ZGEMV( 'No transpose', K-I, N-K+I-J+1,
     $                    -TAU( I ), V( I+1, J ), LDV, V( I, J ), LDV,
     $                    ZERO, T( I+1, I ), 1 )
                     CALL ZLACGV( N-K+I-1-J+1, V( I, J ), LDV )
                     V( I, N-K+I ) = VII
                  END IF
!
!                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
!
                  CALL ZTRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                        T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
                  IF( I.GT.1 ) THEN
                     PREVLASTV = MIN( PREVLASTV, LASTV )
                  ELSE
                     PREVLASTV = LASTV
                  END IF
               END IF
               T( I, I ) = TAU( I )
            END IF
   40    CONTINUE
      END IF
      RETURN
!
!     End of ZLARFT
!
      END
!
!  =====================================================================
!
!> @brief  ILAZLR scans a matrix for its last non-zero row.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       INTEGER FUNCTION ILAZLR( M, N, A, LDA )
!
!       .. Scalar Arguments ..
!       INTEGER            M, N, LDA
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
!> ILAZLR scans A for its last non-zero row.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix A.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix A.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          The m by n matrix A.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A. LDA >= max(1,M).
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
!  =====================================================================
      INTEGER FUNCTION ILAZLR(M, N, A, LDA)
      IMPLICIT NONE
!
!  -- LAPACK auxiliary routine (version 3.2.1)                        --
!
!  -- April 2009                                                      --
!
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!
!     .. Scalar Arguments ..
      INTEGER            M, N, LDA
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ILAZLR scans A for its last non-zero row.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The m by n matrix A.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A. LDA >= max(1,M).
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16       ZERO
      PARAMETER ( ZERO = (0.0D+0, 0.0D+0) )
!     ..
!     .. Local Scalars ..
      INTEGER I, J
!     ..
!     .. Executable Statements ..
!
!     Quick test for the common case where one corner is non-zero.
      IF( M.EQ.0 ) THEN
         ILAZLR = M
      ELSE IF( A(M, 1).NE.ZERO .OR. A(M, N).NE.ZERO ) THEN
         ILAZLR = M
      ELSE
!     Scan up each column tracking the last zero row seen.
         ILAZLR = 0
         DO J = 1, N
            DO I = M, 1, -1
               IF( A(I, J).NE.ZERO ) EXIT
            END DO
            ILAZLR = MAX( ILAZLR, I )
         END DO
      END IF
      RETURN
!
!     End of ILAZLR
!
      END FUNCTION
!
!  =====================================================================
!
!> @brief  ILAZLC scans a matrix for its last non-zero column.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       INTEGER FUNCTION ILAZLC( M, N, A, LDA )
!
!       .. Scalar Arguments ..
!       INTEGER            M, N, LDA
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
!> ILAZLC scans A for its last non-zero column.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix A.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix A.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          The m by n matrix A.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A. LDA >= max(1,M).
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
!  =====================================================================
      INTEGER FUNCTION ILAZLC(M, N, A, LDA)
      IMPLICIT NONE
!
!  -- LAPACK auxiliary routine (version 3.2.1)                        --
!
!  -- April 2009                                                      --
!
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!
!     .. Scalar Arguments ..
      INTEGER            M, N, LDA
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ILAZLC scans A for its last non-zero column.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The m by n matrix A.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A. LDA >= max(1,M).
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16       ZERO
      PARAMETER ( ZERO = (0.0D+0, 0.0D+0) )
!     ..
!     .. Local Scalars ..
      INTEGER I
!     ..
!     .. Executable Statements ..
!
!     Quick test for the common case where one corner is non-zero.
      IF( N.EQ.0 ) THEN
         ILAZLC = N
      ELSE IF( A(1, N).NE.ZERO .OR. A(M, N).NE.ZERO ) THEN
         ILAZLC = N
      ELSE
!     Now scan each column from the end, returning with the first non-zero.
         DO ILAZLC = N, 1, -1
            DO I = 1, M
               IF( A(I, ILAZLC).NE.ZERO ) RETURN
            END DO
         END DO
      END IF
      RETURN
!
!     End of ILAZLC
!
      END FUNCTION
!
!  =====================================================================
!
!> @brief  ZLARFB applies a block reflector or its conjugate-transpose to a general rectangular matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
!                          T, LDT, C, LDC, WORK, LDWORK )
!
!       .. Scalar Arguments ..
!       CHARACTER          DIRECT, SIDE, STOREV, TRANS
!       INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         C( LDC, * ), T( LDT, * ), V( LDV, * ),
!      $                   WORK( LDWORK, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLARFB applies a complex block reflector H or its transpose H**H to a
!> complex M-by-N matrix C, from either the left or the right.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] SIDE
!> \verbatim
!>          SIDE is CHARACTER*1
!>          = 'L': apply H or H**H from the Left
!>          = 'R': apply H or H**H from the Right
!> \endverbatim
!>
!> @param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>          = 'N': apply H (No transpose)
!>          = 'C': apply H**H (Conjugate transpose)
!> \endverbatim
!>
!> @param[in] DIRECT
!> \verbatim
!>          DIRECT is CHARACTER*1
!>          Indicates how H is formed from a product of elementary
!>          reflectors
!>          = 'F': H = H(1) H(2) . . . H(k) (Forward)
!>          = 'B': H = H(k) . . . H(2) H(1) (Backward)
!> \endverbatim
!>
!> @param[in] STOREV
!> \verbatim
!>          STOREV is CHARACTER*1
!>          Indicates how the vectors which define the elementary
!>          reflectors are stored:
!>          = 'C': Columnwise
!>          = 'R': Rowwise
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix C.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix C.
!> \endverbatim
!>
!> @param[in] K
!> \verbatim
!>          K is INTEGER
!>          The order of the matrix T (= the number of elementary
!>          reflectors whose product defines the block reflector).
!> \endverbatim
!>
!> @param[in] V
!> \verbatim
!>          V is COMPLEX*16 array, dimension
!>                                (LDV,K) if STOREV = 'C'
!>                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
!>                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
!>          See Further Details.
!> \endverbatim
!>
!> @param[in] LDV
!> \verbatim
!>          LDV is INTEGER
!>          The leading dimension of the array V.
!>          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
!>          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
!>          if STOREV = 'R', LDV >= K.
!> \endverbatim
!>
!> @param[in] T
!> \verbatim
!>          T is COMPLEX*16 array, dimension (LDT,K)
!>          The triangular K-by-K matrix T in the representation of the
!>          block reflector.
!> \endverbatim
!>
!> @param[in] LDT
!> \verbatim
!>          LDT is INTEGER
!>          The leading dimension of the array T. LDT >= K.
!> \endverbatim
!>
!> @param[in,out] C
!> \verbatim
!>          C is COMPLEX*16 array, dimension (LDC,N)
!>          On entry, the M-by-N matrix C.
!>          On exit, C is overwritten by H*C or H**H*C or C*H or C*H**H.
!> \endverbatim
!>
!> @param[in] LDC
!> \verbatim
!>          LDC is INTEGER
!>          The leading dimension of the array C. LDC >= max(1,M).
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension (LDWORK,K)
!> \endverbatim
!>
!> @param[in] LDWORK
!> \verbatim
!>          LDWORK is INTEGER
!>          The leading dimension of the array WORK.
!>          If SIDE = 'L', LDWORK >= max(1,N);
!>          if SIDE = 'R', LDWORK >= max(1,M).
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
!> @date June 2013
!
!  Further Details:
!  =====================
!>
!> \verbatim
!>
!>  The shape of the matrix V and the storage of the vectors which define
!>  the H(i) is best illustrated by the following example with n = 5 and
!>  k = 3. The elements equal to 1 are not stored; the corresponding
!>  array elements are modified but restored on exit. The rest of the
!>  array is not used.
!>
!>  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
!>
!>               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
!>                   ( v1  1    )                     (     1 v2 v2 v2 )
!>                   ( v1 v2  1 )                     (        1 v3 v3 )
!>                   ( v1 v2 v3 )
!>                   ( v1 v2 v3 )
!>
!>  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
!>
!>               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
!>                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
!>                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
!>                   (     1 v3 )
!>                   (        1 )
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE ZLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
     $                   T, LDT, C, LDC, WORK, LDWORK )
      IMPLICIT NONE
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLARFB applies a complex block reflector H or its transpose H' to a
!  complex M-by-N matrix C, from either the left or the right.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply H or H' from the Left
!          = 'R': apply H or H' from the Right
!
!  TRANS   (input) CHARACTER*1
!          = 'N': apply H (No transpose)
!          = 'C': apply H' (Conjugate transpose)
!
!  DIRECT  (input) CHARACTER*1
!          Indicates how H is formed from a product of elementary
!          reflectors
!          = 'F': H = H(1) H(2) . . . H(k) (Forward)
!          = 'B': H = H(k) . . . H(2) H(1) (Backward)
!
!  STOREV  (input) CHARACTER*1
!          Indicates how the vectors which define the elementary
!          reflectors are stored:
!          = 'C': Columnwise
!          = 'R': Rowwise
!
!  M       (input) INTEGER
!          The number of rows of the matrix C.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C.
!
!  K       (input) INTEGER
!          The order of the matrix T (= the number of elementary
!          reflectors whose product defines the block reflector).
!
!  V       (input) COMPLEX*16 array, dimension
!                                (LDV,K) if STOREV = 'C'
!                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
!                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
!          The matrix V. See further details.
!
!  LDV     (input) INTEGER
!          The leading dimension of the array V.
!          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
!          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
!          if STOREV = 'R', LDV >= K.
!
!  T       (input) COMPLEX*16 array, dimension (LDT,K)
!          The triangular K-by-K matrix T in the representation of the
!          block reflector.
!
!  LDT     (input) INTEGER
!          The leading dimension of the array T. LDT >= K.
!
!  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
!          On entry, the M-by-N matrix C.
!          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace) COMPLEX*16 array, dimension (LDWORK,K)
!
!  LDWORK  (input) INTEGER
!          The leading dimension of the array WORK.
!          If SIDE = 'L', LDWORK >= max(1,N);
!          if SIDE = 'R', LDWORK >= max(1,M).
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      CHARACTER          TRANST
      INTEGER            I, J, LASTV, LASTC
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAZLR, ILAZLC
      EXTERNAL           LSAME, ILAZLR, ILAZLC
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZCOPY, ZGEMM, ZLACGV, ZTRMM
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
!     ..
!     .. Executable Statements ..
!
!     Quick return if possible
!
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
!
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'C'
      ELSE
         TRANST = 'N'
      END IF
!
      IF( LSAME( STOREV, 'C' ) ) THEN
!
         IF( LSAME( DIRECT, 'F' ) ) THEN
!
!           Let  V =  ( V1 )    (first K rows)
!                     ( V2 )
!           where  V1  is unit lower triangular.
!
            IF( LSAME( SIDE, 'L' ) ) THEN
!
!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )
!
               LASTV = MAX( K, ILAZLR( M, K, V, LDV ) )
               LASTC = ILAZLC( LASTV, N, C, LDC )
!
!              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
!
!              W := C1'
!
               DO 10 J = 1, K
                  CALL ZCOPY( LASTC, C( J, 1 ), LDC, WORK( 1, J ), 1 )
                  CALL ZLACGV( LASTC, WORK( 1, J ), 1 )
   10          CONTINUE
!
!              W := W * V1
!
               CALL ZTRMM( 'Right', 'Lower', 'No transpose', 'Unit',
     $              LASTC, K, ONE, V, LDV, WORK, LDWORK )
               IF( LASTV.GT.K ) THEN
!
!                 W := W + C2'*V2
!
                  CALL ZGEMM( 'Conjugate transpose', 'No transpose',
     $                 LASTC, K, LASTV-K, ONE, C( K+1, 1 ), LDC,
     $                 V( K+1, 1 ), LDV, ONE, WORK, LDWORK )
               END IF
!
!              W := W * T'  or  W * T
!
               CALL ZTRMM( 'Right', 'Upper', TRANST, 'Non-unit',
     $              LASTC, K, ONE, T, LDT, WORK, LDWORK )
!
!              C := C - V * W'
!
               IF( M.GT.K ) THEN
!
!                 C2 := C2 - V2 * W'
!
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose',
     $                 LASTV-K, LASTC, K,
     $                 -ONE, V( K+1, 1 ), LDV, WORK, LDWORK,
     $                 ONE, C( K+1, 1 ), LDC )
               END IF
!
!              W := W * V1'
!
               CALL ZTRMM( 'Right', 'Lower', 'Conjugate transpose',
     $              'Unit', LASTC, K, ONE, V, LDV, WORK, LDWORK )
!
!              C1 := C1 - W'
!
               DO 30 J = 1, K
                  DO 20 I = 1, LASTC
                     C( J, I ) = C( J, I ) - DCONJG( WORK( I, J ) )
   20             CONTINUE
   30          CONTINUE
!
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!
!              Form  C * H  or  C * H'  where  C = ( C1  C2 )
!
               LASTV = MAX( K, ILAZLR( N, K, V, LDV ) )
               LASTC = ILAZLR( M, LASTV, C, LDC )
!
!              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
!
!              W := C1
!
               DO 40 J = 1, K
                  CALL ZCOPY( LASTC, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          CONTINUE
!
!              W := W * V1
!
               CALL ZTRMM( 'Right', 'Lower', 'No transpose', 'Unit',
     $              LASTC, K, ONE, V, LDV, WORK, LDWORK )
               IF( LASTV.GT.K ) THEN
!
!                 W := W + C2 * V2
!
                  CALL ZGEMM( 'No transpose', 'No transpose',
     $                 LASTC, K, LASTV-K,
     $                 ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV,
     $                 ONE, WORK, LDWORK )
               END IF
!
!              W := W * T  or  W * T'
!
               CALL ZTRMM( 'Right', 'Upper', TRANS, 'Non-unit',
     $              LASTC, K, ONE, T, LDT, WORK, LDWORK )
!
!              C := C - W * V'
!
               IF( LASTV.GT.K ) THEN
!
!                 C2 := C2 - W * V2'
!
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose',
     $                 LASTC, LASTV-K, K,
     $                 -ONE, WORK, LDWORK, V( K+1, 1 ), LDV,
     $                 ONE, C( 1, K+1 ), LDC )
               END IF
!
!              W := W * V1'
!
               CALL ZTRMM( 'Right', 'Lower', 'Conjugate transpose',
     $              'Unit', LASTC, K, ONE, V, LDV, WORK, LDWORK )
!
!              C1 := C1 - W
!
               DO 60 J = 1, K
                  DO 50 I = 1, LASTC
                     C( I, J ) = C( I, J ) - WORK( I, J )
   50             CONTINUE
   60          CONTINUE
            END IF
!
         ELSE
!
!           Let  V =  ( V1 )
!                     ( V2 )    (last K rows)
!           where  V2  is unit upper triangular.
!
            IF( LSAME( SIDE, 'L' ) ) THEN
!
!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )
!
               LASTV = MAX( K, ILAZLR( M, K, V, LDV ) )
               LASTC = ILAZLC( LASTV, N, C, LDC )
!
!              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
!
!              W := C2'
!
               DO 70 J = 1, K
                  CALL ZCOPY( LASTC, C( LASTV-K+J, 1 ), LDC,
     $                 WORK( 1, J ), 1 )
                  CALL ZLACGV( LASTC, WORK( 1, J ), 1 )
   70          CONTINUE
!
!              W := W * V2
!
               CALL ZTRMM( 'Right', 'Upper', 'No transpose', 'Unit',
     $              LASTC, K, ONE, V( LASTV-K+1, 1 ), LDV,
     $              WORK, LDWORK )
               IF( LASTV.GT.K ) THEN
!
!                 W := W + C1'*V1
!
                  CALL ZGEMM( 'Conjugate transpose', 'No transpose',
     $                 LASTC, K, LASTV-K,
     $                 ONE, C, LDC, V, LDV,
     $                 ONE, WORK, LDWORK )
               END IF
!
!              W := W * T'  or  W * T
!
               CALL ZTRMM( 'Right', 'Lower', TRANST, 'Non-unit',
     $              LASTC, K, ONE, T, LDT, WORK, LDWORK )
!
!              C := C - V * W'
!
               IF( LASTV.GT.K ) THEN
!
!                 C1 := C1 - V1 * W'
!
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose',
     $                 LASTV-K, LASTC, K,
     $                 -ONE, V, LDV, WORK, LDWORK,
     $                 ONE, C, LDC )
               END IF
!
!              W := W * V2'
!
               CALL ZTRMM( 'Right', 'Upper', 'Conjugate transpose',
     $              'Unit', LASTC, K, ONE, V( LASTV-K+1, 1 ), LDV,
     $              WORK, LDWORK )
!
!              C2 := C2 - W'
!
               DO 90 J = 1, K
                  DO 80 I = 1, LASTC
                     C( LASTV-K+J, I ) = C( LASTV-K+J, I ) -
     $                               DCONJG( WORK( I, J ) )
   80             CONTINUE
   90          CONTINUE
!
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!
!              Form  C * H  or  C * H'  where  C = ( C1  C2 )
!
               LASTV = MAX( K, ILAZLR( N, K, V, LDV ) )
               LASTC = ILAZLR( M, LASTV, C, LDC )
!
!              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
!
!              W := C2
!
               DO 100 J = 1, K
                  CALL ZCOPY( LASTC, C( 1, LASTV-K+J ), 1,
     $                 WORK( 1, J ), 1 )
  100          CONTINUE
!
!              W := W * V2
!
               CALL ZTRMM( 'Right', 'Upper', 'No transpose', 'Unit',
     $              LASTC, K, ONE, V( LASTV-K+1, 1 ), LDV,
     $              WORK, LDWORK )
               IF( LASTV.GT.K ) THEN
!
!                 W := W + C1 * V1
!
                  CALL ZGEMM( 'No transpose', 'No transpose',
     $                 LASTC, K, LASTV-K,
     $                 ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
!
!              W := W * T  or  W * T'
!
               CALL ZTRMM( 'Right', 'Lower', TRANS, 'Non-unit',
     $              LASTC, K, ONE, T, LDT, WORK, LDWORK )
!
!              C := C - W * V'
!
               IF( LASTV.GT.K ) THEN
!
!                 C1 := C1 - W * V1'
!
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose',
     $                 LASTC, LASTV-K, K, -ONE, WORK, LDWORK, V, LDV,
     $                 ONE, C, LDC )
               END IF
!
!              W := W * V2'
!
               CALL ZTRMM( 'Right', 'Upper', 'Conjugate transpose',
     $              'Unit', LASTC, K, ONE, V( LASTV-K+1, 1 ), LDV,
     $              WORK, LDWORK )
!
!              C2 := C2 - W
!
               DO 120 J = 1, K
                  DO 110 I = 1, LASTC
                     C( I, LASTV-K+J ) = C( I, LASTV-K+J )
     $                    - WORK( I, J )
  110             CONTINUE
  120          CONTINUE
            END IF
         END IF
!
      ELSE IF( LSAME( STOREV, 'R' ) ) THEN
!
         IF( LSAME( DIRECT, 'F' ) ) THEN
!
!           Let  V =  ( V1  V2 )    (V1: first K columns)
!           where  V1  is unit upper triangular.
!
            IF( LSAME( SIDE, 'L' ) ) THEN
!
!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )
!
               LASTV = MAX( K, ILAZLC( K, M, V, LDV ) )
               LASTC = ILAZLC( LASTV, N, C, LDC )
!
!              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
!
!              W := C1'
!
               DO 130 J = 1, K
                  CALL ZCOPY( LASTC, C( J, 1 ), LDC, WORK( 1, J ), 1 )
                  CALL ZLACGV( LASTC, WORK( 1, J ), 1 )
  130          CONTINUE
!
!              W := W * V1'
!
               CALL ZTRMM( 'Right', 'Upper', 'Conjugate transpose',
     $                     'Unit', LASTC, K, ONE, V, LDV, WORK, LDWORK )
               IF( LASTV.GT.K ) THEN
!
!                 W := W + C2'*V2'
!
                  CALL ZGEMM( 'Conjugate transpose',
     $                 'Conjugate transpose', LASTC, K, LASTV-K,
     $                 ONE, C( K+1, 1 ), LDC, V( 1, K+1 ), LDV,
     $                 ONE, WORK, LDWORK )
               END IF
!
!              W := W * T'  or  W * T
!
               CALL ZTRMM( 'Right', 'Upper', TRANST, 'Non-unit',
     $              LASTC, K, ONE, T, LDT, WORK, LDWORK )
!
!              C := C - V' * W'
!
               IF( LASTV.GT.K ) THEN
!
!                 C2 := C2 - V2' * W'
!
                  CALL ZGEMM( 'Conjugate transpose',
     $                 'Conjugate transpose', LASTV-K, LASTC, K,
     $                 -ONE, V( 1, K+1 ), LDV, WORK, LDWORK,
     $                 ONE, C( K+1, 1 ), LDC )
               END IF
!
!              W := W * V1
!
               CALL ZTRMM( 'Right', 'Upper', 'No transpose', 'Unit',
     $              LASTC, K, ONE, V, LDV, WORK, LDWORK )
!
!              C1 := C1 - W'
!
               DO 150 J = 1, K
                  DO 140 I = 1, LASTC
                     C( J, I ) = C( J, I ) - DCONJG( WORK( I, J ) )
  140             CONTINUE
  150          CONTINUE
!
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!
!              Form  C * H  or  C * H'  where  C = ( C1  C2 )
!
               LASTV = MAX( K, ILAZLC( K, N, V, LDV ) )
               LASTC = ILAZLR( M, LASTV, C, LDC )
!
!              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
!
!              W := C1
!
               DO 160 J = 1, K
                  CALL ZCOPY( LASTC, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          CONTINUE
!
!              W := W * V1'
!
               CALL ZTRMM( 'Right', 'Upper', 'Conjugate transpose',
     $                     'Unit', LASTC, K, ONE, V, LDV, WORK, LDWORK )
               IF( LASTV.GT.K ) THEN
!
!                 W := W + C2 * V2'
!
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose',
     $                 LASTC, K, LASTV-K, ONE, C( 1, K+1 ), LDC,
     $                 V( 1, K+1 ), LDV, ONE, WORK, LDWORK )
               END IF
!
!              W := W * T  or  W * T'
!
               CALL ZTRMM( 'Right', 'Upper', TRANS, 'Non-unit',
     $              LASTC, K, ONE, T, LDT, WORK, LDWORK )
!
!              C := C - W * V
!
               IF( LASTV.GT.K ) THEN
!
!                 C2 := C2 - W * V2
!
                  CALL ZGEMM( 'No transpose', 'No transpose',
     $                 LASTC, LASTV-K, K,
     $                 -ONE, WORK, LDWORK, V( 1, K+1 ), LDV,
     $                 ONE, C( 1, K+1 ), LDC )
               END IF
!
!              W := W * V1
!
               CALL ZTRMM( 'Right', 'Upper', 'No transpose', 'Unit',
     $              LASTC, K, ONE, V, LDV, WORK, LDWORK )
!
!              C1 := C1 - W
!
               DO 180 J = 1, K
                  DO 170 I = 1, LASTC
                     C( I, J ) = C( I, J ) - WORK( I, J )
  170             CONTINUE
  180          CONTINUE
!
            END IF
!
         ELSE
!
!           Let  V =  ( V1  V2 )    (V2: last K columns)
!           where  V2  is unit lower triangular.
!
            IF( LSAME( SIDE, 'L' ) ) THEN
!
!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )
!
               LASTV = MAX( K, ILAZLC( K, M, V, LDV ) )
               LASTC = ILAZLC( LASTV, N, C, LDC )
!
!              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
!
!              W := C2'
!
               DO 190 J = 1, K
                  CALL ZCOPY( LASTC, C( LASTV-K+J, 1 ), LDC,
     $                 WORK( 1, J ), 1 )
                  CALL ZLACGV( LASTC, WORK( 1, J ), 1 )
  190          CONTINUE
!
!              W := W * V2'
!
               CALL ZTRMM( 'Right', 'Lower', 'Conjugate transpose',
     $              'Unit', LASTC, K, ONE, V( 1, LASTV-K+1 ), LDV,
     $              WORK, LDWORK )
               IF( LASTV.GT.K ) THEN
!
!                 W := W + C1'*V1'
!
                  CALL ZGEMM( 'Conjugate transpose',
     $                 'Conjugate transpose', LASTC, K, LASTV-K,
     $                 ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
!
!              W := W * T'  or  W * T
!
               CALL ZTRMM( 'Right', 'Lower', TRANST, 'Non-unit',
     $              LASTC, K, ONE, T, LDT, WORK, LDWORK )
!
!              C := C - V' * W'
!
               IF( LASTV.GT.K ) THEN
!
!                 C1 := C1 - V1' * W'
!
                  CALL ZGEMM( 'Conjugate transpose',
     $                 'Conjugate transpose', LASTV-K, LASTC, K,
     $                 -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
!
!              W := W * V2
!
               CALL ZTRMM( 'Right', 'Lower', 'No transpose', 'Unit',
     $              LASTC, K, ONE, V( 1, LASTV-K+1 ), LDV,
     $              WORK, LDWORK )
!
!              C2 := C2 - W'
!
               DO 210 J = 1, K
                  DO 200 I = 1, LASTC
                     C( LASTV-K+J, I ) = C( LASTV-K+J, I ) -
     $                               DCONJG( WORK( I, J ) )
  200             CONTINUE
  210          CONTINUE
!
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!
!              Form  C * H  or  C * H'  where  C = ( C1  C2 )
!
               LASTV = MAX( K, ILAZLC( K, N, V, LDV ) )
               LASTC = ILAZLR( M, LASTV, C, LDC )
!
!              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
!
!              W := C2
!
               DO 220 J = 1, K
                  CALL ZCOPY( LASTC, C( 1, LASTV-K+J ), 1,
     $                 WORK( 1, J ), 1 )
  220          CONTINUE
!
!              W := W * V2'
!
               CALL ZTRMM( 'Right', 'Lower', 'Conjugate transpose',
     $              'Unit', LASTC, K, ONE, V( 1, LASTV-K+1 ), LDV,
     $              WORK, LDWORK )
               IF( LASTV.GT.K ) THEN
!
!                 W := W + C1 * V1'
!
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose',
     $                 LASTC, K, LASTV-K, ONE, C, LDC, V, LDV, ONE,
     $                 WORK, LDWORK )
               END IF
!
!              W := W * T  or  W * T'
!
               CALL ZTRMM( 'Right', 'Lower', TRANS, 'Non-unit',
     $              LASTC, K, ONE, T, LDT, WORK, LDWORK )
!
!              C := C - W * V
!
               IF( LASTV.GT.K ) THEN
!
!                 C1 := C1 - W * V1
!
                  CALL ZGEMM( 'No transpose', 'No transpose',
     $                 LASTC, LASTV-K, K, -ONE, WORK, LDWORK, V, LDV,
     $                 ONE, C, LDC )
               END IF
!
!              W := W * V2
!
               CALL ZTRMM( 'Right', 'Lower', 'No transpose', 'Unit',
     $              LASTC, K, ONE, V( 1, LASTV-K+1 ), LDV,
     $              WORK, LDWORK )
!
!              C1 := C1 - W
!
               DO 240 J = 1, K
                  DO 230 I = 1, LASTC
                     C( I, LASTV-K+J ) = C( I, LASTV-K+J )
     $                    - WORK( I, J )
  230             CONTINUE
  240          CONTINUE
!
            END IF
!
         END IF
      END IF
!
      RETURN
!
!     End of ZLARFB
!
      END
!
!  =====================================================================
!
!> @brief  ZUNM2R multiplies a general matrix by the unitary matrix from a QR factorization determined by cgeqrf (unblocked algorithm).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZUNM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
!                          WORK, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          SIDE, TRANS
!       INTEGER            INFO, K, LDA, LDC, M, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZUNM2R overwrites the general complex m-by-n matrix C with
!>
!>       Q * C  if SIDE = 'L' and TRANS = 'N', or
!>
!>       Q**H* C  if SIDE = 'L' and TRANS = 'C', or
!>
!>       C * Q  if SIDE = 'R' and TRANS = 'N', or
!>
!>       C * Q**H if SIDE = 'R' and TRANS = 'C',
!>
!> where Q is a complex unitary matrix defined as the product of k
!> elementary reflectors
!>
!>       Q = H(1) H(2) . . . H(k)
!>
!> as returned by ZGEQRF. Q is of order m if SIDE = 'L' and of order n
!> if SIDE = 'R'.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] SIDE
!> \verbatim
!>          SIDE is CHARACTER*1
!>          = 'L': apply Q or Q**H from the Left
!>          = 'R': apply Q or Q**H from the Right
!> \endverbatim
!>
!> @param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>          = 'N': apply Q  (No transpose)
!>          = 'C': apply Q**H (Conjugate transpose)
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix C. M >= 0.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix C. N >= 0.
!> \endverbatim
!>
!> @param[in] K
!> \verbatim
!>          K is INTEGER
!>          The number of elementary reflectors whose product defines
!>          the matrix Q.
!>          If SIDE = 'L', M >= K >= 0;
!>          if SIDE = 'R', N >= K >= 0.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,K)
!>          The i-th column must contain the vector which defines the
!>          elementary reflector H(i), for i = 1,2,...,k, as returned by
!>          ZGEQRF in the first k columns of its array argument A.
!>          A is modified by the routine but restored on exit.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.
!>          If SIDE = 'L', LDA >= max(1,M);
!>          if SIDE = 'R', LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in] TAU
!> \verbatim
!>          TAU is COMPLEX*16 array, dimension (K)
!>          TAU(i) must contain the scalar factor of the elementary
!>          reflector H(i), as returned by ZGEQRF.
!> \endverbatim
!>
!> @param[in,out] C
!> \verbatim
!>          C is COMPLEX*16 array, dimension (LDC,N)
!>          On entry, the m-by-n matrix C.
!>          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
!> \endverbatim
!>
!> @param[in] LDC
!> \verbatim
!>          LDC is INTEGER
!>          The leading dimension of the array C. LDC >= max(1,M).
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension
!>                                   (N) if SIDE = 'L',
!>                                   (M) if SIDE = 'R'
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
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
!  =====================================================================
      SUBROUTINE ZUNM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZUNM2R overwrites the general complex m-by-n matrix C with
!
!        Q * C  if SIDE = 'L' and TRANS = 'N', or
!
!        Q'* C  if SIDE = 'L' and TRANS = 'C', or
!
!        C * Q  if SIDE = 'R' and TRANS = 'N', or
!
!        C * Q' if SIDE = 'R' and TRANS = 'C',
!
!  where Q is a complex unitary matrix defined as the product of k
!  elementary reflectors
!
!        Q = H(1) H(2) . . . H(k)
!
!  as returned by ZGEQRF. Q is of order m if SIDE = 'L' and of order n
!  if SIDE = 'R'.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply Q or Q' from the Left
!          = 'R': apply Q or Q' from the Right
!
!  TRANS   (input) CHARACTER*1
!          = 'N': apply Q  (No transpose)
!          = 'C': apply Q' (Conjugate transpose)
!
!  M       (input) INTEGER
!          The number of rows of the matrix C. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C. N >= 0.
!
!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines
!          the matrix Q.
!          If SIDE = 'L', M >= K >= 0;
!          if SIDE = 'R', N >= K >= 0.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,K)
!          The i-th column must contain the vector which defines the
!          elementary reflector H(i), for i = 1,2,...,k, as returned by
!          ZGEQRF in the first k columns of its array argument A.
!          A is modified by the routine but restored on exit.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!          If SIDE = 'L', LDA >= max(1,M);
!          if SIDE = 'R', LDA >= max(1,N).
!
!  TAU     (input) COMPLEX*16 array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by ZGEQRF.
!
!  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
!          On entry, the m-by-n matrix C.
!          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace) COMPLEX*16 array, dimension
!                                   (N) if SIDE = 'L',
!                                   (M) if SIDE = 'R'
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      COMPLEX*16         AII, TAUI
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARF
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
!
!     NQ is the order of Q
!
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNM2R', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
!
      IF( ( LEFT .AND. .NOT.NOTRAN .OR. .NOT.LEFT .AND. NOTRAN ) ) THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
!
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
!
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
!
!           H(i) or H(i)' is applied to C(i:m,1:n)
!
            MI = M - I + 1
            IC = I
         ELSE
!
!           H(i) or H(i)' is applied to C(1:m,i:n)
!
            NI = N - I + 1
            JC = I
         END IF
!
!        Apply H(i) or H(i)'
!
         IF( NOTRAN ) THEN
            TAUI = TAU( I )
         ELSE
            TAUI = DCONJG( TAU( I ) )
         END IF
         AII = A( I, I )
         A( I, I ) = ONE
         CALL ZLARF( SIDE, MI, NI, A( I, I ), 1, TAUI, C( IC, JC ), LDC,
     $               WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
!
!     End of ZUNM2R
!
      END
!
!  =====================================================================
!
!> @brief  ZUNG2R
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZUNG2R( M, N, K, A, LDA, TAU, WORK, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, K, LDA, M, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZUNG2R generates an m by n complex matrix Q with orthonormal columns,
!> which is defined as the first n columns of a product of k elementary
!> reflectors of order m
!>
!>       Q  =  H(1) H(2) . . . H(k)
!>
!> as returned by ZGEQRF.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix Q. M >= 0.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix Q. M >= N >= 0.
!> \endverbatim
!>
!> @param[in] K
!> \verbatim
!>          K is INTEGER
!>          The number of elementary reflectors whose product defines the
!>          matrix Q. N >= K >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          On entry, the i-th column must contain the vector which
!>          defines the elementary reflector H(i), for i = 1,2,...,k, as
!>          returned by ZGEQRF in the first k columns of its array
!>          argument A.
!>          On exit, the m by n matrix Q.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The first dimension of the array A. LDA >= max(1,M).
!> \endverbatim
!>
!> @param[in] TAU
!> \verbatim
!>          TAU is COMPLEX*16 array, dimension (K)
!>          TAU(i) must contain the scalar factor of the elementary
!>          reflector H(i), as returned by ZGEQRF.
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension (N)
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument has an illegal value
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
!  =====================================================================
      SUBROUTINE ZUNG2R( M, N, K, A, LDA, TAU, WORK, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZUNG2R generates an m by n complex matrix Q with orthonormal columns,
!  which is defined as the first n columns of a product of k elementary
!  reflectors of order m
!
!        Q  =  H(1) H(2) . . . H(k)
!
!  as returned by ZGEQRF.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix Q. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix Q. M >= N >= 0.
!
!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines the
!          matrix Q. N >= K >= 0.
!
!  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
!          On entry, the i-th column must contain the vector which
!          defines the elementary reflector H(i), for i = 1,2,...,k, as
!          returned by ZGEQRF in the first k columns of its array
!          argument A.
!          On exit, the m by n matrix Q.
!
!  LDA     (input) INTEGER
!          The first dimension of the array A. LDA >= max(1,M).
!
!  TAU     (input) COMPLEX*16 array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by ZGEQRF.
!
!  WORK    (workspace) COMPLEX*16 array, dimension (N)
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument has an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J, L
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARF, ZSCAL
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNG2R', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.LE.0 )
     $   RETURN
!
!     Initialise columns k+1:n to columns of the unit matrix
!
      DO 20 J = K + 1, N
         DO 10 L = 1, M
            A( L, J ) = ZERO
   10    CONTINUE
         A( J, J ) = ONE
   20 CONTINUE
!
      DO 40 I = K, 1, -1
!
!        Apply H(i) to A(i:m,i:n) from the left
!
         IF( I.LT.N ) THEN
            A( I, I ) = ONE
            CALL ZLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
         END IF
         IF( I.LT.M )
     $      CALL ZSCAL( M-I, -TAU( I ), A( I+1, I ), 1 )
         A( I, I ) = ONE - TAU( I )
!
!        Set A(1:i-1,i) to zero
!
         DO 30 L = 1, I - 1
            A( L, I ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
!
!     End of ZUNG2R
!
      END
!
!  =====================================================================
!
!> @brief  ZLANHS returns the value of the 1-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of an upper Hessenberg matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       DOUBLE PRECISION FUNCTION ZLANHS( NORM, N, A, LDA, WORK )
!
!       .. Scalar Arguments ..
!       CHARACTER          NORM
!       INTEGER            LDA, N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   WORK( * )
!       COMPLEX*16         A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLANHS  returns the value of the one norm,  or the Frobenius norm, or
!> the  infinity norm,  or the  element of  largest absolute value  of a
!> Hessenberg matrix A.
!> \endverbatim
!>
!> \return ZLANHS
!> \verbatim
!>
!>    ZLANHS = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!>             (
!>             ( norm1(A),         NORM = '1', 'O' or 'o'
!>             (
!>             ( normI(A),         NORM = 'I' or 'i'
!>             (
!>             ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!>
!> where  norm1  denotes the  one norm of a matrix (maximum column sum),
!> normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!> normF  denotes the  Frobenius norm of a matrix (square root of sum of
!> squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] NORM
!> \verbatim
!>          NORM is CHARACTER*1
!>          Specifies the value to be returned in ZLANHS as described
!>          above.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.  When N = 0, ZLANHS is
!>          set to zero.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is COMPLEX*16 array, dimension (LDA,N)
!>          The n by n upper Hessenberg matrix A; the part of A below the
!>          first sub-diagonal is not referenced.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(N,1).
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
!>          where LWORK >= N when NORM = 'I'; otherwise, WORK is not
!>          referenced.
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
!  =====================================================================
      DOUBLE PRECISION FUNCTION ZLANHS( NORM, N, A, LDA, WORK )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   WORK( * )
      COMPLEX*16         A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  ZLANHS  returns the value of the one norm,  or the Frobenius norm, or
!  the  infinity norm,  or the  element of  largest absolute value  of a
!  Hessenberg matrix A.
!
!  Description
!  ===========
!
!  ZLANHS returns the value
!
!     ZLANHS = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!              (
!              ( norm1(A),         NORM = '1', 'O' or 'o'
!              (
!              ( normI(A),         NORM = 'I' or 'i'
!              (
!              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!
!  where  norm1  denotes the  one norm of a matrix (maximum column sum),
!  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!  normF  denotes the  Frobenius norm of a matrix (square root of sum of
!  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!
!  Arguments
!  =========
!
!  NORM    (input) CHARACTER*1
!          Specifies the value to be returned in ZLANHS as described
!          above.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.  When N = 0, ZLANHS is
!          set to zero.
!
!  A       (input) COMPLEX*16 array, dimension (LDA,N)
!          The n by n upper Hessenberg matrix A; the part of A below the
!          first sub-diagonal is not referenced.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(N,1).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
!          where LWORK >= N when NORM = 'I'; otherwise, WORK is not
!          referenced.
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   SCALE, SUM, VALUE
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZLASSQ
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
      IF( N.EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
!
!        Find max(abs(A(i,j))).
!
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, MIN( N, J+1 )
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
!
!        Find norm1(A).
!
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, MIN( N, J+1 )
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
!
!        Find normI(A).
!
         DO 50 I = 1, N
            WORK( I ) = ZERO
   50    CONTINUE
         DO 70 J = 1, N
            DO 60 I = 1, MIN( N, J+1 )
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, N
            VALUE = MAX( VALUE, WORK( I ) )
   80    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
!
!        Find normF(A).
!
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL ZLASSQ( MIN( N, J+1 ), A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
!
      ZLANHS = VALUE
      RETURN
!
!     End of ZLANHS
!
      END
!
!  =====================================================================
!
!> @brief  DLABAD
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DLABAD( SMALL, LARGE )
!
!       .. Scalar Arguments ..
!       DOUBLE PRECISION   LARGE, SMALL
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DLABAD takes as input the values computed by DLAMCH for underflow and
!> overflow, and returns the square root of each of these values if the
!> log of LARGE is sufficiently large.  This subroutine is intended to
!> identify machines with a large exponent range, such as the Crays, and
!> redefine the underflow and overflow limits to be the square roots of
!> the values computed by DLAMCH.  This subroutine is needed because
!> DLAMCH does not compensate for poor arithmetic in the upper half of
!> the exponent range, as is found on a Cray.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in,out] SMALL
!> \verbatim
!>          SMALL is DOUBLE PRECISION
!>          On entry, the underflow threshold as computed by DLAMCH.
!>          On exit, if LOG10(LARGE) is sufficiently large, the square
!>          root of SMALL, otherwise unchanged.
!> \endverbatim
!>
!> @param[in,out] LARGE
!> \verbatim
!>          LARGE is DOUBLE PRECISION
!>          On entry, the overflow threshold as computed by DLAMCH.
!>          On exit, if LOG10(LARGE) is sufficiently large, the square
!>          root of LARGE, otherwise unchanged.
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
!  =====================================================================
      SUBROUTINE DLABAD( SMALL, LARGE )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   LARGE, SMALL
!     ..
!
!  Purpose
!  =======
!
!  DLABAD takes as input the values computed by DLAMCH for underflow and
!  overflow, and returns the square root of each of these values if the
!  log of LARGE is sufficiently large.  This subroutine is intended to
!  identify machines with a large exponent range, such as the Crays, and
!  redefine the underflow and overflow limits to be the square roots of
!  the values computed by DLAMCH.  This subroutine is needed because
!  DLAMCH does not compensate for poor arithmetic in the upper half of
!  the exponent range, as is found on a Cray.
!
!  Arguments
!  =========
!
!  SMALL   (input/output) DOUBLE PRECISION
!          On entry, the underflow threshold as computed by DLAMCH.
!          On exit, if LOG10(LARGE) is sufficiently large, the square
!          root of SMALL, otherwise unchanged.
!
!  LARGE   (input/output) DOUBLE PRECISION
!          On entry, the overflow threshold as computed by DLAMCH.
!          On exit, if LOG10(LARGE) is sufficiently large, the square
!          root of LARGE, otherwise unchanged.
!
!  =====================================================================
!
!     .. Intrinsic Functions ..
      INTRINSIC          LOG10, SQRT
!     ..
!     .. Executable Statements ..
!
!     If it looks like we're on a Cray, take the square root of
!     SMALL and LARGE to avoid overflow and underflow problems.
!
      IF( LOG10( LARGE ).GT.2000.D0 ) THEN
         SMALL = SQRT( SMALL )
         LARGE = SQRT( LARGE )
      END IF
!
      RETURN
!
!     End of DLABAD
!
      END
!
!  =====================================================================
!
!> @brief  ZLADIV performs complex division in real arithmetic, avoiding unnecessary overflow.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       COMPLEX*16     FUNCTION ZLADIV( X, Y )
!
!       .. Scalar Arguments ..
!       COMPLEX*16         X, Y
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLADIV := X / Y, where X and Y are complex.  The computation of X / Y
!> will not overflow on an intermediary step unless the results
!> overflows.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] X
!> \verbatim
!>          X is COMPLEX*16
!> \endverbatim
!>
!> @param[in] Y
!> \verbatim
!>          Y is COMPLEX*16
!>          The complex scalars X and Y.
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
!  =====================================================================
      COMPLEX*16     FUNCTION ZLADIV( X, Y )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      COMPLEX*16         X, Y
!     ..
!
!  Purpose
!  =======
!
!  ZLADIV := X / Y, where X and Y are complex.  The computation of X / Y
!  will not overflow on an intermediary step unless the results
!  overflows.
!
!  Arguments
!  =========
!
!  X       (input) COMPLEX*16
!  Y       (input) COMPLEX*16
!          The complex scalars X and Y.
!
!  =====================================================================
!
!     .. Local Scalars ..
      DOUBLE PRECISION   ZI, ZR
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLADIV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, DIMAG
!     ..
!     .. Executable Statements ..
!
      CALL DLADIV( DBLE( X ), DIMAG( X ), DBLE( Y ), DIMAG( Y ), ZR,
     $             ZI )
      ZLADIV = DCMPLX( ZR, ZI )
!
      RETURN
!
!     End of ZLADIV
!
      END
!
!  =====================================================================
!
!> @brief  ZLARF applies an elementary reflector to a general rectangular matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
!
!       .. Scalar Arguments ..
!       CHARACTER          SIDE
!       INTEGER            INCV, LDC, M, N
!       COMPLEX*16         TAU
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         C( LDC, * ), V( * ), WORK( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLARF applies a complex elementary reflector H to a complex M-by-N
!> matrix C, from either the left or the right. H is represented in the
!> form
!>
!>       H = I - tau * v * v**H
!>
!> where tau is a complex scalar and v is a complex vector.
!>
!> If tau = 0, then H is taken to be the unit matrix.
!>
!> To apply H**H, supply conjg(tau) instead
!> tau.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] SIDE
!> \verbatim
!>          SIDE is CHARACTER*1
!>          = 'L': form  H * C
!>          = 'R': form  C * H
!> \endverbatim
!>
!> @param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix C.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix C.
!> \endverbatim
!>
!> @param[in] V
!> \verbatim
!>          V is COMPLEX*16 array, dimension
!>                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
!>                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
!>          The vector v in the representation of H. V is not used if
!>          TAU = 0.
!> \endverbatim
!>
!> @param[in] INCV
!> \verbatim
!>          INCV is INTEGER
!>          The increment between elements of v. INCV <> 0.
!> \endverbatim
!>
!> @param[in] TAU
!> \verbatim
!>          TAU is COMPLEX*16
!>          The value tau in the representation of H.
!> \endverbatim
!>
!> @param[in,out] C
!> \verbatim
!>          C is COMPLEX*16 array, dimension (LDC,N)
!>          On entry, the M-by-N matrix C.
!>          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
!>          or C * H if SIDE = 'R'.
!> \endverbatim
!>
!> @param[in] LDC
!> \verbatim
!>          LDC is INTEGER
!>          The leading dimension of the array C. LDC >= max(1,M).
!> \endverbatim
!>
!> @param[out] WORK
!> \verbatim
!>          WORK is COMPLEX*16 array, dimension
!>                         (N) if SIDE = 'L'
!>                      or (M) if SIDE = 'R'
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
!  =====================================================================
      SUBROUTINE ZLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
      IMPLICIT NONE
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      COMPLEX*16         TAU
!     ..
!     .. Array Arguments ..
      COMPLEX*16         C( LDC, * ), V( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  ZLARF applies a complex elementary reflector H to a complex M-by-N
!  matrix C, from either the left or the right. H is represented in the
!  form
!
!        H = I - tau * v * v'
!
!  where tau is a complex scalar and v is a complex vector.
!
!  If tau = 0, then H is taken to be the unit matrix.
!
!  To apply H' (the conjugate transpose of H), supply conjg(tau) instead
!  tau.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': form  H * C
!          = 'R': form  C * H
!
!  M       (input) INTEGER
!          The number of rows of the matrix C.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C.
!
!  V       (input) COMPLEX*16 array, dimension
!                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
!                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
!          The vector v in the representation of H. V is not used if
!          TAU = 0.
!
!  INCV    (input) INTEGER
!          The increment between elements of v. INCV <> 0.
!
!  TAU     (input) COMPLEX*16
!          The value tau in the representation of H.
!
!  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
!          On entry, the M-by-N matrix C.
!          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
!          or C * H if SIDE = 'R'.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace) COMPLEX*16 array, dimension
!                         (N) if SIDE = 'L'
!                      or (M) if SIDE = 'R'
!
!  =====================================================================
!
!     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            APPLYLEFT
      INTEGER            I, LASTV, LASTC
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZGEMV, ZGERC
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAZLR, ILAZLC
      EXTERNAL           LSAME, ILAZLR, ILAZLC
!     ..
!     .. Executable Statements ..
!
      APPLYLEFT = LSAME( SIDE, 'L' )
      LASTV = 0
      LASTC = 0
      IF( TAU.NE.ZERO ) THEN
!     Set up variables for scanning V.  LASTV begins pointing to the end
!     of V.
         IF( APPLYLEFT ) THEN
            LASTV = M
         ELSE
            LASTV = N
         END IF
         IF( INCV.GT.0 ) THEN
            I = 1 + (LASTV-1) * INCV
         ELSE
            I = 1
         END IF
!     Look for the last non-zero row in V.
         DO WHILE( LASTV.GT.0 .AND. V( I ).EQ.ZERO )
            LASTV = LASTV - 1
            I = I - INCV
         END DO
         IF( APPLYLEFT ) THEN
!     Scan for the last non-zero column in C(1:lastv,:).
            LASTC = ILAZLC(LASTV, N, C, LDC)
         ELSE
!     Scan for the last non-zero row in C(:,1:lastv).
            LASTC = ILAZLR(M, LASTV, C, LDC)
         END IF
      END IF
!     Note that lastc.eq.0 renders the BLAS operations null; no special
!     case is needed at this level.
      IF( APPLYLEFT ) THEN
!
!        Form  H * C
!
         IF( LASTV.GT.0 ) THEN
!
!           w(1:lastc,1) := C(1:lastv,1:lastc)' * v(1:lastv,1)
!
            CALL ZGEMV( 'Conjugate transpose', LASTV, LASTC, ONE,
     $           C, LDC, V, INCV, ZERO, WORK, 1 )
!
!           C(1:lastv,1:lastc) := C(...) - v(1:lastv,1) * w(1:lastc,1)'
!
            CALL ZGERC( LASTV, LASTC, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
!
!        Form  C * H
!
         IF( LASTV.GT.0 ) THEN
!
!           w(1:lastc,1) := C(1:lastc,1:lastv) * v(1:lastv,1)
!
            CALL ZGEMV( 'No transpose', LASTC, LASTV, ONE, C, LDC,
     $           V, INCV, ZERO, WORK, 1 )
!
!           C(1:lastc,1:lastv) := C(...) - w(1:lastc,1) * v(1:lastv,1)'
!
            CALL ZGERC( LASTC, LASTV, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
      RETURN
!
!     End of ZLARF
!
      END
!
!  =====================================================================
!
!> @brief  ZLACGV conjugates a complex vector.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE ZLACGV( N, X, INCX )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         X( * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> ZLACGV conjugates a complex vector of length N.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The length of the vector X.  N >= 0.
!> \endverbatim
!>
!> @param[in,out] X
!> \verbatim
!>          X is COMPLEX*16 array, dimension
!>                         (1+(N-1)*abs(INCX))
!>          On entry, the vector of length N to be conjugated.
!>          On exit, X is overwritten with conjg(X).
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The spacing between successive elements of X.
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
!  =====================================================================
      SUBROUTINE ZLACGV( N, X, INCX )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
!     ..
!     .. Array Arguments ..
      COMPLEX*16         X( * )
!     ..
!
!  Purpose
!  =======
!
!  ZLACGV conjugates a complex vector of length N.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The length of the vector X.  N >= 0.
!
!  X       (input/output) COMPLEX*16 array, dimension
!                         (1+(N-1)*abs(INCX))
!          On entry, the vector of length N to be conjugated.
!          On exit, X is overwritten with conjg(X).
!
!  INCX    (input) INTEGER
!          The spacing between successive elements of X.
!
! =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, IOFF
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
!     ..
!     .. Executable Statements ..
!
      IF( INCX.EQ.1 ) THEN
         DO 10 I = 1, N
            X( I ) = DCONJG( X( I ) )
   10    CONTINUE
      ELSE
         IOFF = 1
         IF( INCX.LT.0 )
     $      IOFF = 1 - ( N-1 )*INCX
         DO 20 I = 1, N
            X( IOFF ) = DCONJG( X( IOFF ) )
            IOFF = IOFF + INCX
   20    CONTINUE
      END IF
      RETURN
!
!     End of ZLACGV
!
      END
!
!  =====================================================================
!
!> @brief  DLADIV performs complex division in real arithmetic, avoiding unnecessary overflow.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DLADIV( A, B, C, D, P, Q )
!
!       .. Scalar Arguments ..
!       DOUBLE PRECISION   A, B, C, D, P, Q
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DLADIV performs complex division in  real arithmetic
!>
!>                       a + i*b
!>            p + i*q = ---------
!>                       c + i*d
!>
!> The algorithm is due to Michael Baudin and Robert L. Smith
!> and can be found in the paper
!> "A Robust Complex Division in Scilab"
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] A
!> \verbatim
!>          A is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] B
!> \verbatim
!>          B is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] C
!> \verbatim
!>          C is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] D
!> \verbatim
!>          D is DOUBLE PRECISION
!>          The scalars a, b, c, and d in the above expression.
!> \endverbatim
!>
!> @param[out] P
!> \verbatim
!>          P is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[out] Q
!> \verbatim
!>          Q is DOUBLE PRECISION
!>          The scalars p and q in the above expression.
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
!> @date January 2013
!
!  =====================================================================
      SUBROUTINE DLADIV( A, B, C, D, P, Q )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, D, P, Q
!     ..
!
!  Purpose
!  =======
!
!  DLADIV performs complex division in  real arithmetic
!
!                        a + i*b
!             p + i*q = ---------
!                        c + i*d
!
!  The algorithm is due to Robert L. Smith and can be found
!  in D. Knuth, The art of Computer Programming, Vol.2, p.195
!
!  Arguments
!  =========
!
!  A       (input) DOUBLE PRECISION
!  B       (input) DOUBLE PRECISION
!  C       (input) DOUBLE PRECISION
!  D       (input) DOUBLE PRECISION
!          The scalars a, b, c, and d in the above expression.
!
!  P       (output) DOUBLE PRECISION
!  Q       (output) DOUBLE PRECISION
!          The scalars p and q in the above expression.
!
!  =====================================================================
!
!     .. Local Scalars ..
      DOUBLE PRECISION   E, F
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
      IF( ABS( D ).LT.ABS( C ) ) THEN
         E = D / C
         F = C + D*E
         P = ( A+B*E ) / F
         Q = ( B-A*E ) / F
      ELSE
         E = C / D
         F = D + C*E
         P = ( B+A*E ) / F
         Q = ( -A+B*E ) / F
      END IF
!
      RETURN
!
!     End of DLADIV
!
      END
!
!  =====================================================================
!
!> @brief  DLAPY3 returns sqrt(x2+y2+z2).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       DOUBLE PRECISION FUNCTION DLAPY3( X, Y, Z )
!
!       .. Scalar Arguments ..
!       DOUBLE PRECISION   X, Y, Z
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
!> unnecessary overflow.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] X
!> \verbatim
!>          X is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] Y
!> \verbatim
!>          Y is DOUBLE PRECISION
!> \endverbatim
!>
!> @param[in] Z
!> \verbatim
!>          Z is DOUBLE PRECISION
!>          X, Y and Z specify the values x, y and z.
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
!  =====================================================================
      DOUBLE PRECISION FUNCTION DLAPY3( X, Y, Z )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   X, Y, Z
!     ..
!
!  Purpose
!  =======
!
!  DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
!  unnecessary overflow.
!
!  Arguments
!  =========
!
!  X       (input) DOUBLE PRECISION
!  Y       (input) DOUBLE PRECISION
!  Z       (input) DOUBLE PRECISION
!          X, Y and Z specify the values x, y and z.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION   W, XABS, YABS, ZABS
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
!     ..
!     .. Executable Statements ..
!
      XABS = ABS( X )
      YABS = ABS( Y )
      ZABS = ABS( Z )
      W = MAX( XABS, YABS, ZABS )
      IF( W.EQ.ZERO ) THEN
!     W can be zero for max(0,nan,0)
!     adding all three entries together will make sure
!     NaN will not disappear.
         DLAPY3 =  XABS + YABS + ZABS
      ELSE
         DLAPY3 = W*SQRT( ( XABS / W )**2+( YABS / W )**2+
     $            ( ZABS / W )**2 )
      END IF
      RETURN
!
!     End of DLAPY3
!
      END
!
! =======================================================
!
!> @brief  DGETRF
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, LDA, M, N
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * )
!       DOUBLE PRECISION   A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DGETRF computes an LU factorization of a general M-by-N matrix A
!> using partial pivoting with row interchanges.
!>
!> The factorization has the form
!>    A = P * L * U
!> where P is a permutation matrix, L is lower triangular with unit
!> diagonal elements (lower trapezoidal if m > n), and U is upper
!> triangular (upper trapezoidal if m < n).
!>
!> This is the right-looking Level 3 BLAS version of the algorithm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
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
!>          On entry, the M-by-N matrix to be factored.
!>          On exit, the factors L and U from the factorization
!>          A = P*L*U; the unit diagonal elements of L are not stored.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> @param[out] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (min(M,N))
!>          The pivot indices; for 1 <= i <= min(M,N), row i of the
!>          matrix was interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument had an illegal value
!>          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!>                has been completed, but the factor U is exactly
!>                singular, and division by zero will occur if it is used
!>                to solve a system of equations.
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
!  =====================================================================
      SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DGETRF computes an LU factorization of a general M-by-N matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the M-by-N matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!                has been completed, but the factor U is exactly
!                singular, and division by zero will occur if it is used
!                to solve a system of equations.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, IINFO, J, JB, NB
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGEMM, DGETF2, DLASWP, DTRSM, XERBLA
!     ..
!     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRF', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
!
!     Determine the block size for this environment.
!
      NB = ILAENV( 1, 'DGETRF', ' ', M, N, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.MIN( M, N ) ) THEN
!
!        Use unblocked code.
!
         CALL DGETF2( M, N, A, LDA, IPIV, INFO )
      ELSE
!
!        Use blocked code.
!
         DO 20 J = 1, MIN( M, N ), NB
            JB = MIN( MIN( M, N )-J+1, NB )
!
!           Factor diagonal and subdiagonal blocks and test for exact
!           singularity.
!
            CALL DGETF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
!
!           Adjust INFO and the pivot indices.
!
            IF( INFO.EQ.0 .AND. IINFO.GT.0 )
     $         INFO = IINFO + J - 1
            DO 10 I = J, MIN( M, J+JB-1 )
               IPIV( I ) = J - 1 + IPIV( I )
   10       CONTINUE
!
!           Apply interchanges to columns 1:J-1.
!
            CALL DLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
!
            IF( J+JB.LE.N ) THEN
!
!              Apply interchanges to columns J+JB:N.
!
               CALL DLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,
     $                      IPIV, 1 )
!
!              Compute block row of U.
!
               CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', JB,
     $                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ),
     $                     LDA )
               IF( J+JB.LE.M ) THEN
!
!                 Update trailing submatrix.
!
                  CALL DGEMM( 'No transpose', 'No transpose', M-J-JB+1,
     $                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,
     $                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),
     $                        LDA )
               END IF
            END IF
   20    CONTINUE
      END IF
      RETURN
!
!     End of DGETRF
!
      END SUBROUTINE DGETRF
!
! =======================================================
!
!> @brief  DGETF2 computes the LU factorization of a general m-by-n matrix using partial pivoting with row interchanges (unblocked algorithm).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DGETF2( M, N, A, LDA, IPIV, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, LDA, M, N
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * )
!       DOUBLE PRECISION   A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DGETF2 computes an LU factorization of a general m-by-n matrix A
!> using partial pivoting with row interchanges.
!>
!> The factorization has the form
!>    A = P * L * U
!> where P is a permutation matrix, L is lower triangular with unit
!> diagonal elements (lower trapezoidal if m > n), and U is upper
!> triangular (upper trapezoidal if m < n).
!>
!> This is the right-looking Level 2 BLAS version of the algorithm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
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
!>          On entry, the m by n matrix to be factored.
!>          On exit, the factors L and U from the factorization
!>          A = P*L*U; the unit diagonal elements of L are not stored.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> @param[out] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (min(M,N))
!>          The pivot indices; for 1 <= i <= min(M,N), row i of the
!>          matrix was interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -k, the k-th argument had an illegal value
!>          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!>               has been completed, but the factor U is exactly
!>               singular, and division by zero will occur if it is used
!>               to solve a system of equations.
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
!
!  =====================================================================
      SUBROUTINE DGETF2( M, N, A, LDA, IPIV, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the m by n matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION   SFMIN
      INTEGER            I, J, JP
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      INTEGER            IDAMAX
      EXTERNAL           DLAMCH, IDAMAX
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGER, DSCAL, DSWAP, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETF2', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
!
!     Compute machine safe minimum
!
      SFMIN = DLAMCH('S')
!
      DO 10 J = 1, MIN( M, N )
!
!        Find pivot and test for singularity.
!
         JP = J - 1 + IDAMAX( M-J+1, A( J, J ), 1 )
         IPIV( J ) = JP
         IF( A( JP, J ).NE.ZERO ) THEN
!
!           Apply the interchange to columns 1:N.
!
            IF( JP.NE.J )
     $         CALL DSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
!
!           Compute elements J+1:M of J-th column.
!
            IF( J.LT.M ) THEN
               IF( ABS(A( J, J )) .GE. SFMIN ) THEN
                  CALL DSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
               ELSE
                 DO 20 I = 1, M-J
                    A( J+I, J ) = A( J+I, J ) / A( J, J )
   20            CONTINUE
               END IF
            END IF
!
         ELSE IF( INFO.EQ.0 ) THEN
!
            INFO = J
         END IF
!
         IF( J.LT.MIN( M, N ) ) THEN
!
!           Update trailing submatrix.
!
            CALL DGER( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ), LDA,
     $                 A( J+1, J+1 ), LDA )
         END IF
   10 CONTINUE
      RETURN
!
!     End of DGETF2
!
      END SUBROUTINE DGETF2
!
! =======================================================
!
!> @brief  DLASWP performs a series of row interchanges on a general rectangular matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DLASWP( N, A, LDA, K1, K2, IPIV, INCX )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, K1, K2, LDA, N
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * )
!       DOUBLE PRECISION   A( LDA, * )
!       ..
!
!
!  Purpose:
!  =============
!>
!> \verbatim
!>
!> DLASWP performs a series of row interchanges on the matrix A.
!> One row interchange is initiated for each of rows K1 through K2 of A.
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
!> @param[in,out] A
!> \verbatim
!>          A is DOUBLE PRECISION array, dimension (LDA,N)
!>          On entry, the matrix of column dimension N to which the row
!>          interchanges will be applied.
!>          On exit, the permuted matrix.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.
!> \endverbatim
!>
!> @param[in] K1
!> \verbatim
!>          K1 is INTEGER
!>          The first element of IPIV for which a row interchange will
!>          be done.
!> \endverbatim
!>
!> @param[in] K2
!> \verbatim
!>          K2 is INTEGER
!>          The last element of IPIV for which a row interchange will
!>          be done.
!> \endverbatim
!>
!> @param[in] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (K2*abs(INCX))
!>          The vector of pivot indices.  Only the elements in positions
!>          K1 through K2 of IPIV are accessed.
!>          IPIV(K) = L implies rows K and L are to be interchanged.
!> \endverbatim
!>
!> @param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between successive values of IPIV.  If IPIV
!>          is negative, the pivots are applied in reverse order.
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
!>  Modified by
!>   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA
!> \endverbatim
!>
!  =====================================================================
!
      SUBROUTINE DLASWP( N, A, LDA, K1, K2, IPIV, INCX )
!
!  -- LAPACK auxiliary routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, K1, K2, LDA, N
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DLASWP performs a series of row interchanges on the matrix A.
!  One row interchange is initiated for each of rows K1 through K2 of A.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the matrix of column dimension N to which the row
!          interchanges will be applied.
!          On exit, the permuted matrix.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!
!  K1      (input) INTEGER
!          The first element of IPIV for which a row interchange will
!          be done.
!
!  K2      (input) INTEGER
!          The last element of IPIV for which a row interchange will
!          be done.
!
!  IPIV    (input) INTEGER array, dimension (K2*abs(INCX))
!          The vector of pivot indices.  Only the elements in positions
!          K1 through K2 of IPIV are accessed.
!          IPIV(K) = L implies rows K and L are to be interchanged.
!
!  INCX    (input) INTEGER
!          The increment between successive values of IPIV.  If IPIV
!          is negative, the pivots are applied in reverse order.
!
!  Further Details
!  ===============
!
!  Modified by
!   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA
!
! =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, I1, I2, INC, IP, IX, IX0, J, K, N32
      DOUBLE PRECISION   TEMP
!     ..
!     .. Executable Statements ..
!
!     Interchange row I with row IPIV(I) for each of rows K1 through K2.
!
      IF( INCX.GT.0 ) THEN
         IX0 = K1
         I1 = K1
         I2 = K2
         INC = 1
      ELSE IF( INCX.LT.0 ) THEN
         IX0 = 1 + ( 1-K2 )*INCX
         I1 = K2
         I2 = K1
         INC = -1
      ELSE
         RETURN
      END IF
!
      N32 = ( N / 32 )*32
      IF( N32.NE.0 ) THEN
         DO 30 J = 1, N32, 32
            IX = IX0
            DO 20 I = I1, I2, INC
               IP = IPIV( IX )
               IF( IP.NE.I ) THEN
                  DO 10 K = J, J + 31
                     TEMP = A( I, K )
                     A( I, K ) = A( IP, K )
                     A( IP, K ) = TEMP
   10             CONTINUE
               END IF
               IX = IX + INCX
   20       CONTINUE
   30    CONTINUE
      END IF
      IF( N32.NE.N ) THEN
         N32 = N32 + 1
         IX = IX0
         DO 50 I = I1, I2, INC
            IP = IPIV( IX )
            IF( IP.NE.I ) THEN
               DO 40 K = N32, N
                  TEMP = A( I, K )
                  A( I, K ) = A( IP, K )
                  A( IP, K ) = TEMP
   40          CONTINUE
            END IF
            IX = IX + INCX
   50    CONTINUE
      END IF
!
      RETURN
!
!     End of DLASWP
!
      END SUBROUTINE DLASWP
!
! =======================================================
!
!> @brief DTRTI2 computes the inverse of a triangular matrix (unblocked algorithm).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          DIAG, UPLO
!       INTEGER            INFO, LDA, N
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
!> DTRTI2 computes the inverse of a real upper or lower triangular
!> matrix.
!>
!> This is the Level 2 BLAS version of the algorithm.
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
!> @param[in] DIAG
!> \verbatim
!>          DIAG is CHARACTER*1
!>          Specifies whether or not the matrix A is unit triangular.
!>          = 'N':  Non-unit triangular
!>          = 'U':  Unit triangular
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is DOUBLE PRECISION array, dimension (LDA,N)
!>          On entry, the triangular matrix A.  If UPLO = 'U', the
!>          leading n by n upper triangular part of the array A contains
!>          the upper triangular matrix, and the strictly lower
!>          triangular part of A is not referenced.  If UPLO = 'L', the
!>          leading n by n lower triangular part of the array A contains
!>          the lower triangular matrix, and the strictly upper
!>          triangular part of A is not referenced.  If DIAG = 'U', the
!>          diagonal elements of A are also not referenced and are
!>          assumed to be 1.
!>
!>          On exit, the (triangular) inverse of the original matrix, in
!>          the same storage format.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,N).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -k, the k-th argument had an illegal value
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
!
!  =====================================================================
      SUBROUTINE DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRTI2 computes the inverse of a real upper or lower triangular
!  matrix.
!
!  This is the Level 2 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies whether the matrix A is upper or lower triangular.
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular
!
!  DIAG    (input) CHARACTER*1
!          Specifies whether or not the matrix A is unit triangular.
!          = 'N':  Non-unit triangular
!          = 'U':  Unit triangular
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the triangular matrix A.  If UPLO = 'U', the
!          leading n by n upper triangular part of the array A contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading n by n lower triangular part of the array A contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of A is not referenced.  If DIAG = 'U', the
!          diagonal elements of A are also not referenced and are
!          assumed to be 1.
!
!          On exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J
      DOUBLE PRECISION   AJJ
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DSCAL, DTRMV, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTRTI2', -INFO )
         RETURN
      END IF
!
      IF( UPPER ) THEN
!
!        Compute inverse of upper triangular matrix.
!
         DO 10 J = 1, N
            IF( NOUNIT ) THEN
               A( J, J ) = ONE / A( J, J )
               AJJ = -A( J, J )
            ELSE
               AJJ = -ONE
            END IF
!
!           Compute elements 1:j-1 of j-th column.
!
            CALL DTRMV( 'Upper', 'No transpose', DIAG, J-1, A, LDA,
     $                  A( 1, J ), 1 )
            CALL DSCAL( J-1, AJJ, A( 1, J ), 1 )
   10    CONTINUE
      ELSE
!
!        Compute inverse of lower triangular matrix.
!
         DO 20 J = N, 1, -1
            IF( NOUNIT ) THEN
               A( J, J ) = ONE / A( J, J )
               AJJ = -A( J, J )
            ELSE
               AJJ = -ONE
            END IF
            IF( J.LT.N ) THEN
!
!              Compute elements j+1:n of j-th column.
!
               CALL DTRMV( 'Lower', 'No transpose', DIAG, N-J,
     $                     A( J+1, J+1 ), LDA, A( J+1, J ), 1 )
               CALL DSCAL( N-J, AJJ, A( J+1, J ), 1 )
            END IF
   20    CONTINUE
      END IF
!
      RETURN
!
!     End of DTRTI2
!
      END SUBROUTINE DTRTI2
!
! =======================================================
!
!> @brief DTRTRI computes the inverse of a real upper or lower triangular
!> matrix A.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          DIAG, UPLO
!       INTEGER            INFO, LDA, N
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
!> DTRTRI computes the inverse of a real upper or lower triangular
!> matrix A.
!>
!> This is the Level 3 BLAS version of the algorithm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>          = 'U':  A is upper triangular;
!>          = 'L':  A is lower triangular.
!> \endverbatim
!>
!> @param[in] DIAG
!> \verbatim
!>          DIAG is CHARACTER*1
!>          = 'N':  A is non-unit triangular;
!>          = 'U':  A is unit triangular.
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.
!> \endverbatim
!>
!> @param[in,out] A
!> \verbatim
!>          A is DOUBLE PRECISION array, dimension (LDA,N)
!>          On entry, the triangular matrix A.  If UPLO = 'U', the
!>          leading N-by-N upper triangular part of the array A contains
!>          the upper triangular matrix, and the strictly lower
!>          triangular part of A is not referenced.  If UPLO = 'L', the
!>          leading N-by-N lower triangular part of the array A contains
!>          the lower triangular matrix, and the strictly upper
!>          triangular part of A is not referenced.  If DIAG = 'U', the
!>          diagonal elements of A are also not referenced and are
!>          assumed to be 1.
!>          On exit, the (triangular) inverse of the original matrix, in
!>          the same storage format.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,N).
!> \endverbatim
!>
!> @param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          > 0: if INFO = i, A(i,i) is exactly zero.  The triangular
!>               matrix is singular and its inverse can not be computed.
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
!
!  =====================================================================
      SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
!
!  -- LAPACK routine (version 3.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRTRI computes the inverse of a real upper or lower triangular
!  matrix A.
!
!  This is the Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          = 'U':  A is upper triangular;
!          = 'L':  A is lower triangular.
!
!  DIAG    (input) CHARACTER*1
!          = 'N':  A is non-unit triangular;
!          = 'U':  A is unit triangular.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the triangular matrix A.  If UPLO = 'U', the
!          leading N-by-N upper triangular part of the array A contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading N-by-N lower triangular part of the array A contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of A is not referenced.  If DIAG = 'U', the
!          diagonal elements of A are also not referenced and are
!          assumed to be 1.
!          On exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          > 0: if INFO = i, A(i,i) is exactly zero.  The triangular
!               matrix is singular and its inverse can not be computed.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J, JB, NB, NN
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DTRMM, DTRSM, DTRTI2, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTRTRI', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 )
     $   RETURN
!
!     Check for singularity if non-unit.
!
      IF( NOUNIT ) THEN
         DO 10 INFO = 1, N
            IF( A( INFO, INFO ).EQ.ZERO )
     $         RETURN
   10    CONTINUE
         INFO = 0
      END IF
!
!     Determine the block size for this environment.
!
      NB = ILAENV( 1, 'DTRTRI', UPLO // DIAG, N, -1, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
!
!        Use unblocked code
!
         CALL DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
      ELSE
!
!        Use blocked code
!
         IF( UPPER ) THEN
!
!           Compute inverse of upper triangular matrix
!
            DO 20 J = 1, N, NB
               JB = MIN( NB, N-J+1 )
!
!              Compute rows 1:j-1 of current block column
!
               CALL DTRMM( 'Left', 'Upper', 'No transpose', DIAG, J-1,
     $                     JB, ONE, A, LDA, A( 1, J ), LDA )
               CALL DTRSM( 'Right', 'Upper', 'No transpose', DIAG, J-1,
     $                     JB, -ONE, A( J, J ), LDA, A( 1, J ), LDA )
!
!              Compute inverse of current diagonal block
!
               CALL DTRTI2( 'Upper', DIAG, JB, A( J, J ), LDA, INFO )
   20       CONTINUE
         ELSE
!
!           Compute inverse of lower triangular matrix
!
            NN = ( ( N-1 ) / NB )*NB + 1
            DO 30 J = NN, 1, -NB
               JB = MIN( NB, N-J+1 )
               IF( J+JB.LE.N ) THEN
!
!                 Compute rows j+jb:n of current block column
!
                  CALL DTRMM( 'Left', 'Lower', 'No transpose', DIAG,
     $                        N-J-JB+1, JB, ONE, A( J+JB, J+JB ), LDA,
     $                        A( J+JB, J ), LDA )
                  CALL DTRSM( 'Right', 'Lower', 'No transpose', DIAG,
     $                        N-J-JB+1, JB, -ONE, A( J, J ), LDA,
     $                        A( J+JB, J ), LDA )
               END IF
!
!              Compute inverse of current diagonal block
!
               CALL DTRTI2( 'Lower', DIAG, JB, A( J, J ), LDA, INFO )
   30       CONTINUE
         END IF
      END IF
!
      RETURN
!
!     End of DTRTRI
!
      END SUBROUTINE DTRTRI
!
!  =====================================================================
!
!> @brief DGETRS solves a system of linear equations
!>    A * X = B  or  A**T * X = B
!> with a general N-by-N matrix A using the LU factorization computed
!> by DGETRF.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!
!  Definition:
!  ===========
!
!       SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!
!       .. Scalar Arguments ..
!       CHARACTER          TRANS
!       INTEGER            INFO, LDA, LDB, N, NRHS
!       ..
!       .. Array Arguments ..
!       INTEGER            IPIV( * )
!       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
!       ..
!
!
!> Purpose:
!  =============
!>
!> \verbatim
!>
!> DGETRS solves a system of linear equations
!>    A * X = B  or  A**T * X = B
!> with a general N-by-N matrix A using the LU factorization computed
!> by DGETRF.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> @param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>          Specifies the form of the system of equations:
!>          = 'N':  A * X = B  (No transpose)
!>          = 'T':  A**T* X = B  (Transpose)
!>          = 'C':  A**T* X = B  (Conjugate transpose = Transpose)
!> \endverbatim
!>
!> @param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.
!> \endverbatim
!>
!> @param[in] NRHS
!> \verbatim
!>          NRHS is INTEGER
!>          The number of right hand sides, i.e., the number of columns
!>          of the matrix B.  NRHS >= 0.
!> \endverbatim
!>
!> @param[in] A
!> \verbatim
!>          A is DOUBLE PRECISION array, dimension (LDA,N)
!>          The factors L and U from the factorization A = P*L*U
!>          as computed by DGETRF.
!> \endverbatim
!>
!> @param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,N).
!> \endverbatim
!>
!> @param[in] IPIV
!> \verbatim
!>          IPIV is INTEGER array, dimension (N)
!>          The pivot indices from DGETRF; for 1<=i<=N, row i of the
!>          matrix was interchanged with row IPIV(i).
!> \endverbatim
!>
!> @param[in,out] B
!> \verbatim
!>          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
!>          On entry, the right hand side matrix B.
!>          On exit, the solution matrix X.
!> \endverbatim
!>
!> @param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>          The leading dimension of the array B.  LDB >= max(1,N).
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
!
!  =====================================================================
!
      SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            INFO, LDA, LDB, N, NRHS
!     ..
!     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
!     ..
!
!  Purpose
!  =======
!
!  DGETRS solves a system of linear equations
!     A * X = B  or  A' * X = B
!  with a general N-by-N matrix A using the LU factorization computed
!  by DGETRF.
!
!  Arguments
!  =========
!
!  TRANS   (input) CHARACTER*1
!          Specifies the form of the system of equations:
!          = 'N':  A * X = B  (No transpose)
!          = 'T':  A'* X = B  (Transpose)
!          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrix B.  NRHS >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
!          The factors L and U from the factorization A = P*L*U
!          as computed by DGETRF.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  IPIV    (input) INTEGER array, dimension (N)
!          The pivot indices from DGETRF; for 1<=i<=N, row i of the
!          matrix was interchanged with row IPIV(i).
!
!  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
!          On entry, the right hand side matrix B.
!          On exit, the solution matrix X.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NOTRAN
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLASWP, DTRSM, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRS', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
!
      IF( NOTRAN ) THEN
!
!        Solve A * X = B.
!
!        Apply row interchanges to the right hand sides.
!
         CALL DLASWP( NRHS, B, LDB, 1, N, IPIV, 1 )
!
!        Solve L*X = B, overwriting B with X.
!
         CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
!
!        Solve U*X = B, overwriting B with X.
!
         CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
      ELSE
!
!        Solve A' * X = B.
!
!        Solve U'*X = B, overwriting B with X.
!
         CALL DTRSM( 'Left', 'Upper', 'Transpose', 'Non-unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
!
!        Solve L'*X = B, overwriting B with X.
!
         CALL DTRSM( 'Left', 'Lower', 'Transpose', 'Unit', N, NRHS, ONE,
     $               A, LDA, B, LDB )
!
!        Apply row interchanges to the solution vectors.
!
         CALL DLASWP( NRHS, B, LDB, 1, N, IPIV, -1 )
      END IF
!
      RETURN
!
!     End of DGETRS
!
      END SUBROUTINE DGETRS
!
