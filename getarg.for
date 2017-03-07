      SUBROUTINE myGetArg (INPUTFILE)
      CHARACTER*64 INPUTFILE,OUTPUTFILE
      INTEGER STATUS,LENGTH
C added GNU intrinsic call remove Lahey
C alexander 03-07/17
      CALL GET_COMMAND_ARGUMENT(1,INPUTFILE,LENGTH,STATUS)
      IF ( LENGTH .EQ. 0 ) THEN
        CALL PRINT_HELP
      END IF

      IF ( STATUS .EQ. 0 ) THEN
        CALL CROP (INPUTFILE)

        RETURN
      ELSE
        WRITE (*,*)'UNKNOWN COMMAND LINE ARGUMENT'
        IF ( STATUS .NE. 0)CALL EXIT(STATUS)
      END IF

      RETURN
      END

      SUBROUTINE Crop (Word)
!  Crop tidies up the character string Word by left-justifying
!   and blanking out everything after the first trailing blank.
! Ajit J. Thakkar, Chemistry Dept., U. of New Brunswick
      IMPLICIT NONE
      CHARACTER*(*) Word
      INTEGER I,J,K
      CHARACTER*1 Blank
      PARAMETER (Blank=' ')
      DO I=1,LEN(Word)
        IF(Word(I:I).NE.Blank)THEN
          J=I
          GOTO 20
        END IF
      END DO
      RETURN
   20 K=0
      DO I=J,LEN(Word)
        IF(Word(I:I).NE.Blank)THEN
          K=K+1
          Word(K:K)=Word(I:I)
        ELSE
          EXIT
        END IF
      END DO
      DO I=K+1,LEN(Word)
       Word(I:I)=Blank
      END DO
      RETURN
      END

      SUBROUTINE PRINT_HELP()
      WRITE(*,*)" "
      CALL TIMESTAMP()
      WRITE(*,*)'enter input filename.f or .for '
      WRITE(*,*)'output files *.lis *.tid'
      WRITE(*,*)'*.lis - list of error'
      WRITE(*,*)'*.tdy - tidy fortran file'
      WRITE(*,*)'ex: ftidy test.f=> test.lis test.tid'
      CALL EXIT(0)
      RETURN
      END

      SUBROUTINE TIMESTAMP( )
      IMPLICIT NONE
      CHARACTER ( LEN = 8 ) DATE
      CHARACTER ( LEN = 10 ) TIME
      CALL DATE_AND_TIME ( DATE, TIME )
      WRITE ( *, '(A8,2X,A10)' ) DATE, TIME
      RETURN
      END
