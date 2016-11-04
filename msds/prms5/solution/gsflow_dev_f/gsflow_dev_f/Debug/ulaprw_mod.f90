        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 02 07:53:34 2009
        MODULE ULAPRW_mod
          INTERFACE 
            SUBROUTINE ULAPRW(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,   &
     &IOUT)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: BUF(NCOL,NROW)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: IPRN
              INTEGER(KIND=4) :: IOUT
            END SUBROUTINE ULAPRW
          END INTERFACE 
        END MODULE ULAPRW_mod
