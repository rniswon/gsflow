        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 02 07:53:33 2009
        MODULE ULASV3_mod
          INTERFACE 
            SUBROUTINE ULASV3(IDATA,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,   &
     &NROW,ILAY,ICHN,FMTOUT,LBLSAV)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: IDATA(NCOL,NROW)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              REAL(KIND=4) :: PERTIM
              REAL(KIND=4) :: TOTIM
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: ICHN
              CHARACTER(LEN=20) :: FMTOUT
              INTEGER(KIND=4) :: LBLSAV
            END SUBROUTINE ULASV3
          END INTERFACE 
        END MODULE ULASV3_mod
