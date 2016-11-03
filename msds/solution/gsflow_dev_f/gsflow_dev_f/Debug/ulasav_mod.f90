        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 02 07:53:33 2009
        MODULE ULASAV_mod
          INTERFACE 
            SUBROUTINE ULASAV(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,NROW,&
     &ILAY,ICHN)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: BUF(NCOL,NROW)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              REAL(KIND=4) :: PERTIM
              REAL(KIND=4) :: TOTIM
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: ICHN
            END SUBROUTINE ULASAV
          END INTERFACE 
        END MODULE ULASAV_mod
