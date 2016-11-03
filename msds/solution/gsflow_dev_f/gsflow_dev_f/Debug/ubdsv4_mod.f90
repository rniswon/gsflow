        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 02 07:53:33 2009
        MODULE UBDSV4_mod
          INTERFACE 
            SUBROUTINE UBDSV4(KSTP,KPER,TEXT,NAUX,AUXTXT,IBDCHN,NCOL,   &
     &NROW,NLAY,NLIST,IOUT,DELT,PERTIM,TOTIM,IBOUND)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: NAUX
              CHARACTER(LEN=16) :: AUXTXT(*)
              INTEGER(KIND=4) :: IBDCHN
              INTEGER(KIND=4) :: NLIST
              INTEGER(KIND=4) :: IOUT
              REAL(KIND=4) :: DELT
              REAL(KIND=4) :: PERTIM
              REAL(KIND=4) :: TOTIM
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW,NLAY)
            END SUBROUTINE UBDSV4
          END INTERFACE 
        END MODULE UBDSV4_mod
