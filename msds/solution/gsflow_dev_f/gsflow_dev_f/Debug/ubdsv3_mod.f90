        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 02 07:53:33 2009
        MODULE UBDSV3_mod
          INTERFACE 
            SUBROUTINE UBDSV3(KSTP,KPER,TEXT,IBDCHN,BUFF,IBUFF,NOPT,NCOL&
     &,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: IBDCHN
              REAL(KIND=4) :: BUFF(NCOL,NROW,NLAY)
              INTEGER(KIND=4) :: IBUFF(NCOL,NROW)
              INTEGER(KIND=4) :: NOPT
              INTEGER(KIND=4) :: IOUT
              REAL(KIND=4) :: DELT
              REAL(KIND=4) :: PERTIM
              REAL(KIND=4) :: TOTIM
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW,NLAY)
            END SUBROUTINE UBDSV3
          END INTERFACE 
        END MODULE UBDSV3_mod
