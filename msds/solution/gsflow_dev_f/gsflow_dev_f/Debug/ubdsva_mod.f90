        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 02 07:53:33 2009
        MODULE UBDSVA_mod
          INTERFACE 
            SUBROUTINE UBDSVA(IBDCHN,NCOL,NROW,J,I,K,Q,IBOUND,NLAY)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: IBDCHN
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: Q
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW,NLAY)
            END SUBROUTINE UBDSVA
          END INTERFACE 
        END MODULE UBDSVA_mod
