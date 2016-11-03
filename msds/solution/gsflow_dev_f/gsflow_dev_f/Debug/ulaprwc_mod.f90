        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 02 07:53:34 2009
        MODULE ULAPRWC_mod
          INTERFACE 
            SUBROUTINE ULAPRWC(A,NCOL,NROW,ILAY,IOUT,IPRN,ANAME)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: A(NCOL,NROW)
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: IPRN
              CHARACTER(*) :: ANAME
            END SUBROUTINE ULAPRWC
          END INTERFACE 
        END MODULE ULAPRWC_mod
