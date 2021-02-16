!***********************************************************************
! Transfer MODFLOW data to PRMS
!***********************************************************************
!     ******************************************************************
!     Mapping module to convert MODFLOW to PRMS states for use by GSFLOW
!   Declared Parameters
!     gvr_hru_id, gvr_cell_id
!     ******************************************************************
      INTEGER FUNCTION gsflow_mf2prms()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, RUN, DECL
      USE GSFMODFLOW, ONLY: Mfq2inch_conv, Gwc_col, Gwc_row, &
                            Mfl2_to_acre, Mfl_to_inch, Hru_ag_irr
      USE PRMS_SOILZONE, ONLY: Hrucheck, Gvr_hru_id, Gw2sm_grav
      USE PRMS_WATER_USE, ONLY: Dprst_gain
      USE GWFUZFMODULE, ONLY: SEEPOUT
      USE PRMS_MODULE, ONLY: Process_flag, Nhrucell, Gvr_cell_id, Ag_package_active
      USE GWFBASMODULE, ONLY: DELT
      USE GSFMODFLOW, ONLY: Mfl3_to_ft3, Mft_to_sec
      USE GWFAGMODULE, ONLY: NUMIRRWELSP, IRRWELVAR, NUMCELLS, WELLIRRPRMS, IRRROW_SW, &
     &                       NUMIRRDIVERSIONSP, IRRSEG, DVRCH, DIVERSIONIRRPRMS, IRRROW_GW, &
     &                       NUMIRRPONDSP, NUMCELLSPOND, IRRHRU_POND, PONDIRRPRMS, PONDSEGFLOW, &
     &                       IRRPONDVAR
      IMPLICIT NONE
! Functions
      EXTERNAL print_module
! Local Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW MODFLOW to PRMS'
      character(len=*), parameter :: MODNAME = 'gsflow_mf2prms'
      character(len=*), parameter :: Version_gsflow_mf2prms = '2021-01-14'
      INTEGER :: i, j, k, ihru
      integer :: IRWL,NMCL,SGNM
      DOUBLE PRECISION :: mf_q2prms_inchacres
      real :: conversion
!***********************************************************************
      gsflow_mf2prms = 0

      IF ( Process_flag==RUN ) THEN
        DO i = 1, Nhrucell
          IF ( Hrucheck(Gvr_hru_id(i))==1 ) &
     &         Gw2sm_grav(i) = SEEPOUT(Gwc_col(Gvr_cell_id(i)), Gwc_row(Gvr_cell_id(i)))*Mfq2inch_conv(i)
        ENDDO
!
! Add irrigation to HRU from AG Package
!
! From irrigation wells
!
        IF ( Ag_package_active==ACTIVE ) THEN
          mf_q2prms_inchacres = DELT*Mfl2_to_acre*Mfl_to_inch
          Hru_ag_irr = 0.0
          DO J = 1, NUMIRRWELSP
            IRWL = IRRWELVAR(J)
            NMCL = 0
            IF ( IRWL > 0 ) NMCL = NUMCELLS(IRWL)
            DO K = 1, NMCL
              ihru = IRRROW_GW(K,IRWL)
              Hru_ag_irr(ihru) = Hru_ag_irr(ihru) + WELLIRRPRMS(k,IRWL)*SNGL(mf_q2prms_inchacres)
            END DO
          END DO
!
! From segment diversions     
!
          DO J = 1, NUMIRRDIVERSIONSP
            SGNM = IRRSEG(J)
            NMCL = 0
            IF ( SGNM>0 ) NMCL = DVRCH(SGNM)
            DO K=1,NMCL        
              ihru = IRRROW_SW(K,SGNM)
              Hru_ag_irr(ihru) = Hru_ag_irr(ihru) + DIVERSIONIRRPRMS(k,SGNM)*SNGL(mf_q2prms_inchacres)
            END DO
          END DO
!
! From open depression storage reservoirs and streams to storage reservoirs
!
          DO i = 1, NUMIRRPONDSP
            J = IRRPONDVAR(i)
            conversion = Mfl3_to_ft3/Mft_to_sec
            dprst_gain(J) = PONDSEGFLOW(J)*conversion   !need to check that depression storage is active
            DO k = 1, NUMCELLSPOND(i)
              ihru = IRRHRU_POND(k, i)
              Hru_ag_irr(ihru) = Hru_ag_irr(ihru) + PONDIRRPRMS(k, i)*SNGL(mf_q2prms_inchacres)
            END DO
          END DO
        END IF

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_gsflow_mf2prms)
      ENDIF

      END FUNCTION gsflow_mf2prms
