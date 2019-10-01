!***********************************************************************
!     GSFLOW modules
!***********************************************************************
      MODULE PRMS_MODULE
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAXFILE_LENGTH = 256, MAXCONTROL_LENGTH = 32
      INTEGER, PARAMETER :: MAXDIM = 500
      CHARACTER(LEN=68), PARAMETER :: &
     &  EQULS = '===================================================================='
      CHARACTER(LEN=11), PARAMETER :: MODNAME = 'gsflow_prms'
      CHARACTER(LEN=27), PARAMETER :: PRMS_VERSION = 'Version 5.MODSIM 09/30/2019'
      CHARACTER(LEN=8), SAVE :: Process, Arg
      !     Model (0=GSFLOW; 1=PRMS; 2=MODFLOW; 10=MODSIM-GSFLOW; 11=MODSIM-PRMS; 12=MODSIM-MODFLOW; 13=MODSIM)
      INTEGER, PARAMETER :: GSFLOW = 0, PRMS = 1, MODFLOW = 2, MODSIM_GSFLOW = 10
      INTEGER, PARAMETER :: MODSIM_PRMS = 11, MODSIM_MODFLOW = 12, MODSIM = 13, DOCUMENTATION = 99
      CHARACTER(LEN=80), SAVE :: PRMS_versn
      INTEGER, SAVE :: Model, Process_flag, Call_cascade, Ncascade, Ncascdgw
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsub, Nhrucell, Nlake, Ngwcell, Nlake_hrus
      INTEGER, SAVE :: Ntemp, Nrain, Nsol, Nsegment, Ndepl, Nobs, Nevap, Ndeplval, Nsnow
      INTEGER, SAVE :: Starttime(6), Endtime(6)
      INTEGER, SAVE :: Start_year, Start_month, Start_day, End_year, End_month, End_day
      INTEGER, SAVE :: Transp_flag, Sroff_flag, Solrad_flag, Et_flag
      INTEGER, SAVE :: Climate_temp_flag, Climate_precip_flag, Climate_potet_flag, Climate_transp_flag
      INTEGER, SAVE :: Lake_route_flag, Nratetbl, Strmflow_flag, Stream_order_flag
      INTEGER, SAVE :: Temp_flag, Precip_flag, Climate_hru_flag, Climate_swrad_flag
      INTEGER, SAVE :: Precip_combined_flag, Temp_combined_flag
      INTEGER, SAVE :: Inputerror_flag, Timestep
      INTEGER, SAVE :: Humidity_cbh_flag, Windspeed_cbh_flag
      INTEGER, SAVE :: Stream_temp_flag, Strmtemp_humidity_flag, PRMS4_flag
      INTEGER, SAVE :: Grid_flag, Logunt
      INTEGER, SAVE :: Kper_mfo, Kkstp_mfo, PRMS_flag, GSFLOW_flag
      INTEGER, SAVE :: PRMS_output_unit, Restart_inunit, Restart_outunit
      INTEGER, SAVE :: Dynamic_flag, Water_use_flag, Nwateruse, Nexternal, Nconsumed, Npoigages, Prms_warmup
      INTEGER, SAVE :: Elapsed_time_start(8), Elapsed_time_end(8), Elapsed_time_minutes
      INTEGER, SAVE :: mf_timestep, startday, endday, mf_nowtime, Number_timesteps
      INTEGER, SAVE :: Snow_cbh_flag, Gwflow_cbh_flag, Frozen_flag, statsON_OFF, Diversion2soil_flag
      REAL, SAVE :: Execution_time_start, Execution_time_end, Elapsed_time
      INTEGER, SAVE :: Kkiter, Have_lakes, MODSIM_flag
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_In_Out_vol(:)
      REAL, SAVE, ALLOCATABLE :: Hru_ag_irr(:)    !Ag irrigation added to HRU
      CHARACTER(LEN=80), SAVE :: Version_read_control_file, Version_read_parameter_file
!   Declared Parameters
      INTEGER, SAVE :: Mxsziter
      INTEGER, SAVE, ALLOCATABLE :: Gvr_cell_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_cell_pct(:)
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 8=temp_sta
! Control parameters
      INTEGER, SAVE :: Print_debug, MapOutON_OFF, CsvON_OFF, Dprst_flag, Subbasin_flag, Parameter_check_flag
      INTEGER, SAVE :: Init_vars_from_file, Save_vars_to_file, Orad_flag, Cascade_flag, Cascadegw_flag
      INTEGER, SAVE :: NhruOutON_OFF, Gwr_swale_flag, NsubOutON_OFF, BasinOutON_OFF, NsegmentOutON_OFF
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Model_output_file, Var_init_file, Var_save_file
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Csv_output_file, Model_control_file, Param_file
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: mappingFileName, xyFileName
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Temp_module, Srunoff_module, Et_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Strmflow_module, Transp_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Model_mode, Precip_module, Solrad_module
      CHARACTER(LEN=8), SAVE :: Soilzone_module
      INTEGER, SAVE :: Dyn_imperv_flag, Dyn_intcp_flag, Dyn_covden_flag, Dyn_covtype_flag, Dyn_transp_flag, Dyn_potet_flag
      INTEGER, SAVE :: Dyn_soil_flag, Dyn_radtrncf_flag, Dyn_dprst_flag,  Dprst_transferON_OFF
      INTEGER, SAVE :: Dyn_snareathresh_flag, Dyn_transp_on_flag
      INTEGER, SAVE :: Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, Dyn_fallfrost_flag, Dyn_springfrost_flag
      INTEGER, SAVE :: Gwr_transferON_OFF, External_transferON_OFF, Segment_transferON_OFF, Lake_transferON_OFF
      END MODULE PRMS_MODULE

      MODULE GSFMODFLOW
!   Local Variables
      INTEGER, PARAMETER :: ITDIM = 80
      INTEGER, SAVE :: Convfail_cnt, Steady_state, Ncells, Gsflag
      INTEGER, SAVE :: IGRID, KKPER, ICNVG, NSOL, IOUTS,KPERSTART
      INTEGER, SAVE :: KSTP, KKSTP, IERR, Max_iters
      INTEGER, SAVE :: Mfiter_cnt(ITDIM), Iter_cnt(ITDIM), Iterations
      INTEGER, SAVE :: Szcheck, Sziters, INUNIT, KPER, NCVGERR
      INTEGER, SAVE :: Max_sziters, Maxgziter
      INTEGER, SAVE, ALLOCATABLE :: Gwc_col(:), Gwc_row(:)
      REAL, SAVE :: Delt_save
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Stress_dates(:)
      INTEGER, SAVE :: Modflow_skip_stress, Kkper_new, Modflow_skip_time_step
      DOUBLE PRECISION, SAVE :: Modflow_time_in_stress,Modflow_skip_time
      DOUBLE PRECISION, SAVE :: Mft_to_sec, Totalarea_mf
      DOUBLE PRECISION, SAVE :: Mfl2_to_acre, Mfl3_to_ft3, Sfr_conv
      DOUBLE PRECISION, SAVE :: Acre_inches_to_mfl3, Mfl3t_to_cfs
      REAL, SAVE :: Mft_to_days, Mfl_to_inch, Inch_to_mfl_t
      DOUBLE PRECISION, SAVE :: mfstrt_jul  !RGN to get MF to stop at End_time for MODFLOW only
      REAL, SAVE, ALLOCATABLE :: Mfq2inch_conv(:), Cellarea(:)
      REAL, SAVE, ALLOCATABLE :: Gvr2cell_conv(:), Mfvol2inch_conv(:)
      CHARACTER(LEN=80), SAVE :: Version_gsflow_modflow
      CHARACTER(LEN=14), PARAMETER :: MODNAME = 'gsflow_modflow'
      INTEGER, SAVE :: Stopcount
      REAL(8), DIMENSION(5) :: DIVS
!-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION,VERSION2,VERSION3
      CHARACTER*10 MFVNAM
      PARAMETER (VERSION='1.1.4 4/01/2018')
      PARAMETER (VERSION2='1.12.0 02/03/2017')
      PARAMETER (VERSION3='1.04.0 09/15/2016')
      PARAMETER (MFVNAM='-NWT-SWR1')
      INTEGER, SAVE :: IBDT(8)
!   Control Parameters
      INTEGER, SAVE :: Modflow_time_zero(6)
      CHARACTER(LEN=200), SAVE :: Modflow_name
    END MODULE GSFMODFLOW
