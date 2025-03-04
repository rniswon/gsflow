#!/bin/sh

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set init_vars_from_file 0

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set end_time 1983,9,1,0,0,0 -set modflow_name ../input/modflow/sagehen_1.nam -set gsflow_output_file ../output/gsflow_1.out -set model_output_file ../output/prms/prms1.out -set csv_output_file ../output/gsflow_1.csv -set init_vars_from_file 0 -set var_save_file ../output/prms/prms_ic_1 

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,09,02,0,0 -set end_time 1983,09,08,0,0,0 -set modflow_name ../input/modflow/sagehen_2.nam -set gsflow_output_file ../output/gsflow_2.out -set model_output_file ../output/prms/prms2.out -set csv_output_file ../output/gsflow_2.csv -set var_init_file ../output/prms/prms_ic_1 -set var_save_file ../output/prms/prms_ic_2

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,09,09,0,0 -set end_time 1983,09,15,0,0,0 -set modflow_name ../input/modflow/sagehen_3.nam -set gsflow_output_file ../output/gsflow_3.out -set model_output_file ../output/prms/prms3.out -set csv_output_file ../output/gsflow_3.csv -set var_init_file ../output/prms/prms_ic_2 -set var_save_file ../output/prms/prms_ic_3

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,09,16,0,0 -set end_time 1983,09,22,0,0,0 -set modflow_name ../input/modflow/sagehen_4.nam -set gsflow_output_file ../output/gsflow_4.out -set model_output_file ../output/prms/prms4.out -set csv_output_file ../output/gsflow_4.csv -set var_init_file ../output/prms/prms_ic_3 -set var_save_file ../output/prms/prms_ic_4

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,09,23,0,0 -set end_time 1983,09,29,0,0,0 -set modflow_name ../input/modflow/sagehen_5.nam -set gsflow_output_file ../output/gsflow_5.out -set model_output_file ../output/prms/prms5.out -set csv_output_file ../output/gsflow_5.csv -set var_init_file ../output/prms/prms_ic_4 -set var_save_file ../output/prms/prms_ic_5

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,09,30,0,0 -set end_time 1983,10,06,0,0,0 -set modflow_name ../input/modflow/sagehen_6.nam -set gsflow_output_file ../output/gsflow_6.out -set model_output_file ../output/prms/prms6.out -set csv_output_file ../output/gsflow_6.csv -set var_init_file ../output/prms/prms_ic_5 -set var_save_file ../output/prms/prms_ic_6

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,10,07,0,0 -set end_time 1983,10,13,0,0,0 -set modflow_name ../input/modflow/sagehen_7.nam -set gsflow_output_file ../output/gsflow_7.out -set model_output_file ../output/prms/prms7.out -set csv_output_file ../output/gsflow_7.csv -set var_init_file ../output/prms/prms_ic_6 -set var_save_file ../output/prms/prms_ic_7

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,10,14,0,0 -set end_time 1983,10,20,0,0,0 -set modflow_name ../input/modflow/sagehen_8.nam -set gsflow_output_file ../output/gsflow_8.out -set model_output_file ../output/prms/prms8.out -set csv_output_file ../output/gsflow_8.csv -set var_init_file ../output/prms/prms_ic_7 -set var_save_file ../output/prms/prms_ic_8

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,10,21,0,0 -set end_time 1983,10,27,0,0,0 -set modflow_name ../input/modflow/sagehen_9.nam -set gsflow_output_file ../output/gsflow_9.out -set model_output_file ../output/prms/prms9.out -set csv_output_file ../output/gsflow_9.csv -set var_init_file ../output/prms/prms_ic_8 -set var_save_file ../output/prms/prms_ic_9

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,10,28,0,0 -set end_time 1983,11,03,0,0,0 -set modflow_name ../input/modflow/sagehen_10.nam -set gsflow_output_file ../output/gsflow_10.out -set model_output_file ../output/prms/prms10.out -set csv_output_file ../output/gsflow_10.csv -set var_init_file ../output/prms/prms_ic_9 -set var_save_file ../output/prms/prms_ic_10

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,11,04,0,0 -set end_time 1983,11,10,0,0,0 -set modflow_name ../input/modflow/sagehen_11.nam -set gsflow_output_file ../output/gsflow_11.out -set model_output_file ../output/prms/prms11.out -set csv_output_file ../output/gsflow_11.csv -set var_init_file ../output/prms/prms_ic_10 -set var_save_file ../output/prms/prms_ic_11

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,11,11,0,0 -set end_time 1983,11,17,0,0,0 -set modflow_name ../input/modflow/sagehen_12.nam -set gsflow_output_file ../output/gsflow_12.out -set model_output_file ../output/prms/prms12.out -set csv_output_file ../output/gsflow_12.csv -set var_init_file ../output/prms/prms_ic_11 -set var_save_file ../output/prms/prms_ic_12

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,11,18,0,0 -set end_time 1983,11,24,0,0,0 -set modflow_name ../input/modflow/sagehen_13.nam -set gsflow_output_file ../output/gsflow_13.out -set model_output_file ../output/prms/prms13.out -set csv_output_file ../output/gsflow_13.csv -set var_init_file ../output/prms/prms_ic_12 -set var_save_file ../output/prms/prms_ic_13

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,11,25,0,0 -set end_time 1983,12,01,0,0,0 -set modflow_name ../input/modflow/sagehen_14.nam -set gsflow_output_file ../output/gsflow_14.out -set model_output_file ../output/prms/prms14.out -set csv_output_file ../output/gsflow_14.csv -set var_init_file ../output/prms/prms_ic_13 -set var_save_file ../output/prms/prms_ic_14

echo
echo '##########'
echo

../../../bin/gsflow ./gsflow.control -set start_time 1983,12,02,0,0 -set modflow_name ../input/modflow/sagehen_15.nam -set gsflow_output_file ../output/gsflow_15.out -set model_output_file ../output/prms/prms15.out -set csv_output_file ../output/gsflow_15.csv -set var_init_file ../output/prms/prms_ic_14 -set var_save_file ../output/prms/prms_ic_15

echo
echo '##########'
echo

../../../bin/CSV_merge

echo
echo '##########'
echo

