# use: "caffeinate -u make -i -k"

all:
#	cd ground_control_points/gcp_positions/ && $(MAKE) #prepare ground control point positions
#	cd ground_control_panels/control_panel_positions/ && $(MAKE) #prepare ground control panel positions
#	cd ground_control_points/make_ground_panel_templates/ && $(MAKE) #prepare ground control point templates

	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_07_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_09_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_12_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_14_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_16_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_19_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_21_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_23_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_26_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_06_30_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_07_03_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_07_05_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_07_07_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	scripts/bash/create_gcloud_instances_vc
	cd working_photos/btn_2017_07_10_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	scripts/bash/delete_gcloud_instances_vc
	cd working_photos/btn_2018_06_11_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	cd working_photos/btn_2018_06_13_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	cd working_photos/btn_2018_06_15_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	cd working_photos/btn_2018_06_20_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	cd working_photos/btn_2018_06_22_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	cd working_photos/btn_2018_06_25_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt
	cd working_photos/btn_2018_06_29_flight_1_120ft/ && $(MAKE) -j4 -i -k &> run_log_all.txt

clean:
#	cd working_photos/btn_2017_06_07_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2017_06_09_flight_1_120ft/ && $(MAKE) clean
	cd working_photos/btn_2017_06_12_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2017_06_14_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2017_06_16_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2017_06_19_flight_1_120ft/ && $(MAKE) clean
	cd working_photos/btn_2017_06_21_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2017_06_23_flight_1_120ft/ && $(MAKE) clean
	cd working_photos/btn_2017_06_26_flight_1_120ft/ && $(MAKE) clean
	cd working_photos/btn_2017_06_30_flight_1_120ft/ && $(MAKE) clean
	cd working_photos/btn_2017_07_03_flight_1_120ft/ && $(MAKE) clean
	cd working_photos/btn_2017_07_05_flight_1_120ft/ && $(MAKE) clean
	cd working_photos/btn_2017_07_07_flight_1_120ft/ && $(MAKE) clean
	cd working_photos/btn_2017_07_10_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2018_06_11_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2018_06_13_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2018_06_15_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2018_06_20_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2018_06_22_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2018_06_25_flight_1_120ft/ && $(MAKE) clean
#	cd working_photos/btn_2018_06_29_flight_1_120ft/ && $(MAKE) clean

clean_partial:
#	cd working_photos/btn_2017_06_07_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2017_06_09_flight_1_120ft/ && $(MAKE) clean_partial
	cd working_photos/btn_2017_06_12_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2017_06_14_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2017_06_16_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2017_06_19_flight_1_120ft/ && $(MAKE) clean_partial
	cd working_photos/btn_2017_06_21_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2017_06_23_flight_1_120ft/ && $(MAKE) clean_partial
	cd working_photos/btn_2017_06_26_flight_1_120ft/ && $(MAKE) clean_partial
	cd working_photos/btn_2017_06_30_flight_1_120ft/ && $(MAKE) clean_partial
	cd working_photos/btn_2017_07_03_flight_1_120ft/ && $(MAKE) clean_partial
	cd working_photos/btn_2017_07_05_flight_1_120ft/ && $(MAKE) clean_partial
	cd working_photos/btn_2017_07_07_flight_1_120ft/ && $(MAKE) clean_partial
	cd working_photos/btn_2017_07_10_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2018_06_11_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2018_06_13_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2018_06_15_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2018_06_20_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2018_06_22_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2018_06_25_flight_1_120ft/ && $(MAKE) clean_partial
#	cd working_photos/btn_2018_06_29_flight_1_120ft/ && $(MAKE) clean_partial


partial:
#	cd ground_control_points/gcp_positions/ && $(MAKE) #prepare ground control point positions
#	cd ground_control_panels/control_panel_positions/ && $(MAKE) #prepare ground control panel positions
#	cd ground_control_points/make_ground_panel_templates/ && $(MAKE) #prepare ground control point templates
#	cd working_photos/btn_2017_06_07_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2017_06_09_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
	cd working_photos/btn_2017_06_12_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2017_06_14_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2017_06_16_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2017_06_19_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
	cd working_photos/btn_2017_06_21_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2017_06_23_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
	cd working_photos/btn_2017_06_26_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
	cd working_photos/btn_2017_06_30_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
	cd working_photos/btn_2017_07_03_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
	cd working_photos/btn_2017_07_05_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
	cd working_photos/btn_2017_07_07_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
	cd working_photos/btn_2017_07_10_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2018_06_11_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2018_06_13_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2018_06_15_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2018_06_20_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2018_06_22_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2018_06_25_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt
#	cd working_photos/btn_2018_06_29_flight_1_120ft/ && $(MAKE) -j4 -i -k status/04_gcp_lists_complete.txt &> run_log_partial.txt