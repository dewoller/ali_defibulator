
read_xlsx("../../reference_datasets/Metadata_2021_GCP_DataPack_R1_R2.xlsx", sheet = "Cell Descriptors Information") %>%
		clean_names() %>%
		filter(str_detect(data_packfile, "G11")) %>%
		filter(str_detect(long, "TOTAL")) %>%
		filter(str_detect(long, "Total$")) %>%
	pull(long)

		filter(long %in% c(
			"TOTAL_Uses_other_language_and_speaks_English_Total_Total"
			, "TOTAL_Uses_other_language_and_speaks_English_Not_well_or_not_at_all_Total"
			# , "TOTAL_Uses_other_language_and_speaks_English_Proficiency_in_English_not_stated_Total"
		)) %>%
		pull(short)


