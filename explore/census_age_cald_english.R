dir_ls( '../../reference_datasets')
dir_ls( '../../reference_datasets/2021_Census_GCP_Statistical_Area_1_for_VIC')

read_xlsx('../../reference_datasets/Metadata_2021_GCP_DataPack_R1_R2.xlsx', sheet='Cell Descriptors Information') %>%
clean_names() %>%
filter(  str_detect( data_packfile, 'G11' )) %>%
select( long ) %>%
filter(  str_detect( long, 'English' )) %>%
filter(  !str_detect( long, 'Year' )) %>%
filter(  !str_detect( long, 'AGED' )) %>%
pull( long ) %>% clipr::write_clip()




    read_xlsx("../../reference_datasets/Metadata_2021_GCP_DataPack_R1_R2.xlsx", sheet = "Cell Descriptors Information") %>%
        clean_names() %>%
        # filter(str_detect(data_packfile, "G01")) %>%
        filter(str_detect(short, "Age_6")) %>%
        filter(str_detect(long, "^Age[_ ]groups[_ ][678].*Persons$")) %>%
        pull(short)



read_xlsx('../../reference_datasets/Metadata_2021_GCP_DataPack_R1_R2.xlsx', sheet='Cell Descriptors Information') %>%


a %>%
select( ends_with( '_T') ) %>% names()

a %>%
select( starts_with( 'T_UOLSE') ) %>%

read_csv( '../../reference_datasets/2021_Census_GCP_Statistical_Area_1_for_VIC/2021Census_G11D_VIC_SA1.csv')  %>%
select( SA1_CODE_2021, T_UOLSE_NWNAA_T, T_UOLSE_PinE_NS_T)




read_xlsx('../../reference_datasets/Metadata_2021_GCP_DataPack_R1_R2.xlsx', sheet='Cell Descriptors Information') %>%
clean_names() %>%
filter(  str_detect( data_packfile, 'G01$' )) %>%
filter(  str_detect( columnheadingdescriptioninprofile, 'Persons' )) %>%
pull( long ) %>% clipr::write_clip()


a=c("T_UOLSE_NWNAA_T", "T_UOLSE_PinE_NS_T") 

read_csv( '../../reference_datasets/2021_Census_GCP_Statistical_Area_1_for_VIC/2021Census_G11D_VIC_SA1.csv')  %>%
select( SA1_CODE_2021, T_UOLSE_NWNAA_T, T_UOLSE_PinE_NS_T)


mutate( cald  sum( ))

read_csv( '../../reference_datasets/2021_Census_GCP_Statistical_Area_1_for_VIC/2021Census_G11D_VIC_SA1.csv')  %>%
select( SA1_CODE_2021, T_UOLSE_NWNAA_T, T_UOLSE_PinE_NS_T)

Age_groups_65_74_years_Persons
Age_groups_75_84_years_Persons
Age_groups_85_years_and_over_Persons