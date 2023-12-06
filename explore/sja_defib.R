victoria_defib_sua %>%
mutate( company = str_to_upper( company)) %>%
filter( str_detect( company, 'DEFIB IN')) %>%
pull( company) %>%


{.} -> filt