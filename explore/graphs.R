
library(ggplot2)

mesh_closest_defib_transistion %>%
	mutate( diff = pre_sja_closest_distance - post_sja_closest_distance) %>%
	mutate( seifa_decile = as.factor( seifa_decile)) %>%
	ggplot(aes( seifa_decile, diff, fill=seifa_decile)) +
geom_violin() 



mesh_closest_defib_transistion %>%
	mutate( diff = pre_sja_closest_distance - post_sja_closest_distance) %>%
	mutate( seifa_decile = as.factor( seifa_decile)) %>%
	ggplot(aes( seifa_score, diff, color=seifa_decile)) +
geom_point() 



mesh_closest_defib_transistion %>%
	mutate( diff = pre_sja_closest_distance - post_sja_closest_distance) %>%
	mutate( seifa_decile = as.factor( seifa_decile)) %>%
	ggplot(aes( seifa_score, post_sja_closest_distance, color=seifa_decile)) +
geom_point() 



mesh_closest_defib_transistion %>%
	mutate( diff = pre_sja_closest_distance - post_sja_closest_distance) %>%
	mutate( seifa_decile = as.factor( seifa_decile)) %>%
	ggplot(aes( seifa_decile, post_sja_closest_distance, fill=seifa_decile)) +
geom_violin() 


mesh_closest_defib_transistion %>%
	mutate( diff = pre_sja_closest_distance - post_sja_closest_distance) %>%
	mutate( seifa_decile = as.factor( seifa_decile)) %>%
	ggplot(aes( seifa_decile, post_sja_closest_distance, fill=seifa_decile)) +
geom_boxplot() 
