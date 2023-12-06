targets::tar_dir({ # tar_dir() runs code from a temporary directory.
targets::tar_script({
		library(tidyverse)
  tarchetypes::tar_map2_count(
    x,
    command1 = tibble::tibble(
      a = r
     ) ,
    command2 = {r %>% str(); tibble()},
    values = 

    	tibble::tibble(arg1 = letters[seq_len(26)]) %>%
    		mutate( r = row_number() %% 2+1) %>%
     		nest( data = c(arg1))

    	,
    batches = 5
   )
})
targets::tar_make()
targets::tar_read(x)
})
