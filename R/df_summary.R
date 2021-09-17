# summary of models, materials, measurements and total in tidy format
# materials combined
df_summary <-
  df %>% 
  # group by model, materials and measurements, summarised by total needed
  group_by(model, materi, ukuran) %>% 
  summarise(total = sum(`perlu per unit`)*unit) %>% 
  ungroup() %>% 
  # ! for some reason, there is still duplicates - so we ran it again and it worked ¯\_(ツ)_/¯
  group_by(model, materi, ukuran) %>% 
  summarise(total = sum(total)) %>% 
  ungroup()

# summary of models, materials, measurements and total in tidy format
# materials separated

df_summary2 <-
  df %>% 
  # separate materials 
  separate(materi, c("materi1", "materi2"), " [+] ", extra = "merge") %>% 
  separate(materi2, c("materi2", "materi3"), " [+] ") %>% 
  # convert back into one column
  pivot_longer(cols = starts_with("materi"), 
               names_to = "label",
               values_to = "materi") %>% 
  # filter redundant rows
  dplyr::filter(! is.na(materi)) %>% 
  # remove labels (unnecessary)
  select(-label) %>% 
  
  # same as df_summary chunk
  group_by(model, materi, ukuran) %>% 
  summarise(total = sum(`perlu per unit`)*unit) %>% 
  ungroup() %>% 
  group_by(model, materi, ukuran) %>% 
  summarise(total = sum(total)) %>% 
  ungroup()