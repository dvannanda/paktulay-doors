### By model, type and length

# make summary data to save to csv
df_summary <- 
  df %>% 
  group_by(model, primary, type, length) %>% 
  # get number of pieces needed according to group 
  tally() %>% 
  ungroup()

# save summary data to csv
write_csv(df_summary, file = "data/summary.csv")

# print summary data
df_summary %>% 
  datatable(
    colnames = c("Model", "Primary", "Type", "Length", "Number of Pieces Needed"),
    options = list(
      pageLength = 6
    )
  )

# ------------------------------------------------------------------------------------
### By model and length

# print summary by model and length
df %>% 
  group_by(model, length) %>% 
  tally() %>% 
  # formatting
  datatable(
    colnames = c("Model", "Length", "Number of Pieces Needed"),
    options = list(
      pageLength = 6
    )
  )


# ------------------------------------------------------------------------------------
### By length and model
# print summary by length and model
df %>% 
  group_by(length, model) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(length)) %>% 
  # formatting
  datatable(
    colnames = c("Length", "Model","Number of Pieces Needed"),
    options = list(
      pageLength = 6
    )
  )