# ------------------------------------------------------------------------------------
### Overall, grouped by length

# make table calculations
df_length <-
  df %>% 
  # group by length and tally
  group_by(length) %>% 
  tally() %>% 
  ungroup() %>% 
  # calculations
  mutate(each_bar_gives = bar_length %/% length,
         bar_needed = ceiling(n/each_bar_gives),
         remainder_length_total = (bar_length*bar_needed) - (length*n), # also remainder_at_max_cap * bars_needed + unused capacity
         remainder_at_max_cap = round(bar_length - (each_bar_gives*length), 2),
         unused_capacity = (bar_needed*each_bar_gives - n)*length)

# print table
df_length %>% 
  arrange(desc(length)) %>% 
  DT::datatable(
    colnames = c("Length", "Number of Pieces", "Each Bar (6m) Gives", "Number of Bars Needed", "Total Remainder Length", "Remainder at Max Cap", "Unused Capacity")
  )

# ------------------------------------------------------------------------------------
### Overall, grouped by length (with bins)

df_with_bins <-
  df %>%
  # make bins
  mutate(
    bins = cut(length, 
               breaks = c(seq(0, 6, 0.5)),
               labels = c(seq(0.5, 6, 0.5)) %>% 
                 as.character(),
               right = FALSE)
  ) %>% 
  # group by bins and tally
  group_by(bins) %>% 
  tally() %>% 
  ungroup() %>% 
  # rename bins to length and arrange to desc order
  rename(length = bins) %>% 
  arrange(desc(length)) %>% 
  
  mutate(
    # change bins (fct) to numeric variable
    length = as.character(length),
    length = as.double(length),
    
    # calculations
    each_bar_gives = bar_length %/% length,
    bar_needed = ceiling(n/each_bar_gives),
    remainder_length_total = (bar_length*bar_needed) - (length*n), # also remainder_at_max_cap * bars_needed + unused capacity
    remainder_at_max_cap = bar_length - (each_bar_gives*length),
    unused_capacity = (bar_needed*each_bar_gives - n)*length
  )

# print table
df_with_bins %>% 
  datatable(
    colnames = c("Length (with Bins)", "Number of Pieces", "Each Bar (6m) Gives", "Number of Bars Needed", "Total Remainder Length", "Remainder at Max Cap", "Unused Capacity"),
    options = list(
      pageLength = 11
    )
  )