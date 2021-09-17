# function to do calculations with bins 
# default values
#     - bin.min = 0 
#     - bin.max = 600 (in cm)------------> also used as bar length
#     - bin.step = 20
#
# OUTPUT: same as materials needed table

foo <- function(x, bin.min = 0, bin.max = 600, bin.step = 20) {
  
  bins <- tibble(
    max.bin = seq(bin.min, bin.max, bin.step)
  ) %>% 
    mutate(
      bins = cut(max.bin, breaks = c(seq(bin.min, bin.max, bin.step)))
    )
  
  df <-
    x %>% 
    dplyr::filter(ukuran <= 6000) %>% 
    mutate(
      ukuran = ukuran/10,
      bins = cut(ukuran, breaks = c(seq(bin.min, bin.max, bin.step)))
    ) %>% 
    left_join(bins, by = "bins")
  
  ukuran_bar = bin.max
  
  df_calc <-
    df %>%
    mutate(ukuran = max.bin) %>% # IMPORTANT
    group_by(materi, ukuran) %>% 
    summarise(total = sum(total)) %>% 
    ungroup() %>% 
    mutate(
      # how many can one bar make
      "satu bar 6m bisa" = ukuran_bar %/% ukuran,
      # number of bars needed
      "nomer bar perlu" = ceiling(total / `satu bar 6m bisa`),
      # total remainder in mm 
      "sisa total" = (ukuran_bar * `nomer bar perlu`) - (ukuran * total), # or (remainder_max + unused capacity)
      # remainder in each bar even if the maximum capacity is used
      "sisa (max kepakai)" = ukuran_bar - (`satu bar 6m bisa` * ukuran),
      # unused capacity
      "kapasitas tidak terpakai" = (`nomer bar perlu` * `satu bar 6m bisa` - total) * ukuran
    ) %>% 
    # rename columns
    rename(
      "ukuran (mm)" = "ukuran",
      "sisa total (mm)" = "sisa total",
      "sisa max kapasitas (mm)" = "sisa (max kepakai)",
      "kapasitas tidak terpakai (mm)" = "kapasitas tidak terpakai"
    ) %>% 
    # relocate remainder columns, put total remainder as last
    relocate(
      c(`kapasitas tidak terpakai (mm)` ,`sisa max kapasitas (mm)`, `sisa total (mm)`),
      .after = `nomer bar perlu`
    )
  
  df_result <-
    df_calc %>% 
    # remove bar length needed > 6m
    dplyr::filter(! is.infinite(`nomer bar perlu`)) %>% 
    # group by material and sum total
    group_by(materi) %>% 
    summarise(total = sum(`nomer bar perlu`)) %>% 
    ungroup() %>% 
    # arrange by descending total
    arrange(desc(total))
  
  return(df_result)
  
}