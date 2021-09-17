optimized_material <- function(x, type) {
  df_x <-
    x %>% 
    left_join(tipe, by = "model") %>% 
    dplyr::filter(tipe == type)
  
  step1    <- df_x %>% foo(bin.step = 1)  
  step10   <- df_x %>% foo(bin.step = 10)
  step20   <- df_x %>% foo(bin.step = 20)
  step30   <- df_x %>% foo(bin.step = 30)
  step40   <- df_x %>% foo(bin.step = 40)
  step50   <- df_x %>% foo(bin.step = 50)
  step100  <- df_x %>% foo(bin.step = 100)
  step150  <- df_x %>% foo(bin.step = 150)
  step200  <- df_x %>% foo(bin.step = 200)
  step250  <- df_x %>% foo(bin.step = 250)
  step300  <- df_x %>% foo(bin.step = 300)
  step350  <- df_x %>% foo(bin.step = 350)
  step400  <- df_x %>% foo(bin.step = 400)
  step450  <- df_x %>% foo(bin.step = 450)
  step500  <- df_x %>% foo(bin.step = 500)
  step550  <- df_x %>% foo(bin.step = 550)
  step600  <- df_x %>% foo(bin.step = 600)
  
  steps <-
    step1 %>% 
    left_join(step20, by = "materi") %>%
    left_join(step20, by = "materi") %>% 
    left_join(step30, by = "materi") %>% 
    left_join(step40, by = "materi") %>% 
    left_join(step50, by = "materi") %>% 
    left_join(step100, by = "materi") %>% 
    left_join(step150, by = "materi") %>% 
    left_join(step200, by = "materi") %>% 
    left_join(step250, by = "materi") %>% 
    left_join(step300, by = "materi") %>% 
    left_join(step350, by = "materi") %>% 
    left_join(step400, by = "materi") %>% 
    left_join(step450, by = "materi") %>% 
    left_join(step500, by = "materi") %>% 
    left_join(step550, by = "materi") %>% 
    left_join(step600, by = "materi")
  
  steps_col <- tibble(
    name = colnames(steps)[-1],
    steps = c(1, !!! seq(10, 40, 10), !!! seq(50, 600, 50))
  )
  
  # min_total <-
  #   steps %>% 
  #   pivot_longer(cols = starts_with("total"),
  #                names_to = "label",
  #                values_to = "total") %>% 
  #   group_by(materi) %>% 
  #   summarise(total = min(total)) %>% 
  #   ungroup() %>% 
  #   arrange(desc(total))
  
  min_total_with_steps <-
    steps %>% 
    pivot_longer(cols = starts_with("total"),
                 names_to = "label",
                 values_to = "total") %>% 
    left_join(steps_col, by = c("label" = "name")) %>% 
    group_by(materi) %>% 
    arrange(total) %>%
    slice(1) %>% 
    ungroup() %>% 
    select(-label)
  
  return(min_total_with_steps)
}


