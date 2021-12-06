#' Function to calculate number of aluminium bar needed 
#' 
#' e.g. countalum(x, barLength = 6000)
#' 
#' nested-function: onebar(x, barLength = 6000)


# function to calculate how many cuts each bar fits
onebar <- function(x, barLength = 6000) {
  r = barLength
  barCut = c()
  needLeft = c()
  
  for (i in x) {
    if (i <= r) {
      r = r - i
      index = length(barCut) + 1
      barCut[index] = i
    } else {
      index = length(needLeft) + 1
      needLeft[index] = i
    }
  }
  
  output = list(barCut, needLeft)
  
  return(output)
}

# function to calculate how many bars each material needs
countalum <- function(x, barLength = 6000) {
  
  barNeeded = 0
  needLeft = x
  barcuts = c()
  
  while (length(needLeft > 0)) {
    
    barNeeded = barNeeded + 1
    x = onebar(needLeft, barLength = barLength)
    needLeft = x[[2]]
    
    index = length(barcuts) + 1
    barcuts[[index]] <- x[[1]]
  }
  
  remainder = list()
  
  for (i in barcuts) {
    index = length(remainder) + 1
    remainder[index] = 6000 - sum(i)
  }
  
  output <-
    tibble(
      bar = 1:barNeeded, barcuts, remainder = remainder
    ) %>% 
    unnest(cols = c(barcuts, remainder)) %>% 
    group_by(bar) %>% 
    summarise(
      bar_cuts = paste(barcuts, collapse = " + "),
      remainder = min(remainder)
    )
  
  return(output)
  
}



