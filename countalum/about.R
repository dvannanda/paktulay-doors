#' About page for countalum shinyapps

column(
  width = 10,
  offset = 1,
  
  h2("Github Repository"),
  
  tags$a(href = "https://github.com/dvannanda/paktulay-doors", "Click here"),
  
  h2("References"),
  
  p("Greg Lin (2020). reactable: Interactive Data Tables Based on 'React Table'. R package version 0.2.3.
  https://CRAN.R-project.org/package=reactable"),
  
  p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for
  Statistical Computing, Vienna, Austria. URL https://www.R-project.org/."),
  
  p("Victor Perrier, Fanny Meyer and David Granjon (2021). shinyWidgets: Custom Inputs Widgets for Shiny.
  R package version 0.6.2. https://CRAN.R-project.org/package=shinyWidgets"),
  
  p("Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686,
  https://doi.org/10.21105/joss.01686"),
  
  p("Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen,
  Jonathan McPherson, Alan Dipert and Barbara Borges (2021). shiny: Web Application Framework for R. R
  package version 1.6.0. https://CRAN.R-project.org/package=shiny")
  
)