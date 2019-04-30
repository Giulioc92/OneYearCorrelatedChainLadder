#### from spot to forward
library(readxl)
library(dplyr)
rfr <- read_excel("C:/Users/gcarnevale/Desktop/Eiopa Stuff/Eioparfr311218.xlsx", 
                  col_names = FALSE)
head(rfr)

forward <- function(term_structure,years_ahead){
  
  fwd <- ( (1 + term_structure[-1,2]) / unlist((1 + term_structure[years_ahead,2])) ) - 1
  return(unname(fwd))
}


fwd_curve <- forward(rfr,1)

