#### from spot to forward

file <- "Eioparfr311218.xlsx"
rfr <- read_excel(paste(getwd(),file,sep='/'), col_names = FALSE)

head(rfr)

forward <- function(term_structure,years_ahead){
  
  fwd <- ( (1 + term_structure[-c(1:years_ahead),2]) / unlist((1 + term_structure[years_ahead,2])) ) - 1
  return(unname(fwd))
}

fwd_curve <- forward(rfr,1)

