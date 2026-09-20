calculate_chisq <- function(data) {
  stopifnot("no" %in% names(data),
            "ei" %in% names(data))
  
  sum((data$no - data$ei)^2 / data$ei)
}

chisq_formula <- function(data) {
  stopifnot("no" %in% names(data),
            "ei" %in% names(data))
  
  start <- "$$\n \\chi^2="
  end <- "\n $$"
  
  parts <- character()
  for (row in 1:2) {
    parts <- c(parts,
               sprintf("\\frac{(%g-%g)^2}{%g}",
                       data$no[row],
                       data$ei[row],
                       data$ei[row]))
  }
  
  formula <- paste0(start, 
                    paste0(parts, collapse="+"),
                    "+ ... ",
                    "=", sprintf("%.2g", calculate_chisq(data)), 
                    end)
  
  formula
}

dffit_formula <- function(data) {
  paste0("$$\n df = ",
         nrow(data),
         "- 1 =",
         nrow(data) - 1,
         "\n $$")
}