library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(scales)
library(huxtable)

format_as_crosstab <- function(df, row_var, col_var, cells = "observed") {
  tab <- df %>% 
    tabyl(.data[[row_var]], .data[[col_var]]) %>% 
    adorn_totals(where = c("row", "col"), name = "összesen") 
  
  if (cells %in% c("row", "col", "all")) {
    tab <- tab %>% 
      adorn_percentages(cells) %>% 
      adorn_pct_formatting()
  }
  
  if (cells %in% c("expected", "res", "stdres")) {
    test <- tab %>% 
      chisq.test(correct = FALSE)
    
    if (cells == "expected") {
      tab <- test$expected %>% 
        adorn_totals(where = c("row", "col"), name = "összesen")
    } else if (cells == "res") {
      tab <- test$observed[-1] - test$expected[-1]
    } else if (cells == "stdres") {
      tab <- test$stdres
    }
  }
  
  if (cells == "res") {
    tab <- tab %>% 
      as_hux(add_rownames = TRUE)
  } else {
    tab <- tab %>%
      as_hux()
  }
  
  if (cells %in% c("observed", "expected")) {
    number_format(tab) <- "%d"
  }
  
  if (cells == "expected") {
    number_format(tab[2:(nrow(tab)-1),2:(ncol(tab)-1)]) <- "%.2f"
  }
  
  captions <- c("Tapasztalati kereszttábla", 
                "Elméleti kereszttábla", 
                "Sorszázalék", 
                "Oszlopszázalék",
                "Cellaszázalék",
                "Reziduálisok",
                "Korrigált standardizált reziduálisok") %>% 
    setNames(c("observed", "expected", "row", "col", "all", "res", "stdres"))
  
  tab <- tab %>% 
    set_bold(1, everywhere) %>%
    set_bold(everywhere, 1) %>% 
    set_markdown_contents(1, 1, "") %>% 
    set_caption(captions[cells]) %>% 
    set_align(2:nrow(tab), 2:ncol(tab), "right")
  
  if (cells %in% c("res", "stdres")) {
    tab <- tab %>% 
      set_bottom_border(c(1, nrow(tab)), everywhere) %>% 
      set_right_border(everywhere, c(1, ncol(tab)))
  } else {
    tab <- tab %>% 
      set_bottom_border(c(1, nrow(tab)-1), everywhere) %>%
      set_right_border(everywhere, c(1, ncol(tab)-1)) 
  }
  
  tab
}

visualize_crosstab <- function(df, x_var, fill_var, x_label, fill_label, y_label="Megoszlás") {
  st_data <- df %>% 
    tabyl(.data[[x_var]], .data[[fill_var]]) %>% 
    pivot_longer(
      cols = -.data[[x_var]],
      names_to = fill_label,
      values_to = "count"
    )
  
  ggplot(
    st_data,
    aes(
      x = .data[[x_var]],
      y = count,
      fill = .data[[fill_label]]
    )
  ) + 
    geom_bar(
      position = "fill", 
      stat = "identity", 
      alpha = 0.7, 
      color = "black" ) + 
    theme_classic() + 
    scale_fill_brewer(palette = "Set1") + 
    scale_y_continuous(labels = percent) + 
    xlab(x_label) + 
    ylab(y_label) 
}

chi2_formula <- function(data, row_col, col_col, per_line = 3) {
  observed <- table(data[[row_col]], data[[col_col]]) 
  
  test <- chisq.test(observed) 
  expected <- test$expected 
  
  parts <- character() 
  
  for (i in seq_len(nrow(observed))) { 
    for (j in seq_len(ncol(observed))) { 
      parts <- c( 
        parts, 
        sprintf( "\\frac{(%g-%0.2f)^2}{%0.2f}", 
                 observed[i, j], 
                 expected[i, j], 
                 expected[i, j] 
        ) 
      ) 
    } 
  }
  
  groups <- split( 
    parts, 
    ceiling(seq_along(parts) / per_line) 
  )
  
  lines <- c( 
    paste0("\\chi^2&=", paste(groups[[1]], collapse = "+")) 
  )
  
  if (length(groups) > 1) { 
    lines <- c( 
      lines, 
      vapply( 
        groups[-1], 
        \(x) paste0("& +", paste(x, collapse = "+")), 
        character(1) 
      ) 
    ) 
  }
  
  lines[length(lines)] <- paste0( 
    lines[length(lines)], 
    "=", 
    sprintf("%.2f", test$statistic) 
  )
  
  paste0( 
    "$$\\begin{aligned}\n", 
    paste(lines, collapse = " \\\\\n"), 
    "\n\\end{aligned}$$" 
  )
}

df_formula <- function(data, row_col, col_col) {
  o <- table(data[[row_col]], data[[col_col]])
  
  paste0("$$\n df=(", dim(o)[1], "-1)\\ast(", dim(o)[2], "-1)=", prod(dim(o)-1), "\n$$")
}
