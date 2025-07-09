create_mic_frequency_tables <- function(data, group_by_var, ab, mo, type, species, guideline) {
  
  if (group_by_var == "Year") {
    data <- data %>% mutate(Year = as.character(lubridate::year(Date)))
  }
  
  if (group_by_var == "Month") {
    data <- data %>% mutate(Month = format(as.Date(Date), "%Y-%m"))
  }
  
  if (type == "Urinary") {
    uti <- TRUE
  } else {
    uti <- FALSE
  }
  
  sources <- unique(data$Source)

  if (type == "Urinary") {
    sources <- sources[str_detect(tolower(sources), "urin|ureth|freecatch|cysto")]
  } else {
    sources <- sources[!str_detect(tolower(sources), "urin|ureth|freecatch|cysto")]
  }
  
  sir_interpretation_history(clean = T)
  
  x <- as.mic(2)
  sir_result <- as.sir(x, mo = mo, ab = ab, guideline = guideline, host = species, uti = uti, SDD = T)
  hold <- sir_interpretation_history(clean = T)
  
  hold <- hold %>% 
    separate(breakpoint_S_R, into = c("bp_s", "bp_r"), sep = "-", fill = "right")
  
  bp_s <- as.numeric(hold$bp_s[1])
  bp_r <- as.numeric(hold$bp_r[1])
  
  MIC_table_long <- data %>%
    filter(Antimicrobial == ab,
           Microorganism == mo,
           Source %in% sources,
           Species == species) %>% 
    group_by(Antimicrobial, !!sym(group_by_var), MIC) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = MIC,
      values_from = n,
      values_fill = 0
    ) %>%
    
    {
      
      df <- .
      mic_sort_value <- function(x) {
        if (grepl("^<=\\d+(\\.\\d+)?$", x)) {
          return(as.numeric(sub("<=*", "", x)) - 0.001)
        } else if (grepl("^<\\d+(\\.\\d+)?$", x)) {
          return(as.numeric(sub("<", "", x)) - 0.002)
        } else if (grepl("^>=\\d+(\\.\\d+)?$", x)) {
          return(as.numeric(sub(">=", "", x)) + 0.002)
        } else if (grepl("^>\\d+(\\.\\d+)?$", x)) {
          return(as.numeric(sub(">", "", x)) + 0.001)
        } else if (grepl("^\\d+(\\.\\d+)?$", x)) {
          return(as.numeric(x))
        } else {
          return(Inf)
        }
        
        
      }
      non_mic_cols <- c("Antimicrobial", group_by_var)
      mic_cols <- setdiff(names(df), non_mic_cols)
      sorted_mic_cols <- mic_cols[order(sapply(mic_cols, mic_sort_value))]
      df %>%
        arrange(!!sym(group_by_var)) %>%
        select(all_of(non_mic_cols), all_of(sorted_mic_cols))
    }
  
  if (nrow(MIC_table_long) < 1) {
    
    return(NULL)
    
  } else {
  
  overall_rows <- MIC_table_long %>%
    group_by(Antimicrobial) %>%
    summarise(
      across(where(is.numeric), ~sum(., na.rm = TRUE)),
      !!sym(group_by_var) := "Overall"
    )
  
  MIC_table_long <- bind_rows(
    overall_rows,
    MIC_table_long
  ) %>%
    group_by(Antimicrobial) %>%
    ungroup()
  
  bp_label <- if(!is.na(bp_s) && !is.na(bp_r)){
    paste0("Breakpoints applied: S ≤ ", bp_s,
           if (!is.na(bp_r)) paste0(", R ≥ ", bp_r) else "",
           " (", guideline, ")")
  } else {
    paste0("No breakpoints available.")
  }
  
  MIC_gt <- MIC_table_long %>%
    select(!!sym(group_by_var), everything(), -Antimicrobial) %>%
    gt() %>%
    tab_header(title = unique(MIC_table_long$Antimicrobial),
               subtitle = md(paste0("**", bp_label, "**")))
  
  mic_cols <- setdiff(names(MIC_table_long), c("Antimicrobial", group_by_var))
  
  get_color <- function(label, bp_s, bp_r) {
    label <- as.character(label)
    
    if (!is.na(bp_s) && !is.na(bp_r)) {
      
      if (grepl("^<\\d+(\\.\\d+)?$", label)) {
        x <- as.numeric(sub("^<", "", label))
        return(if (x <= bp_s) "#44CDC4" else "white")
        
      } else if (grepl("^<=\\d+(\\.\\d+)?$", label)) {
        x <- as.numeric(sub("^<=", "", label))
        return(if (x <= bp_s) "#44CDC4" else "white")
        
      } else if (grepl("^>\\=?\\d+(\\.\\d+)?$", label)) {
        x <- as.numeric(gsub("[^0-9.]", "", label))
        return(if (x >= bp_r) "#D73027" else "white")
        
      } else if (grepl("^\\d+(\\.\\d+)?$", label)) {
        x <- as.numeric(label)
        if (x <= bp_s) {
          return("#44CDC4")
        } else if (x < bp_r) {
          return("#FEE08B")
        } else {
          return("#D73027")
        }
        
      } else {
        return("white")
      }
      
    } else {
      return("white")
    }
  }
  
  
  
  mic_colours <- setNames(
    sapply(mic_cols, get_color, bp_s = as.numeric(bp_s), bp_r = as.numeric(bp_r)),
    mic_cols
  )
  
  for (col in names(mic_colours)) {
    MIC_gt <- MIC_gt %>%
      data_color(
        columns = all_of(col),
        fn = function(x) rep(mic_colours[[col]], length(x))
      )
  }
  
  legend_html <- html(
    paste0(
      '<span style="background:#44CDC4;padding:0.15em 0.7em;margin:0 0.7em;border-radius:3px;"> </span>S (≤ ', bp_s, ')&nbsp;&nbsp;',
      '<span style="background:#FEE08B;padding:0.15em 0.7em;margin:0 0.7em;border-radius:3px;"> </span>I (', bp_s, '-', bp_r, ')&nbsp;&nbsp;',
      '<span style="background:#D73027;padding:0.15em 0.7em;margin:0 0.7em;border-radius:3px;"> </span>R (≥ ', bp_r, ')',
      '<span style="background:#FFF;border: solid 2px #777;padding:0.12em 0.65em;margin:0 0.7em;border-radius:3px;"> </span>NI'
    )
  )
  
  MIC_gt <- MIC_gt %>%
    tab_source_note(legend_html)
  
  return(MIC_gt)
  }
  
}
