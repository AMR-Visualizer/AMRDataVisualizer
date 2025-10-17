create_mic_frequency_tables <- function(
    data,
    group_by_var,
    ab,
    mo,
    type,
    species,
    guideline,
    s_bp = NULL,
    r_bp = NULL,
    use_single_bp_as_both = TRUE,
    reference_data = AMR::clinical_breakpoints
) {

  if (group_by_var == "Year") {
    data <- dplyr::mutate(data, Year = as.character(lubridate::year(.data$Date)))
  } else if (group_by_var == "Month") {
    data <- dplyr::mutate(data, Month = format(as.Date(.data$Date), "%Y-%m"))
  }
  
  uti <- identical(type, "Urinary")

  sources <- unique(data$Source)
  sources <- sources[!is.na(sources)]
  rx <- "urin|urine|ureth|free.?catch|void|cysto"
  if (uti) {
    sources <- sources[stringr::str_detect(tolower(sources), rx)]
  } else {
    sources <- sources[!stringr::str_detect(tolower(sources), rx)]
  }

  bp_s <- suppressWarnings(as.numeric(s_bp))
  bp_r <- suppressWarnings(as.numeric(r_bp))
  
  if ((is.na(bp_s) || is.na(bp_r)) &&
      !is.null(guideline) && length(guideline) == 1 && !is.na(guideline) && nzchar(guideline)) {

    AMR::sir_interpretation_history(clean = TRUE)
    x <- AMR::as.mic(2)
    try({
      invisible(AMR::as.sir(
        x,
        mo = mo,
        ab = ab,
        guideline = guideline,
        reference_data = reference_data,
        host = species,
        uti = uti,
        SDD = TRUE
      ))
      hold <- AMR::sir_interpretation_history(clean = FALSE)
      hold <- tidyr::separate(hold, breakpoint_S_R, into = c("bp_s", "bp_r"),
                              sep = "-", fill = "right")
      bp_s <- if (is.na(bp_s)) suppressWarnings(as.numeric(hold$bp_s[1])) else bp_s
      bp_r <- if (is.na(bp_r)) suppressWarnings(as.numeric(hold$bp_r[1])) else bp_r
    }, silent = TRUE)
  }
  
  single_bp_applied <- FALSE
  if (use_single_bp_as_both && xor(is.na(bp_s), is.na(bp_r))) {
    if (!is.na(bp_s)) bp_r <- bp_s else bp_s <- bp_r
    single_bp_applied <- TRUE
  }
  
  # --- build frequency table (works even if no BPs)
  MIC_table_long <- data %>%
    dplyr::filter(
      .data$Antimicrobial == ab,
      .data$Microorganism == mo,
      .data$Source %in% sources,
      .data$Species == species
    ) %>%
    dplyr::group_by(.data$Antimicrobial, !!rlang::sym(group_by_var), .data$MIC) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$MIC, values_from = .data$n, values_fill = 0) %>%
    { # sort MIC columns numerically with handling for <, <=, >, >=
      df <- .
      mic_sort_value <- function(x) {
        if (grepl("^<=\\d+(\\.\\d+)?$", x)) return(as.numeric(sub("^<=", "", x)) - 0.001)
        if (grepl("^<\\d+(\\.\\d+)?$", x))  return(as.numeric(sub("^<",  "", x)) - 0.002)
        if (grepl("^>=\\d+(\\.\\d+)?$", x)) return(as.numeric(sub("^>=", "", x)) + 0.002)
        if (grepl("^>\\d+(\\.\\d+)?$", x))  return(as.numeric(sub("^>",  "", x)) + 0.001)
        if (grepl("^\\d+(\\.\\d+)?$", x))   return(as.numeric(x))
        Inf
      }
      non_mic_cols <- c("Antimicrobial", group_by_var)
      mic_cols <- setdiff(names(df), non_mic_cols)
      sorted_mic_cols <- mic_cols[order(sapply(mic_cols, mic_sort_value))]
      df %>% dplyr::arrange(!!rlang::sym(group_by_var)) %>%
        dplyr::select(dplyr::all_of(non_mic_cols), dplyr::all_of(sorted_mic_cols))
    }
  
  if (nrow(MIC_table_long) < 1) return(NULL)
  
  # overall row
  overall_rows <- MIC_table_long %>%
    dplyr::group_by(.data$Antimicrobial) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), ~sum(., na.rm = TRUE)),
                     !!rlang::sym(group_by_var) := "Overall")
  
  MIC_table_long <- dplyr::bind_rows(overall_rows, MIC_table_long) %>%
    dplyr::ungroup()
  
  # --- subtitle
  bp_label <- getBpLabel(bp_s, bp_r, guideline)
  
  MIC_gt <- MIC_table_long %>%
    dplyr::select(!!rlang::sym(group_by_var), dplyr::everything(), -Antimicrobial) %>%
    gt::gt() %>%
    gt::tab_header(
      title = unique(MIC_table_long$Antimicrobial),
      subtitle = gt::md(paste0("**", bp_label, "**"))
    )
  
  # --- conditional coloring (only if both BPs present)
  if (!is.na(bp_s) && !is.na(bp_r)) {
    mic_cols <- setdiff(names(MIC_table_long), c("Antimicrobial", group_by_var))
    
    get_color <- function(label, bp_s, bp_r) {
      label <- as.character(label)
      if (grepl("^<\\d+(\\.\\d+)?$", label)) {
        x <- as.numeric(sub("^<", "", label));  if (x <= bp_s) "#44CDC4" else "white"
      } else if (grepl("^<=\\d+(\\.\\d+)?$", label)) {
        x <- as.numeric(sub("^<=", "", label)); if (x <= bp_s) "#44CDC4" else "white"
      } else if (grepl("^>\\=?\\d+(\\.\\d+)?$", label)) {
        x <- as.numeric(gsub("[^0-9.]", "", label)); if (x >= bp_r) "#D73027" else "white"
      } else if (grepl("^\\d+(\\.\\d+)?$", label)) {
        x <- as.numeric(label)
        if (x <= bp_s) "#44CDC4" else if (x < bp_r) "#FEE08B" else "#D73027"
      } else "white"
    }
    
    mic_colours <- setNames(
      sapply(mic_cols, get_color, bp_s = as.numeric(bp_s), bp_r = as.numeric(bp_r)),
      mic_cols
    )
    
    for (col in names(mic_colours)) {
      MIC_gt <- MIC_gt %>%
        gt::data_color(columns = dplyr::all_of(col),
                       fn = function(x) rep(mic_colours[[col]], length(x)))
    }
    
    legend_html <- gt::html(
      paste0(
        '<span style="background:#44CDC4;padding:0.15em 0.7em;margin:0 0.7em;border-radius:3px;"> </span>S (≤ ', bp_s, ')&nbsp;&nbsp;',
        '<span style="background:#FEE08B;padding:0.15em 0.7em;margin:0 0.7em;border-radius:3px;"> </span>I (', bp_s, '-', bp_r, ')&nbsp;&nbsp;',
        '<span style="background:#D73027;padding:0.15em 0.7em;margin:0 0.7em;border-radius:3px;"> </span>R (≥ ', bp_r, ')',
        '<span style="background:#FFF;border: solid 2px #777;padding:0.12em 0.65em;margin:0 0.7em;border-radius:3px;"> </span>NI'
      )
    )
    MIC_gt <- MIC_gt %>%
      tab_source_note(legend_html)
  }
  
  MIC_gt
}
