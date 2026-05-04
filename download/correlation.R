#Credit to 
#https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/#correlation_matrix

correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = "",
                               leading_zero = TRUE){
  
  # check arguments
  stopifnot(
    "digits must be numeric" = is.numeric(digits),
    "digits must be non-negative" = digits >= 0,
    "use must be one of 'all', 'upper', 'lower'" = use %in% c("all", "upper", "lower"),
    "replace_diagonal must be logical" = is.logical(replace_diagonal),
    "show_significance must be logical" = is.logical(show_significance),
    "replacement must be character" = is.character(replacement),
    "leading_zero must be logical" = is.logical(leading_zero)
  )

  # we need the Hmisc package for this
   if (!requireNamespace("Hmisc", quietly = TRUE)) {
   stop("Package 'Hmisc' is required. Install it with install.packages('Hmisc').")
  }
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
  message("Dropping non-numeric/-boolean column(s): ", 
          paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ", "))
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  cor_result <- Hmisc::rcorr(x, type = type)
  R <- cor_result$r # Matrix of correlation coefficients
  p <- cor_result$P # Matrix of p-values
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)

  # APA style: strip leading zero from correlations (which are bounded by ±1)
  if (!leading_zero) {
  Rformatted = sub("^(-?)0", "\\1", Rformatted)
  }
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(!is.na(R) & R < 0) > 0) {
    Rformatted = ifelse(!is.na(R) & R >= 0, paste0(" ", Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
    Rformatted = paste0(Rformatted, stars)
  }
  
  # make all character strings equally long
  max_length = max(nchar(Rformatted))
  Rformatted = vapply(Rformatted, function(x) {
    current_length = nchar(x)
    difference = max_length - current_length
    return(paste0(x, paste(rep(" ", difference), collapse = ''), sep = ''))
  }, FUN.VALUE = character(1))
  
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(Rnew) <- colnames(x)
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}




cor2flex <- function(x, var_labels = NULL, extra_cols = NULL) {

  # Check dependency
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required. Install it with install.packages('flextable').")
  }
  
  flextable::set_flextable_defaults(
    border.color = "black",
    border.width = 0.5,
    line_spacing = 1,
    padding = 2,
    table.layout = "autofit",
    font.family = "Times New Roman"
  )
  
  out <- as.data.frame(x)
  n_rows <- nrow(out)
  
  # Detect extra columns automatically if not specified.
  # Heuristic: a column is "extra" if its name isn't already a row name
  # (i.e., it's not one of the correlation variables).
  if (is.null(extra_cols)) {
    extra_cols <- setdiff(names(out), rownames(out))
  }
  n_extra <- length(extra_cols)
  n_cor <- ncol(out) - n_extra
  
  # Use provided labels or fall back to row names
  if (is.null(var_labels)) {
    var_labels <- rownames(out)
  } else {
    stopifnot(
      "var_labels must match the number of rows" = 
        length(var_labels) == n_rows
    )
  }
  
  # Prepend Variable column
  out <- cbind(Variable = var_labels, out)
  rownames(out) <- NULL
  
  # Header: "Variable", any extra column names as-is, then 1, 2, 3, ... for correlations
  names(out) <- c("Variable", extra_cols, as.character(seq_len(n_cor)))
  
  ft <- flextable::flextable(out)
  ft <- flextable::autofit(ft)
  ft <- flextable::padding(ft, padding.top = 0, padding.bottom = 0)
  
  return(ft)
}

