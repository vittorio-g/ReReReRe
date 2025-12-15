ReReReRe <- function(data,
                     corThreshold = 0.70,
                     cutOff = 0.95,
                     iterations = 100,
                     progress = FALSE) {
  
  
  # Keep only numeric values
  
  data <- data %>%
    select(where(is.numeric))
  
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # Early exit if data is too small
  if (n_rows < 2 || n_cols < 2) {
    warning("Data has fewer than 2 rows or 2 columns. Returning NA.")
    return(data.frame(
      result = rep(NA_real_, n_rows),
      indCors = rep(NA_real_, n_rows),
      flagged = rep(NA, n_rows)
    ))
  }
  
  # Computing correlations (absolute values)
  corMat <- cor(data, use = "pairwise.complete.obs") %>% abs()
  
  # Substituting upper triangle (diagonal included) with NAs
  corMat[upper.tri(corMat, diag = TRUE)] <- NA
  
  # Getting the row and col names with values higher than threshold
  coupples <- which(corMat > corThreshold, arr.ind = TRUE)
  
  # Guard: No pairs exceed threshold
  if (nrow(coupples) == 0) {
    warning(paste0(
      "No item pairs exceed corThreshold (", corThreshold, "). ",
      "Consider lowering the threshold. Returning NA for all."
    ))
    return(data.frame(
      result = rep(NA_real_, n_rows),
      indCors = rep(NA_real_, n_rows),
      flagged = rep(NA, n_rows)
    ))
  }
  
  # Progress update
  if (progress) {
    cat("\n Getting the correlation for each individual \n")
    cat(" Using", nrow(coupples), "item pairs above threshold", corThreshold, "\n")
  }
  
  # Convert to matrix (faster for subsetting)
  data_mat <- as.matrix(data)
  
  # Extract the values for the "left" and "right" side of the couples for all people
  left_side_values  <- data_mat[, coupples[, 1], drop = FALSE]
  right_side_values <- data_mat[, coupples[, 2], drop = FALSE]
  
  # Helper function for safe correlation calculation
  safe_cor <- function(x, y, use_method = "complete.obs") {
    # Find complete pairs
    complete_idx <- complete.cases(x, y)
    n_complete <- sum(complete_idx)
    
    # Need at least 2 complete pairs for correlation
    if (n_complete < 2) {
      return(NA_real_)
    }
    
    x_complete <- x[complete_idx]
    y_complete <- y[complete_idx]
    
    # Check for zero variance
    sd_x <- sd(x_complete, na.rm = TRUE)
    sd_y <- sd(y_complete, na.rm = TRUE)
    
    if (is.na(sd_x) || is.na(sd_y) || sd_x == 0 || sd_y == 0) {
      return(NA_real_)
    }
    
    # Safe to compute correlation
    cor(x_complete, y_complete)
  }
  
  # Compute correlation for each person (row) across these pairs
  rowCors <- vapply(seq_len(n_rows), function(i) {
    abs(safe_cor(left_side_values[i, ], right_side_values[i, ]))
  }, FUN.VALUE = numeric(1))
  
  # Small function to transform the numbers to the smallest even number
  make_even <- function(x) {
    ifelse(x %% 2 == 0, x, x - 1)
  }
  
  # Number of columns to use (must be even)
  n_cols_even <- make_even(n_cols)
  
  # Guard: Need at least 2 columns for split-half
  if (n_cols_even < 2) {
    warning("Not enough columns for split-half correlation. Returning NA.")
    return(data.frame(
      result = rep(NA_real_, n_rows),
      indCors = rowCors,
      flagged = rep(NA, n_rows)
    ))
  }
  
  # Preallocate matrix
  all_RIC <- matrix(nrow = n_rows, ncol = iterations)
  
  # Progress update
  if (progress) {
    cat("\n Computing random correlations \n")
    pb <- txtProgressBar(min = 0, max = 1, style = 3)
  }
  
  # Computing random correlations
  for (i in 1:iterations) {
    
    # Giving the columns a random order
    col_random_order <- sample(n_cols, n_cols_even)
    
    # Splitting the columns in two halves
    n_half <- n_cols_even / 2
    indexes_half1 <- col_random_order[1:n_half]
    indexes_half2 <- col_random_order[(n_half + 1):n_cols_even]
    
    # Extract submatrices
    half1 <- data_mat[, indexes_half1, drop = FALSE]
    half2 <- data_mat[, indexes_half2, drop = FALSE]
    
    # Row-wise correlation with safety checks
    RIC <- vapply(seq_len(n_rows), function(row) {
      safe_cor(half1[row, ], half2[row, ], use_method = "pairwise.complete.obs")
    }, FUN.VALUE = numeric(1))
    
    # Take absolute value, preserving NAs
    all_RIC[, i] <- abs(RIC)
    
    if (progress) {
      setTxtProgressBar(pb, i / iterations)
    }
  }
  
  if (progress) {
    close(pb)
  }
  
  # Compute the comparison index
  # For each person: proportion of random correlations that are LESS than their observed correlation
  # Handle NAs appropriately
  corComparedIndex <- vapply(seq_len(n_rows), function(i) {
    if (is.na(rowCors[i])) {
      return(NA_real_)
    }
    
    random_cors <- all_RIC[i, ]
    valid_random <- random_cors[!is.na(random_cors)]
    
    if (length(valid_random) == 0) {
      return(NA_real_)
    }
    
    mean(valid_random < rowCors[i])
  }, FUN.VALUE = numeric(1))
  
  if (progress) {
    cat("\n FINISHED \n")
    cat(" The index is based on", nrow(coupples), "pairs of items \n")
    
    # Report any problematic cases
    n_na_rowCors <- sum(is.na(rowCors))
    if (n_na_rowCors > 0) {
      cat(" Warning:", n_na_rowCors, "respondents had NA correlations",
          "(constant responses or missing data)\n")
    }
  }
  
  return(data.frame(
    result = corComparedIndex,
    indCors = rowCors,
    flagged = corComparedIndex <= cutOff
  ))
}