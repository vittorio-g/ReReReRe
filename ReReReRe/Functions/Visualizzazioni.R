#### Analysis and Visualization of Multiverse Results ####
library(ggplot2)
library(tidyr)
library(patchwork)
library(caret)

# Funzione per calcolare metriche di performance (with NA handling)
calculate_metrics <- function(flagged, careless_labels) {
  
  
  # Safety check: ensure inputs exist and are not NULL
  if (is.null(flagged) || is.null(careless_labels)) {
    return(tibble(
      accuracy = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_,
      f1 = NA_real_,
      false_positive_rate = NA_real_,
      false_negative_rate = NA_real_,
      n_flagged = NA_integer_,
      n_careless = NA_integer_,
      n_total = NA_integer_,
      n_na = NA_integer_,
      pct_na = NA_real_
    ))
  }
  
  # Extract the careless vector
  actual <- careless_labels$careless
  
  # Check lengths match
  if (length(flagged) != length(actual)) {
    warning(paste0(
      "Length mismatch: flagged (", length(flagged), 
      ") vs careless_labels (", length(actual), ")"
    ))
    return(tibble(
      accuracy = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_,
      f1 = NA_real_,
      false_positive_rate = NA_real_,
      false_negative_rate = NA_real_,
      n_flagged = NA_integer_,
      n_careless = NA_integer_,
      n_total = NA_integer_,
      n_na = NA_integer_,
      pct_na = NA_real_
    ))
  }
  
  # Count NAs before removing them
  n_total_original <- length(flagged)
  n_na <- sum(is.na(flagged))
  pct_na <- n_na / n_total_original
  
  # Remove cases where flagged is NA
  valid_idx <- !is.na(flagged)
  flagged_clean <- flagged[valid_idx]
  actual_clean <- actual[valid_idx]
  
  # Check if we have enough data after NA removal
  if (length(flagged_clean) == 0) {
    return(tibble(
      accuracy = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_,
      f1 = NA_real_,
      false_positive_rate = NA_real_,
      false_negative_rate = NA_real_,
      n_flagged = 0L,
      n_careless = sum(careless_labels$careless),
      n_total = 0L,
      n_na = as.integer(n_na),
      pct_na = pct_na
    ))
  }
  
  # Create confusion matrix with clean data
  confusion <- table(
    Predicted = flagged_clean,
    Actual = actual_clean
  )
  
  # Gestione caso in cui non ci siano abbastanza categorie
  # (e.g., all predictions are TRUE or all are FALSE)
  if (nrow(confusion) < 2 || ncol(confusion) < 2) {
    
    # Try to extract what we can
    n_flagged <- sum(flagged_clean)
    n_careless <- sum(actual_clean)
    
    return(tibble(
      accuracy = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_,
      f1 = NA_real_,
      false_positive_rate = NA_real_,
      false_negative_rate = NA_real_,
      n_flagged = as.integer(n_flagged),
      n_careless = as.integer(n_careless),
      n_total = as.integer(length(flagged_clean)),
      n_na = as.integer(n_na),
      pct_na = pct_na
    ))
  }
  
  # Extract confusion matrix values
  # Note: table() orders levels, so FALSE comes before TRUE
  # confusion[1,1] = Predicted FALSE, Actual FALSE = TN
  # confusion[1,2] = Predicted FALSE, Actual TRUE = FN
  # confusion[2,1] = Predicted TRUE, Actual FALSE = FP
  # confusion[2,2] = Predicted TRUE, Actual TRUE = TP
  
  TP <- confusion[2, 2]  # True Positives
  TN <- confusion[1, 1]  # True Negatives
  FP <- confusion[2, 1]  # False Positives
  FN <- confusion[1, 2]  # False Negatives
  
  # Calculate metrics with division-by-zero protection
  accuracy <- (TP + TN) / sum(confusion)
  
  sensitivity <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
  specificity <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
  precision <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
  
  f1 <- if (!is.na(precision) && !is.na(sensitivity) && (precision + sensitivity) > 0) {
    2 * (precision * sensitivity) / (precision + sensitivity)
  } else {
    NA_real_
  }
  
  fpr <- if ((FP + TN) > 0) FP / (FP + TN) else NA_real_
  fnr <- if ((TP + FN) > 0) FN / (TP + FN) else NA_real_
  
  tibble(
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    f1 = f1,
    false_positive_rate = fpr,
    false_negative_rate = fnr,
    n_flagged = as.integer(sum(flagged_clean)),
    n_careless = as.integer(sum(actual_clean)),
    n_total = as.integer(length(flagged_clean)),
    n_na = as.integer(n_na),
    pct_na = pct_na
  )
}

# Calcola metriche per tutti gli universi
results <- a %>%
  mutate(
    metrics = map2(flagged, carelessLabels, calculate_metrics),
    pct_careless = as.numeric(pct_careless),
    corThreshold = as.numeric(corThreshold),
    cutOff = as.numeric(cutOff)
  ) %>%
  unnest(metrics)

# Check for universes with high NA rates
high_na_universes <- results %>%
  filter(pct_na > 0.1) %>%
  nrow()

if (high_na_universes > 0) {
  cat("\n⚠️ Warning:", high_na_universes, "universes have >10% NA flagged values\n")
  cat("   This may indicate problematic parameter combinations.\n\n")
}

# Summary of NA rates
cat("\n=== NA SUMMARY ===\n")
cat("Mean % NA across universes:", round(mean(results$pct_na, na.rm = TRUE) * 100, 2), "%\n")
cat("Max % NA:", round(max(results$pct_na, na.rm = TRUE) * 100, 2), "%\n")
cat("Universes with any NAs:", sum(results$n_na > 0, na.rm = TRUE), "/", nrow(results), "\n\n")

# Rest of your visualization code...
# 1. HEATMAP: F1-score in funzione dei parametri
p1 <- ggplot(results, aes(x = corThreshold, y = cutOff, fill = f1)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  facet_wrap(~paste0("Careless: ", pct_careless*100, "%")) +
  labs(
    title = "F1-Score across Parameter Space",
    x = "Correlation Threshold",
    y = "Cut-off",
    fill = "F1-Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

# 2. SENSITIVITY vs SPECIFICITY trade-off
p2 <- ggplot(results, aes(x = 1 - specificity, y = sensitivity, 
                          color = corThreshold, shape = factor(pct_careless))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_viridis_c(option = "viridis") +
  labs(
    title = "ROC-like: Sensitivity vs Specificity Trade-off",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Cor Threshold",
    shape = "% Careless"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# 3. MULTIPLE METRICS COMPARISON
metrics_long <- results %>%
  select(.universe, pct_careless, corThreshold, cutOff, 
         accuracy, sensitivity, specificity, precision, f1) %>%
  pivot_longer(cols = c(accuracy, sensitivity, specificity, precision, f1),
               names_to = "metric",
               values_to = "value")

p3 <- ggplot(metrics_long, aes(x = corThreshold, y = value, color = metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(pct_careless ~ cutOff, 
             labeller = label_both) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "All Metrics Across Parameter Combinations",
    x = "Correlation Threshold",
    y = "Metric Value",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(size = 8)
  )

# 4. NEW: Visualize NA patterns
p4 <- ggplot(results, aes(x = corThreshold, y = cutOff, fill = pct_na * 100)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", na.value = "grey50") +
  facet_wrap(~paste0("Careless: ", pct_careless*100, "%")) +
  labs(
    title = "Percentage of NA Flagged Values",
    subtitle = "High values indicate problematic parameter combinations",
    x = "Correlation Threshold",
    y = "Cut-off",
    fill = "% NA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

# 5. IDENTIFICAZIONE BEST PARAMETERS (excluding high NA universes)
results_valid <- results %>%
  filter(pct_na < 0.2)  # Exclude universes with >20% NA

if (nrow(results_valid) == 0) {
  cat("\n⚠️ All universes have >20% NA. Using all results.\n")
  results_valid <- results
}

best_f1 <- results_valid %>%
  filter(!is.na(f1)) %>%
  filter(f1 == max(f1, na.rm = TRUE)) %>%
  slice(1)

best_balanced <- results_valid %>%
  filter(!is.na(sensitivity) & !is.na(specificity)) %>%
  mutate(
    balance_score = abs(sensitivity - specificity)
  ) %>%
  filter(balance_score == min(balance_score, na.rm = TRUE)) %>%
  slice(1)

if (nrow(best_f1) > 0) {
  cat("\n=== BEST PARAMETERS (F1-Score) ===\n")
  cat("pct_careless:", best_f1$pct_careless, "\n")
  cat("corThreshold:", best_f1$corThreshold, "\n")
  cat("cutOff:", best_f1$cutOff, "\n")
  cat("F1-Score:", round(best_f1$f1, 3), "\n")
  cat("Accuracy:", round(best_f1$accuracy, 3), "\n")
  cat("Sensitivity:", round(best_f1$sensitivity, 3), "\n")
  cat("Specificity:", round(best_f1$specificity, 3), "\n")
  cat("% NA:", round(best_f1$pct_na * 100, 2), "%\n\n")
} else {
  cat("\n⚠️ No valid F1 scores found.\n\n")
}

if (nrow(best_balanced) > 0) {
  cat("=== BEST BALANCED (Sensitivity ≈ Specificity) ===\n")
  cat("pct_careless:", best_balanced$pct_careless, "\n")
  cat("corThreshold:", best_balanced$corThreshold, "\n")
  cat("cutOff:", best_balanced$cutOff, "\n")
  cat("F1-Score:", round(best_balanced$f1, 3), "\n")
  cat("Sensitivity:", round(best_balanced$sensitivity, 3), "\n")
  cat("Specificity:", round(best_balanced$specificity, 3), "\n")
  cat("% NA:", round(best_balanced$pct_na * 100, 2), "%\n")
} else {
  cat("⚠️ No valid balanced parameters found.\n")
}

# 6. VISUALIZZAZIONE COMPLESSIVA
print(p1)
print(p2)
print(p3)
print(p4)

# SALVA GRAFICI
ggsave("multiverse_heatmap.png", p1, width = 12, height = 8, dpi = 300)
ggsave("multiverse_roc.png", p2, width = 10, height = 7, dpi = 300)
ggsave("multiverse_metrics.png", p3, width = 14, height = 10, dpi = 300)
ggsave("multiverse_na_patterns.png", p4, width = 12, height = 8, dpi = 300)



#### Detailed Analysis by Careless Type ####

# Funzione per calcolare metriche per tipo
calculate_metrics_by_type <- function(flagged, careless_labels) {
  careless_labels %>%
    mutate(flagged = flagged) %>%
    filter(careless) %>%  # Solo casi careless
    group_by(pattern) %>%
    summarise(
      n_cases = n(),
      n_detected = sum(flagged),
      detection_rate = mean(flagged),
      .groups = "drop"
    )
}

# Calcola per tutti gli universi
results_by_type <- a %>%
  mutate(
    pct_careless = as.numeric(pct_careless),
    corThreshold = as.numeric(corThreshold),
    cutOff = as.numeric(cutOff),
    type_metrics = map2(flagged, carelessLabels, calculate_metrics_by_type)
  ) %>%
  unnest(type_metrics)

# Visualizzazione detection rate per tipo
p4 <- ggplot(results_by_type, aes(x = corThreshold, y = detection_rate, 
                                  color = pattern, linetype = factor(cutOff))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~paste0("Careless: ", pct_careless*100, "%")) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Detection Rate by Careless Pattern Type",
    x = "Correlation Threshold",
    y = "Detection Rate",
    color = "Pattern Type",
    linetype = "Cut-off"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

print(p4)
ggsave("multiverse_by_type.png", p4, width = 12, height = 8, dpi = 300)

# Tabella riassuntiva per tipo
summary_by_type <- results_by_type %>%
  group_by(pattern) %>%
  summarise(
    mean_detection = mean(detection_rate, na.rm = TRUE),
    sd_detection = sd(detection_rate, na.rm = TRUE),
    min_detection = min(detection_rate, na.rm = TRUE),
    max_detection = max(detection_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_detection))

cat("\n=== DETECTION RATE BY PATTERN TYPE ===\n")
print(summary_by_type, n = Inf)

# Trova parametri migliori per ogni tipo
best_by_type <- results_by_type %>%
  group_by(pattern) %>%
  filter(detection_rate == max(detection_rate, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

cat("\n=== BEST PARAMETERS FOR EACH PATTERN ===\n")
print(best_by_type %>% 
        select(pattern, pct_careless, corThreshold, cutOff, detection_rate), 
      n = Inf)


#### False Positive/Negative Analysis ####

# Visualizzazione FP vs FN rate
p5 <- ggplot(results, aes(x = false_positive_rate, y = false_negative_rate)) +
  geom_point(aes(color = f1, size = pct_careless), alpha = 0.7) +
  geom_abline(slope = -1, intercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title = "False Positive vs False Negative Rates",
    x = "False Positive Rate",
    y = "False Negative Rate",
    color = "F1-Score",
    size = "% Careless"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p5)
ggsave("multiverse_fp_fn.png", p5, width = 10, height = 7, dpi = 300)

# Trova parametri con miglior bilanciamento FP/FN
best_fp_fn_balance <- results %>%
  mutate(
    fp_fn_diff = abs(false_positive_rate - false_negative_rate)
  ) %>%
  filter(fp_fn_diff == min(fp_fn_diff, na.rm = TRUE)) %>%
  slice(1)

cat("\n=== BEST FP/FN BALANCE ===\n")
cat("pct_careless:", best_fp_fn_balance$pct_careless, "\n")
cat("corThreshold:", best_fp_fn_balance$corThreshold, "\n")
cat("cutOff:", best_fp_fn_balance$cutOff, "\n")
cat("FP Rate:", round(best_fp_fn_balance$false_positive_rate, 3), "\n")
cat("FN Rate:", round(best_fp_fn_balance$false_negative_rate, 3), "\n")
cat("F1-Score:", round(best_fp_fn_balance$f1, 3), "\n")

# Esporta risultati completi
write.csv(results, "multiverse_results_complete.csv", row.names = FALSE)
write.csv(results_by_type, "multiverse_results_by_type.csv", row.names = FALSE)
