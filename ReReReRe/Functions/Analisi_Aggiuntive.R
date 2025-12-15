#### 1. ANALISI PIÙ DETTAGLIATA DEL TRADE-OFF ####

# Calcola costo pesato (se FN costa più di FP)
results <- results %>%
  mutate(
    # Esempio: FN costa 3x più di FP
    weighted_cost = false_positive_rate + 3 * false_negative_rate,
    # Youden's Index (J statistic)
    youden_index = sensitivity + specificity - 1
  )

# Trova parametri ottimali con Youden's Index
best_youden <- results %>%
  filter(youden_index == max(youden_index, na.rm = TRUE))

cat("\n=== BEST BY YOUDEN'S INDEX ===\n")
print(best_youden %>% 
        select(pct_careless, corThreshold, cutOff, 
               sensitivity, specificity, youden_index, f1))

# Visualizza Youden's Index
p_youden <- ggplot(results, aes(x = corThreshold, y = cutOff, fill = youden_index)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  facet_wrap(~paste0("Careless: ", pct_careless*100, "%")) +
  labs(title = "Youden's Index (Sensitivity + Specificity - 1)",
       fill = "Youden's J") +
  theme_minimal()

print(p_youden)


#### 2. FOCUS SUL PROBLEMA DEI FALSI NEGATIVI ####

# Analizza chi sono i falsi negativi
analyze_false_negatives <- function(flagged, careless_labels) {
  careless_labels %>%
    mutate(flagged = flagged) %>%
    filter(careless & !flagged) %>%  # Solo FN
    count(pattern, careless_pct) %>%
    mutate(
      total_fn = sum(n),
      prop = n / total_fn
    )
}

fn_analysis <- a %>%
  mutate(
    pct_careless = as.numeric(pct_careless),
    corThreshold = as.numeric(corThreshold),
    cutOff = as.numeric(cutOff),
    fn_breakdown = map2(flagged, carelessLabels, analyze_false_negatives)
  ) %>%
  unnest(fn_breakdown)

# Visualizza quale tipo di careless viene perso di più
p_fn <- ggplot(fn_analysis, aes(x = corThreshold, y = prop, 
                                color = pattern, linetype = factor(round(careless_pct, .1)))) +
  geom_line(linewidth = 1) +
  facet_grid(pct_careless ~ cutOff, labeller = label_both) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "False Negative Breakdown by Pattern Type",
    subtitle = "Which careless types are being missed?",
    y = "Proportion of False Negatives",
    color = "Pattern",
    linetype = "Careless Level"
  ) +
  theme_minimal()

print(p_fn)

#### 3. SENSITIVITY ANALYSIS SU LIVELLI DI CORRUZIONE ####

# Analizza detection rate per livello di corruzione
detection_by_level <- function(flagged, careless_labels) {
  careless_labels %>%
    mutate(flagged = flagged) %>%
    filter(careless) %>%
    group_by(careless_pct) %>%
    summarise(
      n = n(),
      detected = sum(flagged),
      detection_rate = mean(flagged),
      .groups = "drop"
    )
}

level_analysis <- a %>%
  mutate(
    pct_careless = as.numeric(pct_careless),
    corThreshold = as.numeric(corThreshold),
    cutOff = as.numeric(cutOff),
    level_metrics = map2(flagged, carelessLabels, detection_by_level)
  ) %>%
  unnest(level_metrics)

# Trova il "punto di rottura" - quale livello di corruzione non viene più rilevato
p_levels <- ggplot(level_analysis, 
                   aes(x = careless_pct, y = detection_rate, 
                       color = corThreshold, group = interaction(corThreshold, cutOff))) +
  geom_line(alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "loess", color = "black", linewidth = 1.5) +
  scale_color_viridis_c() +
  facet_wrap(~paste0("Careless: ", pct_careless*100, "%")) +
  labs(
    title = "Detection Rate by Corruption Level",
    subtitle = "Does the algorithm struggle with mild corruption?",
    x = "Corruption Level",
    y = "Detection Rate",
    color = "Cor Threshold"
  ) +
  theme_minimal()

print(p_levels)

#### 4. INTERACTION EFFECTS ####

# Modello per capire effetti di interazione
library(mgcv)

model_data <- results %>%
  filter(!is.na(f1))

# GAM per catturare non-linearità
gam_model <- lm(f1 ~ corThreshold*cutOff*pct_careless,
                 data = model_data)

summary(gam_model)

# Visualizza superfici di risposta
library(gratia)
draw(gam_model, residuals = TRUE)

# Alternative: effetti marginali
library(ggeffects)
plot(ggpredict(gam_model, terms = c("corThreshold [all]", "cutOff [all]", "pct_careless")))


#### 5. STABILITY ANALYSIS ####

# Quanto sono stabili i risultati tra universi simili?
results <- results %>%
  arrange(pct_careless, corThreshold, cutOff) %>%
  group_by(pct_careless) %>%
  mutate(
    f1_change = f1 - lag(f1),
    f1_volatility = abs(f1_change)
  ) %>%
  ungroup()

p_stability <- ggplot(results, aes(x = corThreshold, y = f1_volatility, color = factor(cutOff))) +
  geom_line() +
  facet_wrap(~paste0("Careless: ", pct_careless*100, "%")) +
  labs(
    title = "Parameter Stability: F1 Volatility",
    subtitle = "How sensitive is performance to small parameter changes?",
    y = "Absolute F1 Change",
    color = "Cut-off"
  ) +
  theme_minimal()

print(p_stability)

#### 6. SUMMARY TABLE PER PAPER ####

summary_table <- results %>%
  group_by(pct_careless) %>%
  summarise(
    n_universes = n(),
    
    # F1 stats
    mean_f1 = mean(f1, na.rm = TRUE),
    sd_f1 = sd(f1, na.rm = TRUE),
    max_f1 = max(f1, na.rm = TRUE),
    
    # Sensitivity stats
    mean_sens = mean(sensitivity, na.rm = TRUE),
    sd_sens = sd(sensitivity, na.rm = TRUE),
    
    # Specificity stats
    mean_spec = mean(specificity, na.rm = TRUE),
    sd_spec = sd(specificity, na.rm = TRUE),
    
    # Best parameters
    best_corThreshold = corThreshold[which.max(f1)],
    best_cutOff = cutOff[which.max(f1)],
    
    .groups = "drop"
  )

library(knitr)
library(kableExtra)

summary_table %>%
  kable(digits = 3, format = "html", 
        caption = "Algorithm Performance Summary Across Multiverse") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
