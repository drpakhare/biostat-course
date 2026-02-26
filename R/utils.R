# =============================================================
# Biostatistics for Clinicians — Shared Utility Functions
# =============================================================

library(tidyverse)

# --- Consistent Theme ---
theme_clinical <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(colour = "grey40", size = base_size),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(15, 10, 10, 10)
    )
}

# --- Color Palettes ---
clinical_colours <- list(
  primary = "#2c3e50",
  blue = "#3498db",
  red = "#e74c3c",
  green = "#2ecc71",
  orange = "#e67e22",
  purple = "#9b59b6",
  grey = "#7f8c8d",
  light_blue = "#85c1e9",
  light_red = "#f1948a"
)

# Two-group comparison
palette_two <- c("#3498db", "#e74c3c")

# Multi-group
palette_multi <- c("#3498db", "#e74c3c", "#2ecc71", "#e67e22", "#9b59b6")

# --- Helper: 2x2 Table Summary ---
diagnostic_2x2 <- function(tp, fp, fn, tn) {
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  ppv <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  accuracy <- (tp + tn) / (tp + fp + fn + tn)
  lr_pos <- sensitivity / (1 - specificity)
  lr_neg <- (1 - sensitivity) / specificity
  prevalence <- (tp + fn) / (tp + fp + fn + tn)

  tibble(
    Metric = c("Sensitivity", "Specificity", "PPV", "NPV",
               "Accuracy", "LR+", "LR-", "Prevalence"),
    Value = c(sensitivity, specificity, ppv, npv,
              accuracy, lr_pos, lr_neg, prevalence),
    Percent = scales::percent(Value, accuracy = 0.1)
  )
}

# --- Helper: Risk Measures from 2x2 ---
risk_measures <- function(a, b, c, d) {
  # a = exposed & outcome, b = exposed & no outcome
  # c = unexposed & outcome, d = unexposed & no outcome
  risk_exposed <- a / (a + b)
  risk_unexposed <- c / (c + d)
  rr <- risk_exposed / risk_unexposed
  or <- (a * d) / (b * c)
  arr <- risk_exposed - risk_unexposed
  rrr <- arr / risk_unexposed
  nnt <- ifelse(arr != 0, 1 / abs(arr), NA)

  tibble(
    Measure = c("Risk (Exposed)", "Risk (Unexposed)", "RR", "OR",
                "ARR", "RRR", "NNT"),
    Value = round(c(risk_exposed, risk_unexposed, rr, or, arr, rrr, nnt), 3)
  )
}

# --- Helper: Format p-value ---
format_pvalue <- function(p) {
  if (p < 0.001) return("< 0.001")
  paste0("= ", round(p, 3))
}
