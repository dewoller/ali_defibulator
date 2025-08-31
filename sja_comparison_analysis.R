# Statistical Analysis: SJA vs Non-SJA AED Cardiac Arrest Coverage
# Independent samples test for difference in arrests within catchments

library(targets)
library(dplyr)
library(broom)

cat("=== SJA vs NON-SJA AED COMPARISON ANALYSIS ===\n\n")

# Load the data
tar_load('sua_noh_mesh_export')

# Prepare the data for analysis
sja_analysis <- sua_noh_mesh_export %>%
  select(sua_id, company, is_sja_defib, n_vacar_arrest, sua_area, cald_pop, age_55_plus_pop) %>%
  mutate(
    n_vacar_arrest = coalesce(n_vacar_arrest, 0),
    sja_group = ifelse(is_sja_defib, "SJA", "Non-SJA"),
    sua_area_km2 = as.numeric(sua_area) / 1000000,  # Convert to km²
    cald_pop = as.numeric(cald_pop),
    age_55_plus_pop = as.numeric(age_55_plus_pop)
  )

cat("SAMPLE SIZE:\n")
cat("- Total AEDs:", nrow(sja_analysis), "\n")
cat("- SJA AEDs:", sum(sja_analysis$is_sja_defib), "\n") 
cat("- Non-SJA AEDs:", sum(!sja_analysis$is_sja_defib), "\n\n")

# Descriptive statistics by group
cat("DESCRIPTIVE STATISTICS:\n")
desc_stats <- sja_analysis %>%
  group_by(sja_group) %>%
  summarise(
    n = n(),
    mean_arrests = mean(n_vacar_arrest),
    sd_arrests = sd(n_vacar_arrest),
    median_arrests = median(n_vacar_arrest),
    min_arrests = min(n_vacar_arrest),
    max_arrests = max(n_vacar_arrest),
    prop_with_arrests = mean(n_vacar_arrest > 0),
    mean_area_m2 = mean(sua_area_km2 * 1000000),
    .groups = 'drop'
  )

print(desc_stats)
cat("\n")

# Extract data for statistical tests
sja_arrests <- sja_analysis$n_vacar_arrest[sja_analysis$is_sja_defib]
nonsja_arrests <- sja_analysis$n_vacar_arrest[!sja_analysis$is_sja_defib]

cat("DETAILED GROUP BREAKDOWN:\n")
cat("SJA AEDs - Arrests per catchment:", paste(sja_arrests, collapse = ", "), "\n")
cat("Non-SJA AEDs - Arrests per catchment:", paste(nonsja_arrests, collapse = ", "), "\n\n")

# Test for normality (Shapiro-Wilk)
cat("NORMALITY TESTS (Shapiro-Wilk):\n")
sja_shapiro <- shapiro.test(sja_arrests)
nonsja_shapiro <- shapiro.test(nonsja_arrests)

cat("SJA group: W =", round(sja_shapiro$statistic, 4), ", p-value =", 
    ifelse(sja_shapiro$p.value < 0.001, "< 0.001", round(sja_shapiro$p.value, 4)), "\n")
cat("Non-SJA group: W =", round(nonsja_shapiro$statistic, 4), ", p-value =", 
    ifelse(nonsja_shapiro$p.value < 0.001, "< 0.001", round(nonsja_shapiro$p.value, 4)), "\n")

normal_assumption <- sja_shapiro$p.value > 0.05 && nonsja_shapiro$p.value > 0.05
cat("Normal distribution assumption:", ifelse(normal_assumption, "MET", "VIOLATED"), "\n\n")

# Test for equal variances (F-test)
cat("EQUAL VARIANCES TEST (F-test):\n")
var_test <- var.test(sja_arrests, nonsja_arrests)
cat("F =", round(var_test$statistic, 4), ", df1 =", var_test$parameter[1], 
    ", df2 =", var_test$parameter[2], ", p-value =", 
    ifelse(var_test$p.value < 0.001, "< 0.001", round(var_test$p.value, 4)), "\n")
equal_var_assumption <- var_test$p.value > 0.05
cat("Equal variances assumption:", ifelse(equal_var_assumption, "MET", "VIOLATED"), "\n\n")

# Independent samples t-test (parametric)
cat("INDEPENDENT SAMPLES T-TEST:\n")
if (equal_var_assumption) {
  t_test <- t.test(sja_arrests, nonsja_arrests, var.equal = TRUE)
  cat("(Equal variances assumed)\n")
} else {
  t_test <- t.test(sja_arrests, nonsja_arrests, var.equal = FALSE)
  cat("(Welch's t-test - unequal variances)\n")
}

cat("t =", round(t_test$statistic, 4), ", df =", round(t_test$parameter, 2), 
    ", p-value =", ifelse(t_test$p.value < 0.001, "< 0.001", round(t_test$p.value, 4)), "\n")
cat("95% CI for difference in means: [", round(t_test$conf.int[1], 4), ", ", 
    round(t_test$conf.int[2], 4), "]\n")
cat("Mean difference (SJA - Non-SJA):", round(mean(sja_arrests) - mean(nonsja_arrests), 4), "\n\n")

# Mann-Whitney U test (non-parametric alternative)
cat("MANN-WHITNEY U TEST (non-parametric):\n")
wilcox_test <- wilcox.test(sja_arrests, nonsja_arrests, exact = FALSE)
cat("W =", round(wilcox_test$statistic, 4), ", p-value =", 
    ifelse(wilcox_test$p.value < 0.001, "< 0.001", round(wilcox_test$p.value, 4)), "\n\n")

# Fisher's exact test for proportion with arrests
cat("PROPORTION WITH ARRESTS COMPARISON:\n")
sja_with_arrests <- sum(sja_arrests > 0)
nonsja_with_arrests <- sum(nonsja_arrests > 0)
sja_total <- length(sja_arrests)
nonsja_total <- length(nonsja_arrests)

cat("SJA: ", sja_with_arrests, "/", sja_total, " (", 
    round(sja_with_arrests/sja_total*100, 1), "%) have arrests\n")
cat("Non-SJA: ", nonsja_with_arrests, "/", nonsja_total, " (", 
    round(nonsja_with_arrests/nonsja_total*100, 1), "%) have arrests\n")

# Fisher's exact test
contingency_table <- matrix(c(sja_with_arrests, sja_total - sja_with_arrests,
                             nonsja_with_arrests, nonsja_total - nonsja_with_arrests),
                           nrow = 2, byrow = TRUE,
                           dimnames = list(c("SJA", "Non-SJA"), c("With Arrests", "No Arrests")))

cat("\nContingency Table:\n")
print(contingency_table)

fisher_test <- fisher.test(contingency_table)
cat("\nFisher's Exact Test: p-value =", 
    ifelse(fisher_test$p.value < 0.001, "< 0.001", round(fisher_test$p.value, 4)), "\n")
cat("Odds Ratio =", round(fisher_test$estimate, 4), 
    ", 95% CI: [", round(fisher_test$conf.int[1], 4), ", ", 
    round(fisher_test$conf.int[2], 4), "]\n\n")

# Effect size (Cohen's d) for t-test
pooled_sd <- sqrt(((length(sja_arrests)-1)*var(sja_arrests) + (length(nonsja_arrests)-1)*var(nonsja_arrests)) / 
                  (length(sja_arrests) + length(nonsja_arrests) - 2))
cohens_d <- (mean(sja_arrests) - mean(nonsja_arrests)) / pooled_sd

cat("EFFECT SIZE:\n")
cat("Cohen's d =", round(cohens_d, 4))
if (abs(cohens_d) < 0.2) {
  cat(" (negligible effect)")
} else if (abs(cohens_d) < 0.5) {
  cat(" (small effect)")
} else if (abs(cohens_d) < 0.8) {
  cat(" (medium effect)")
} else {
  cat(" (large effect)")
}
cat("\n\n")

# Control for catchment area (arrests per unit area)
cat("AREA-ADJUSTED ANALYSIS:\n")
sja_analysis <- sja_analysis %>%
  mutate(arrests_per_km2 = n_vacar_arrest / sua_area_km2)

sja_rate <- sja_analysis$arrests_per_km2[sja_analysis$is_sja_defib]
nonsja_rate <- sja_analysis$arrests_per_km2[!sja_analysis$is_sja_defib]

cat("Mean arrests per km²:\n")
cat("- SJA:", round(mean(sja_rate), 2), "± ", round(sd(sja_rate), 2), "\n")
cat("- Non-SJA:", round(mean(nonsja_rate), 2), "± ", round(sd(nonsja_rate), 2), "\n")

# Test area-adjusted rates
area_adjusted_test <- t.test(sja_rate, nonsja_rate)
cat("Area-adjusted t-test: t =", round(area_adjusted_test$statistic, 4), 
    ", p-value =", ifelse(area_adjusted_test$p.value < 0.001, "< 0.001", 
                         round(area_adjusted_test$p.value, 4)), "\n\n")

# CONCLUSIONS
cat("=== STATISTICAL CONCLUSIONS ===\n")
alpha <- 0.05

cat("1. PRIMARY HYPOTHESIS TEST:\n")
if (t_test$p.value < alpha) {
  cat("   ✓ SIGNIFICANT DIFFERENCE found between SJA and Non-SJA AEDs\n")
  cat("   ✓ p-value (", ifelse(t_test$p.value < 0.001, "< 0.001", round(t_test$p.value, 4)), 
      ") < α (", alpha, ")\n")
  if (mean(sja_arrests) > mean(nonsja_arrests)) {
    cat("   → SJA AEDs have significantly MORE cardiac arrests in their catchments\n")
  } else {
    cat("   → Non-SJA AEDs have significantly MORE cardiac arrests in their catchments\n")
  }
} else {
  cat("   ✗ NO SIGNIFICANT DIFFERENCE found between SJA and Non-SJA AEDs\n")
  cat("   ✗ p-value (", round(t_test$p.value, 4), ") ≥ α (", alpha, ")\n")
  cat("   → Cannot reject null hypothesis of equal means\n")
}

cat("\n2. NON-PARAMETRIC CONFIRMATION:\n")
if (wilcox_test$p.value < alpha) {
  cat("   ✓ Mann-Whitney U test CONFIRMS significant difference\n")
} else {
  cat("   ✗ Mann-Whitney U test shows NO significant difference\n")
}

cat("\n3. PROPORTION ANALYSIS:\n")
if (fisher_test$p.value < alpha) {
  cat("   ✓ SIGNIFICANT DIFFERENCE in proportion of AEDs with arrests\n")
} else {
  cat("   ✗ NO SIGNIFICANT DIFFERENCE in proportion of AEDs with arrests\n")
}

cat("\n4. PRACTICAL SIGNIFICANCE:\n")
cat("   Effect size:", ifelse(abs(cohens_d) >= 0.5, "MEANINGFUL", "SMALL"), "\n")
cat("   Mean difference:", round(mean(sja_arrests) - mean(nonsja_arrests), 3), "arrests per catchment\n")

cat("\n=== END ANALYSIS ===\n")