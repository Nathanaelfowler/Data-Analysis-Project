# ==============================================================================
# STEP 3: PRE-TEST VS. POST-TEST COMPARISON & PREDICTOR ANALYSIS
# Objective 4 & Research Question 5 | Matched Cohort (N = 43)
# ==============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(flextable)
library(officer)

# Ensure baseline clean_df exists
if (!exists("clean_df")) {
  stop("Please run Step 1 (01_clean_and_score.R) first so 'clean_df' is in your workspace.")
}

# 1. Load and clean Post-Test dataset (Post data.xlsx)

post_raw <- read_excel("~/Glory/Post data.xlsx", sheet = 1)

post_clean <- post_raw %>%
  remove_empty(c("rows", "cols")) %>%
  rename(
    id           = `Questionnaire ID:`,
    dysmenorrhea = `Dysmenorrhea: Do you experience painful menstruation/abdominal cramps during your period?`,
    pms          = `Premenstrual Syndrome (PMS): Do you experience at least one of the following symptoms: bloating, breast pain, fatigue, irritability, mood swings, food cravings, before your period?`,
    pmdd         = `Premenstrual Dysphoric Disorder (PMDD): Do you experience at least one of the following symptoms: severe mood swings, marked irritability, depressed mood, severe anxiety, before your period?`,
    vas_pain     = `How would you rate your menstrual pain on a scale of 1 to 10 (VAS)`
  ) %>%
  mutate(
    across(where(is.character), str_squish),
    vas_pain_post = as.numeric(vas_pain),
    vas_cat_post  = factor(
      case_when(
        vas_pain_post <= 3 ~ "Mild Pain (1–3)",
        vas_pain_post >= 4 & vas_pain_post <= 7 ~ "Moderate Pain (4–7)",
        vas_pain_post >= 8 ~ "Severe Pain (8–10)"
      ),
      levels = c("Mild Pain (1–3)", "Moderate Pain (4–7)", "Severe Pain (8–10)")
    )
  )

# Re-score Post-Test FFQ (keeping Poundo Yam)
post_clean <- post_clean %>%
  mutate(across(any_of(ffq_names), ffq_recode, .names = "ffq_{.col}")) %>%
  rowwise() %>%
  mutate(
    ffq_total_post       = sum(c_across(starts_with("ffq_")), na.rm = TRUE),
    ffq_anti_inflam_post = sum(c_across(any_of(anti_inflam_items)), na.rm = TRUE),
    ffq_pro_inflam_post  = sum(c_across(any_of(pro_inflam_items)), na.rm = TRUE),
    diet_net_post        = ffq_anti_inflam_post - ffq_pro_inflam_post
  ) %>%
  ungroup()
# 2. Merge Baseline (Pre) and Post-Test Data by Questionnaire ID
paired_df <- clean_df %>%
  select(
    id, age, level, bmi, bmi_cat, whr,
    dysmenorrhea_pre = dysmenorrhea,
    pms_pre          = pms,
    pmdd_pre         = pmdd,
    vas_pain_pre     = vas_pain,
    vas_cat_pre      = vas_cat,
    ffq_total_pre    = ffq_total_score,
    ffq_anti_pre     = ffq_anti_inflam_score,
    ffq_pro_pre      = ffq_pro_inflam_score,
    diet_net_pre     = diet_quality_net_score
  ) %>%
  inner_join(
    post_clean %>%
      select(
        id,
        dysmenorrhea_post = dysmenorrhea,
        pms_post          = pms,
        pmdd_post         = pmdd,
        vas_pain_post,
        vas_cat_post,
        ffq_total_post,
        ffq_anti_post     = ffq_anti_inflam_post,
        ffq_pro_post      = ffq_pro_inflam_post,
        diet_net_post
      ),
    by = "id"
  ) %>%
  mutate(
    vas_change      = vas_pain_post - vas_pain_pre,
    ffq_change      = ffq_total_post - ffq_total_pre,
    diet_net_change = diet_net_post - diet_net_pre
  )

# 3. Paired Continuous Comparisons (Pre vs. Post)
run_paired_summary <- function(pre_vec, post_vec, label) {
  diff_vec <- post_vec - pre_vec
  # Handle zero-variance columns (e.g., identical FFQ scores) cleanly
  if (sd(diff_vec, na.rm = TRUE) == 0) {
    t_str <- "0.000 (p = 1.000)"
    w_str <- "0.00 (p = 1.000)"
  } else {
    tt <- t.test(post_vec, pre_vec, paired = TRUE)
    wt <- wilcox.test(post_vec, pre_vec, paired = TRUE, exact = FALSE)
    t_str <- sprintf("%.3f (p = %.4f)", tt$statistic, tt$p.value)
    w_str <- sprintf("%.1f (p = %.4f)", wt$statistic, wt$p.value)
  }
  tibble(
    `Outcome Metric`         = label,
    `Baseline (Mean ± SD)`   = sprintf("%.2f ± %.2f", mean(pre_vec), sd(pre_vec)),
    `Post-Test (Mean ± SD)`  = sprintf("%.2f ± %.2f", mean(post_vec), sd(post_vec)),
    `Mean Change (Δ ± SD)`   = sprintf("%+.2f ± %.2f", mean(diff_vec), sd(diff_vec)),
    `Paired t-test (p)`      = t_str,
    `Wilcoxon Test (p)`      = w_str
  )
}

paired_continuous_tbl <- bind_rows(
  run_paired_summary(paired_df$vas_pain_pre, paired_df$vas_pain_post, "VAS Menstrual Pain Score (1–10)"),
  run_paired_summary(paired_df$ffq_total_pre, paired_df$ffq_total_post, "Total FFQ Consumption Score"),
  run_paired_summary(paired_df$ffq_anti_pre, paired_df$ffq_anti_post, "Anti-Inflammatory Diet Score"),
  run_paired_summary(paired_df$ffq_pro_pre, paired_df$ffq_pro_post, "Pro-Inflammatory Diet Score"),
  run_paired_summary(paired_df$diet_net_pre, paired_df$diet_net_post, "Net Healthy Diet Index Score")
)

print(paired_continuous_tbl)

# 4. Categorical Shift Comparison (VAS Pain Severity Categories Pre vs. Post)
vas_shift_ct <- table(Baseline = paired_df$vas_cat_pre, Post_Test = paired_df$vas_cat_post)
print(vas_shift_ct)

# Stuart-Maxwell / Symmetry test for 3x3 paired categories
symmetry_test <- mcnemar.test(vas_shift_ct)
print(symmetry_test)

# 5. Predictors of VAS Pain Change (Research Question 5: Multiple Linear Regression)
# Evaluates whether baseline Age, BMI, WHR, or Diet Quality predict post-intervention VAS change
predictor_model <- lm(vas_change ~ age + bmi + whr + diet_net_pre, data = paired_df)
summary(predictor_model)

reg_tbl <- broom::tidy(predictor_model, conf.int = TRUE) %>%
  mutate(
    term = recode(term,
                  "(Intercept)"  = "Intercept",
                  "age"          = "Age (years)",
                  "bmi"          = "Baseline BMI (kg/m²)",
                  "whr"          = "Baseline Waist-to-Hip Ratio (WHR)",
                  "diet_net_pre" = "Baseline Net Healthy Diet Score"
    ),
    `Beta (B)`     = sprintf("%.3f", estimate),
    `Std. Error`   = sprintf("%.3f", std.error),
    `95% CI`       = sprintf("[%.3f, %.3f]", conf.low, conf.high),
    `t-statistic`  = sprintf("%.3f", statistic),
    `p-value`      = sprintf("%.4f", p.value)
  ) %>%
  select(Predictor = term, `Beta (B)`, `Std. Error`, `95% CI`, `t-statistic`, `p-value`)

# 6. Export Pre-Post Tables to Word (.docx)
ft_paired <- flextable(paired_continuous_tbl) %>%
  autofit() %>%
  theme_box() %>%
  bold(part = "header") %>%
  align(j = 2:6, align = "center", part = "all") %>%
  set_caption("Table 6: Paired Pre-Test vs. Post-Test Comparison of Menstrual Pain (VAS) and Dietary Scores (N = 43)")

ft_reg <- flextable(reg_tbl) %>%
  autofit() %>%
  theme_box() %>%
  bold(part = "header") %>%
  align(j = 2:6, align = "center", part = "all") %>%
  set_caption("Table 7: Multiple Linear Regression Predicting Post-Intervention VAS Pain Score Change (N = 43)")

if (!dir.exists("outputs")) dir.create("outputs")

doc_post <- read_docx() %>%
  body_add_par("PRE-TEST VS. POST-TEST INTERVENTION ANALYSIS (OBJECTIVE 4 & RQ 5)", style = "heading 1") %>%
  body_add_flextable(ft_paired) %>%
  body_add_break() %>%
  body_add_flextable(ft_reg)

print(doc_post, target = "~/Glory/Pre_Post_Comparison_Tables.docx")
cat("\nSUCCESS: Pre-post comparison tables exported to '~/Glory/Pre_Post_Comparison_Tables.docx'!\n")