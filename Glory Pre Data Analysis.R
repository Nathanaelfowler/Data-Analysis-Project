
# STEP 1: DATA CLEANING, UNIT CORRECTION & INDEX SCORING
# Study: Videographic-Based Nutrition Education & Menstrual Health (Baseline N = 43)


library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

# 1. Load the updated Excel dataset
file_path <- "~/Glory/INFLUENCE_OF_VIDEO-GRAPHIC_NUTRITION_EDUCATION_ON_DIETARY_HABITS_NUTRITIONAL_STATUS_AND_MENSTRUAL_DISORDER_SYMPTOMS_AMONG_FEMALE_UNDERGRADUATE_STUDENTS.xlsx"
raw_df <- read_excel(file_path, sheet = 1)

# 2. Drop completely empty columns (section headers) and KoboToolbox system metadata
clean_df <- raw_df %>%
  remove_empty(c("rows", "cols")) %>%
  select(
    -starts_with("_"),
    -starts_with("meta/"),
    -starts_with("Thank you"),
    -starts_with("Please specify other")
  ) %>%
  filter(`Are you willing to participate in this study?` == "Yes")

# 3. Rename core sociodemographic, menstrual, dietary habit & anthropometric columns
clean_df <- clean_df %>%
  rename(
    consent             = `Are you willing to participate in this study?`,
    id                  = `Questionnaire ID:`,
    age                 = `Age:`,
    religion            = `Religion:`,
    ethnicity           = `Ethnic group:`,
    level               = `Current Level of Study:`,
    age_menarche        = `Age at menarche (first menstruation):`,
    dysmenorrhea        = `Dysmenorrhea: Do you experience painful menstruation/abdominal cramps during your period?`,
    pms                 = `Premenstrual Syndrome (PMS): Do you experience at least one of the following symptoms: bloating, breast pain, fatigue, irritability, mood swings, food cravings, before your period?`,
    pmdd                = `Premenstrual Dysphoric Disorder (PMDD): Do you experience at least one of the following symptoms: severe mood swings, marked irritability, depressed mood, severe anxiety, before your period?`,
    period_days         = `How many days does your period usually last?`,
    cycle_length        = `What is your average cycle length?`,
    intermenstrual_spot = `Apart from your regular period days, do you ever see blood stains in your underwear?`,
    heavy_bleeding      = `Have you experienced very heavy bleeding? (e.g., soaking through pad hourly, large clots)`,
    pads_per_day        = `Number of sanitary pads used per day (average):`,
    contraceptive_meds  = `Are you currently using any medications or devices (e.g., contraceptives, IUD) that affect bleeding?`,
    diagnosed_pathology = `Have you ever been diagnosed with fibroids, endometriosis, PCOS, or polyps?`,
    vas_pain            = `How would you rate your menstrual pain on a scale of 1 to 10 (VAS)`,
    amenorrhea_2m       = `Have you ever missed your period for 2 months or more (not due to pregnancy)?`,
    skip_breakfast      = `Do you skip breakfast?`,
    three_meals         = `Do you consume at least three meals per day?`,
    snack_between       = `Do you eat snacks in between meals?`,
    weight_kg           = `Weight (kg):`,
    height_cm           = `Height (cm):`,
    waist_raw           = `Waist circumference (cm):`,
    hip_raw             = `Hip circumference (cm):`
  )

# 4. Clean text strings, convert anthropometry units (inches -> cm), & compute indices
clean_df <- clean_df %>%
  mutate(
    across(where(is.character), str_squish),
    
    # Harmonize Religion & Ethnicity
    religion_clean = if_else(religion == "Others", "Others (Mixed)", religion),
    level = factor(level, levels = c("100L", "200L", "300L", "400L", "500L/Above")),
    
    # Anthropometry unit fixes:
    # Step A: Convert inches (< 50) to cm (* 2.54)
    waist_cm_tmp = if_else(waist_raw < 50, waist_raw * 2.54, waist_raw),
    hip_cm_tmp   = if_else(hip_raw < 50, hip_raw * 2.54, hip_raw),
    
    # Step B: Fix swapped waist/hip on ID 004 (where waist > hip)
    waist_cm = if_else(waist_cm_tmp > hip_cm_tmp, hip_cm_tmp, waist_cm_tmp),
    hip_cm   = if_else(waist_cm_tmp > hip_cm_tmp, waist_cm_tmp, hip_cm_tmp),
    
    # Compute BMI (kg/m^2) and WHO Classification
    height_m = height_cm / 100,
    bmi      = weight_kg / (height_m^2),
    bmi_cat  = factor(
      case_when(
        bmi < 18.5 ~ "Underweight (<18.5)",
        bmi >= 18.5 & bmi < 25.0 ~ "Normal Weight (18.5–24.9)",
        bmi >= 25.0 & bmi < 30.0 ~ "Overweight (25.0–29.9)",
        bmi >= 30.0 ~ "Obese (≥30.0)"
      ),
      levels = c("Underweight (<18.5)", "Normal Weight (18.5–24.9)", "Overweight (25.0–29.9)", "Obese (≥30.0)")
    ),
    
    # Compute Waist-to-Hip Ratio (WHR) & WHO Abdominal Obesity Cutoff (>= 0.85 for women)
    whr     = waist_cm / hip_cm,
    whr_cat = factor(
      if_else(whr >= 0.85, "Substantial Risk (WHR ≥ 0.85)", "Normal Risk (WHR < 0.85)"),
      levels = c("Normal Risk (WHR < 0.85)", "Substantial Risk (WHR ≥ 0.85)")
    ),
    
    # VAS Menstrual Pain Severity Categorization
    vas_pain = as.numeric(vas_pain),
    vas_cat  = factor(
      case_when(
        vas_pain <= 3 ~ "Mild Pain (1–3)",
        vas_pain >= 4 & vas_pain <= 7 ~ "Moderate Pain (4–7)",
        vas_pain >= 8 ~ "Severe Pain (8–10)"
      ),
      levels = c("Mild Pain (1–3)", "Moderate Pain (4–7)", "Severe Pain (8–10)")
    )
  ) %>%
  select(-waist_cm_tmp, -hip_cm_tmp)

# 5. Score the Food Frequency Questionnaire (FFQ)
# Convert 8-point frequency responses to numeric weights (0 to 7):
# Never = 0, Once a month = 1, 2–4 times a month = 2, Once a week = 3,
# 2–4 times a week = 4, 5–6 times a week = 5, Once a day = 6, 2–3 times per day = 7
ffq_recode <- function(x) {
  case_when(
    str_detect(x, "Never") ~ 0,
    str_detect(x, "Once a month") ~ 1,
    str_detect(x, "2–4 times a month|2-4 times a month") ~ 2,
    str_detect(x, "Once a week") ~ 3,
    str_detect(x, "2–4 times a week|2-4 times a week") ~ 4,
    str_detect(x, "5–6 times a week|5-6 times a week") ~ 5,
    str_detect(x, "Once a day") ~ 6,
    str_detect(x, "2–3 times per day|2-3 times per day") ~ 7,
    TRUE ~ NA_real_
  )
}

# Identify FFQ item columns (from Rice through Sweeteners, excluding 'Others')
ffq_start <- which(names(clean_df) == "Rice")
ffq_end   <- which(names(clean_df) == "Sweeteners (Sugar, Honey, Saccharin, Aspartame…).")
ffq_names <- names(clean_df)[ffq_start:ffq_end]
ffq_names <- ffq_names[!str_detect(ffq_names, "^Others")]

# Create numeric FFQ columns (prefix: ffq_)
clean_df <- clean_df %>%
  mutate(across(all_of(ffq_names), ffq_recode, .names = "ffq_{.col}"))

# Define Anti-inflammatory (Nutrient-Dense) vs. Pro-inflammatory FFQ items per Chapter 2 & 3
anti_inflam_items <- paste0("ffq_", c(
  "Wheat", "Pap/Eko", "Amala (Plantain flour)", "Sweet potatoes", "Plantain",
  "Beans", "Moimoi", "Akara", "Gbegiri", "Groundnut", "Egusi", "Soybean/Soybean products", "Locust beans/iru", "Walnut (Asala)",
  "Ewedu", "Efo tete", "Efo soko", "Ewuro/bitter leaf", "Ugwu", "Efirin/scent leaf", "Gbure/waterleaf", "Okro", "Tomato", "Onion", "Pepper", "Carrot", "Cucumber", "Cabbage",
  "Pawpaw", "Pineapple", "Apple", "Watermelon", "Orange", "Tangerine", "Avocado pear", "Banana", "Mango", "Cashew", "Guava", "Almond (Fruit)", "Cherry/agbalumo",
  "Fish", "Milk and milk products", "Eggs"
))

pro_inflam_items <- paste0("ffq_", c(
  "Noodles", "Biscuits", "Soft drinks (Coca-cola, Fanta)", "Juices and milk drinks (Bobo, vijumilk)",
  "Butter or Margarine", "Snacks", "Sweeteners (Sugar, Honey, Saccharin, Aspartame…)."
))

clean_df <- clean_df %>%
  rowwise() %>%
  mutate(
    # Total FFQ Consumption Frequency Score
    ffq_total_score        = sum(c_across(starts_with("ffq_")), na.rm = TRUE),
    # Anti-inflammatory / Nutrient-dense Food Score
    ffq_anti_inflam_score  = sum(c_across(any_of(anti_inflam_items)), na.rm = TRUE),
    # Pro-inflammatory / Processed Food Score
    ffq_pro_inflam_score   = sum(c_across(any_of(pro_inflam_items)), na.rm = TRUE),
    # Net Healthy Diet Index (Anti-inflammatory minus Pro-inflammatory)
    diet_quality_net_score = ffq_anti_inflam_score - ffq_pro_inflam_score
  ) %>%
  ungroup() %>%
  mutate(
    # Categorize Overall Diet Quality into Tertiles (Low, Moderate, High)
    diet_quality_cat = factor(
      ntile(diet_quality_net_score, 3),
      levels = c(1, 2, 3),
      labels = c("Poor Diet Quality (Low Tertile)", "Moderate Diet Quality (Middle Tertile)", "Good Diet Quality (High Tertile)")
    )
  )

# 6. Verification Check
cat("Cleaned Baseline Dataset Dimensions:", dim(clean_df), "\n")
summary(clean_df %>% select(age, bmi, whr, vas_pain, ffq_total_score, diet_quality_net_score))


# STEP 2: BASELINE DESCRIPTIVES, FFQ BREAKDOWN & ASSOCIATION ANALYSIS
# Objectives 1 & 3 (Research Question 4) | N = 43 Respondents


library(tidyverse)
library(gtsummary)
library(flextable)
library(officer)

theme_gtsummary_compact()

# Ensure clean_df exists in environment
if (!exists("clean_df")) {
  stop("Please run Step 1 (01_clean_and_score.R) first so 'clean_df' is loaded.")
}


# 1. Normality Diagnostics (Shapiro-Wilk Test)

norm_vars <- c("age", "age_menarche", "pads_per_day", "vas_pain", "bmi", "whr",
               "ffq_total_score", "ffq_anti_inflam_score", "ffq_pro_inflam_score", "diet_quality_net_score")

normality_check <- map_dfr(norm_vars, function(v) {
  x <- na.omit(clean_df[[v]])
  sw <- shapiro.test(x)
  tibble(
    Variable = v,
    n = length(x),
    Mean = round(mean(x), 2),
    SD = round(sd(x), 2),
    Median = round(median(x), 2),
    IQR = sprintf("%.2f - %.2f", quantile(x, 0.25), quantile(x, 0.75)),
    Shapiro_W = round(sw$statistic, 3),
    p_value = round(sw$p.value, 4),
    Distribution = if_else(sw$p.value >= 0.05, "Normal (Parametric)", "Skewed (Non-Parametric)")
  )
})

print(normality_check, n = 10)


# 2. Table 1: Socio-Demographic Characteristics (Section A)

table_1 <- clean_df %>%
  select(
    `Age (years)` = age,
    `Religion` = religion_clean,
    `Ethnic Group` = ethnicity,
    `Current Level of Study` = level
  ) %>%
  tbl_summary(
    type = list(`Age (years)` ~ "continuous2"),
    statistic = list(
      all_continuous2() ~ c("{mean} ± {sd}", "{median} ({p25}, {p75})"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous2() ~ 2
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 1: Socio-Demographic Characteristics of Respondents (N = 43)**")


# 3. Table 2: Menstrual Disorder Profile & VAS Pain Indexing (Section C)

table_2 <- clean_df %>%
  select(
    `Age at Menarche (years)` = age_menarche,
    `Dysmenorrhea (Painful Menstruation)` = dysmenorrhea,
    `Premenstrual Syndrome (PMS)` = pms,
    `Premenstrual Dysphoric Disorder (PMDD)` = pmdd,
    `Usual Bleeding Duration` = period_days,
    `Average Cycle Length` = cycle_length,
    `Intermenstrual Spotting` = intermenstrual_spot,
    `Heavy Menstrual Bleeding` = heavy_bleeding,
    `Average Sanitary Pads per Day` = pads_per_day,
    `Missed Period ≥ 2 Months (Amenorrhea)` = amenorrhea_2m,
    `VAS Pain Intensity Score (1–10)` = vas_pain,
    `VAS Pain Severity Category` = vas_cat
  ) %>%
  tbl_summary(
    type = list(
      c(`Age at Menarche (years)`, `Average Sanitary Pads per Day`, `VAS Pain Intensity Score (1–10)`) ~ "continuous2"
    ),
    statistic = list(
      all_continuous2() ~ c("{mean} ± {sd}", "{median} ({p25}, {p75})"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous2() ~ 2,
    missing_text = "Not Reported"
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 2: Baseline Menstrual Disorder Profile and VAS Pain Severity (N = 43)**")


# 4. Table 3: Nutritional Status & Dietary Habits Summary (Sections D & E)

table_3 <- clean_df %>%
  select(
    `Weight (kg)` = weight_kg,
    `Height (cm)` = height_cm,
    `Body Mass Index (BMI, kg/m²)` = bmi,
    `WHO BMI Classification` = bmi_cat,
    `Waist Circumference (cm)` = waist_cm,
    `Hip Circumference (cm)` = hip_cm,
    `Waist-to-Hip Ratio (WHR)` = whr,
    `WHR Abdominal Risk Classification` = whr_cat,
    `Skips Breakfast` = skip_breakfast,
    `Consumes ≥ 3 Meals per Day` = three_meals,
    `Eats Snacks Between Meals` = snack_between,
    `Total FFQ Frequency Score` = ffq_total_score,
    `Anti-Inflammatory Food Score` = ffq_anti_inflam_score,
    `Pro-Inflammatory Food Score` = ffq_pro_inflam_score,
    `Net Healthy Diet Index Score` = diet_quality_net_score,
    `Overall Diet Quality Tertile` = diet_quality_cat
  ) %>%
  tbl_summary(
    type = list(
      c(`Weight (kg)`, `Height (cm)`, `Body Mass Index (BMI, kg/m²)`,
        `Waist Circumference (cm)`, `Hip Circumference (cm)`, `Waist-to-Hip Ratio (WHR)`,
        `Total FFQ Frequency Score`, `Anti-Inflammatory Food Score`,
        `Pro-Inflammatory Food Score`, `Net Healthy Diet Index Score`) ~ "continuous2"
    ),
    statistic = list(
      all_continuous2() ~ c("{mean} ± {sd}", "{median} ({p25}, {p75})"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous2() ~ 2
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3: Baseline Anthropometric Nutritional Status and Dietary Habits (N = 43)**")


# 5. Table 4: Complete Item-by-Item Food Frequency Distribution (All 76 Items)

freq_levels <- c(
  "Never", "Once a month", "2–4 times a month", "Once a week",
  "2–4 times a week", "5–6 times a week", "Once a day", "2–3 times per day"
)

ffq_table_df <- map_dfr(ffq_names, function(item) {
  vals <- factor(clean_df[[item]], levels = freq_levels)
  counts <- table(vals)
  pcts <- round(prop.table(counts) * 100, 1)
  formatted <- sprintf("%d (%.1f%%)", counts, pcts)
  names(formatted) <- freq_levels
  tibble(`Food Item` = item) %>% bind_cols(as_tibble_row(formatted))
})

table_4_ft <- flextable(ffq_table_df) %>%
  autofit() %>%
  theme_box() %>%
  bold(part = "header") %>%
  fontsize(size = 8, part = "all") %>%
  align(j = 2:9, align = "center", part = "all") %>%
  set_caption("Table 4: Baseline 4-Week Food Frequency Questionnaire (FFQ) Distribution Across All Food Items (N = 43)")


# 6. Table 5A: Bivariate Correlations with VAS Menstrual Pain Score (Objective 3)

corr_predictors <- list(
  "Body Mass Index (BMI, kg/m²)"       = "bmi",
  "Waist-to-Hip Ratio (WHR)"           = "whr",
  "Total FFQ Consumption Score"        = "ffq_total_score",
  "Anti-Inflammatory Diet Score"       = "ffq_anti_inflam_score",
  "Pro-Inflammatory Diet Score"        = "ffq_pro_inflam_score",
  "Net Healthy Diet Index Score"       = "diet_quality_net_score",
  "Age (years)"                        = "age",
  "Age at Menarche (years)"            = "age_menarche"
)

corr_df <- imap_dfr(corr_predictors, function(col_name, label) {
  x <- clean_df[[col_name]]
  y <- clean_df$vas_pain
  sp <- cor.test(x, y, method = "spearman", exact = FALSE)
  pe <- cor.test(x, y, method = "pearson")
  tibble(
    `Predictor Variable` = label,
    `Spearman's rho (ρ)` = sprintf("%.3f", sp$estimate),
    `Spearman p-value`   = sprintf("%.4f", sp$p.value),
    `Pearson's r`        = sprintf("%.3f", pe$estimate),
    `Pearson p-value`    = sprintf("%.4f", pe$p.value),
    `Interpretation`     = if_else(sp$p.value < 0.05 | pe$p.value < 0.05, "Statistically Significant", "Not Significant")
  )
})

table_5a_ft <- flextable(corr_df) %>%
  autofit() %>%
  theme_box() %>%
  bold(part = "header") %>%
  align(j = 2:6, align = "center", part = "all") %>%
  set_caption("Table 5A: Baseline Bivariate Correlations Between Nutritional Indices, Dietary Scores, and VAS Pain Score (N = 43)")


# 7. Table 5B: Cross-Tabulation of Nutritional Status & Diet Quality vs. Symptoms

table_5b_bmi <- clean_df %>%
  select(
    `WHO BMI Category` = bmi_cat,
    `Dysmenorrhea` = dysmenorrhea,
    `PMS` = pms,
    `Heavy Bleeding` = heavy_bleeding,
    `Cycle Length` = cycle_length,
    `VAS Pain Severity` = vas_cat
  ) %>%
  tbl_summary(by = `WHO BMI Category`, missing = "no") %>%
  add_p(test = list(all_categorical() ~ "fisher.test"), test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)) %>%
  bold_labels() %>%
  modify_caption("**Table 5B: Association Between WHO BMI Classification and Menstrual Disorder Outcomes (N = 43)**")

table_5b_diet <- clean_df %>%
  select(
    `Diet Quality Tertile` = diet_quality_cat,
    `Dysmenorrhea` = dysmenorrhea,
    `PMS` = pms,
    `Heavy Bleeding` = heavy_bleeding,
    `Cycle Length` = cycle_length,
    `VAS Pain Severity` = vas_cat
  ) %>%
  tbl_summary(by = `Diet Quality Tertile`, missing = "no") %>%
  add_p(test = list(all_categorical() ~ "fisher.test"), test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)) %>%
  bold_labels() %>%
  modify_caption("**Table 5C: Association Between Diet Quality Tertiles and Menstrual Disorder Outcomes (N = 43)**")


# 8. Export All Baseline Tables to Word Document (.docx)

if (!dir.exists("outputs")) dir.create("outputs")

doc <- read_docx() %>%
  body_add_par("BASELINE STATISTICAL ANALYSIS RESULTS (OBJECTIVES 1 & 3)", style = "heading 1") %>%
  body_add_flextable(as_flex_table(table_1)) %>%
  body_add_break() %>%
  body_add_flextable(as_flex_table(table_2)) %>%
  body_add_break() %>%
  body_add_flextable(as_flex_table(table_3)) %>%
  body_add_break() %>%
  body_add_flextable(table_5a_ft) %>%
  body_add_break() %>%
  body_add_flextable(as_flex_table(table_5b_bmi)) %>%
  body_add_break() %>%
  body_add_flextable(as_flex_table(table_5b_diet)) %>%
  body_add_break() %>%
  body_add_flextable(table_4_ft)

print(doc, target = "~/Glory/Pre_Data_Analysis_Tables.docx")
cat("\nSUCCESS: Pre Data tables exported to '~/Glory/Baseline_Analysis_Tables.docx'!\n")