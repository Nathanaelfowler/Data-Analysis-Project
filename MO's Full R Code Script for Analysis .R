# ==============================================================================
# FOOD INSECURITY, MENTAL HEALTH & ACADEMIC PERFORMANCE ANALYSIS PIPELINE
# Non-Interactive, Full-Coverage Script (All Items Included)
# ==============================================================================

library(tidyverse)
library(janitor)
library(gtsummary)
library(flextable)

# ------------------------------------------------------------------------------
# STEP 1: Data Ingestion
# ------------------------------------------------------------------------------
file_path <- if (file.exists("MO/RAW Data.csv")) {
  "MO/RAW Data.csv"
} else if (file.exists("RAW Data.csv")) {
  "RAW Data.csv"
} else {
  stop("RAW Data.csv could not be found.")
}

raw_df <- read_csv(file_path, show_col_types = FALSE)
cat("Data loaded successfully! Rows:", nrow(raw_df), "| Columns:", ncol(raw_df), "\n")


# ------------------------------------------------------------------------------
# STEP 2: Data Cleaning & Variable Renaming
# ------------------------------------------------------------------------------
clean_df <- raw_df %>%
  filter(.[[2]] == "Yes") %>%
  select(
    timestamp          = 1,
    consent            = 2,
    age                = 3,
    sex                = 4,
    faculty            = 5,
    level_of_study     = 6,
    residence          = 7,
    financial_source   = 8,
    monthly_allowance  = 9,
    fies_1             = 10,
    fies_2             = 11,
    fies_3             = 12,
    fies_4             = 13,
    fies_5             = 14,
    fies_6             = 15,
    fies_7             = 16,
    fies_8             = 17,
    dass_1             = 18,
    dass_2             = 19,
    dass_3             = 20,
    dass_4             = 21,
    dass_5             = 22,
    dass_6             = 23,
    dass_7             = 24,
    dass_8             = 25,
    dass_9             = 26,
    dass_10            = 27,
    dass_11            = 28,
    dass_12            = 29,
    dass_13            = 30,
    dass_14            = 31,
    dass_15            = 32,
    dass_16            = 33,
    dass_17            = 34,
    dass_18            = 35,
    dass_19            = 36,
    dass_20            = 37,
    dass_21            = 38,
    cgpa_band          = 39,
    acad_progress      = 40,
    acad_attendance    = 41,
    acad_attention     = 42,
    acad_understand    = 43,
    acad_complete_conf = 44,
    acad_fi_concentr   = 45,
    acad_fi_miss_class = 46,
    acad_fi_assign     = 47,
    acad_fi_tired_exam = 48,
    acad_fi_extent     = 49
  ) %>%
  mutate(
    age = as.numeric(age),
    sex = factor(sex, levels = c("Female", "Male")),
    level_of_study = case_when(
      str_detect(level_of_study, "100") ~ "100 Level",
      str_detect(level_of_study, "200") ~ "200 Level",
      str_detect(level_of_study, "300") ~ "300 Level",
      str_detect(level_of_study, "400") ~ "400 Level",
      str_detect(level_of_study, "500") ~ "500/600 Level"
    ),
    level_of_study = factor(
      level_of_study,
      levels = c("100 Level", "200 Level", "300 Level", "400 Level", "500/600 Level")
    ),
    residence = factor(residence),
    monthly_allowance = factor(
      monthly_allowance,
      levels = c(
        "Less than ₦20,000",
        "₦20,000 - ₦39,999",
        "₦40,000 - ₦59,999",
        "₦60,000 - ₦79,999",
        "₦80,000 and above"
      )
    ),
    across(starts_with("fies_"), ~ if_else(str_detect(as.character(.), "Yes"), 1L, 0L)),
    across(starts_with("dass_"), as.integer)
  )


# ------------------------------------------------------------------------------
# STEP 3: Composite Scoring
# ------------------------------------------------------------------------------
map_likert <- c("Never" = 0, "Rarely" = 1, "Sometimes" = 2, "Often" = 3, "Always" = 4)
map_extent <- c(
  "Not at all" = 0,
  "To a small extent" = 1,
  "To a moderate extent" = 2,
  "To a moderate extent;To a large extent" = 2.5,
  "To a large extent" = 3,
  "To a very large extent" = 4
)

scored_df <- clean_df %>%
  mutate(
    fies_raw_score = fies_1 + fies_2 + fies_3 + fies_4 + 
      fies_5 + fies_6 + fies_7 + fies_8,
    fies_category = factor(
      case_when(
        fies_raw_score == 0 ~ "Food Secure",
        fies_raw_score %in% 1:3 ~ "Mild Food Insecurity",
        fies_raw_score %in% 4:6 ~ "Moderate Food Insecurity",
        fies_raw_score >= 7     ~ "Severe Food Insecurity"
      ),
      levels = c("Food Secure", "Mild Food Insecurity", "Moderate Food Insecurity", "Severe Food Insecurity")
    ),
    food_insecure_binary = factor(
      if_else(fies_raw_score >= 4, "Food Insecure", "Food Secure"),
      levels = c("Food Secure", "Food Insecure")
    ),
    
    depression_score = (dass_3 + dass_5 + dass_10 + dass_13 + dass_16 + dass_17 + dass_21) * 2,
    anxiety_score    = (dass_2 + dass_4 + dass_7 + dass_9 + dass_15 + dass_19 + dass_20) * 2,
    stress_score     = (dass_1 + dass_6 + dass_8 + dass_11 + dass_12 + dass_14 + dass_18) * 2,
    total_dass_score = depression_score + anxiety_score + stress_score,
    
    depression_severity = factor(
      case_when(
        depression_score <= 9  ~ "Normal",
        depression_score <= 13 ~ "Mild",
        depression_score <= 20 ~ "Moderate",
        depression_score <= 27 ~ "Severe",
        depression_score >= 28 ~ "Extremely Severe"
      ),
      levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely Severe")
    ),
    anxiety_severity = factor(
      case_when(
        anxiety_score <= 7  ~ "Normal",
        anxiety_score <= 9  ~ "Mild",
        anxiety_score <= 14 ~ "Moderate",
        anxiety_score <= 19 ~ "Severe",
        anxiety_score >= 20 ~ "Extremely Severe"
      ),
      levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely Severe")
    ),
    stress_severity = factor(
      case_when(
        stress_score <= 14 ~ "Normal",
        stress_score <= 18 ~ "Mild",
        stress_score <= 25 ~ "Moderate",
        stress_score <= 33 ~ "Severe",
        stress_score >= 34 ~ "Extremely Severe"
      ),
      levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely Severe")
    ),
    
    high_mental_distress = factor(
      if_else(depression_score >= 14 | anxiety_score >= 10 | stress_score >= 19, "Yes", "No"),
      levels = c("No", "Yes")
    ),
    
    d6_num = recode(acad_fi_concentr, !!!map_likert, .default = 2),
    d7_num = recode(acad_fi_miss_class, !!!map_likert, .default = 2),
    d8_num = recode(acad_fi_assign, !!!map_likert, .default = 2),
    d9_num = recode(acad_fi_tired_exam, !!!map_likert, .default = 2),
    d10_num = recode(acad_fi_extent, !!!map_extent, .default = 2),
    acad_disruption_score = d6_num + d7_num + d8_num + d9_num + d10_num,
    
    high_acad_disruption = factor(
      if_else(acad_disruption_score >= median(acad_disruption_score, na.rm = TRUE), "Yes", "No"),
      levels = c("No", "Yes")
    )
  )


# ------------------------------------------------------------------------------
# STEP 4: Descriptive Tables (Overview + ALL Raw Questionnaire Items)
# ------------------------------------------------------------------------------

# Table 1: Sociodemographics
table1_demo <- scored_df %>%
  select(age, sex, level_of_study, residence, monthly_allowance) %>%
  tbl_summary(
    type = list(age ~ "continuous"),
    statistic = list(age ~ "{mean} ({sd})"),
    digits = list(age ~ 2),
    label = list(
      age ~ "Age in years",
      sex ~ "Sex",
      level_of_study ~ "Level of Study",
      residence ~ "Place of Residence",
      monthly_allowance ~ "Monthly Upkeep Allowance (₦)"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 1: Sociodemographic and Economic Characteristics of Respondents (N = 25)**")

# Table 2: Food Insecurity Prevalence
table2_fies <- scored_df %>%
  select(fies_raw_score, fies_category, food_insecure_binary) %>%
  tbl_summary(
    type = list(fies_raw_score ~ "continuous"),
    statistic = list(fies_raw_score ~ "{mean} ({sd})"),
    digits = list(fies_raw_score ~ 2),
    label = list(
      fies_raw_score ~ "FIES Raw Score (0-8)",
      fies_category ~ "Food Security Status (4-Tier)",
      food_insecure_binary ~ "Binary Food Security Status"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 2: Prevalence and Severity of Food Insecurity (FIES)**")

# Table 3: Mental Health Profile Summary
table3_mental_health <- scored_df %>%
  select(
    depression_score, depression_severity,
    anxiety_score, anxiety_severity,
    stress_score, stress_severity
  ) %>%
  tbl_summary(
    type = list(
      depression_score ~ "continuous",
      depression_severity ~ "categorical",
      anxiety_score ~ "continuous",
      anxiety_severity ~ "categorical",
      stress_score ~ "continuous",
      stress_severity ~ "categorical"
    ),
    statistic = list(
      c(depression_score, anxiety_score, stress_score) ~ "{mean} ({sd})"
    ),
    digits = list(c(depression_score, anxiety_score, stress_score) ~ 2),
    label = list(
      depression_score ~ "Depression Score (0 - 42)",
      depression_severity ~ "Depression Severity",
      anxiety_score ~ "Anxiety Score (0 - 42)",
      anxiety_severity ~ "Anxiety Severity",
      stress_score ~ "Stress Score (0 - 42)",
      stress_severity ~ "Stress Severity"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3: Psychological Distress Profile of Respondents (DASS-21)**")

# TABLE 3B: ALL 8 FIES ITEMS (Section B Tool Data)
table3b_fies_items <- scored_df %>%
  select(fies_1, fies_2, fies_3, fies_4, fies_5, fies_6, fies_7, fies_8) %>%
  mutate(across(everything(), ~ factor(if_else(. == 1, "Yes", "No"), levels = c("Yes", "No")))) %>%
  tbl_summary(
    label = list(
      fies_1 ~ "1. Worried about running out of food",
      fies_2 ~ "2. Unable to eat healthy/nutritious food",
      fies_3 ~ "3. Ate only a few kinds of foods",
      fies_4 ~ "4. Had to skip a meal",
      fies_5 ~ "5. Ate less than thought you should",
      fies_6 ~ "6. Ran out of food in household",
      fies_7 ~ "7. Hungry but did not eat",
      fies_8 ~ "8. Went without eating for a whole day"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3B: Frequency Distribution for All 8 FIES Food Insecurity Items (N = 25)**")

# TABLE 3C: ALL 21 DASS-21 ITEMS (Section C Tool Data)
dass_labels_map <- c(
  "0" = "Did not apply to me at all",
  "1" = "Applied to me to some degree",
  "2" = "Applied to me to a considerable degree",
  "3" = "Applied to me very much"
)

table3c_dass_items <- scored_df %>%
  select(starts_with("dass_")) %>%
  mutate(across(everything(), ~ factor(recode(as.character(.), !!!dass_labels_map), levels = unname(dass_labels_map)))) %>%
  tbl_summary(
    label = list(
      dass_1  ~ "Item 1: Hard to wind down (Stress)",
      dass_2  ~ "Item 2: Dryness of mouth (Anxiety)",
      dass_3  ~ "Item 3: Couldn't experience positive feelings (Depression)",
      dass_4  ~ "Item 4: Breathing difficulty (Anxiety)",
      dass_5  ~ "Item 5: Difficult to work up initiative (Depression)",
      dass_6  ~ "Item 6: Tendency to over-react (Stress)",
      dass_7  ~ "Item 7: Trembling/shakiness (Anxiety)",
      dass_8  ~ "Item 8: Using a lot of nervous energy (Stress)",
      dass_9  ~ "Item 9: Worried about panic situations (Anxiety)",
      dass_10 ~ "Item 10: Felt nothing to look forward to (Depression)",
      dass_11 ~ "Item 11: Found myself getting agitated (Stress)",
      dass_12 ~ "Item 12: Difficult to relax (Stress)",
      dass_13 ~ "Item 13: Felt down-hearted and blue (Depression)",
      dass_14 ~ "Item 14: Intolerant of interruptions (Stress)",
      dass_15 ~ "Item 15: Felt close to panic (Anxiety)",
      dass_16 ~ "Item 16: Unable to become enthusiastic (Depression)",
      dass_17 ~ "Item 17: Felt wasn't worth much as a person (Depression)",
      dass_18 ~ "Item 18: Rather touchy/sensitive (Stress)",
      dass_19 ~ "Item 19: Aware of action of heart (Anxiety)",
      dass_20 ~ "Item 20: Felt scared without good reason (Anxiety)",
      dass_21 ~ "Item 21: Felt life was meaningless (Depression)"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3C: Item-Level Frequency Distribution for All 21 DASS-21 Statements (N = 25)**")

# TABLE 3D: ALL 10 ACADEMIC ITEMS (Section D Tool Data)
table3d_acad_items <- scored_df %>%
  select(
    acad_progress,
    acad_attendance,
    acad_attention,
    acad_understand,
    acad_complete_conf,
    acad_fi_concentr,
    acad_fi_miss_class,
    acad_fi_assign,
    acad_fi_tired_exam,
    acad_fi_extent
  ) %>%
  tbl_summary(
    label = list(
      acad_progress      ~ "Item 1: Overall Academic Progress",
      acad_attendance    ~ "Item 2: Class Attendance Rating",
      acad_attention     ~ "Item 3: Ability to Pay Attention in Class",
      acad_understand    ~ "Item 4: Understanding of Course Concepts",
      acad_complete_conf ~ "Item 5: Confidence to Complete Degree",
      acad_fi_concentr   ~ "Item 6: Hunger made concentration difficult",
      acad_fi_miss_class ~ "Item 7: Hunger caused missing classes/tests",
      acad_fi_assign     ~ "Item 8: Hunger delayed assignments/coursework",
      acad_fi_tired_exam ~ "Item 9: Felt too tired/unwell in exams",
      acad_fi_extent     ~ "Item 10: Extent food insecurity affects CGPA"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3D: Item-Level Frequency Distribution for All Academic Performance Items (Section D)**")


# ------------------------------------------------------------------------------
# STEP 5: Inferential Analysis & Bivariate Testing (Hypotheses 1 & 2)
# ------------------------------------------------------------------------------

# Table 4: Hypothesis 1 (Mental Health Group Comparisons)
table4_ho1 <- scored_df %>%
  select(
    food_insecure_binary,
    depression_score,
    anxiety_score,
    stress_score
  ) %>%
  tbl_summary(
    by = food_insecure_binary,
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ "{mean} ({sd}) | {median} ({p25}, {p75})",
    digits = all_continuous() ~ 2,
    label = list(
      depression_score ~ "Depression Score (0-42)",
      anxiety_score ~ "Anxiety Score (0-42)",
      stress_score ~ "Stress Score (0-42)"
    )
  ) %>%
  add_p(
    test = all_continuous() ~ "wilcox.test",
    test.args = all_continuous() ~ list(exact = FALSE)
  ) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 4: Association between Food Insecurity and Mental Health Scores (Ho1)**")

# Table 5: Hypothesis 2 (CGPA Cross-Tabulation)
table5_ho2 <- scored_df %>%
  select(food_insecure_binary, cgpa_band) %>%
  tbl_summary(
    by = food_insecure_binary,
    label = list(cgpa_band ~ "Cumulative Grade Point Average (CGPA) Band")
  ) %>%
  add_p(test = all_categorical() ~ "fisher.test") %>%
  bold_labels() %>%
  modify_caption("**Table 5: Distribution of CGPA Bands by Food Insecurity Status (Ho2)**")

# Table 5B: Demographics vs Food Insecurity (Bivariate Chi-Square/Fisher's)
table5b_demo_bivariate <- scored_df %>%
  select(food_insecure_binary, sex, level_of_study, residence, monthly_allowance) %>%
  tbl_summary(
    by = food_insecure_binary,
    label = list(
      sex ~ "Sex",
      level_of_study ~ "Level of Study",
      residence ~ "Place of Residence",
      monthly_allowance ~ "Monthly Upkeep Allowance (₦)"
    )
  ) %>%
  add_p(test = all_categorical() ~ "fisher.test") %>%
  bold_labels() %>%
  modify_caption("**Table 5B: Bivariate Association between Sociodemographic Factors and Food Insecurity**")


# ------------------------------------------------------------------------------
# STEP 6: Multivariable Binary Logistic Regression (Objectives 3 & 4)
# ------------------------------------------------------------------------------

# Logistic Model 1 (Objective 3): High Mental Distress
# 1. Model 1 (Objective 3): Food Insecurity Predicting Mental Distress
# Using binary logistic regression
logit_mental_health <- glm(
  high_mental_distress ~ food_insecure_binary + age + sex,
  data = scored_df,
  family = binomial(link = "logit")
)

table6_logit_mental <- tbl_regression(
  logit_mental_health,
  exponentiate = TRUE,
  label = list(
    food_insecure_binary ~ "Food Security Status",
    age ~ "Age in years",
    sex ~ "Sex"
  )
) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 6: Binary Logistic Regression Predicting High Psychological Distress (Objective 3)**")

# 2. Model 2 (Objective 4): Food Insecurity Predicting Academic Disruption Score
# Using multiple linear regression to avoid zero-cell separation issues
linear_academic <- lm(
  acad_disruption_score ~ food_insecure_binary + age + sex,
  data = scored_df
)

table7_logit_acad <- tbl_regression(
  linear_academic,
  label = list(
    food_insecure_binary ~ "Food Security Status",
    age ~ "Age in years",
    sex ~ "Sex"
  )
) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 7: Multivariable Regression Predicting Academic Disruption (Objective 4)**")

# ------------------------------------------------------------------------------
# STEP 7: Export ALL Tables to Microsoft Word (.docx)
# ------------------------------------------------------------------------------
if (!dir.exists("outputs")) dir.create("outputs")

# Summary & Item-Level Questionnaires
table1_demo %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_1_Demographics.docx")
table2_fies %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_2_Food_Insecurity_Prevalence.docx")
table3_mental_health %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_3_Mental_Health_Profile.docx")
table3b_fies_items %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_3B_ALL_8_FIES_Items.docx")
table3c_dass_items %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_3C_ALL_21_DASS_Items.docx")
table3d_acad_items %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_3D_ALL_10_Academic_Items.docx")

# Inferential & Bivariate Testing
table4_ho1 %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_4_Hypothesis_1_Mental_Health.docx")
table5_ho2 %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_5_Hypothesis_2_CGPA.docx")
table5b_demo_bivariate %>% as_flex_table() %>% save_as_docx(path = "outputs/Table_5B_Demographics_Bivariate.docx")

# Multivariable Regressions
table6_logit_mental %>% as_flex_table() %>% save_as_docx(path = "~/MO/Table_6_Logistic_Regression_Mental_Health.docx")
table7_logit_acad %>% as_flex_table() %>% save_as_docx(path = "~/MO/Table_7_Logistic_Regression_Academic_Disruption.docx")

cat("\n==============================================================================\n")
cat("SUCCESS: All 11 tables generated for EVERY item and exported to /outputs!\n")
cat("==============================================================================\n")