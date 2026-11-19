# ==============================================================================
# COMPLETE QUESTION-BY-QUESTION & OBJECTIVE-BY-OBJECTIVE R ANALYSIS SCRIPT
# Project: Double Burden of Malnutrition (DBM) Among Mother–Child Pairs in Ibadan
# Sample: N = 301 Eligible Approached -> N = 238 Consented (Ojoo=111, Mokola=127)
# ==============================================================================

# Install packages once if not yet installed:
# install.packages(c("readxl", "tidyverse", "anthro", "gtsummary", "flextable", "officer", "writexl"))

library(readxl)
library(tidyverse)
library(anthro)
library(gtsummary)
library(flextable)
library(officer)
library(writexl)

# ==============================================================================
# PART 0: IMPORT DATA & PARTICIPATION FLOW (N = 301 -> N = 238)
# ==============================================================================
file_path <- file.choose() # Select: DBM_Questionnaire_-_Mother-Child_Pairs_Barracks_Ibadan...xlsx
df_raw <- read_excel(file_path, sheet = 1)

cat("\n================ PART 0: PARTICIPATION FLOW (N = 301) ================\n")
print(addmargins(table(
  Barracks = df_raw[[3]],
  Consent  = df_raw[[10]],
  useNA    = "ifany"
)))

# Filter strictly to eligible & consented participants (N = 238, retaining age disparity)
df <- df_raw %>%
  filter(.[[10]] == "Yes") %>%
  mutate(
    Barracks = factor(.[[3]],
                      levels = c("Ojoo", "MOKOLA"),
                      labels = c("Ojoo Barracks", "Mokola Barracks"))
  )

cat("\nConfirmed Analytical Sample Size: N =", nrow(df),
    "(Ojoo =", sum(df$Barracks == "Ojoo Barracks"),
    "| Mokola =", sum(df$Barracks == "Mokola Barracks"), ")\n")

# Helper function to print uncollapsed question-by-question frequencies (Ojoo | Mokola | Total)
print_question_breakdown <- function(data, col_indices, section_title) {
  cat("\n====================================================================\n")
  cat(section_title, "\n")
  cat("====================================================================\n")
  
  n_oj <- sum(data$Barracks == "Ojoo Barracks")
  n_mk <- sum(data$Barracks == "Mokola Barracks")
  n_tt <- nrow(data)
  
  for (idx in col_indices) {
    q_name <- colnames(data)[idx]
    vals   <- data[[idx]]
    nn     <- sum(!is.na(vals))
    cat(sprintf("\n[Col %03d] %s (Answered: %d/%d)\n", idx, q_name, nn, n_tt))
    
    if (nn == 0) {
      cat("   -> All empty/blank\n")
      next
    }
    
    # Continuous summary if >15 unique numeric values, otherwise exact individual counts
    if (is.numeric(vals) && n_distinct(vals, na.rm = TRUE) > 15) {
      for (grp in c("Ojoo Barracks", "Mokola Barracks", "Total")) {
        v <- if (grp == "Total") na.omit(vals) else na.omit(vals[data$Barracks == grp])
        if (length(v) > 0) {
          cat(sprintf("   %-16s (n=%d): Mean=%.2f, SD=%.2f, Median=%.2f, IQR=(%.2f-%.2f), Min=%.2f, Max=%.2f\n",
                      grp, length(v), mean(v), sd(v), median(v),
                      quantile(v, 0.25), quantile(v, 0.75), min(v), max(v)))
        }
      }
    } else {
      u_vals <- names(sort(table(vals), decreasing = TRUE))
      for (uv in u_vals) {
        o_c <- sum(data$Barracks == "Ojoo Barracks" & as.character(vals) == uv, na.rm = TRUE)
        m_c <- sum(data$Barracks == "Mokola Barracks" & as.character(vals) == uv, na.rm = TRUE)
        t_c <- sum(as.character(vals) == uv, na.rm = TRUE)
        cat(sprintf("   %-45s: Ojoo=%3d (%5.1f%%) | Mokola=%3d (%5.1f%%) | Total=%3d (%5.1f%%)\n",
                    uv, o_c, 100 * o_c / n_oj, m_c, 100 * m_c / n_mk, t_c, 100 * t_c / n_tt))
      }
    }
  }
}

# ==============================================================================
# OBJECTIVE 1: EVERY SINGLE QUESTION UNCOLLAPSED (SECTIONS B, C, D, E, F, G, H, I, L)
# ==============================================================================

# 1A. Section B & C: Individual Socio-Demographic & Household Asset Questions
print_question_breakdown(
  df,
  c(12, 14, 15, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 30, 31, 32, 33, 35,
    38:49, 50, 52, 54, 55, 56),
  "OBJECTIVE 1A: SECTION B & C (INDIVIDUAL SOCIO-DEMOGRAPHIC & ASSET QUESTIONS)"
)

# 1B. Section D: Every Individual WASH Question (D1 - D14)
print_question_breakdown(
  df,
  c(58, 60, 61, 62, 64, 65, 66, 67, 68, 69, 70, 71, 72, 74:78, 79, 81),
  "OBJECTIVE 1B: SECTION D (EVERY INDIVIDUAL WASH QUESTION D1–D14)"
)

# 1C. Section E: Every Individual HFIAS Question (E1 - E9 + Calculated Score)
print_question_breakdown(
  df,
  83:101,
  "OBJECTIVE 1C: SECTION E (EVERY INDIVIDUAL HFIAS QUESTION E1–E9)"
)

# 1D. Section F: Every Individual Maternal Autonomy Question (F1 - F5)
print_question_breakdown(
  df,
  c(103, 105, 107, 109, 110),
  "OBJECTIVE 1D: SECTION F (EVERY INDIVIDUAL MATERNAL AUTONOMY QUESTION F1–F5)"
)

# 1E. Section G: Every Single Maternal Dietary Item (G1 - G29 + MDD-W Score)
print_question_breakdown(
  df,
  113:145,
  "OBJECTIVE 1E: SECTION G (EVERY SINGLE MDD-W DIETARY ITEM G1–G29)"
)

# 1F(i). Section H: Every Child Feeding Question for Children Aged 0–23 Months (n = 56)
df_u24 <- df %>% filter(as.numeric(.[[15]]) < 24)
print_question_breakdown(
  df_u24,
  c(149, 150, 151, 152, 153, 155:163, 167:184, 186, 188:193),
  "OBJECTIVE 1F(i): SECTION H (EVERY IYCF QUESTION FOR CHILDREN 0–23 MONTHS, n = 56)"
)

# 1F(ii). Section I: Every Child Feeding Question for Children Aged 24+ Months (n = 182)
df_o24 <- df %>% filter(as.numeric(.[[15]]) >= 24)
print_question_breakdown(
  df_o24,
  c(197:203, 205, 206),
  "OBJECTIVE 1F(ii): SECTION I (EVERY FEEDING QUESTION FOR CHILDREN 24+ MONTHS, n = 182)"
)

# 1G. Section L: Recent Child Morbidity (L1 - L3, N = 238)
print_question_breakdown(
  df,
  c(234, 235, 236),
  "OBJECTIVE 1G: SECTION L (RECENT CHILD MORBIDITY L1–L3, N = 238)"
)

# ==============================================================================
# OBJECTIVE 2: MATERNAL NUTRITIONAL STATUS (EXACT BMI + 95% CIs, N = 238)
# (Re-derives WHO BMI classes directly from Col 214 to fix 26 Kobo tick-box typos)
# ==============================================================================
df <- df %>%
  mutate(
    m_weight     = as.numeric(.[[210]]), # Col 210: Maternal average weight (kg)
    m_height     = as.numeric(.[[213]]), # Col 213: Maternal average height (cm)
    maternal_bmi = as.numeric(.[[214]]), # Col 214: Maternal BMI (kg/m²)
    maternal_bmi_class = factor(
      case_when(
        maternal_bmi < 18.5  ~ "Underweight (<18.5)",
        maternal_bmi < 25.0  ~ "Normal (18.5–24.9)",
        maternal_bmi < 30.0  ~ "Overweight (25.0–29.9)",
        maternal_bmi >= 30.0 ~ "Obese (≥30.0)"
      ),
      levels = c("Underweight (<18.5)", "Normal (18.5–24.9)", "Overweight (25.0–29.9)", "Obese (≥30.0)")
    ),
    maternal_ow_ob = if_else(maternal_bmi >= 25.0, 1L, 0L)
  )

cat("\n====================================================================\n")
cat("OBJECTIVE 2: MATERNAL NUTRITIONAL STATUS (WHO BMI CLASSIFICATION)\n")
cat("====================================================================\n")
print(addmargins(table(Barracks = df$Barracks, BMI_Class = df$maternal_bmi_class)))

# ==============================================================================
# OBJECTIVE 3 & OBJECTIVE 4: CHILD Z-SCORES (WHO 2006) & HOUSEHOLD DBM PREVALENCE
# ==============================================================================
df <- df %>%
  mutate(
    dob         = as.Date(.[[13]]),                       # Col 13: K2 Date of birth
    doi         = as.Date(.[[7]]),                        # Col 7: Date of interview
    age_days    = as.numeric(doi - dob),
    sex_num     = if_else(.[[12]] == "Male", 1L, 2L),     # Col 12: K1 Sex
    c_weight    = as.numeric(.[[220]]),                   # Col 220: Child average weight (kg)
    c_lenhei    = as.numeric(.[[223]]),                   # Col 223: Child average length/height (cm)
    measure_pos = if_else(.[[224]] == "Lying (length)", "l", "h") # Col 224: Position used
  )

who_z <- anthro_zscores(
  sex = df$sex_num, age = df$age_days, is_age_in_month = FALSE,
  weight = df$c_weight, lenhei = df$c_lenhei, measure = df$measure_pos
)

df <- df %>%
  mutate(
    # Populate exact z-score columns (applying WHO plausibility flags per Section 3.9)
    HAZ = if_else(who_z$flen == 0, round(who_z$zlen, 2), NA_real_),
    WAZ = if_else(who_z$fwei == 0, round(who_z$zwei, 2), NA_real_),
    WHZ = if_else(who_z$fwfl == 0, round(who_z$zwfl, 2), NA_real_),
    
    `HAZ/LAZ (z-score value)` = HAZ,
    `WAZ (z-score value)`     = WAZ,
    `WHZ/BMIZ (z-score value, as age-appropriate)` = WHZ,
    
    # Objective 3 Binary Indicators
    stunted     = if_else(!is.na(HAZ) & HAZ < -2, 1L, 0L, missing = NA_integer_),
    sev_stunted = if_else(!is.na(HAZ) & HAZ < -3, 1L, 0L, missing = NA_integer_),
    underweight = if_else(!is.na(WAZ) & WAZ < -2, 1L, 0L, missing = NA_integer_),
    sev_underwt = if_else(!is.na(WAZ) & WAZ < -3, 1L, 0L, missing = NA_integer_),
    wasted      = if_else(!is.na(WHZ) & WHZ < -2, 1L, 0L, missing = NA_integer_),
    sev_wasted  = if_else(!is.na(WHZ) & WHZ < -3, 1L, 0L, missing = NA_integer_),
    
    child_undernourished = case_when(
      stunted == 1 | underweight == 1 | wasted == 1 ~ 1L,
      is.na(HAZ) & is.na(WAZ) & is.na(WHZ) ~ NA_integer_,
      TRUE ~ 0L
    ),
    
    # Objective 4: Household-Level Double Burden of Malnutrition (DBM)
    DBM = case_when(
      is.na(child_undernourished) ~ NA_integer_,
      maternal_ow_ob == 1 & child_undernourished == 1 ~ 1L,
      TRUE ~ 0L
    ),
    DBM_status = factor(DBM, levels = c(0, 1), labels = c("No DBM", "DBM Present"))
  )

# Compute exact Prevalence + 95% Confidence Intervals (per Section 3.9)
calc_prev_95ci <- function(data, var_col, label) {
  by_loc <- data %>%
    filter(!is.na(.data[[var_col]])) %>%
    group_by(Barracks) %>%
    summarise(
      Indicator = label,
      Cases = sum(.data[[var_col]] == 1),
      Valid_N = n(),
      Prevalence_Pct = round(100 * Cases / Valid_N, 1),
      CI_95 = sprintf("%.1f%% – %.1f%%",
                      100 * prop.test(Cases, Valid_N)$conf.int[1],
                      100 * prop.test(Cases, Valid_N)$conf.int[2]),
      .groups = "drop"
    )
  
  tot_loc <- data %>%
    filter(!is.na(.data[[var_col]])) %>%
    summarise(
      Barracks = factor("Total (N = 238)"),
      Indicator = label,
      Cases = sum(.data[[var_col]] == 1),
      Valid_N = n(),
      Prevalence_Pct = round(100 * Cases / Valid_N, 1),
      CI_95 = sprintf("%.1f%% – %.1f%%",
                      100 * prop.test(Cases, Valid_N)$conf.int[1],
                      100 * prop.test(Cases, Valid_N)$conf.int[2])
    )
  
  bind_rows(by_loc, tot_loc)
}

obj2_3_4_table <- bind_rows(
  calc_prev_95ci(df, "maternal_ow_ob", "Objective 2: Maternal Overweight/Obesity (BMI ≥ 25.0 kg/m²)"),
  calc_prev_95ci(df, "stunted", "Objective 3: Child Stunting (HAZ < -2 SD)"),
  calc_prev_95ci(df, "sev_stunted", "Objective 3: Severe Child Stunting (HAZ < -3 SD)"),
  calc_prev_95ci(df, "underweight", "Objective 3: Child Underweight (WAZ < -2 SD)"),
  calc_prev_95ci(df, "sev_underwt", "Objective 3: Severe Child Underweight (WAZ < -3 SD)"),
  calc_prev_95ci(df, "wasted", "Objective 3: Child Wasting (WHZ < -2 SD)"),
  calc_prev_95ci(df, "sev_wasted", "Objective 3: Severe Child Wasting (WHZ < -3 SD)"),
  calc_prev_95ci(df, "child_undernourished", "Objective 3: Any Child Undernutrition (HAZ, WAZ, or WHZ < -2 SD)"),
  calc_prev_95ci(df, "DBM", "Objective 4: Household Double Burden of Malnutrition (DBM)")
)

cat("\n====================================================================\n")
cat("OBJECTIVES 2, 3 & 4: PREVALENCE SUMMARY WITH 95% CONFIDENCE INTERVALS\n")
cat("====================================================================\n")
print(obj2_3_4_table, n = 30)

# ==============================================================================
# OBJECTIVE 5: BIVARIATE & MULTIVARIABLE LOGISTIC REGRESSION FOR DBM
# ==============================================================================
df <- df %>%
  mutate(
    m_age         = as.numeric(.[[17]]),
    c_age         = as.numeric(.[[15]]),
    c_sex         = factor(.[[12]]),
    m_edu         = factor(.[[24]]),
    m_occ         = factor(.[[25]]),
    hh_size       = as.numeric(.[[31]]),
    hh_income     = factor(.[[35]]),
    hfias_score   = as.numeric(.[[101]]),
    mdd_w_score   = as.numeric(.[[144]]),
    mdd_w_met     = factor(as.numeric(.[[145]]), levels = c(0, 1), labels = c("No (<5 groups)", "Yes (≥5 groups)")),
    water_insuff  = factor(.[[61]]),
    toilet_shared = factor(.[[64]]),
    soap_avail    = factor(.[[69]]),
    diarrhoea_2wk = factor(.[[234]]),
    aut_f1 = case_when(.[[103]] == "Respondent alone" ~ 2, .[[103]] == "Respondent and husband/partner jointly" ~ 1, TRUE ~ 0),
    aut_f2 = case_when(.[[105]] == "Respondent alone" ~ 2, .[[105]] == "Respondent and husband/partner jointly" ~ 1, TRUE ~ 0),
    aut_f3 = case_when(.[[107]] == "Respondent alone" ~ 2, .[[107]] == "Respondent and husband/partner jointly" ~ 1, TRUE ~ 0),
    autonomy_score = aut_f1 + aut_f2 + aut_f3
  )

cat("\n====================================================================\n")
cat("OBJECTIVE 5A: BIVARIATE ASSOCIATIONS WITH HOUSEHOLD DBM (POOLED N = 238)\n")
cat("====================================================================\n")
tbl_biv_dbm <- df %>%
  filter(!is.na(DBM_status)) %>%
  select(
    DBM_status, Barracks, m_age, c_age, c_sex, m_edu, m_occ, hh_size, hh_income,
    hfias_score, mdd_w_score, mdd_w_met, autonomy_score,
    water_insuff, toilet_shared, soap_avail, diarrhoea_2wk
  ) %>%
  tbl_summary(
    by = DBM_status,
    statistic = list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p() %>%
  bold_labels() %>%
  bold_p(t = 0.05)

print(tbl_biv_dbm)

cat("\n====================================================================\n")
cat("OBJECTIVE 5B: MULTIVARIABLE LOGISTIC REGRESSION FOR DBM (aOR & 95% CI)\n")
cat("====================================================================\n")
dbm_logit <- glm(
  DBM ~ m_age + c_age + c_sex + Barracks + hfias_score + mdd_w_score +
    autonomy_score + water_insuff + diarrhoea_2wk,
  data = df,
  family = binomial(link = "logit")
)

tbl_reg_dbm <- tbl_regression(dbm_logit, exponentiate = TRUE) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

print(tbl_reg_dbm)

# ==============================================================================
# EXPORT ALL CLEANED DATA & WORD TABLES
# ==============================================================================
write_xlsx(
  list(
    "Cleaned_Dataset_N238" = df,
    "Objectives_2_3_4_95CI" = obj2_3_4_table
  ),
  path = "DBM_Complete_Analysis_Objectives_1_to_5.xlsx"
)

tbl_biv_dbm %>% as_flex_table() %>% autofit() %>%
  save_as_docx(path = "Objective_5A_Bivariate_DBM.docx")

tbl_reg_dbm %>% as_flex_table() %>% autofit() %>%
  save_as_docx(path = "Objective_5B_Multivariable_Logistic_Regression_DBM.docx")

cat("\nALL DONE! Exported Excel workbook and Word tables to your working directory.\n")

# ==============================================================================
# EXPORT ALL DBM RESULTS (OBJECTIVES 1 TO 5) INTO ONE MASTER WORD DOCUMENT
# ==============================================================================

library(tidyverse)
library(gtsummary)
library(flextable)
library(officer)

# 1. Helper function to turn any group of uncollapsed questions into a formatted flextable
build_uncollapsed_flextable <- function(data, col_indices) {
  n_oj <- sum(data$Barracks == "Ojoo Barracks")
  n_mk <- sum(data$Barracks == "Mokola Barracks")
  n_tt <- nrow(data)
  
  rows_list <- list()
  
  for (idx in col_indices) {
    q_name <- colnames(data)[idx]
    vals   <- data[[idx]]
    nn     <- sum(!is.na(vals))
    if (nn == 0) next
    
    # Question header row
    rows_list[[length(rows_list) + 1]] <- tibble(
      Question_Response = paste0(q_name, " (n = ", nn, ")"),
      Ojoo_Barracks     = "",
      Mokola_Barracks   = "",
      Total             = ""
    )
    
    if (is.numeric(vals) && n_distinct(vals, na.rm = TRUE) > 15) {
      v_oj <- na.omit(vals[data$Barracks == "Ojoo Barracks"])
      v_mk <- na.omit(vals[data$Barracks == "Mokola Barracks"])
      v_tt <- na.omit(vals)
      
      rows_list[[length(rows_list) + 1]] <- tibble(
        Question_Response = "   Mean ± SD (Median)",
        Ojoo_Barracks     = if (length(v_oj) > 0) sprintf("%.2f ± %.2f (%.1f)", mean(v_oj), sd(v_oj), median(v_oj)) else "—",
        Mokola_Barracks   = if (length(v_mk) > 0) sprintf("%.2f ± %.2f (%.1f)", mean(v_mk), sd(v_mk), median(v_mk)) else "—",
        Total             = if (length(v_tt) > 0) sprintf("%.2f ± %.2f (%.1f)", mean(v_tt), sd(v_tt), median(v_tt)) else "—"
      )
    } else {
      u_vals <- names(sort(table(vals), decreasing = TRUE))
      for (uv in u_vals) {
        o_c <- sum(data$Barracks == "Ojoo Barracks" & as.character(vals) == uv, na.rm = TRUE)
        m_c <- sum(data$Barracks == "Mokola Barracks" & as.character(vals) == uv, na.rm = TRUE)
        t_c <- sum(as.character(vals) == uv, na.rm = TRUE)
        
        rows_list[[length(rows_list) + 1]] <- tibble(
          Question_Response = paste0("   ", uv),
          Ojoo_Barracks     = sprintf("%d (%.1f%%)", o_c, 100 * o_c / n_oj),
          Mokola_Barracks   = sprintf("%d (%.1f%%)", m_c, 100 * m_c / n_mk),
          Total             = sprintf("%d (%.1f%%)", t_c, 100 * t_c / n_tt)
        )
      }
    }
  }
  
  bind_rows(rows_list) %>%
    flextable() %>%
    set_header_labels(
      Question_Response = "Question / Response Category",
      Ojoo_Barracks     = paste0("Ojoo Barracks\n(n = ", n_oj, ")"),
      Mokola_Barracks   = paste0("Mokola Barracks\n(n = ", n_mk, ")"),
      Total             = paste0("Total\n(N = ", n_tt, ")")
    ) %>%
    bold(i = ~ Ojoo_Barracks == "", j = 1) %>%
    bg(i = ~ Ojoo_Barracks == "", bg = "#F0F4F8") %>%
    theme_booktabs() %>%
    fontsize(size = 9, part = "all") %>%
    autofit()
}

# 2. Build all individual section flextables
ft_part0 <- as.data.frame.matrix(addmargins(table(Barracks = df_raw[[3]], Consent = df_raw[[10]]))) %>%
  rownames_to_column("Barracks / Location") %>%
  flextable() %>%
  theme_booktabs() %>%
  autofit()

ft_obj1a <- build_uncollapsed_flextable(df, c(12, 14, 15, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 30, 31, 32, 33, 35, 38:49, 50, 52, 54, 55, 56))
ft_obj1b <- build_uncollapsed_flextable(df, c(58, 60, 61, 62, 64, 65, 66, 67, 68, 69, 70, 71, 72, 74:78, 79, 81))
ft_obj1c <- build_uncollapsed_flextable(df, 83:101)
ft_obj1d <- build_uncollapsed_flextable(df, c(103, 105, 107, 109, 110))
ft_obj1e <- build_uncollapsed_flextable(df, 113:145)
ft_obj1f_u24 <- build_uncollapsed_flextable(df_u24, c(149, 150, 151, 152, 153, 155:163, 167:184, 186, 188:193))
ft_obj1f_o24 <- build_uncollapsed_flextable(df_o24, c(197:203, 205, 206))
ft_obj1g <- build_uncollapsed_flextable(df, c(234, 235, 236))

ft_obj2_bmi <- as.data.frame.matrix(addmargins(table(Barracks = df$Barracks, BMI_Class = df$maternal_bmi_class))) %>%
  rownames_to_column("Barracks") %>%
  flextable() %>%
  theme_booktabs() %>%
  autofit()

ft_obj234_ci <- obj2_3_4_table %>%
  flextable() %>%
  theme_booktabs() %>%
  fontsize(size = 9, part = "all") %>%
  autofit()

ft_obj5_biv <- tbl_biv_dbm %>% as_flex_table() %>% fontsize(size = 9, part = "all") %>% autofit()
ft_obj5_reg <- tbl_reg_dbm %>% as_flex_table() %>% fontsize(size = 9, part = "all") %>% autofit()

# 3. Combine EVERY result sequentially into ONE single Word document
master_doc <- read_docx() %>%
  body_add_par("CHAPTER FOUR: COMPLETE DATA ANALYSIS RESULTS (DBM IBADAN BARRACKS)", style = "heading 1") %>%
  body_add_par("Part 0: Study Participation and Non-Response Flow (N = 301 Approached)", style = "heading 2") %>%
  body_add_flextable(ft_part0) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 1A: Socio-Demographic and Household Asset Characteristics (Sections B & C)", style = "heading 2") %>%
  body_add_flextable(ft_obj1a) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 1B: Water, Sanitation and Hygiene (WASH) Profile (Section D)", style = "heading 2") %>%
  body_add_flextable(ft_obj1b) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 1C: Household Food Insecurity Access Scale (HFIAS) Profile (Section E)", style = "heading 2") %>%
  body_add_flextable(ft_obj1c) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 1D: Maternal Decision-Making Autonomy Profile (Section F)", style = "heading 2") %>%
  body_add_flextable(ft_obj1d) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 1E: Maternal Dietary Diversity (MDD-W) 24-Hour Recall (Section G)", style = "heading 2") %>%
  body_add_flextable(ft_obj1e) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 1F(i): Infant and Young Child Feeding (IYCF) Practices, 0–23 Months (Section H, n = 56)", style = "heading 2") %>%
  body_add_flextable(ft_obj1f_u24) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 1F(ii): Child Feeding Practices, 24+ Months (Section I, n = 182)", style = "heading 2") %>%
  body_add_flextable(ft_obj1f_o24) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 1G: Recent Child Morbidity (Section L, N = 238)", style = "heading 2") %>%
  body_add_flextable(ft_obj1g) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 2: Maternal Nutritional Status Distribution (WHO BMI Classification)", style = "heading 2") %>%
  body_add_flextable(ft_obj2_bmi) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objectives 2, 3 & 4: Maternal Overweight/Obesity, Child Undernutrition, and Household DBM Prevalence (with 95% CIs)", style = "heading 2") %>%
  body_add_flextable(ft_obj234_ci) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 5A: Bivariate Associations with Household Double Burden of Malnutrition (N = 238)", style = "heading 2") %>%
  body_add_flextable(ft_obj5_biv) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("Objective 5B: Multivariable Logistic Regression of Factors Associated with Household DBM", style = "heading 2") %>%
  body_add_flextable(ft_obj5_reg)

# 4. Save the single master Word document in your working directory
print(master_doc, target = "DBM_Ibadan_Barracks_All_Results_Objectives_1_to_5.docx")
cat("\nSUCCESS! All results saved into one Word file: 'DBM_Ibadan_Barracks_All_Results_Objectives_1_to_5.docx'\n")