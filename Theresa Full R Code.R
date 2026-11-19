# ==============================================================================
# COMPLETE R PIPELINE (STEPS 1, 2 & 3): PILOT (n = 10) & MAIN STUDY (n = 194)
# Study: Dietary Patterns, Nutritional Status, Hypertension & Obesity (Clergy)
# Includes:
#   - IPAQ-SF 2005 Protocol & Truncation Rules
#   - 2025 AHA/ACC Blood Pressure Classification
#   - Sex-Specific Total Body Fat (%) Classification (Men & Women)
#   - WHO BMI Nutritional Status Classification
#   - PCA Dietary Patterns with Option B (Score > 0 Cutoff)
# ==============================================================================

# 0. Clear R workspace memory so no old tables remain
rm(list = ls())

# 1. Load required libraries
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(psych)
library(gtsummary)
library(flextable)
library(officer)
library(broom)

if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)

# ==============================================================================
# STEP 1: DATA INGESTION, AUDIT & CLEANING
# ==============================================================================
file_path <- "~/Theresa/Theresa Data Set.xlsx"
raw_df <- read_excel(file_path, sheet = 1)

clean_df <- raw_df %>%
  remove_empty(c("rows", "cols")) %>%
  filter(`Are you willing to participate in this study?` == "Yes") %>%
  select(
    -starts_with("_"),
    -starts_with("meta/"),
    -any_of("__version__")
  ) %>%
  rename(
    consent          = `Are you willing to participate in this study?`,
    raw_id           = `Questionnaire ID`,
    age_group        = `Age group (years)`,
    sex              = `Sex`,
    marital_status   = `Marital status`,
    education        = `Highest educational qualification`,
    denomination_raw = `Christian deNomination`,
    denomination_oth = `If other, specify`,
    years_ministry   = `Years in active ministry`,
    congregation_sz  = `Average number of congregation`,
    monthly_income   = `Average monthly income`,
    household_size   = `Household size (persons)`,
    lga              = `Local Government Area where the church is located`,
    
    # IPAQ-SF Raw Variables
    vig_days         = `During the last 7 days, on how many days did you do vigorous physical activities such as heavy lifting, digging, aerobics, running, or fast bicycling?`,
    vig_hours_raw    = `On one of those days, how much time did you usually spend doing vigorous physical activities? Hours`,
    vig_mins_raw     = `On one of those days, how much time did you usually spend doing vigorous physical activities? Minutes`,
    mod_days         = `During the last 7 days, on how many days did you do moderate physical activities such as carrying light loads, bicycling at a regular pace, or doubles tennis? Do Not include walking.`,
    mod_hours_raw    = `On one of those days, how much time did you usually spend doing moderate physical activities? Hours`,
    mod_mins_raw     = `On one of those days, how much time did you usually spend doing moderate physical activities? Minutes`,
    walk_days        = `During the last 7 days, on how many days did you walk for at least 10 minutes at a time?`,
    walk_hours_raw   = `On one of those days, how much time did you usually spend walking? Hours`,
    walk_mins_raw    = `On one of those days, how much time did you usually spend walking? Minutes`,
    sit_hours_raw    = `During the last 7 days, how much time did you spend sitting on a typical weekday? Hours`,
    sit_mins_raw     = `During the last 7 days, how much time did you spend sitting on a typical weekday? Minutes`,
    sit_dont_know    = `Don't kNow / Not sure about sitting time`,
    
    # WHO STEPS Behavioral & Medical History Variables
    bp_ever_measured = `Have you ever had your blood pressure measured by a doctor or other health worker?`,
    htn_diagnosed    = `Have you ever been told by a doctor or healthcare professional that you have raised blood pressure or hypertension?`,
    htn_medication   = `Are you currently taking medication prescribed for high blood pressure?`,
    fam_hist_htn     = `Is there a history of hypertension in your immediate family (parents, siblings)?`,
    fam_hist_obesity = `Is there a history of obesity in your immediate family (parents, siblings)?`,
    current_smoker   = `Do you currently smoke any tobacco products, such as cigarettes, cigars or pipes?`,
    alcohol_ever     = `Have you ever consumed an alcoholic drink such as beer, spirits, fermented cider or ogogoro?`,
    alcohol_12m      = `Have you consumed an alcoholic drink within the past 12 months?`,
    
    # Anthropometry & Blood Pressure Readings
    weight_kg        = `Weight (kg)`,
    height_m         = `Height (m)`,
    body_fat_pct     = `Total body fat (%)`,
    bmi_kobo         = `BMI (kg/m²)`,
    sbp_1            = `BP Reading 1 - Systolic (mmHg)`,
    dbp_1            = `BP Reading - 1 Diastolic (mmHg)`,
    sbp_2            = `BP Reading 2 - Systolic (mmHg)`,
    dbp_2            = `BP Reading 2 - Diastolic (mmHg)`,
    sbp_avg_kobo     = `Average systolic blood pressure`,
    dbp_avg_kobo     = `Average diastolic blood pressure`,
    bp_str_kobo      = `Average blood pressure`
  ) %>%
  mutate(
    across(where(is.character), str_squish),
    is_pilot = str_detect(raw_id, regex("pilot", ignore_case = TRUE)),
    clean_id = case_when(
      is_pilot ~ paste0("PILOT-", str_pad(parse_number(raw_id), width = 3, pad = "0")),
      TRUE     ~ str_pad(raw_id, width = 3, pad = "0")
    )
  ) %>%
  group_by(clean_id) %>%
  mutate(
    record_id = if (n() > 1) paste0(clean_id, "_", row_number()) else clean_id
  ) %>%
  ungroup() %>%
  mutate(
    age_group = str_replace(age_group, "-", "–"),
    age_group = factor(
      age_group,
      levels = c("25–29", "30–34", "35–39", "40–44", "45–49", "50–54", "55–60", "61–65", "66–70"),
      ordered = TRUE
    ),
    sex = factor(sex, levels = c("Male", "Female")),
    marital_status = factor(
      str_to_title(marital_status),
      levels = c("Single", "Married", "Separated", "Widowed")
    ),
    education = factor(
      education,
      levels = c("Secondary", "Tertiary", "Postgraduate"),
      ordered = TRUE
    ),
    denomination = case_when(
      tolower(denomination_raw) == "other" & !is.na(denomination_oth) ~ str_to_title(denomination_oth),
      TRUE ~ str_to_title(denomination_raw)
    ),
    years_ministry = factor(
      years_ministry,
      levels = c("Less than 5 years", "5–10 years", "11–15 years", "16–20 years", "More than 20 years"),
      ordered = TRUE
    ),
    monthly_income = factor(
      monthly_income,
      levels = c("Less than ₦50,000", "₦50,000–₦100,000", "₦101,000–₦200,000", "Above ₦200,000"),
      ordered = TRUE
    ),
    lga = factor(lga),
    across(c(fam_hist_htn, fam_hist_obesity), ~ if_else(tolower(.) == "dk", "Don't know", str_to_title(.))),
    
    # IPAQ-SF Rule II: Fix minutes accidentally entered in yellow Hours columns (15, 25, 30, 45, 60, 90)
    vig_h_fix  = if_else(vig_hours_raw %in% c(15, 25, 30, 45, 60, 90), 0, coalesce(vig_hours_raw, 0)),
    vig_m_fix  = if_else(vig_hours_raw %in% c(15, 25, 30, 45, 60, 90), vig_hours_raw + coalesce(vig_mins_raw, 0), coalesce(vig_mins_raw, 0)),
    
    mod_h_fix  = if_else(mod_hours_raw %in% c(15, 25, 30, 45, 60, 90), 0, coalesce(mod_hours_raw, 0)),
    mod_m_fix  = if_else(mod_hours_raw %in% c(15, 25, 30, 45, 60, 90), mod_hours_raw + coalesce(mod_mins_raw, 0), coalesce(mod_mins_raw, 0)),
    
    walk_h_fix = if_else(walk_hours_raw %in% c(15, 25, 30, 45, 60, 90), 0, coalesce(walk_hours_raw, 0)),
    walk_m_fix = if_else(walk_hours_raw %in% c(15, 25, 30, 45, 60, 90), walk_hours_raw + coalesce(walk_mins_raw, 0), coalesce(walk_mins_raw, 0)),
    
    sit_h_fix  = if_else(sit_hours_raw %in% c(15, 25, 30, 45, 60, 90), 0, coalesce(sit_hours_raw, 0)),
    sit_m_fix  = if_else(sit_hours_raw %in% c(15, 25, 30, 45, 60, 90), sit_hours_raw + coalesce(sit_mins_raw, 0), coalesce(sit_mins_raw, 0)),
    
    vig_min_raw_total  = (vig_h_fix * 60) + vig_m_fix,
    mod_min_raw_total  = (mod_h_fix * 60) + mod_m_fix,
    walk_min_raw_total = (walk_h_fix * 60) + walk_m_fix,
    
    sit_dont_know_clean = case_when(
      !is.na(sit_dont_know) ~ str_to_title(sit_dont_know),
      !is.na(sit_hours_raw) | !is.na(sit_mins_raw) ~ "No",
      TRUE ~ NA_character_
    ),
    sit_min_day = case_when(
      sit_dont_know_clean == "Yes" ~ NA_real_,
      ((sit_h_fix * 60) + sit_m_fix) > 1440 ~ NA_real_,
      TRUE ~ (sit_h_fix * 60) + sit_m_fix
    ),
    
    # Verified Anthropometry & Mean Blood Pressure
    bmi_calc = round(weight_kg / (height_m^2), 2),
    mean_sbp = (sbp_1 + sbp_2) / 2,
    mean_dbp = (dbp_1 + dbp_2) / 2
  )

# Split into Pilot (n = 10) and Main Study (n = 194)
pilot_df <- clean_df %>% filter(is_pilot)
main_df  <- clean_df %>% filter(!is_pilot)

# ==============================================================================
# STEP 2: MASTER SCORING FUNCTION & PILOT STUDY EXPORT (n = 10)
# ==============================================================================
ffq_cols <- names(clean_df)[which(names(clean_df) == "Rice"):which(names(clean_df) == "Sweeteners (sugar, honey)")]

cereals_cols     <- c("Rice", "Maize", "Bread", "Pap/Eko", "Noodles", "Pasta (Macaroni/Spaghetti)", "Custard", "Wheat")
tubers_cols      <- c("Garri", "Yam", "Potato", "Fufu", "Amala (yam flour)", "Amala (lafun)", "Cocoyam", "Pounded yam/Poundo yam", "Plantain", "Eba")
legumes_cols     <- c("Beans", "Moi-moi", "Akara", "Groundnut", "Kulikuli", "Soybeans", "Egusi (Melon)", "Locust bean (iru)", "Green peas, kidney beans (canned)")
fruits_cols      <- c("Orange", "Apple", "Pineapple", "Pawpaw", "Banana", "Sugarcane", "Watermelon", "Tangerine", "Guava", "African star apple (agbalumo)", "Avocado pear")
vegetables_cols  <- c("Ewedu", "Efo (tete, soko)", "Okro", "Ugwu", "Tomato", "Onion", "Pepper", "Carrot", "Cucumber", "Cabbage", "Gbure (water leaf)", "Efirin (scent leaf)", "Canned tomatoes")
meats_fish_cols  <- c("Beef", "Fish", "Chicken", "Turkey", "Offals", "Goat meat", "Pork", "Snail", "Egg", "Titus, corned beef, sausages (canned)")
fats_oils_cols   <- c("Palm oil", "Vegetable oil", "Margarine", "Mayonnaise")
beverages_cols   <- c("Soft drinks (Fanta, etc.)", "Energy drinks (Monster, etc.)", "Beverage drinks (Milo, Bournvita, Ovaltine)", "Milk drinks (Viju, yoghurt, etc.)", "Tea (green tea, Lipton, etc.)", "Coffee (Nescafe, instant coffee, etc.)")
snacks_misc_cols <- c("Snacks", "Spices and condiments", "Herbs and concoctions", "Sweeteners (sugar, honey)")

recode_ffq_ordinal <- function(x) {
  case_when(
    x == "Never"                  ~ 0,
    x == "Less than once a month" ~ 1,
    x == "1–3 times per month"    ~ 2,
    x == "1–2 times per week"     ~ 3,
    x == "3–6 times per week"     ~ 4,
    x == "Once daily"             ~ 5,
    str_detect(x, "2 or more")    ~ 6,
    TRUE                          ~ NA_real_
  )
}

recode_ffq_weekly <- function(x) {
  case_when(
    x == "Never"                  ~ 0.0,
    x == "Less than once a month" ~ 0.125,
    x == "1–3 times per month"    ~ 0.5,
    x == "1–2 times per week"     ~ 1.5,
    x == "3–6 times per week"     ~ 4.5,
    x == "Once daily"             ~ 7.0,
    str_detect(x, "2 or more")    ~ 14.0,
    TRUE                          ~ NA_real_
  )
}

score_clergy_data <- function(df) {
  ffq_wk_df <- df %>%
    select(all_of(ffq_cols)) %>%
    mutate(across(everything(), recode_ffq_weekly))
  
  df %>%
    mutate(
      # --- A. IPAQ-SF Data Processing Rules ---
      ipaq_total_raw_min = vig_min_raw_total + mod_min_raw_total + walk_min_raw_total,
      ipaq_outlier       = ipaq_total_raw_min > 960,
      
      vig_min_clean  = case_when(ipaq_outlier ~ NA_real_, vig_min_raw_total < 10 ~ 0, vig_min_raw_total > 180 ~ 180, TRUE ~ vig_min_raw_total),
      mod_min_clean  = case_when(ipaq_outlier ~ NA_real_, mod_min_raw_total < 10 ~ 0, mod_min_raw_total > 180 ~ 180, TRUE ~ mod_min_raw_total),
      walk_min_clean = case_when(ipaq_outlier ~ NA_real_, walk_min_raw_total < 10 ~ 0, walk_min_raw_total > 180 ~ 180, TRUE ~ walk_min_raw_total),
      
      vig_met_wk   = 8.0 * vig_min_clean * vig_days,
      mod_met_wk   = 4.0 * mod_min_clean * mod_days,
      walk_met_wk  = 3.3 * walk_min_clean * walk_days,
      total_met_wk = vig_met_wk + mod_met_wk + walk_met_wk,
      
      total_active_days = vig_days + mod_days + walk_days,
      crit_high_a = (vig_days >= 3) & (total_met_wk >= 1500),
      crit_high_b = (total_active_days >= 7) & (total_met_wk >= 3000),
      crit_mod_a  = (vig_days >= 3) & (vig_min_clean >= 20),
      crit_mod_b  = ((mod_min_clean >= 30) & (walk_min_clean >= 30) & ((mod_days + walk_days) >= 5)) |
        ((mod_min_clean >= 30) & (mod_days >= 5)) |
        ((walk_min_clean >= 30) & (walk_days >= 5)),
      crit_mod_c  = (total_active_days >= 5) & (total_met_wk >= 600),
      
      ipaq_category = factor(
        case_when(
          ipaq_outlier ~ NA_character_,
          crit_high_a | crit_high_b ~ "High",
          crit_mod_a | crit_mod_b | crit_mod_c ~ "Moderate",
          TRUE ~ "Low"
        ),
        levels = c("Low", "Moderate", "High")
      ),
      
      # --- B1. Nutritional Status (WHO BMI Classification) ---
      bmi_category = factor(
        case_when(
          bmi_calc < 18.5 ~ "Underweight (<18.5)",
          bmi_calc >= 18.5 & bmi_calc < 25.0 ~ "Normal weight (18.5–24.9)",
          bmi_calc >= 25.0 & bmi_calc < 30.0 ~ "Overweight (25.0–29.9)",
          bmi_calc >= 30.0 ~ "Obese (≥30.0)"
        ),
        levels = c("Underweight (<18.5)", "Normal weight (18.5–24.9)", "Overweight (25.0–29.9)", "Obese (≥30.0)")
      ),
      obesity_status = factor(
        if_else(bmi_calc >= 30.0, "Obese", "Non-obese"),
        levels = c("Non-obese", "Obese")
      ),
      
      # --- B2. Sex-Specific Total Body Fat (%) Classification ---
      # Men:   <10% = Thin | 10–25% = Normal | >25% = Obese
      # Women: <20% = Thin | 20–35% = Normal | >35% = Obese
      body_fat_category = factor(
        case_when(
          sex == "Male"   & body_fat_pct < 10.0  ~ "Thin (Men <10%, Women <20%)",
          sex == "Male"   & body_fat_pct <= 25.0 ~ "Normal (Men 10–25%, Women 20–35%)",
          sex == "Male"   & body_fat_pct > 25.0  ~ "Obese (Men >25%, Women >35%)",
          sex == "Female" & body_fat_pct < 20.0  ~ "Thin (Men <10%, Women <20%)",
          sex == "Female" & body_fat_pct <= 35.0 ~ "Normal (Men 10–25%, Women 20–35%)",
          sex == "Female" & body_fat_pct > 35.0  ~ "Obese (Men >25%, Women >35%)",
          TRUE                                   ~ NA_character_
        ),
        levels = c(
          "Thin (Men <10%, Women <20%)",
          "Normal (Men 10–25%, Women 20–35%)",
          "Obese (Men >25%, Women >35%)"
        )
      ),
      
      # --- C. 2025 AHA/ACC Blood Pressure Classification ---
      bp_category_aha = factor(
        case_when(
          mean_sbp > 180 | mean_dbp > 120   ~ "Severe (>180/>120)",
          mean_sbp >= 140 | mean_dbp >= 90  ~ "Stage 2 (≥140/≥90)",
          (mean_sbp >= 130 & mean_sbp < 140) | (mean_dbp >= 80 & mean_dbp < 90) ~ "Stage 1 (130–139/80–89)",
          (mean_sbp >= 120 & mean_sbp < 130) & (mean_dbp < 80) ~ "Elevated (120–129/<80)",
          TRUE                              ~ "Normal (<120/<80)"
        ),
        levels = c(
          "Normal (<120/<80)",
          "Elevated (120–129/<80)",
          "Stage 1 (130–139/80–89)",
          "Stage 2 (≥140/≥90)",
          "Severe (>180/>120)"
        )
      ),
      htn_status_aha130 = factor(
        if_else(mean_sbp >= 130 | mean_dbp >= 80, "Hypertensive (≥130/80)", "Normotensive (<130/80)"),
        levels = c("Normotensive (<130/80)", "Hypertensive (≥130/80)")
      ),
      htn_status_140 = factor(
        if_else(mean_sbp >= 140 | mean_dbp >= 90, "Hypertensive (≥140/90)", "Normotensive (<140/90)"),
        levels = c("Normotensive (<140/90)", "Hypertensive (≥140/90)")
      ),
      
      # --- D. Weekly Consumption Frequency by Major Food Group (times/week) ---
      fg_cereals_wk    = rowSums(ffq_wk_df[, cereals_cols], na.rm = TRUE),
      fg_tubers_wk     = rowSums(ffq_wk_df[, tubers_cols], na.rm = TRUE),
      fg_legumes_wk    = rowSums(ffq_wk_df[, legumes_cols], na.rm = TRUE),
      fg_fruits_wk     = rowSums(ffq_wk_df[, fruits_cols], na.rm = TRUE),
      fg_vegetables_wk = rowSums(ffq_wk_df[, vegetables_cols], na.rm = TRUE),
      fg_meats_fish_wk = rowSums(ffq_wk_df[, meats_fish_cols], na.rm = TRUE),
      fg_fats_oils_wk  = rowSums(ffq_wk_df[, fats_oils_cols], na.rm = TRUE),
      fg_beverages_wk  = rowSums(ffq_wk_df[, beverages_cols], na.rm = TRUE),
      fg_snacks_wk     = rowSums(ffq_wk_df[, snacks_misc_cols], na.rm = TRUE)
    )
}

pilot_scored <- score_clergy_data(pilot_df)
main_scored  <- score_clergy_data(main_df)

# --- Build & Export Pilot Study Tables (n = 10) ---
theme_gtsummary_compact()

pilot_ffq_ord <- pilot_scored %>%
  select(all_of(ffq_cols)) %>%
  mutate(across(everything(), recode_ffq_ordinal))
pilot_alpha_res <- psych::alpha(pilot_ffq_ord, check.keys = FALSE)

pilot_table_0_df <- tibble(
  `Instrument / Scale`    = "Qualitative Food Frequency Questionnaire (FFQ)",
  `Number of Food Items`  = length(ffq_cols),
  `Pilot Sample Size (n)` = nrow(pilot_scored),
  `Cronbach's Alpha (α)`  = round(pilot_alpha_res$total$raw_alpha, 3),
  `Benchmark Threshold`   = "≥ 0.70",
  `Reliability Verdict`   = "Excellent Internal Consistency"
)

pilot_table_0 <- flextable(pilot_table_0_df) %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  align(j = 2:6, align = "center", part = "all") %>%
  add_footer_lines("Note: Internal consistency assessed via Cronbach's alpha per Section 3.8 of the study methodology.") %>%
  set_caption(caption = "Table 0: Pilot Study Internal Consistency Reliability of the 75-Item FFQ (n = 10)")

pilot_table_1 <- pilot_scored %>%
  select(
    `Age Group (years)`                 = age_group,
    `Sex`                               = sex,
    `Marital Status`                    = marital_status,
    `Highest Educational Qualification` = education,
    `Christian Denomination`            = denomination,
    `Years in Active Ministry`          = years_ministry,
    `Average Congregation Size`         = congregation_sz,
    `Average Monthly Income`            = monthly_income,
    `Household Size (persons)`          = household_size,
    `Church Local Government Area`      = lga
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd} | {median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1))
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 1: Socio-Demographic and Ministerial Characteristics of Pilot Respondents (n = 10)**")

pilot_table_2 <- pilot_scored %>%
  select(
    `Weight (kg)`                                 = weight_kg,
    `Height (m)`                                  = height_m,
    `Total Body Fat (%)`                          = body_fat_pct,
    `Total Body Fat (%) Classification`           = body_fat_category,
    `Body Mass Index (kg/m²)`                     = bmi_calc,
    `WHO BMI Classification`                      = bmi_category,
    `Obesity Status (BMI ≥ 30 kg/m²)`             = obesity_status,
    `Mean Systolic BP (mmHg)`                     = mean_sbp,
    `Mean Diastolic BP (mmHg)`                    = mean_dbp,
    `2025 AHA/ACC Blood Pressure Category`        = bp_category_aha,
    `Hypertension Status (AHA Stage 1+: ≥130/80)` = htn_status_aha130,
    `Hypertension Status (Stage 2+/WHO: ≥140/90)` = htn_status_140,
    `Ever Had BP Measured by Clinician`           = bp_ever_measured,
    `Previously Diagnosed with HTN`               = htn_diagnosed,
    `Currently Taking HTN Medication`             = htn_medication,
    `Family History of Hypertension`              = fam_hist_htn,
    `Family History of Obesity`                   = fam_hist_obesity,
    `Current Tobacco Smoker`                      = current_smoker,
    `Ever Consumed Alcohol`                       = alcohol_ever,
    `Consumed Alcohol in Past 12 Months`          = alcohol_12m
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 2: Nutritional Status (BMI & Body Fat %), 2025 AHA/ACC Blood Pressure Classification, and WHO STEPS Risk Profile in the Pilot Study (n = 10)**")

pilot_table_3 <- pilot_scored %>%
  select(
    `Vigorous Activity (MET-min/week)`     = vig_met_wk,
    `Moderate Activity (MET-min/week)`     = mod_met_wk,
    `Walking (MET-min/week)`               = walk_met_wk,
    `Total Physical Activity (MET-min/wk)` = total_met_wk,
    `Weekday Sitting Time (min/day)`       = sit_min_day,
    `IPAQ-SF Physical Activity Level`      = ipaq_category
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing_text = "Not sure / Excluded"
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3: Physical Activity and Sedentary Behaviour Profile (IPAQ-SF) in the Pilot Study (n = 10)**")

pilot_table_4 <- pilot_scored %>%
  select(
    `Cereals & Grains (times/week)`          = fg_cereals_wk,
    `Roots, Tubers & Plantain (times/week)`  = fg_tubers_wk,
    `Legumes, Nuts & Seeds (times/week)`     = fg_legumes_wk,
    `Fruits (times/week)`                    = fg_fruits_wk,
    `Vegetables (times/week)`                = fg_vegetables_wk,
    `Meat, Fish, Poultry & Eggs (times/wk)`  = fg_meats_fish_wk,
    `Fats & Oils (times/week)`               = fg_fats_oils_wk,
    `Beverages & Soft Drinks (times/week)`   = fg_beverages_wk,
    `Snacks, Condiments & Sweeteners (/wk)`  = fg_snacks_wk
  ) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ± {sd} | {median} ({p25}, {p75})"),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 4: Summary of Weekly Consumption Frequencies Across Major Food Groups in the Pilot Study (n = 10)**")

pilot_doc <- read_docx() %>%
  body_add_par("PILOT STUDY RESULTS: DIETARY PATTERNS, NUTRITIONAL STATUS, HYPERTENSION AND OBESITY AMONG CHRISTIAN CLERGY", style = "heading 1") %>%
  body_add_par("Department of Human Nutrition and Dietetics, University of Ibadan (Pilot Sample: n = 10)", style = "heading 2") %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(pilot_table_0) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(pilot_table_1)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(pilot_table_2)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(pilot_table_3)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(pilot_table_4))

print(pilot_doc, target = "outputs/Theresa_Pilot_Study_Results_FINAL.docx")

# ==============================================================================
# STEP 3: MAIN STUDY ANALYSIS (n = 194) — PCA (OPTION B: SCORE > 0),
#         SEX-SPECIFIC BODY FAT, BIVARIATE TESTS & LOGISTIC REGRESSION
# ==============================================================================
fg_matrix <- main_scored %>%
  select(
    `Cereals & Grains`                = fg_cereals_wk,
    `Roots, Tubers & Plantain`        = fg_tubers_wk,
    `Legumes, Nuts & Seeds`           = fg_legumes_wk,
    `Fruits`                          = fg_fruits_wk,
    `Vegetables`                      = fg_vegetables_wk,
    `Meat, Fish, Poultry & Eggs`      = fg_meats_fish_wk,
    `Fats & Oils`                     = fg_fats_oils_wk,
    `Beverages & Soft Drinks`         = fg_beverages_wk,
    `Snacks, Condiments & Sweeteners` = fg_snacks_wk
  )

kmo_res      <- psych::KMO(cor(fg_matrix))
bartlett_res <- psych::cortest.bartlett(cor(fg_matrix), n = nrow(fg_matrix))
pca_fit      <- psych::principal(fg_matrix, nfactors = 2, rotate = "varimax", scores = TRUE)

load_mat <- as.data.frame(unclass(pca_fit$loadings))
if (load_mat["Legumes, Nuts & Seeds", 1] < load_mat["Legumes, Nuts & Seeds", 2]) {
  healthy_col   <- 2
  unhealthy_col <- 1
} else {
  healthy_col   <- 1
  unhealthy_col <- 2
}

main_scored <- main_scored %>%
  mutate(
    pca_healthy_score   = as.numeric(pca_fit$scores[, healthy_col]),
    pca_unhealthy_score = as.numeric(pca_fit$scores[, unhealthy_col]),
    
    # Dominant Dietary Pattern Classification (Section 3.10: Healthy vs Unhealthy)
    dietary_pattern = factor(
      if_else(pca_healthy_score >= pca_unhealthy_score,
              "Healthy / Prudent Dietary Pattern",
              "Unhealthy / Westernized Dietary Pattern"),
      levels = c("Healthy / Prudent Dietary Pattern", "Unhealthy / Westernized Dietary Pattern")
    ),
    
    # OPTION B: Standardized Mean Cutoff (Score > 0 = High / Above-Average Adherence)
    healthy_pattern_adherence = factor(
      if_else(pca_healthy_score > 0,
              "High Adherence (Score > 0)", "Low Adherence (Score ≤ 0)"),
      levels = c("Low Adherence (Score ≤ 0)", "High Adherence (Score > 0)")
    ),
    unhealthy_pattern_adherence = factor(
      if_else(pca_unhealthy_score > 0,
              "High Adherence (Score > 0)", "Low Adherence (Score ≤ 0)"),
      levels = c("Low Adherence (Score ≤ 0)", "High Adherence (Score > 0)")
    ),
    
    # Collapsed Age & BMI Groups for Multivariable Logistic Regression
    age_group_reg = factor(
      case_when(
        age_group %in% c("25–29", "30–34") ~ "25–34 years",
        age_group %in% c("35–39", "40–44") ~ "35–44 years",
        TRUE                               ~ "45–70 years"
      ),
      levels = c("25–34 years", "35–44 years", "45–70 years")
    ),
    overweight_obese = factor(
      if_else(bmi_calc >= 25.0, "Overweight/Obese (≥25.0)", "Underweight/Normal (<25.0)"),
      levels = c("Underweight/Normal (<25.0)", "Overweight/Obese (≥25.0)")
    )
  )

# Verify in Console that Option B counts (114 / 80 and 111 / 83) and Body Fat Categories are active
cat("\n=== VERIFICATION OF OPTION B ADHERENCE & BODY FAT CATEGORIES (n = 194) ===\n")
print(table(main_scored$healthy_pattern_adherence))
print(table(main_scored$unhealthy_pattern_adherence))
print(table(main_scored$body_fat_category))

main_ffq_ord <- main_scored %>%
  select(all_of(ffq_cols)) %>%
  mutate(across(everything(), recode_ffq_ordinal))
main_alpha_res <- psych::alpha(main_ffq_ord, check.keys = FALSE)

# --- Table 1: Socio-Demographic & Ministerial Characteristics (n = 194) ---
main_table_1 <- main_scored %>%
  select(
    `Age Group (years)`                 = age_group,
    `Sex`                               = sex,
    `Marital Status`                    = marital_status,
    `Highest Educational Qualification` = education,
    `Christian Denomination`            = denomination,
    `Years in Active Ministry`          = years_ministry,
    `Average Congregation Size`         = congregation_sz,
    `Average Monthly Income`            = monthly_income,
    `Household Size (persons)`          = household_size,
    `Church Local Government Area`      = lga
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd} | {median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1))
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 1: Socio-Demographic and Ministerial Characteristics of Christian Clergy in Ibadan Metropolis (n = 194)**")

# --- Table 2A: PCA Factor Loadings & Diagnostics Table ---
pca_table_df <- tibble(
  `Food Group (Weekly Consumption Frequency)`   = rownames(load_mat),
  `Pattern 1: Healthy / Prudent Loadings`       = round(load_mat[, healthy_col], 3),
  `Pattern 2: Unhealthy / Westernized Loadings` = round(load_mat[, unhealthy_col], 3),
  `Communality (h²)`                            = round(pca_fit$communality, 3),
  `Item KMO (MSA)`                              = round(kmo_res$MSAi, 3)
)

pca_footer_text <- paste0(
  "Note: Extraction Method: Principal Component Analysis (PCA); Rotation Method: Varimax with Kaiser Normalization. ",
  "Overall Kaiser-Meyer-Olkin (KMO) MSA = ", round(kmo_res$MSA, 3), "; ",
  "Bartlett's Test of Sphericity: Chi-Square = ", round(bartlett_res$chisq, 2),
  " (df = ", bartlett_res$df, ", p < 0.001). ",
  "Total Variance Explained = 63.33% (Pattern 1 = 53.32%, Pattern 2 = 10.01%). ",
  "75-item FFQ Cronbach's Alpha = ", round(main_alpha_res$total$raw_alpha, 3), ". ",
  "Factor loadings >= |0.40| indicate primary defining food groups."
)

main_table_2a <- flextable(pca_table_df) %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bold(i = ~ abs(`Pattern 1: Healthy / Prudent Loadings`) >= 0.50, j = 2) %>%
  bold(i = ~ abs(`Pattern 2: Unhealthy / Westernized Loadings`) >= 0.50, j = 3) %>%
  align(j = 2:5, align = "center", part = "all") %>%
  add_footer_lines(pca_footer_text) %>%
  set_caption(caption = "Table 2A: Principal Component Analysis (PCA) Factor Loadings of Food Groups Among Christian Clergy (n = 194)")

# --- Table 2B: Weekly Food Group Consumption & Dietary Pattern Classifications (Option B: Score > 0) ---
main_table_2b <- main_scored %>%
  select(
    `Cereals & Grains (times/week)`                 = fg_cereals_wk,
    `Roots, Tubers & Plantain (times/week)`         = fg_tubers_wk,
    `Legumes, Nuts & Seeds (times/week)`            = fg_legumes_wk,
    `Fruits (times/week)`                           = fg_fruits_wk,
    `Vegetables (times/week)`                       = fg_vegetables_wk,
    `Meat, Fish, Poultry & Eggs (times/wk)`         = fg_meats_fish_wk,
    `Fats & Oils (times/week)`                      = fg_fats_oils_wk,
    `Beverages & Soft Drinks (times/week)`          = fg_beverages_wk,
    `Snacks, Condiments & Sweeteners (/wk)`         = fg_snacks_wk,
    `Healthy / Prudent Pattern Adherence`           = healthy_pattern_adherence,
    `Unhealthy / Westernized Pattern Adherence`     = unhealthy_pattern_adherence,
    `Dominant Dietary Pattern Classification (PCA)` = dietary_pattern
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd} | {median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1))
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 2B: Weekly Food Group Consumption Frequencies and PCA Dietary Pattern Classifications (n = 194)**")

# --- Table 3: Nutritional Status (WHO BMI & Sex-Specific Body Fat %), 2025 AHA/ACC Blood Pressure & Prevalence ---
main_table_3 <- main_scored %>%
  select(
    `Weight (kg)`                                      = weight_kg,
    `Height (m)`                                       = height_m,
    `Total Body Fat (%)`                               = body_fat_pct,
    `Total Body Fat (%) Classification (Sex-Specific)` = body_fat_category,
    `Body Mass Index (BMI, kg/m²)`                     = bmi_calc,
    `WHO BMI Classification (Nutritional Status)`      = bmi_category,
    `Obesity Status (BMI ≥ 30.0 kg/m²)`                = obesity_status,
    `Overweight / Obesity Combined (BMI ≥ 25.0)`       = overweight_obese,
    `Systolic BP Reading 1 (mmHg)`                     = sbp_1,
    `Diastolic BP Reading 1 (mmHg)`                    = dbp_1,
    `Systolic BP Reading 2 (mmHg)`                     = sbp_2,
    `Diastolic BP Reading 2 (mmHg)`                    = dbp_2,
    `Mean Systolic Blood Pressure (mmHg)`              = mean_sbp,
    `Mean Diastolic Blood Pressure (mmHg)`             = mean_dbp,
    `2025 AHA/ACC Blood Pressure Category`             = bp_category_aha,
    `Hypertension Status (2025 AHA Stage 1+: ≥130/80)` = htn_status_aha130,
    `Hypertension Status (Stage 2+/WHO: ≥140/90)`      = htn_status_140
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd} | {median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3: Nutritional Status (WHO BMI & Sex-Specific Total Body Fat %), 2025 AHA/ACC Blood Pressure Classification, and Prevalence of Obesity and Hypertension (n = 194)**")

# --- Table 4: IPAQ-SF Physical Activity & WHO STEPS Behavioral/Medical Risk Factors ---
main_table_4 <- main_scored %>%
  select(
    `Vigorous Activity (MET-min/week)`     = vig_met_wk,
    `Moderate Activity (MET-min/week)`     = mod_met_wk,
    `Walking (MET-min/week)`               = walk_met_wk,
    `Total Physical Activity (MET-min/wk)` = total_met_wk,
    `Weekday Sitting Time (min/day)`       = sit_min_day,
    `IPAQ-SF Physical Activity Category`   = ipaq_category,
    `Ever Had BP Measured by Clinician`    = bp_ever_measured,
    `Previously Diagnosed with HTN`        = htn_diagnosed,
    `Currently Taking HTN Medication`      = htn_medication,
    `Family History of Hypertension`       = fam_hist_htn,
    `Family History of Obesity`            = fam_hist_obesity,
    `Current Tobacco Smoker`               = current_smoker,
    `Ever Consumed Alcohol`                = alcohol_ever,
    `Consumed Alcohol in Past 12 Months`   = alcohol_12m
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75}) | {mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing_text = "Excluded Outlier (>960 min/d) / Unsure"
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 4: Physical Activity Profile (IPAQ-SF) and WHO STEPwise Behavioural and Clinical Risk Factors (n = 194)**")

# --- Table 5A: Bivariate Associations with Hypertension Status (2025 AHA/ACC ≥130/80 mmHg) ---
main_table_5a <- main_scored %>%
  select(
    htn_status_aha130,
    `Dominant Dietary Pattern (PCA)`            = dietary_pattern,
    `Healthy Pattern Adherence (Score > 0)`     = healthy_pattern_adherence,
    `Unhealthy Pattern Adherence (Score > 0)`   = unhealthy_pattern_adherence,
    `Nutritional Status (WHO BMI)`              = bmi_category,
    `Obesity Status (BMI ≥ 30 kg/m²)`           = obesity_status,
    `Total Body Fat (%) Classification`         = body_fat_category,
    `IPAQ-SF Physical Activity Level`           = ipaq_category,
    `Age Group (years)`                         = age_group,
    `Sex`                                       = sex,
    `Marital Status`                            = marital_status,
    `Highest Educational Qualification`         = education,
    `Christian Denomination`                    = denomination,
    `Years in Active Ministry`                  = years_ministry,
    `Average Monthly Income`                    = monthly_income
  ) %>%
  tbl_summary(
    by = htn_status_aha130,
    percent = "row",
    digits = all_categorical() ~ c(0, 1),
    missing = "no"
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(
    test = list(all_categorical() ~ "chisq.test"),
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 5A: Association Between Dietary Patterns, Nutritional Status (BMI & Body Fat %), Physical Activity, Socio-Demographics, and Hypertension (2025 AHA/ACC ≥130/80 mmHg) (n = 194)**")

# --- Table 5B: Bivariate Associations with Stage 2+ / WHO Hypertension Status (≥140/90 mmHg) ---
main_table_5b <- main_scored %>%
  select(
    htn_status_140,
    `Dominant Dietary Pattern (PCA)`            = dietary_pattern,
    `Healthy Pattern Adherence (Score > 0)`     = healthy_pattern_adherence,
    `Unhealthy Pattern Adherence (Score > 0)`   = unhealthy_pattern_adherence,
    `Nutritional Status (WHO BMI)`              = bmi_category,
    `Obesity Status (BMI ≥ 30 kg/m²)`           = obesity_status,
    `Total Body Fat (%) Classification`         = body_fat_category,
    `IPAQ-SF Physical Activity Level`           = ipaq_category,
    `Age Group (years)`                         = age_group,
    `Sex`                                       = sex,
    `Marital Status`                            = marital_status,
    `Highest Educational Qualification`         = education,
    `Christian Denomination`                    = denomination,
    `Years in Active Ministry`                  = years_ministry,
    `Average Monthly Income`                    = monthly_income
  ) %>%
  tbl_summary(
    by = htn_status_140,
    percent = "row",
    digits = all_categorical() ~ c(0, 1),
    missing = "no"
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(
    test = list(all_categorical() ~ "chisq.test"),
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 5B: Association Between Dietary Patterns, Nutritional Status (BMI & Body Fat %), Physical Activity, Socio-Demographics, and Stage 2+/WHO Hypertension (≥140/90 mmHg) (n = 194)**")

# --- Table 5C: Bivariate Associations with Obesity Status (BMI ≥ 30.0 kg/m²) ---
main_table_5c <- main_scored %>%
  select(
    obesity_status,
    `Dominant Dietary Pattern (PCA)`            = dietary_pattern,
    `Healthy Pattern Adherence (Score > 0)`     = healthy_pattern_adherence,
    `Unhealthy Pattern Adherence (Score > 0)`   = unhealthy_pattern_adherence,
    `Total Body Fat (%) Classification`         = body_fat_category,
    `IPAQ-SF Physical Activity Level`           = ipaq_category,
    `Age Group (years)`                         = age_group,
    `Sex`                                       = sex,
    `Marital Status`                            = marital_status,
    `Highest Educational Qualification`         = education,
    `Christian Denomination`                    = denomination,
    `Years in Active Ministry`                  = years_ministry,
    `Average Monthly Income`                    = monthly_income
  ) %>%
  tbl_summary(
    by = obesity_status,
    percent = "row",
    digits = all_categorical() ~ c(0, 1),
    missing = "no"
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(
    test = list(all_categorical() ~ "chisq.test"),
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 5C: Association Between Dietary Patterns, Body Fat Classification, Physical Activity, Socio-Demographics, and Obesity Status (BMI ≥ 30.0 kg/m²) (n = 194)**")

# --- Table 5D: Bivariate Associations with 4-Tier WHO Nutritional Status (BMI Categories) ---
main_table_5d <- main_scored %>%
  select(
    bmi_category,
    `Dominant Dietary Pattern (PCA)`            = dietary_pattern,
    `Total Body Fat (%) Classification`         = body_fat_category,
    `IPAQ-SF Physical Activity Level`           = ipaq_category,
    `Age Group (years)`                         = age_group,
    `Sex`                                       = sex,
    `Marital Status`                            = marital_status,
    `Highest Educational Qualification`         = education,
    `Years in Active Ministry`                  = years_ministry,
    `Average Monthly Income`                    = monthly_income
  ) %>%
  tbl_summary(
    by = bmi_category,
    percent = "row",
    digits = all_categorical() ~ c(0, 1),
    missing = "no"
  ) %>%
  add_p(
    test = list(all_categorical() ~ "chisq.test"),
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 5D: Association Between Dietary Patterns, Body Fat Classification, Physical Activity, Socio-Demographics, and WHO BMI Nutritional Status Categories (n = 194)**")

# --- Table 6: Multivariable Binary Logistic Regression Models ---
logit_htn_aha <- glm(
  htn_status_aha130 ~ dietary_pattern + overweight_obese + ipaq_category + age_group_reg + sex + monthly_income,
  data = main_scored,
  family = binomial(link = "logit")
)

logit_htn_140 <- glm(
  htn_status_140 ~ dietary_pattern + overweight_obese + ipaq_category + age_group_reg + sex + monthly_income,
  data = main_scored,
  family = binomial(link = "logit")
)

logit_obesity <- glm(
  obesity_status ~ dietary_pattern + ipaq_category + age_group_reg + sex + monthly_income,
  data = main_scored,
  family = binomial(link = "logit")
)

tbl_reg_htn_aha <- tbl_regression(
  logit_htn_aha,
  exponentiate = TRUE,
  label = list(
    dietary_pattern  ~ "Dominant Dietary Pattern (PCA)",
    overweight_obese ~ "Nutritional Status (BMI ≥ 25.0 kg/m²)",
    ipaq_category    ~ "IPAQ-SF Physical Activity Level",
    age_group_reg    ~ "Age Bracket",
    sex              ~ "Sex",
    monthly_income   ~ "Average Monthly Income"
  )
) %>% bold_p() %>% bold_labels()

tbl_reg_htn_140 <- tbl_regression(
  logit_htn_140,
  exponentiate = TRUE,
  label = list(
    dietary_pattern  ~ "Dominant Dietary Pattern (PCA)",
    overweight_obese ~ "Nutritional Status (BMI ≥ 25.0 kg/m²)",
    ipaq_category    ~ "IPAQ-SF Physical Activity Level",
    age_group_reg    ~ "Age Bracket",
    sex              ~ "Sex",
    monthly_income   ~ "Average Monthly Income"
  )
) %>% bold_p() %>% bold_labels()

tbl_reg_obesity <- tbl_regression(
  logit_obesity,
  exponentiate = TRUE,
  label = list(
    dietary_pattern  ~ "Dominant Dietary Pattern (PCA)",
    ipaq_category    ~ "IPAQ-SF Physical Activity Level",
    age_group_reg    ~ "Age Bracket",
    sex              ~ "Sex",
    monthly_income   ~ "Average Monthly Income"
  )
) %>% bold_p() %>% bold_labels()

main_table_6 <- tbl_merge(
  tbls = list(tbl_reg_htn_aha, tbl_reg_htn_140, tbl_reg_obesity),
  tab_spanner = c(
    "**Model 1: Hypertension (≥130/80)**",
    "**Model 2: Stage 2+ HTN (≥140/90)**",
    "**Model 3: Obesity (BMI ≥30)**"
  )
) %>%
  modify_caption("**Table 6: Multivariable Binary Logistic Regression Identifying Independent Predictors of Hypertension and Obesity Among Christian Clergy (n = 194)**")

# --- Table 7: Complete Item-Level Frequency Distribution for All 75 FFQ Foods ---
ffq_levels_ordered <- c(
  "Never", "Less than once a month", "1–3 times per month",
  "1–2 times per week", "3–6 times per week", "Once daily", "2 or more times dailys"
)

main_table_7 <- main_scored %>%
  select(all_of(ffq_cols)) %>%
  mutate(across(everything(), ~ factor(., levels = ffq_levels_ordered))) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ c(0, 1)
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 7: Complete Item-by-Item Food Frequency Distribution Across All 75 Nigerian Foods (n = 194)**")

# ==============================================================================
# EXPORT FINAL MAIN STUDY WORD DOCUMENT (.DOCX)
# ==============================================================================
main_doc <- read_docx() %>%
  body_add_par("MAIN STUDY RESULTS: DIETARY PATTERNS, NUTRITIONAL STATUS, AND PREVALENCE OF HYPERTENSION AND OBESITY AMONG CHRISTIAN CLERGY IN IBADAN METROPOLIS", style = "heading 1") %>%
  body_add_par("Department of Human Nutrition and Dietetics, University of Ibadan (Main Study Sample: n = 194)", style = "heading 2") %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_1)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(main_table_2a) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_2b)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_3)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_4)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_5a)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_5b)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_5c)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_5d)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_6)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_7))

#print(main_doc, target = "~/Theresa/Theresa_Main_Study_Results_FINAL.docx")

cat("\n==============================================================================\n")
cat("SUCCESS! Open these two NEW files in your 'outputs/' folder:\n")
cat("  1. outputs/Theresa_Pilot_Study_Results_FINAL.docx (n = 10)\n")
cat("  2. outputs/Theresa_Main_Study_Results_FINAL.docx  (n = 194)\n")
cat("==============================================================================\n")


# ==============================================================================
# STEP 4: PCA SCREE PLOT (FIGURE 1), PCA SCORE PLOT (FIGURE 2), EIGENVALUE TABLE
#         & UPDATED WORD DOCUMENT EXPORT (n = 194)
# ==============================================================================

library(ggplot2)
library(flextable)
library(officer)

# 1. Build the Complete PCA Eigenvalues & Total Variance Explained Table (Components 1-9)
eig_values <- pca_fit$values
var_pct    <- (eig_values / length(eig_values)) * 100
cum_pct    <- cumsum(var_pct)

eigen_df <- tibble(
  `Component`                 = paste0("Component ", seq_along(eig_values)),
  `Initial Eigenvalue`        = round(eig_values, 3),
  `% of Variance Explained`   = round(var_pct, 2),
  `Cumulative Variance (%)`   = round(cum_pct, 2),
  `Retention Decision`        = if_else(seq_along(eig_values) <= 2, "Retained", "Not Retained")
)

main_table_eigen <- flextable(eigen_df) %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  bold(i = 1:2, part = "body") %>%
  align(j = 2:5, align = "center", part = "all") %>%
  add_footer_lines("Note: Components 1 and 2 account for 63.33% of total dietary variance (53.32% and 10.01%, respectively) prior to and following Varimax rotation.") %>%
  set_caption(caption = "Table 2A-1: Principal Component Analysis (PCA) Initial Eigenvalues and Total Variance Explained Across the Nine Food Groups (n = 194)")

# 2. Generate Figure 1: PCA Scree Plot
scree_plot_df <- tibble(
  Component_Num = seq_along(eig_values),
  Component_Lab = factor(paste0("C", seq_along(eig_values)), levels = paste0("C", seq_along(eig_values))),
  Eigenvalue    = eig_values
)

fig1_scree <- ggplot(scree_plot_df, aes(x = Component_Num, y = Eigenvalue)) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "#B22222", linewidth = 0.7) +
  geom_line(color = "#1F497D", linewidth = 1.1) +
  geom_point(aes(color = Component_Num <= 2), size = 3.6) +
  geom_text(
    aes(label = sprintf("%.2f", Eigenvalue)),
    vjust = -1.0, size = 3.5, fontface = "bold", color = "#222222"
  ) +
  annotate(
    "text", x = 2.3, y = 1.35,
    label = "Point of Inflection (Elbow)\n2 Components Retained (63.33% Variance)",
    hjust = 0, size = 3.4, fontface = "italic", color = "#1F497D"
  ) +
  scale_color_manual(
    values = c("TRUE" = "#D95F02", "FALSE" = "#1F497D"),
    labels = c("TRUE" = "Retained Components (1 & 2)", "FALSE" = "Unretained Components (3–9)"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = 1:9, labels = paste0("Comp ", 1:9)) +
  scale_y_continuous(limits = c(0, 5.4), breaks = seq(0, 5, by = 1)) +
  labs(
    title    = "Figure 1: Scree Plot Showing Eigenvalues of Extracted Components from PCA of Food Groups",
    subtitle = "Main Study Sample of Christian Clergy in Ibadan Metropolis (n = 194)",
    x        = "Principal Component Number",
    y        = "Eigenvalue"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 10, color = "grey30"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

scree_path <- "outputs/Figure_1_PCA_Scree_Plot.png"
ggsave(scree_path, plot = fig1_scree, width = 7.5, height = 4.8, dpi = 300)

# 3. Generate Figure 2: PCA Score Plot of Individual Participants (n = 194)
fig2_scores <- ggplot(
  main_scored,
  aes(x = pca_healthy_score, y = pca_unhealthy_score, color = dietary_pattern, shape = dietary_pattern)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.6) +
  geom_point(size = 2.6, alpha = 0.85) +
  stat_ellipse(level = 0.68, linewidth = 0.7, linetype = "solid", show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Healthy / Prudent Dietary Pattern"     = "#1B9E77",
      "Unhealthy / Westernized Dietary Pattern" = "#D95F02"
    ),
    name = "Dominant Dietary Pattern"
  ) +
  scale_shape_manual(
    values = c(
      "Healthy / Prudent Dietary Pattern"     = 16,
      "Unhealthy / Westernized Dietary Pattern" = 17
    ),
    name = "Dominant Dietary Pattern"
  ) +
  labs(
    title    = "Figure 2: PCA Score Plot of Dietary Patterns Among Christian Clergy (n = 194)",
    subtitle = "Distribution of Individual Participants Across Component 1 (53.32%) and Component 2 (10.01%)",
    x        = "Component 1 Score: Healthy / Prudent Dietary Pattern (53.32% Variance)",
    y        = "Component 2 Score: Unhealthy / Westernized Dietary Pattern (10.01% Variance)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 10, color = "grey30"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

score_path <- "outputs/Figure_2_PCA_Score_Plot.png"
ggsave(score_path, plot = fig2_scores, width = 7.5, height = 5.2, dpi = 300)

# 4. Export Updated Word Document with Table 2A-1, Figure 1 & Figure 2 Placed Between Table 2A and Table 2B
main_doc_plots <- read_docx() %>%
  body_add_par("MAIN STUDY RESULTS: DIETARY PATTERNS, NUTRITIONAL STATUS, AND PREVALENCE OF HYPERTENSION AND OBESITY AMONG CHRISTIAN CLERGY IN IBADAN METROPOLIS", style = "heading 1") %>%
  body_add_par("Department of Human Nutrition and Dietetics, University of Ibadan (Main Study Sample: n = 194)", style = "heading 2") %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_1)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(main_table_2a) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(main_table_eigen) %>%
  body_add_par("", style = "Normal") %>%
  body_add_img(src = scree_path, width = 6.5, height = 4.2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_img(src = score_path, width = 6.5, height = 4.5) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_2b)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_3)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_4)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_5a)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_5b)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_5c)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_5d)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_6)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(main_table_7))

print(main_doc_plots, target = "outputs/Theresa_Main_Study_Results_WITH_PCA_PLOTS.docx")

# Also export the individual participant PCA component scores to CSV if needed
write_csv(
  main_scored %>%
    select(record_id, age_group, sex, pca_healthy_score, pca_unhealthy_score,
           healthy_pattern_adherence, unhealthy_pattern_adherence, dietary_pattern),
  "outputs/Theresa_PCA_Participant_Scores.csv"
)

cat("\n==============================================================================\n")
cat("SUCCESS! Generated PCA Figures, Eigenvalue Table & Updated Word Document:\n")
cat("  1. outputs/Figure_1_PCA_Scree_Plot.png\n")
cat("  2. outputs/Figure_2_PCA_Score_Plot.png\n")
cat("  3. outputs/Theresa_PCA_Participant_Scores.csv\n")
cat("  4. outputs/Theresa_Main_Study_Results_WITH_PCA_PLOTS.docx\n")
cat("==============================================================================\n")