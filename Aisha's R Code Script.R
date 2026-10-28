install.packages("skimr")

library (skimr)
library(readxl)
library(tidyverse)   
library(psych)       
library(gtsummary)   
library(janitor)
library (stringr)


#define file path
raw_file_path1 <- "~/Aisha/Emotional_Eating_BMI_and_Body_Size_Perception_among_Female_Undergraduates_at_the_University_of_Ibadan_-_all_versions_-_labels_-_2026-09-16-18-00-18.xlsx"

raw_data1 <- read_excel("~/Aisha/Emotional_Eating_BMI_and_Body_Size_Perception_among_Female_Undergraduates_at_the_University_of_Ibadan_-_all_versions_-_labels_-_2026-09-16-18-00-18.xlsx")

dim (raw_data1)

pilot_clean <- read_excel(raw_file_path1, sheet = 1) %>%
  remove_empty(c("rows", "cols")) %>%
  select(-starts_with("_"), -starts_with("meta/"), -starts_with("Thank you"))

#renaming column names to simpler variables
pilot_clean <- pilot_clean %>%
  rename(
    consent            = `Are you willing to participate in this study?`,
    id                 = `Questionnaire ID:`,
    age                = `Age (as at last birthday in years)`,
    faculty            = `Faculty`,
    department         = `Department`,
    level              = `Level`,
    religion           = `Religion`,
    ethnicity          = `Ethnicity`,
    ethnicity_other    = `Please specify other ethnicity:`,
    monthly_allowance  = `Monthly allowance`,
    residence          = `Do you live`,
    skip_meals         = `Do you skip meals?`,
    weight_1           = `Weight (kg) - 1st reading`,
    weight_2           = `Weight (kg) - 2nd reading`,
    height_1           = `Height (m) - 1st reading`,
    height_2           = `Height (m) - 2nd reading`,
    waist_1            = `Waist circumference (cm) - 1st reading`,
    waist_2            = `Waist circumference (cm) - 2nd reading`,
    weight_avg_raw     = `weight_average`,
    height_avg_raw     = `height_average`,
    waist_avg_raw      = `waist_average`,
    bmi_raw            = `bmi`,
    tfeq_11            = `When I smell a delicious food, I find it very difficult to keep from eating, even if I have just finished a meal.`,
    tfeq_12            = `I deliberately take smaller portions as a means of controlling my weight.`,
    tfeq_13            = `When I feel anxious, I find myself eating.`,
    tfeq_14            = `Sometimes when I start eating, I just can’t seem to stop.`,
    tfeq_15            = `Being with someone who is eating often makes me hungry enough to eat also.`,
    tfeq_16            = `When I'm feeling down, I tend to eat too much.`,
    tfeq_17            = `When I see a real delicacy, I often get so hungry that I have to eat right away.`,
    tfeq_18            = `I get so hungry that my stomach often seems like a bottomless pit.`,
    tfeq_19            = `I am always hungry so it is hard for me to stop eating before I finish the food on my plate.`,
    tfeq_20            = `When I feel lonely, I console myself by eating.`,
    tfeq_21            = `I consciously hold back at meals in order not to weight gain.`,
    tfeq_22            = `I do not eat some foods because they make me fat.`,
    tfeq_23            = `I am always hungry enough to eat at any time.`,
    tfeq_24            = `How often do you feel hungry?`,
    tfeq_25            = `How frequently do you avoid “stocking up” on tempting foods?`,
    tfeq_26            = `How likely are you to consciously eat less than you want?`,
    tfeq_27            = `Do you go on eating binges though you are not hungry?`,
    tfeq_28            = `On a scale of 1 to 8, where 1 means no restraint in eating and 8 means total restraint, what number would you give yourself?`,
    silhouette_current = `Which body figure best represents your current body size?`,
    silhouette_ideal   = `Which body figure represents your ideal body size?`,
    discrepancy_raw    = `body_size_discrepancy`
  )

#extracting numeric values and text harmonization
pilot_clean <- pilot_clean %>%
  
  mutate(across(starts_with("tfeq_"), ~ parse_number(as.character(.)))) %>%
  mutate(
    silhouette_current = as.integer(silhouette_current),
    silhouette_ideal = as.integer(silhouette_ideal)
  ) %>%

#clean_data <- clean_data %>%
  #cleaning the whitespaces and trim characters present
  mutate(across(c(faculty, department, ethnicity, religion), str_squish)) %>%
  
  #for faculty
  mutate(
    faculty_clean = case_when(
      faculty %in% c("Art", "Arts") ~ "Arts",
      str_detect(faculty, regex("multidisciplinary", ignore_case = TRUE)) ~ "Multidisciplinary Studies",
      str_detect(faculty, regex("african studies", ignore_case = TRUE)) ~ "Institute of African Studies",
      str_detect(faculty, regex("social science", ignore_case = TRUE)) ~ "Social Sciences",
      str_detect(faculty, regex("environmental design", ignore_case = TRUE)) ~ "Environmental Design & Management",
      str_detect(faculty, regex("business", ignore_case = TRUE)) ~ "School of Business",
      TRUE ~ faculty
    )
  ) %>%
  
  #for department
  mutate(
    department_clean = case_when(
      str_detect(department, regex("food tech", ignore_case = TRUE)) ~ "Food Technology",
      str_detect(department, regex("Data and information science", ignore_case = TRUE)) ~ "Data and Information Science",
      str_detect(department, regex("Library archival and information science", ignore_case = TRUE)) ~ "Library Archival and Information Science",
      str_detect(department, regex("social work", ignore_case = TRUE)) ~ "Social WOrk",
      department == "ASSE" ~ "Arts & Social Sciences Education (ASSE)",
      department == "Management's" ~ "Management",
      TRUE ~ department
    )
  ) %>%
  
  mutate(
    ethnicity_clean = case_when(
      ethnicity == "Others" & !is.na(ethnicity_other) ~ str_to_title(ethnicity_other),
      TRUE ~ ethnicity
    )
  )

#VERIFY
pilot_clean %>% count(faculty_clean)
pilot_clean %>% count(department_clean)
pilot_clean %>% count(ethnicity_clean)



  
    
    #BSP Discrepancy
pilot_transformed <- pilot_transformed %>%
  mutate(
    discrepancy = silhouette_current - silhouette_ideal,
    body_image_status = factor(
      case_when(
        discrepancy == 0 ~ "Satisfied",
        discrepancy > 0 ~ "Desires smaller size",
        discrepancy < 0 ~ "Desires larger size"
      ),
      levels = c("Satisfied", "Desires smaller size", "Desires larger size")
    )
  )
  

#check BMI categories and mean BRI
pilot_transformed %>%
  group_by(bmi_category) %>%
  summarise(
    n = n(),
    mean_bmi = round(mean(bmi_calc), 2),
    mean_bri = round(mean(bri), 2)
  )

#pilot_transformed %>%
  #count(ee_level, name = "Frequency") %>%
  #mutate(
    #Percentage = round((Frequency / sum(Frequency)) * 100, 1)
  #)

#checks EES
#summary(pilot_transformed$ee_score_100)
#table(pilot_transformed$ee_score_100)
library (dplyr)

pilot_transformed <- pilot_clean %>%
  mutate(
    #Antropometry validation and re-calculation
    weight_kg = (weight_1 + weight_2) / 2,
    height_m = (height_1 + height_2) / 2,
    waist_cm = (waist_1 + waist_2) / 2,
    
    #BMI calculation and categorization
    bmi_calc = weight_kg / (height_m^2),
    bmi_category = factor(
      case_when(
        bmi_calc < 18.5 ~ "Underweight",
        bmi_calc >= 18.5 & bmi_calc < 25.0 ~ "Normal weight",
        bmi_calc >= 25.0 & bmi_calc < 30.0 ~ "Overweight",
        bmi_calc >= 30.0 ~ "Obese"
      ),
      levels = c("Underweight", "Normal weight", "Overweight", "Obese")
    ),
    
    #Body Roundness Index
    bri = 364.2 - 365.5 * sqrt(1- (((waist_cm / 100) / (2 * pi))^2) /
                                 ((0.5 * height_m)^2)),
    
    #EES
    ee_raw = tfeq_13 + tfeq_16 + tfeq_20,
    ee_score_100 = ((ee_raw - 3) / 9) * 100,
    
    pilot_transformed <- pilot_transformed %>%
      mutate(
        # Approach A: Data-driven Tertiles (Low, Moderate, High)
        ee_tertiles = ntile(ee_score_100, 3),
        ee_tertile_label = factor(
          case_when(
            ee_tertiles == 1 ~ "Low",
            ee_tertiles == 2 ~ "Moderate",
            ee_tertiles == 3 ~ "High"
          ),
          levels = c("Low", "Moderate", "High")
        ),
  
  # Approach B: Behavioral Endorsement (Average response >= 2.5 on Likert scale)
  # raw score >= 8 indicates affirmative responses ('mostly true' / 'definitely true')
      ee_behavioral_status = if_else(
        ee_raw >= 8, 
        "Emotionally Driven Eater", 
        "Non-Emotional Eater"
      )
    )
)

pilot_transformed %>% count(ee_tertile_label)
pilot_transformed %>% count(ee_behavioral_status)

#check BSP discrepancy
table(pilot_transformed$body_image_status)


#For reliability and normality checks
install.packages("moments")
library(psych)
library(moments)

#cronbach's alpha for EES tfeq items 13, 16, 20
ee_items <- pilot_transformed %>%
  select(tfeq_13, tfeq_16, tfeq_20)

ee_alpha <- psych::alpha(ee_items)
print(ee_alpha$total)

#shapiro-wilk Normality tests & skewness check
normality_table <- tibble(
  Variable = c("Age", "BMI", "BRI", "Emotional Eating Score",
               "Body Image Discrepancy"),
  Shapiro_W = c(
    shapiro.test(pilot_transformed$age)$statistic,
    shapiro.test(pilot_transformed$bmi_calc)$statistic,
    shapiro.test(pilot_transformed$bri)$statistic,
    shapiro.test(pilot_transformed$ee_score_100)$statistic,
    shapiro.test(pilot_transformed$discrepancy)$statistic
  ),
  
  P_Value = c(
    shapiro.test(pilot_transformed$age)$p.value,
    shapiro.test(pilot_transformed$bmi_calc)$p.value,
    shapiro.test(pilot_transformed$bri)$p.value,
    shapiro.test(pilot_transformed$ee_score_100)$p.value,
    shapiro.test(pilot_transformed$discrepancy)$p.value
  ),
  Skewness = c(
    skewness(pilot_transformed$age),
    skewness(pilot_transformed$bmi_calc),
    skewness(pilot_transformed$bri),
    skewness(pilot_transformed$ee_score_100),
    skewness(pilot_transformed$discrepancy)
  )
)

print(normality_table)

#Inferential statisitcs and hypothesis testing
library(dplyr)
library(tibble)

corr_vars <- pilot_transformed %>%
  select(
    `Emotional Eating` = ee_score_100,
    `BMI`              = bmi_calc,
    `BRI`              = bri,
    `Discrepancy (BID)`= discrepancy
  )

run_spearman <- function(v1, v2, data) {
  test <- cor.test(data[[v1]], data[[v2]], method = "spearman", exact = FALSE)
  tibble(
    Relationship = paste(v1, "vs.", v2),
    Spearman_rho = round(test$estimate, 3),
    P_Value      = round(test$p.value, 4)
  )
}

correlation_results <- bind_rows(
  run_spearman("Emotional Eating", "BMI", corr_vars),
  run_spearman("Emotional Eating", "BRI", corr_vars),
  run_spearman("Emotional Eating", "Discrepancy (BID)", corr_vars),
  run_spearman("BMI", "Discrepancy (BID)", corr_vars),
  run_spearman("BRI", "Discrepancy (BID)", corr_vars),
  run_spearman("BMI", "BRI", corr_vars)
)

print(correlation_results)

#emotional eating across BMI categories
kw_bmi <- kruskal.test(ee_score_100 ~ bmi_category, data = pilot_transformed)
cat("Kruskal-Wallis Test (EE across BMI categories):\n")
cat("H =", round(kw_bmi$statistic, 3), ", df =", kw_bmi$parameter, ", p =", round(kw_bmi$p.value, 4), "\n\n")

# Median (IQR) Emotional Eating by BMI Category
pilot_transformed %>%
  group_by(bmi_category) %>%
  summarise(
    N = n(),
    Median_EE = round(median(ee_score_100), 1),
    Q1 = round(quantile(ee_score_100, 0.25), 1),
    Q3 = round(quantile(ee_score_100, 0.75), 1),
    IQR = round(IQR(ee_score_100), 1)
  )

#Emotional eating across BIS
kw_bid <- kruskal.test(ee_score_100 ~ body_image_status, data = pilot_transformed)
cat("\nKruskal-Wallis Test (EE across Body Image Status):\n")
cat("H =", round(kw_bid$statistic, 3), ", df =", kw_bid$parameter, ", p =", round(kw_bid$p.value, 4), "\n\n")

#Fisher's Exact Test for Categorical Association
table_bmi_ee <- table(pilot_transformed$bmi_category, pilot_transformed$ee_tertile_label)
fisher_test <- fisher.test(table_bmi_ee)
print(fisher_test)


#Table formatting and client deliverables
library(tidyverse)
library(gtsummary)
library(flextable)
library(officer)

# Use compact APA-friendly styling for summary tables
theme_gtsummary_compact()

#Socio-demographic profile
table_1_data <- pilot_transformed %>%
  select(
    `Age (years)`         = age,
    `Faculty`             = faculty_clean,
    `Ethnicity`           = ethnicity_clean,
    `Religion`            = religion,
    `Monthly Allowance`   = monthly_allowance,
    `Residential Status`  = residence,
    `Skips Meals`         = skip_meals
  )

table_1 <- table_1_data %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 1: Socio-Demographic Profile of Female Undergraduates (N = 35)**")

table_1

#Anthro and body size perception
table_2_data <- pilot_transformed %>%
  select(
    `Weight (kg)`                   = weight_kg,
    `Height (m)`                    = height_m,
    `Waist Circumference (cm)`      = waist_cm,
    `Body Mass Index (kg/m²)`       = bmi_calc,
    `BMI Classification (WHO)`      = bmi_category,
    `Body Roundness Index (BRI)`    = bri,
    `Perceived Current Silhouette`  = silhouette_current,
    `Perceived Ideal Silhouette`    = silhouette_ideal,
    `Silhouette Discrepancy (BID)`  = discrepancy,
    `Body Image Satisfaction`       = body_image_status
  )

table_2 <- table_2_data %>%
  tbl_summary(
    statistic = list(
      c(`Weight (kg)`, `Height (m)`, `Waist Circumference (cm)`, 
        `Body Mass Index (kg/m²)`, `Body Roundness Index (BRI)`) ~ "{mean} ± {sd}",
      c(`Perceived Current Silhouette`, `Perceived Ideal Silhouette`, 
        `Silhouette Discrepancy (BID)`) ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 2: Anthropometric Profile and Body Size Perception Metrics (N = 35)**")

table_2

#EES
table_3_data <- pilot_transformed %>%
  select(
    `Q13: Anxious Eating (Raw 1-4)`     = tfeq_13,
    `Q16: Depressed Eating (Raw 1-4)`   = tfeq_16,
    `Q20: Lonely Eating (Raw 1-4)`      = tfeq_20,
    `Raw EE Score (3-12)`              = ee_raw,
    `Standardized EE Score (0-100)`     = ee_score_100,
    `Emotional Eating Tertile`          = ee_tertile_label,
    `Behavioral Eating Pattern`         = ee_behavioral_status
  )

table_3 <- table_3_data %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3: Emotional Eating Subscale Characteristics and Tertile Distribution (TFEQ-R18, N = 35)**")

table_3

#Bivariate correlations
table_4_df <- tibble(
  `Variable Comparison` = c(
    "Emotional Eating Score vs. BMI",
    "Emotional Eating Score vs. BRI",
    "Emotional Eating Score vs. Silhouette Discrepancy",
    "BMI vs. Silhouette Discrepancy",
    "BRI vs. Silhouette Discrepancy",
    "BMI vs. BRI"
  ),
  `Spearman Rho (ρ)` = c("0.102", "0.138", "0.244", "0.598***", "0.522***", "0.902***"),
  `p-value`          = c("0.561", "0.430", "0.159", "< 0.001", "0.001", "< 0.001"),
  `Interpretation`   = c(
    "Negligible, non-significant",
    "Negligible, non-significant",
    "Weak positive, non-significant",
    "Moderate positive, highly significant",
    "Moderate positive, highly significant",
    "Very strong positive, highly significant"
  )
)

table_4 <- flextable(table_4_df) %>%
  autofit() %>%
  theme_booktabs() %>%
  add_footer_lines(c("*** Correlation is significant at the 0.001 level (2-tailed).")) %>%
  set_caption(caption = "Table 4: Non-Parametric Spearman Correlation Matrix Among Key Study Variables (N = 35)")

table_4

#BMI vs EES
table_5 <- pilot_transformed %>%
  tbl_cross(
    row = bmi_category,
    col = ee_tertile_label,
    percent = "row",
    margin = "column"
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 5: Cross-Tabulation of BMI Classification and Emotional Eating Tertiles (Fisher's Exact p = 0.171)**")

table_5

#Export all tables
# Initialize Word document
doc <- read_docx()

# Add document titles and flextables
doc <- doc %>%
  body_add_par("STUDY PILOT RESULTS: EMOTIONAL EATING, BMI, AND BODY SIZE PERCEPTION", style = "heading 1") %>%
  body_add_par("Department of Human Nutrition and Dietetics, University of Ibadan", style = "heading 2") %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(table_1)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(table_2)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(table_3)) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(table_4) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(as_flex_table(table_5))

# Write out file
print(doc, target = "~/Aisha/Pilot_Study_Results_Tables.docx")
