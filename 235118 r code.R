#Install required packages 
install.packages(c("readxl", "tidyverse", "psych", "gtsummary", "janitor"))

#load libraries 
library(readxl)      
library(tidyverse)   
library(psych)       
library(gtsummary)   
library(janitor)

#df_raw <- read_xlsx("KAIdata.xlsx", sheet = 1)

df_raw <- read_excel("C:/Users/HomePC/Desktop/Main Study Data Set.xlsx")
#This is the line of code that removes the empty row:
#df_raw <- df_raw[-6, ]

KAIdata <- df_raw
#renaming columns in order of the questionnaire
colnames(df) <- c(
  "age", "religion", "ethnicity", "level_study", "faculty", 
  "hall", "marriage_timing", "bf_discussion", "info_source",
  "k1", "k2", "k3", "k4", "k5", "k6", 
  "k7", "k8", "k9", "k10", "k11", "k12",
  "a1", "a2", "a3", "a4", "a5", "a6", 
  "a7", "a8", "a9", "a10", "a11", "a12",
  "i1", "i2", "i3", "i4", "i5", 
  "i6", "i7", "i8", "i9", "i10"
)

#KNOWLEDGE

k_data <- KAIdata[, 17:28]
colnames(k_data) <- paste0("k", 1:12)

#score the 12 individual knowledge items
k_scored <- k_data %>%
  mutate(
    k1_sc  = if_else(str_to_lower(str_trim(as.character(k1))) == "true", 1, 0, missing = 0),
    k2_sc  = if_else(str_detect(as.character(k2), "6"), 1, 0, missing = 0),
    k3_sc  = if_else(str_to_lower(str_trim(as.character(k3))) == "false", 1, 0, missing = 0),
    k4_sc  = if_else(str_to_lower(str_trim(as.character(k4))) == "true", 1, 0, missing = 0),
    k5_sc  = if_else(str_to_lower(str_trim(as.character(k5))) == "false", 1, 0, missing = 0),
    k6_sc  = if_else(str_to_lower(str_trim(as.character(k6))) == "true", 1, 0, missing = 0),
    k7_sc  = if_else(str_to_lower(str_trim(as.character(k7))) == "false", 1, 0, missing = 0),
    k8_sc  = if_else(str_detect(str_to_lower(as.character(k8)), "demand"), 1, 0, missing = 0),
    k9_sc  = if_else(str_to_lower(str_trim(as.character(k9))) == "true", 1, 0, missing = 0),
    k10_sc = if_else(str_to_lower(str_trim(as.character(k10))) == "false", 1, 0, missing = 0),
    k11_sc = if_else(str_to_lower(str_trim(as.character(k11))) == "true", 1, 0, missing = 0),
    k12_sc = if_else(str_to_lower(str_trim(as.character(k12))) == "false", 1, 0, missing = 0)
  )

#Calculating the Total Score and 70% Cut-off Categorization (>= 8.4 = Good)
k_scored <- k_scored %>%
  mutate(
    # Explicitly sum the 12 generated score columns
    k_total = k1_sc + k2_sc + k3_sc + k4_sc + k5_sc + k6_sc +
      k7_sc + k8_sc + k9_sc + k10_sc + k11_sc + k12_sc,
    k_category = factor(
      if_else(k_total >= 7, "Good Knowledge", "Poor Knowledge"),
      levels = c("Poor Knowledge", "Good Knowledge")
    )
  )
head(k_scored %>% select(k1_sc, k2_sc, k_total, k_category))


#ATTITUDES
#defining scoring helpers 
score_att_pos <- function(x) case_when(
  str_to_lower(str_trim(as.character(x))) == "agree" ~ 3,
  str_to_lower(str_trim(as.character(x))) == "undecided" ~ 2,
  str_to_lower(str_trim(as.character(x))) == "disagree" ~ 1,
  TRUE ~ 0
)
score_att_neg <- function(x) case_when(
  str_to_lower(str_trim(as.character(x))) == "agree" ~ 1,
  str_to_lower(str_trim(as.character(x))) == "undecided" ~ 2,
  str_to_lower(str_trim(as.character(x))) == "disagree" ~ 3,
  TRUE ~ 0
)

#extracting attitude columns
a_data <- KAIdata[, 29:40]
colnames(a_data) <- paste0("a", 1:12)

#score attitude items
a_scored <- a_data %>%
  mutate(
    a1_sc = score_att_pos(a1),
    a2_sc = score_att_pos(a2),
    a3_sc = score_att_pos(a3),
    a4_sc = score_att_pos(a4),
    a5_sc = score_att_pos(a5),
    a6_sc = score_att_pos(a6),
    a7_sc = score_att_neg(a7),#reverse-scored
    a8_sc = score_att_pos(a8),
    a9_sc = score_att_neg(a9),#reverse-scored
    a10_sc = score_att_neg(a10),#reverse-scored
    a11_sc = score_att_neg(a11),#reverse-scored
    a12_sc = score_att_pos(a12)
  )

a_scored <- a_scored %>%
  mutate(
    a_total = a1_sc + a2_sc + a3_sc + a4_sc + a5_sc + a6_sc +
      a7_sc + a8_sc + a9_sc + a10_sc + a11_sc + a12_sc,
    a_category = factor(
      if_else(a_total >= 24, "Positive Attitude", "Negative/Neutral Attitude"),
      levels = c("Negative/Neutral Attitude", "Positive Attitude")
    )
  )
head(a_scored %>% select(a1, a1_sc, a7, a7_sc, a_total, a_category))


#INTENTIONS
score_intention <- function(x) {
  case_when(
    str_to_lower(str_trim(as.character(x))) == "strongly agree" ~ 5,
    str_to_lower(str_trim(as.character(x))) == "agree" ~ 4,
    str_to_lower(str_trim(as.character(x))) == "undecided" ~ 3,
    str_to_lower(str_trim(as.character(x))) == "disagree" ~ 2,
    str_to_lower(str_trim(as.character(x))) == "strongly disagree" ~ 1,
    TRUE ~ 0
  )
}

i_data <- KAIdata[, 41:50]
colnames(i_data) <- paste0("i", 1:10)
i_scored <- i_data %>%
  mutate(
    i1_sc = score_intention(i1),
    i2_sc = score_intention(i2),
    i3_sc = score_intention(i3),
    i4_sc = score_intention(i4),
    i5_sc = score_intention(i5),
    i6_sc = score_intention(i6),
    i7_sc = score_intention(i7),
    i8_sc = score_intention(i8),
    i9_sc = score_intention(i9),
    i10_sc = score_intention(i10),
  )
i_scored <- i_scored %>%
  mutate(
    i_total = i1_sc + i2_sc + i3_sc + i4_sc + i5_sc +
      i6_sc + i7_sc + i8_sc + i9_sc + i10_sc,
    i_category = factor(
      if_else(i_total >= 30, "High Intention", "Low Intention"),
      levels = c("Low Intention", "High Intention")
    )
  )
head(i_scored %>% select(i1, i1_sc, i2, i2_sc, i_total, i_category))

#Attitude and Intention scale reliability
#psych::alpha(a_scored %>% select(ends_with("_sc")))
#psych::alpha(i_scored %>% select(ends_with("_sc")))

# Helper function to find column by keyword
find_col <- function(pattern, data = df_raw) {
  idx <- grep(pattern, colnames(data), ignore.case = TRUE)
  if (length(idx) > 0) return(as.character(data[[idx[1]]]))
  return(rep(NA_character_, nrow(data)))
}

# Build complete dataset
df_final <- tibble(
  age             = as.character(KAIdata[[3]]),   
  religion        = as.character(KAIdata[[4]]),   
  ethnicity       = as.character(KAIdata[[6]]),   
  level_study     = as.character(KAIdata[[8]]),   
  faculty         = as.character(KAIdata[[10]]),
  hall            = as.character(KAIdata[[12]]),
  marriage_timing = as.character(KAIdata[[14]]),  
  bf_discussion   = as.character(KAIdata[[15]]),  
  info_source     = as.character(KAIdata[[16]]),  
  k_category      = as.character(k_scored$k_category),
  a_category      = as.character(a_scored$a_category),
  i_category      = as.character(i_scored$i_category)
)
# Replace any blank spaces with NA and drop completely empty columns
df_final[df_final == ""] <- NA

#generating summary table
table1 <- df_final %>%
  select(
    age, religion, ethnicity, level_study, faculty,
    hall, marriage_timing, bf_discussion, info_source,
    k_category, a_category, i_category
  ) %>%
  tbl_summary(
    type = everything() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = list(
      age             ~ "Age Group",
      religion        ~ "Religion",
      ethnicity       ~ "Ethnic Group",
      level_study     ~ "Postgraduate Programme Level",
      faculty         ~ "College / Faculty",
      hall            ~ "Postgraduate Hall of Residence",
      marriage_timing ~ "Intended Marriage Timing",
      bf_discussion   ~ "Prior Breastfeeding Discussion",
      info_source     ~ "Source of Breastfeeding Information",
      k_category      ~ "Knowledge Level",
      a_category      ~ "Attitude Level",
      i_category      ~ "Support Intention Level"
    ),
    missing = "no"
  ) %>%
  bold_labels()

# Render Table 1 in the Viewer Pane
table1


# 1. Define item vectors from your scored datasets
s1_items <- paste0("k", 1:12, "_sc")
s2_items <- paste0("a", 1:12, "_sc")
s3_items <- paste0("i", 1:10, "_sc")

# 2. Extract numeric subscales (as base data frames)
s1_data <- as.data.frame(k_scored[, s1_items])
s2_data <- as.data.frame(a_scored[, s2_items])
s3_data <- as.data.frame(i_scored[, s3_items])

# 3. Calculate Cronbach's alpha for each section
cronbach_s1 <- psych::alpha(s1_data, check.keys = TRUE)$total$raw_alpha
cronbach_s2 <- psych::alpha(s2_data, check.keys = TRUE)$total$raw_alpha
cronbach_s3 <- psych::alpha(s3_data, check.keys = TRUE)$total$raw_alpha

# For binary/dichotomous items (Knowledge), KR-20 is mathematically identical to Cronbach's alpha
kr20_section1 <- cronbach_s1

# 4. Build the summary table dataframe
reliability_summary <- data.frame(
  Section        = c(
    "Section 1: Knowledge (12 items)", 
    "Section 2: Attitude (12 items)", 
    "Section 3: Intention (10 items)"
  ),
  N_items        = c(length(s1_items), length(s2_items), length(s3_items)),
  KR20           = c(round(kr20_section1, 3), NA, NA),
  Cronbach_Alpha = round(c(cronbach_s1, cronbach_s2, cronbach_s3), 3)
)

# 5. Print the output
cat("\n================ RELIABILITY COEFFICIENTS ================\n")
print(reliability_summary, row.names = FALSE)

#running the reliability command
#psych::alpha(a_scored[, paste0("a", 1:12, "_sc")])
#psych::alpha(i_scored[, paste0("i", 1:10, "_sc")])
#psych::alpha(k_scored[, paste0("k", 1:9, "_sc")])

