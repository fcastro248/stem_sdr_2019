library(dplyr)
library(here)
library(haven)
library(gtsummary)
library(survey)
library(lmtest)
library(sandwich)
library(WeightIt)
library(cobalt)

# Code for replicating results for:
# "STEM doctorate recipients with disabilities experienced early in life earn lower salaries and are underrepresented among higher academic positions"
# Nature Human Behaviour 8, 72–81 (2024). https://doi.org/10.1038/s41562-023-01745-z

# The dataset from the Survey of Doctorate Recipients is publicly available at the National Center for Science and Engineering Statistics' (NCSES) website:
# https://ncses.nsf.gov/explore-data/microdata/doctorate-recipients
# Licensed for use under NCSES Public Use Microdata terms. See https://ncses.nsf.gov/explore-data/microdata for details.

# Download "epsd19.sas7bdat" and place it in the "input" folder
df_raw <- read_sas(here("input", "epsd19.sas7bdat"))

# 1.Recoding variables and subsetting data ----

# Original var name: WTSURVY
# New var name: svyweights
df_raw <-
  df_raw |>
  mutate(surveyweights = WTSURVY)

# Original var name: GENDER
# New var name: sex
# Factors: 1 = "Female", 2 = "Male"
df_raw <- df_raw |>
  mutate(
    sex = case_when(
      GENDER == "F" ~ 1L,            # Female
      GENDER == "M" ~ 2L,            # Male
      TRUE          ~ NA_integer_
    ),
    sex = factor(sex, levels = c(1L, 2L))
  )

# Original var name: AGEGRP
# New var name: age_broader
# Factors: 1="29 or younger", 2="30-34", 3="35-39", 4="40-44", 5="45-49", 6="50-54", 7="55-59", 8="60-64", 9="65-69", 10="70-75"
df_raw <- df_raw |>
  mutate(
    age_broader = case_when(
      AGEGRP == 25 ~ 1L,  # 29 or younger
      AGEGRP == 30 ~ 2L,  # 30-34
      AGEGRP == 35 ~ 3L,  # 35-39
      AGEGRP == 40 ~ 4L,  # 40-44
      AGEGRP == 45 ~ 5L,  # 45-49
      AGEGRP == 50 ~ 6L,  # 50-54
      AGEGRP == 55 ~ 7L,  # 55-59
      AGEGRP == 60 ~ 8L,  # 60-64
      AGEGRP == 65 ~ 9L,  # 65-69
      AGEGRP == 70 ~ 10L, # 70-75
      TRUE        ~ NA_integer_
    ),
    age_broader = factor(age_broader, levels = 1L:10L)
  )

# Original var name: AGEGRP
# New var name: age_narrow
# Factors: 1="34 or younger", 2="35-44", 3="45-54", 4="55-64", 5="65 and older"
df_raw <- df_raw |>
  mutate(
    age_narrow = case_when(
      AGEGRP == 25 ~ 1L,  # 34 or younger  (orig: 29 or younger)
      AGEGRP == 30 ~ 1L,  # 34 or younger  (30–34)
      AGEGRP == 35 ~ 2L,  # 35–44          (35–39)
      AGEGRP == 40 ~ 2L,  # 35–44          (40–44)
      AGEGRP == 45 ~ 3L,  # 45–54          (45–49)
      AGEGRP == 50 ~ 3L,  # 45–54          (50–54)
      AGEGRP == 55 ~ 4L,  # 55–64          (55–59)
      AGEGRP == 60 ~ 4L,  # 55–64          (60–64)
      AGEGRP == 65 ~ 5L,  # 65 and older   (65–69)
      AGEGRP == 70 ~ 5L,  # 65 and older   (70–75)
      TRUE        ~ NA_integer_
    ),
    age_narrow = factor(age_narrow, levels = 1L:5L)
  )

# Original var name: MARSTA
# New var name: maritalst
# Factors: 1="Never married", 2="Separated, Divorced", 3="Widowed", 4="Married or living in a marriage-like relationship"
df_raw <- df_raw |>
  mutate(
    maritalst = case_when(
      MARSTA == 6 ~ 1L,              # Never married
      MARSTA %in% c(4, 5) ~ 2L,      # Separated, Divorced
      MARSTA == 3 ~ 3L,              # Widowed
      MARSTA %in% c(1, 2) ~ 4L,      # Married or living in a marriage-like relationship
      TRUE ~ NA_integer_
    ),
    maritalst = factor(maritalst, levels = 1L:4L)
  )

# Original var name: RACETHMP
# New var name: race
# Factors: 1="White only, NH", 2="Hispanic", 3="Black only, NH", 4="Asian only, NH", 5="Other, including multiracial, NH"
df_raw <- df_raw |>
  mutate(
    race = case_when(
      RACETHMP == 5 ~ 1L,  # White only, NH
      RACETHMP == 4 ~ 2L,  # Hispanic
      RACETHMP == 3 ~ 3L,  # Black only, NH
      RACETHMP == 1 ~ 4L,  # Asian only, NH
      RACETHMP == 7 ~ 5L,  # Other, including multiracial, NH
      TRUE         ~ NA_integer_
    ),
    race = factor(race, levels = 1L:5L)
  )

# Original var name: NSDRMEMTOD
# New var name: phdfield
# Factors: 1="Biological, agricultural, and environmental life sciences", 2="Computer and information sciences", 3="Mathematics and statistics", 4="Physical sciences, geosciences, atmospheric, and ocean sciences", 5="Psychology", 6="Social sciences", 7="Engineering", 8="Health"
df_raw <- df_raw |>
  mutate(
    phdfield = factor(NSDRMEMTOD, levels = 1L:8L)
  )

# Original var name: LFSTAT
# New var name: employstat
# Factors: 0="Unemployed", 1="Employed"
df_raw <- df_raw |>
  mutate(
    employstat = case_when(
      LFSTAT == 2 ~ 0L,  # Unemployed
      LFSTAT == 1 ~ 1L,  # Employed
      TRUE        ~ NA_integer_  # exclude "Not in labor force" (LFSTAT == 3)
    ),
    employstat = factor(employstat, levels = c(0L, 1L))
  )

# Original var name: SALARYP
# New var name: salary
# Conversion: set 9999998 → NA (numeric)
df_raw <- df_raw |>
  mutate(
    salary = case_when(
      SALARYP < 9999998  ~ SALARYP,   # non-missing salary
      SALARYP == 9999998 ~ NA_real_   # missing salary code → NA
    )
  )

# Original var name: EMRGP
# New var name: jobregion
# Factors: 1="West", 2="Midwest", 3="Northeast", 4="South" (codes "05" and "L" → NA)
df_raw <- df_raw |>
  mutate(
    jobregion = case_when(
      EMRGP == "01" ~ 1L,  # West
      EMRGP == "02" ~ 2L,  # Midwest
      EMRGP == "03" ~ 3L,  # Northeast
      EMRGP == "04" ~ 4L,  # South
      EMRGP %in% c("05", "L") ~ NA_integer_  # Abroad or logical skip → NA
    ),
    jobregion = factor(jobregion, levels = 1L:4L)
  )

# Original var name: N2OCPRMG
# New var name: jobfield
# Factors: 1="Computer and mathematical sciences", 2="Biological, agricultural, and other life sciences", 3="Physical and related scientists", 4="Social and related scientists", 5="Engineers", 6="Other S&E related occupations"
df_raw <- df_raw |>
  mutate(
    jobfield = case_when(
      N2OCPRMG == 1 ~ 1L,  # Computer and mathematical sciences
      N2OCPRMG == 2 ~ 2L,  # Biological, agricultural, and other life sciences
      N2OCPRMG == 3 ~ 3L,  # Physical and related scientists
      N2OCPRMG == 4 ~ 4L,  # Social and related scientists
      N2OCPRMG == 5 ~ 5L,  # Engineers
      N2OCPRMG == 6 ~ 6L,  # Other S&E related occupations
      N2OCPRMG > 6 ~ NA_integer_  # non-S&E (7) or logical skip (8) → NA
    ),
    jobfield = factor(jobfield, levels = 1L:6L)
  )

# Original var name: ACADNA / ACADADMN / ACADRCHF / ACADTCHF / ACADADJF / ACADPDOC / ACADOTHP
# New var name: academic_position
# Factors: (ACADNA -> NA), 1="Postdoc, research or teaching assistant", 2="Adjunct faculty", 3="Teaching faculty", 4="Research faculty", 5="Dean or president"
#Note: some participants might be categorized as "Y" for two positions at the same time. However, this sequence guarantees we categorize them based on the highest position they hold
#Note: this version of the variable is not used in the propensity score analysis
df_raw <- df_raw |>
  mutate(
    academic_position = case_when(
      ACADADMN == "Y"                    ~ 5L,           # Dean or president
      ACADRCHF == "Y"                    ~ 4L,           # Research faculty
      ACADTCHF == "Y"                    ~ 3L,           # Teaching faculty
      ACADADJF == "Y"                    ~ 2L,           # Adjunct faculty
      ACADPDOC == "Y" | ACADOTHP == "Y"  ~ 1L,           # Postdoc / research or teaching assistant
      TRUE                               ~ NA_integer_
    ),
    academic_position = factor(academic_position, levels = 1L:5L)
  )

# Original var name: ACADNA / ACADPDOC / ACADOTHP / ACADADJF / ACADTCHF / ACADRCHF / ACADADMN
# New var name: academic_administrator
# Factors: 0="Not an academic administrator", 1="Yes, academic administrator (Dean or president)"
df_raw <- df_raw |>
  mutate(
    academic_administrator = case_when(
      ACADADMN == "Y"                    ~ 1L,           # Dean or president → Admin
      ACADRCHF == "Y"                    ~ 0L,           # Research faculty → Not admin
      ACADTCHF == "Y"                    ~ 0L,           # Teaching faculty → Not admin
      ACADADJF == "Y"                    ~ 0L,           # Adjunct faculty → Not admin
      ACADPDOC == "Y" | ACADOTHP == "Y"  ~ 0L,           # Postdoc / research or teaching assistant → Not admin
      TRUE                               ~ NA_integer_
    ),
    academic_administrator = factor(academic_administrator, levels = c(0L, 1L))
  )

# Original var name: FACRANK
# New var name: facultyrank
# Factors: 1="Instructor/Lecturer", 2="Assistant Professor", 3="Associate Professor", 4="Professor"
df_raw <- df_raw |>
  mutate(
    facultyrank = case_when(
      FACRANK %in% c("L", "1", "2", "8") ~ NA_integer_,  # logical skip / not applicable → NA
      FACRANK %in% c("6", "7")          ~ 1L,            # Instructor / Lecturer
      FACRANK == "5"                    ~ 2L,            # Assistant Professor
      FACRANK == "4"                    ~ 3L,            # Associate Professor
      FACRANK == "3"                    ~ 4L,            # Professor
      TRUE                              ~ NA_integer_
    ),
    facultyrank = factor(facultyrank, levels = 1L:4L)
  )

# Original var name: TENSTA
# New var name: tenurestatus
# Factors: 1="Not on tenure track", 2="On tenure-track but not tenured", 3="Tenured"
df_raw <- df_raw |>
  mutate(
    tenurestatus = case_when(
      TENSTA %in% c("L", "1", "2") ~ NA_integer_,  # logical skip / not applicable → NA
      TENSTA == "5"                ~ 1L,           # Not on tenure track
      TENSTA == "4"                ~ 2L,           # On tenure-track but not tenured
      TENSTA == "3"                ~ 3L,           # Tenured
      TRUE                         ~ NA_integer_
    ),
    tenurestatus = factor(tenurestatus, levels = 1L:3L)
  )

# Original var name: HCAPIN
# New var name: disability_binary
# Factors: 0="No disability", 1="Yes disability"
#Note: not used for any of the analyses
df_raw <- df_raw |>
  mutate(
    disability_binary = case_when(
      HCAPIN == "N" ~ 0L,  # No disability
      HCAPIN == "Y" ~ 1L,  # Yes disability
      TRUE          ~ NA_integer_
    ),
    disability_binary = factor(disability_binary, levels = c(0L, 1L))
  )

# Original var name: HCAPIN / DIFAGEGR
# New var name: disability_age_groups
# Factors: 0="No disability", 1="Disability, onset at 25 years or later", 2="Disability, onset before 25 years of age"
# Final version of the disability variable used in all analyses
df_raw <- df_raw |>
  mutate(
    disability_age_groups = case_when(
      HCAPIN == "N"                  ~ 0L,  # No disability
      HCAPIN == "Y" & DIFAGEGR >= 25 ~ 1L,  # Disability, onset ≥25
      HCAPIN == "Y" & DIFAGEGR == 20 ~ 2L,  # Disability, onset <25 (coded 20)
      TRUE                           ~ NA_integer_
    ),
    disability_age_groups = factor(disability_age_groups, levels = c(0L, 1L, 2L))
  )

# Original var name: EDTP
# New var name: acad_inst_indicator
# Factors: 0="No, does not work at acad institutions (as defined in the manuscript)", 1="Yes, does work at acad institutions (as defined in the manuscript)"
df_raw <- df_raw |>
  mutate(
    acad_inst_indicator = case_when(
      EDTP %in% c("1", "2", "6", "L") ~ 0L,  # K–12, 2-year, Other, or logical skip → No
      EDTP %in% c("3", "4", "5")      ~ 1L,  # 4-year, Medical school, Research institute → Yes
      TRUE                             ~ NA_integer_
    ),
    acad_inst_indicator = factor(acad_inst_indicator, levels = c(0L, 1L))
  )

# Original var name: EMUS
# New var name: employer_us
# Factors: 0="No", 1="Yes"
df_raw <- df_raw |>
  mutate(
    employer_us = case_when(
      EMUS == "N" ~ 0L,             # No (Non-U.S.)
      EMUS == "Y" ~ 1L,             # Yes (U.S.)
      EMUS == "L" ~ NA_integer_,    # Logical skip → NA
      TRUE        ~ NA_integer_
    ),
    employer_us = factor(employer_us, levels = c(0L, 1L))
  )

# Original var name: FNINUS
# New var name: living_us
# Factors: 0="No", 1="Yes"
df_raw <- df_raw |>
  mutate(
    living_us = case_when(
      FNINUS == "N" ~ 0L,  # Not living in the US
      FNINUS == "Y" ~ 1L,  # Living in the US
      TRUE          ~ NA_integer_
    ),
    living_us = factor(living_us, levels = c(0L, 1L))
  )

# Original var name: WKSYR / HRSWKP
# New var name: fulltime
# Factors: 0="Not full-time (not 52 weeks or <36 hours/week)", 1="Full-time (52 weeks & ≥36 hours/week)"
df_raw <- df_raw |>
  mutate(
    fulltime = case_when(
      WKSYR != "1"                        ~ 0L,  # not 52 weeks/year
      WKSYR == "1" & HRSWKP %in% c(1, 2) ~ 0L,  # 20 or less / 21–35 hrs
      WKSYR == "1" & HRSWKP %in% c(3, 4) ~ 1L,  # 36–40 / >40 hrs
      WKSYR == "1" & HRSWKP == 98        ~ NA_integer_,  # logical skip
      TRUE                                ~ NA_integer_
    ),
    fulltime = factor(fulltime, levels = c(0L, 1L))
  )

# Original var name: GOVSUP
# New var name: funding_indic
# Factors: 0="No", 1="Yes"
df_raw <- df_raw |>
  mutate(
    funding_indic = case_when(
      GOVSUP == "N" ~ 0L,           # No federal funding
      GOVSUP == "Y" ~ 1L,           # Yes federal funding
      TRUE          ~ NA_integer_
    ),
    funding_indic = factor(funding_indic, levels = c(0L, 1L))
  )

# 2.Producing Table 1 ----
# Subsets used here are similar, but differentthan those used for the propensity score analyses

# 2.1. Top portion of Table 1 (applies to participants working in STEM in the U.S.)
# Creating flag: workstemus_flag
# Subsetting participants working in STEM in the U.S.
df_raw <- df_raw |>
  mutate(
    workstemus_flag = case_when(
      employer_us == "1" & #employes is located in the U.S.
      living_us  == "1" & #living in the U.S.
      employstat == "1" & #currently employed
      jobfield %in% 1:6 ~ 1L, #job field is S&E-related
          TRUE ~ 0L
      ),
    workstemus_flag = factor(workstemus_flag, levels = c(0L, 1L))
  )

# Subset of participants working in STEM in the U.S.
df_clean_stemus <-
  filter(df_raw, workstemus_flag == 1)

#Producing Table 1, top portion
table1_a <-
  survey::svydesign(~ 1, data = df_clean_stemus, weights = ~ surveyweights) |>
  tbl_svysummary(by = disability_age_groups,
  include = c(sex, age_broader, race, maritalst, phdfield, jobfield, 
  jobregion, fulltime, funding_indic, acad_inst_indicator),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
  missing = "no"
  )

# 2.2. Bottom portion of Table 1 (applies to participants working in STEM in the U.S. at academic institutions)
# Creating flag: academic_broad_flag
# Subsetting participants working in STEM in the U.S. at academic institutions
# Defined only by institutions that are 4-yr schools, medical schools, or research institutes
df_raw <- df_raw |>
  mutate(
    academic_broad_flag = case_when(
      workstemus_flag == "1" & #working in STEM in the U.S.
      acad_inst_indicator == "1" ~ 1L, #working at academic institution
          TRUE ~ 0L
      ),
    academic_broad_flag = factor(academic_broad_flag, levels = c(0L, 1L))
  )

# Subset of participants working in STEM in the U.S., at academic institutions
df_clean_academic_broad <-
  filter(df_raw, academic_broad_flag == 1)

# Producing Table 1, bottom portion
table1_b <-
  survey::svydesign(~ 1, data = df_clean_academic_broad, weights = ~ surveyweights) |>
  tbl_svysummary(by = disability_age_groups,
  include = c(facultyrank, tenurestatus, academic_administrator),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
  missing = "no"
  )

# Notes for following sections on propensity score analyses

# Sections 3 and 4 aim to reproduce the exact results from the manuscript using the original propensity score weights (.RData file included in repository) produced in the analyses in 2022 using the package "WeightIt"

# Section 5 (optional) provides the code to produce de novo propensity score weights. However, since we used a generalized boosted model approach without specifying a random seed value, it is not possible to replicate the exact results using de novo propensity score weights.


# 3.Propensity score weighting and linear regression - Table 2 (original paper) ----
# Participants working in STEM in the U.S.

# 3.1.Subsetting data
# Creating flag: table2_subset_flag
# Subsetting participants working in STEM in the U.S.
df_raw <- df_raw |>
  mutate(
    table2_subset_flag = case_when(
      workstemus_flag == "1" & #working in STEM in the U.S.
      funding_indic  %in% 0:1 ~ 1L, #information on funding by federal govt available
      TRUE ~ 0L
      ),
    table2_subset_flag = factor(table2_subset_flag, levels = c(0L, 1L))
  )

# Subset of participants working in STEM in the U.S.
table2_subset <-
  filter(df_raw, table2_subset_flag == 1)

#51,099 observations (unweighted)
nrow(table2_subset)

#Loading propensity score weights from original analyses (Sep 2022) using the package weightit
load(here("input", "w.out2_3_strat_09192022.RData"))

#PS weights used for the Table 2 analysis
ps_weights_table2 <- w.out2_strat_09192022

#Model for Table 2, stratifying
model2_strat <- lm(salary ~ disability_age_groups 
+ race + sex + maritalst + phdfield + jobregion + fulltime + jobfield
+ age_broader + funding_indic
, data = table2_subset, weights = ps_weights_table2$weights*ps_weights_table2$s.weights)

model2_coef_strat <- coeftest(model2_strat, vcov. = vcovCL, robust = "HC3", cluster =~age_narrow)
model2_ci_strat <- coefci(model2_strat, vcov. = vcovCL, robust = "HC3", cluster =~age_narrow)



# 4.Propensity score weighting and linear regression - Table 3  (original paper) ----
# For participants working in STEM in the U.S. at academic institutions

# Creating flag: table3_subset_flag
# Subsetting participants working in STEM in the U.S. at academic institutions
# Institutions that are 4-yr schools, medical schools, or research institutes
# Additionally, institutions where faculty ranks, tenure, and academic positions are available
df_raw <- df_raw |>
  mutate(
    table3_subset_flag = case_when(
      workstemus_flag == "1" & #working in STEM in the U.S.
      acad_inst_indicator == "1" & #working at academic institution
      facultyrank %in% 1:4 & #faculty ranks available
      tenurestatus %in% 1:3 & #tenure status available
      academic_position %in% 1:5 & #academic positions available
      funding_indic  %in% 0:1 ~ 1L, #information on funding by federal govt available
          TRUE ~ 0L
      ),
    table3_subset_flag = factor(table3_subset_flag, levels = c(0L, 1L))
  )

# Subset of participants working in STEM in the U.S., at academic institutions
table3_subset <-
  filter(df_raw, table3_subset_flag == 1)

#16,325 observations (unweighted)
nrow(table3_subset)

#PS weights used for the Table 3 analysis
ps_weights_table3 <- w.out3_strat_09192022

#Model for Table 3, stratifying
model3_strat <- lm(salary ~ disability_age_groups 
+ race + sex + maritalst + phdfield + jobregion + fulltime + jobfield
+ facultyrank + tenurestatus + academic_administrator
+ age_broader + funding_indic
, data = table3_subset, weights = ps_weights_table3$weights*ps_weights_table3$s.weights)

model3_coef_strat <- coeftest(model3_strat, vcov. = vcovCL, robust = "HC3", cluster =~age_narrow)
model3_ci_strat <- coefci(model3_strat, vcov. = vcovCL, robust = "HC3", cluster =~age_narrow)


# 5.Optional: code used for producing de novo propensity score weights. ----
# Generalized boosted model approach without specifying a random seed value. Not possible to replicate exact results due to lack of random seed in original analyses.

# 5.1.Optional: de novo analysis for Table 2 ----
# Producing propensity score weights for Table 2
# ps_weights_table2 <- weightit(disability_age_groups ~ race + sex + maritalst + phdfield + jobregion + fulltime + jobfield + age_broader + funding_indic, data = table2_subset, 
# method = "gbm", estimand = "ATE", stop.method = "ks.mean", s.weights = 'surveyweights',
# by = 'age_narrow')

# Examining covariate balance for Table 2
# bal.tab(ps_weights_table2, which.treat = c("No disab"), stat = "mean.diffs")
# love.plot(ps_weights_table2,
# which.treat = c("No disab"), stat = "mean.diffs",
# colors = c("red", "blue"),
# shapes = c("triangle filled", "circle filled"),
# line = FALSE)

# Model for Table 2, stratifying
# model2_strat <- lm(salary ~ disability_age_groups 
# + race + sex + maritalst + phdfield + jobregion + fulltime + jobfield
# + age_broader + funding_indic
# , data = table2_subset, weights = ps_weights_table2$weights*ps_weights_table2$s.weights)
# 
# model2_coef_strat <- coeftest(model2_strat, vcov. = vcovCL, robust = "HC3", cluster =~age_narrow)
# model2_ci_strat <- coefci(model2_strat, vcov. = vcovCL, robust = "HC3", cluster =~age_narrow)

# 5.2.Optional: de novo analysis for Table 3 ----
# Producing propensity score weights for Table 3
# ps_weights_table3 <- weightit(disability_age_groups ~ race + sex + maritalst + phdfield + jobregion + fulltime + jobfield + age_broader + funding_indic + facultyrank + tenurestatus + academic_administrator, data = table3_subset, method = "gbm", estimand = "ATE", stop.method = "ks.mean", s.weights = 'surveyweights',
# by = 'age_narrow')

# Examining covariate balance for Table 3
# bal.tab(ps_weights_table3, which.treat = c("No disab"), stat = "mean.diffs")
# love.plot(ps_weights_table3,
# which.treat = c("No disab"), stat = "mean.diffs",
# colors = c("red", "blue"),
# shapes = c("triangle filled", "circle filled"),
# line = FALSE)

# Model for Table 3, stratifying
# model3_strat <- lm(salary ~ disability_age_groups 
# + race + sex + maritalst + phdfield + jobregion + fulltime + jobfield
# + facultyrank + tenurestatus + academic_administrator
# + age_broader + funding_indic
# , data = table3_subset, weights = ps_weights_table3$weights*ps_weights_table3$s.weights)
# 
# model3_coef_strat <- coeftest(model3_strat, vcov. = vcovCL, robust = "HC3", cluster =~age_narrow)
# model3_ci_strat <- coefci(model3_strat, vcov. = vcovCL, robust = "HC3", cluster =~age_narrow)
