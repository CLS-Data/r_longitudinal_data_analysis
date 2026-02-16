# Interlude: Data Cleaning {#sec-interlude} ----

library(tidyverse)
library(haven)
library(glue)
library(labelled)
library(magrittr)

mcs_fld <- Sys.getenv("mcs_fld")
ncds_fld <- Sys.getenv("ncds_fld")

negative_to_na <- function(x){
  na_range(x) <- c(-Inf, -1)
  user_na_to_na(x)
}

values_to_na <- function(x, values){
  na_values(x) <- values
  user_na_to_na(x)
}

## NCDS ----
ncds_raw <- list()
ncds_clean <- list()

load_ncds <- function(file, vars = c()){
  glue("{ncds_fld}/{file}") %>%
    read_dta(col_select = c(matches("NCDSID|ncdsid"),
                            all_of(vars)))
}

## a. Sample, Sex and Ethnicity ----
ncds_raw$xwave <- load_ncds("xwave/ncds_response.dta",
                            c("n622", "ethnicid"))

ncds_clean$xwave <- ncds_raw$xwave %>%
  mutate(
    sex = negative_to_na(n622) %>% as_factor(),
    ethnic_group = case_when(
      ethnicid == 1 ~ "White",
      ethnicid %in% 2:6 ~ "Non-White"
    ) %>%
      factor(c("White", "Non-White"))
  ) %>%
  select(ncdsid, sex, ethnic_group)

## b. Parental Characteristics and Cohort Member Cognitive Ability ----
ncds_raw$sweep_0y_16y <- load_ncds("0y-16y/ncds0123.dta",
                                   c("n914", "n2928", "n2396", "n2397",
                                     "n1196", "n1199", "n1202", "n1205"))

lookup_ncds_parent_edu <- c(7:13, 15, 17, 19) 

ncds_clean$sweep_0y_16y <- ncds_raw$sweep_0y_16y %>%
  mutate(
    across(where(is.labelled), negative_to_na),
    
    father_edu_years = lookup_ncds_parent_edu[n2396],
    mother_edu_years = lookup_ncds_parent_edu[n2397],
    
    father_weight_kg = n1196 * 0.5 * 6.35029, # Increments 1/2 stone units
    father_height_m = n1199 * 2.54 / 100, # In inches
    father_bmi = father_weight_kg / (father_height_m^2),
    mother_weight_kg = n1202 * 0.5 * 6.35029,
    mother_height_m = n1199 * 2.54 / 100,
    mother_bmi = mother_weight_kg / (mother_height_m^2),
    
    verbal_11 = as.double(n914),
    vocab_16 = as.double(n2928),
  ) %>%
  select(ncdsid, father_edu_years:vocab_16,
         -matches("_(height_m|weight_kg)$"))

## c. Cohort Member BMI ----
ncds_raw$anthropometrics <- tribble(
  ~file, ~vars,
  "0y-16y/ncds0123.dta", c("dvht07", "dvht11", "dvht16", "dvwt07", "dvwt11", "dvwt16"),
  "23y/ncds4.dta", c("dvwt23", "dvht23"),
  "33y/ncds5cmi.dta", c("n504731", "n504734"),
  "42y/ncds6.dta", c("htmetre2", "htcms2", "htfeet2", "htinche2", 
                     "wtkilos2", "wtstone2", "wtpound2"),
  "42y-44y Biomedical/ncds42-4_biomedical_eul.dta", c("htres", "wtres"),
  "50y/ncds_2008_followup.dta", c("DVHT50", "DVWT50"),
  "55y/ncds_2013_derived.dta", c("ND9HGHTM", "ND9WGHTK"),
) %$% # magrittr 'exposition' pipe - allows you to extract columns within pipe
  map2(file, vars, 
       ~ load_ncds(.x, .y) %>%
         rename_with(str_to_lower)) %>%
  reduce(~ full_join(.x, .y, by = "ncdsid"))

ncds_clean$anthropometrics <- ncds_raw$anthropometrics %>%
  rename_with(~ str_replace(.x, "^dvht", "height_") %>%
                str_replace("^dvwt", "weight_")) %>%
  rename(height_55 = nd9hghtm,
         weight_55 = nd9wghtk) %>%
  mutate(
    across(where(is.labelled), negative_to_na),
    
    height_33 = values_to_na(n504731, 0) / 100,
    weight_33 = n504734,
    
    height_42 = case_when(
      between(htmetre2, 1, 2) & between(htcms2, 0, 100) ~ htmetre2 + (htcms2 / 100),
      between(htfeet2, 3, 8) & between(htinche2, 0, 11) ~ ((htfeet2 * 12) + htinche2) * 2.54 / 100
    ),
    weight_42 = case_when(
      !is.na(wtkilos2) ~ wtkilos2,
      between(wtstone2, 5, 31) & between(wtpound2, 0, 15) ~ ((wtstone2 * 16) + wtpound2) * 0.453592,
    ),
    
    height_44 = htres / 100,
    weight_44 = wtres,
  ) %>%
  select(ncdsid, matches("^(h|w)eight_\\d\\d$")) %>%
  pivot_longer(-ncdsid,
               names_pattern = "(.eight)_(..)",
               names_to = c(".value", "fup"),
               names_transform = list(fup = as.integer)) %>%
  mutate(height_20 = ifelse(fup >= 20, height, NA)) %>%
  group_by(ncdsid) %>%
  fill(height_20, .direction = "downup") %>%
  ungroup() %>%
  mutate(height = ifelse(fup >= 20 & is.na(height), height_20, height),
         bmi = weight / (height ^ 2),
         obese = ifelse(bmi >= 30, "Obese", "Not Obese") %>%
           factor(c("Not Obese", "Obese"))) %>%
  select(-height_20)

## d. Combine Cleaned Data ----
df_ncds <- reduce(ncds_clean, ~ left_join(.x, .y, by = "ncdsid")) %>%
  relocate(fup, .after = ncdsid) %>%
  rename(iid = ncdsid) %>%
  zap_labels() %>%
  zap_formats() %>%
  set_variable_labels(
    iid = "Individual ID",
    fup = "Follow-Up Age",
    sex = "Sex",
    ethnic_group = "Ethnic Group",
    father_edu_years = "Father's Educational Years",
    mother_edu_years = "Mother's Educational Years",
    father_bmi = "Father's BMI",
    mother_bmi = "Mother's BMI",
    verbal_11 = "Verbal Score @ Age 11y",
    vocab_16 = "Vocabulary Score @ Age 16y",
    height = "Height (m)",
    weight = "Weight (kg)",
    bmi = "Body Mass Index (kg/m^2^)",
    obese = "Obese (BMI ≥ 30 kg/m^2^)"
  )

## MCS ----
mcs_raw <- list()
mcs_clean <- list()

mcs_fups <- c(0, 3, 5, 7, 11, 14, 17)

load_mcs <- function(file, vars = c()){
  glue("{mcs_fld}/{file}") %>%
    read_dta(col_select = c(matches("^(MCSID|.(C|P)NUM00)$"),
                            all_of(vars)))
}

## a. Sample ----
mcs_raw$xwave <- load_mcs("xwave/mcs_longitudinal_family_file.dta",
                          c("NOCMHH", "DATA_AVAILABILITY"))

mcs_clean$xwave <- mcs_raw$xwave %>%
  filter(DATA_AVAILABILITY == 0) %>%
  uncount(NOCMHH, .id = "CNUM00") %>%
  select(MCSID, CNUM00)

## b. Household Grid ----
# To determine parents and cohort member sex
load_mcs_hhgrid <- function(sweep){
  fup <- mcs_fups[sweep]
  
  glue("{mcs_fld}/{fup}y/mcs{sweep}_hhgrid.dta") %>%
    read_dta(col_select = matches("MCSID|CNUM|CSEX|PNUM|PSEX|CREL")) %>%
    rename(CNUM00 = matches("^.CNUM00"),
           PNUM00 = matches("^.PNUM00"),
           rel = matches("CREL"),
           cm_sex = matches("CSEX"),
           p_sex = matches("PSEX")) %>%
    mutate(sweep = !!sweep,
           .before = 1)
}

mcs_raw$hhgrid <- map_dfr(1:7, load_mcs_hhgrid)

## c. Cohort Member Sex ----
mcs_clean$cm_sex <- mcs_raw$hhgrid %>%
  filter(CNUM00 %in% 1:2) %>%
  mutate(sex = negative_to_na(cm_sex) %>%
           factor(1:2, labels = c("Male", "Female"))) %>%
  drop_na(sex) %>%
  count(MCSID, CNUM00, sex) %>%
  group_by(MCSID, CNUM00) %>%
  slice_max(n) %>% # Keep most common response (in case of changes)
  slice(1) %>% # Keep first most common so unique per individual
  ungroup() %>%
  select(-n)

## d. Cohort Member Ethnic Group ----
load_cm_ethnicity <- function(sweep){
  fup <- mcs_fups[sweep]
  
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_derived.dta") %>%
    read_dta(col_select = matches("MCSID|CNUM|CE06|C06E")) %>%
    rename(CNUM00 = matches("^.CNUM00"),
           ethnicity_raw = matches("CE06|C06E")) %>%
    mutate(sweep = !!sweep)
}

mcs_raw$cm_ethnicity <- map_dfr(1:6, load_cm_ethnicity)

mcs_clean$cm_ethnicity <- mcs_raw$cm_ethnicity %>%
  mutate(
    ethnic_group = case_when(ethnicity_raw == 1 ~ "White",
                             ethnicity_raw %in% 2:6 ~ "Non-White") %>% 
      factor(c("White", "Non-White"))
  ) %>%
  drop_na(ethnic_group) %>%
  group_by(MCSID, CNUM00) %>%
  summarise(ethnic_group = first(ethnic_group),
            .groups = "drop")


## e. Parental Characteristics ----
mcs_clean$parent_grid <- mcs_raw$hhgrid %>%
  filter(rel %in% 7:10) %>% # Natural, adoptive, foster or step-parent.
  mutate(parent_type = case_when(p_sex == 1 ~ "father", 
                                 p_sex == 2 ~ "mother")) %>%
  drop_na(parent_type) %>%
  group_by(MCSID, parent_type) %>%
  slice_min(sweep) %>% # First parent of parent type
  slice_min(rel) %>%
  slice(1) %>%
  ungroup() %>%
  select(MCSID, PNUM00, parent_type)

load_parent_data <- function(sweep){
  fup <- mcs_fups[sweep]
  
  glue("{mcs_fld}/{fup}y/mcs{sweep}_parent_derived.dta") %>%
    read_dta(col_select = matches("MCSID|PNUM|DDBMI00|ACAQ00")) %>%
    rename(PNUM00 = matches("^.PNUM00"),
           bmi_raw = matches("^.DDBMI00"),
           nvq_raw = matches("ACAQ00")) %>%
    mutate(sweep = !!sweep)
}

mcs_raw$parent_char <- map_dfr(1:7, load_parent_data)

mcs_clean$parent_char <- mcs_raw$parent_char %>%
  mutate(across(where(is.labelled), negative_to_na),
         edu_level = ordered(nvq_raw, 
                                   levels = c(96, 95, 1:5),
                                   labels = c("No Qualifications", 
                                              "Foreign Qualification", 
                                              glue("NVQ Level {1:5}")))) %>%
  group_by(MCSID, PNUM00) %>%
  summarise(bmi = first(bmi_raw, na_rm = TRUE),
            edu_level = max(edu_level, na.rm = TRUE),
            .groups = "drop") %>%
  inner_join(mcs_clean$parent_grid, 
             by = c("MCSID", "PNUM00")) %>%
  select(-PNUM00) %>%
  pivot_wider(id_cols = MCSID,
              names_from = parent_type,
              values_from = c(bmi, edu_level),
              names_glue = "{parent_type}_{.value}") 


## f. Cohort Member Cognitive Ability ----
mcs_raw$verbal_11 <- load_mcs("11y/mcs5_cm_derived.dta", "EVSABIL")

mcs_raw$vocab_14 <- load_mcs("14y/mcs6_cm_cognitive_assessment.dta",
                             glue("FCCMCOG{LETTERS[1:20]}"))

clean_vocab_item <- function(item){
  value_labels <- val_labels(item)
  correct_answer <- value_labels[str_detect(names(value_labels), "\\(correct answer\\)")]
  
  case_when(item == correct_answer ~ 1,
            item %in% 1:5 ~ 0)
}

mcs_clean$cog <- mcs_raw$verbal_11 %>%
  full_join(mcs_raw$vocab_14, by = c("MCSID", ECNUM00 = "FCNUM00")) %>%
  mutate(
    verbal_11 = negative_to_na(EVSABIL),
    
    across(matches("FCCMCOG[A-T]"), clean_vocab_item),
    vocab_14 = pick(matches("FCCMCOG[A-T]")) %>% rowSums(na.rm = TRUE),
    vocab_14_obs = pick(matches("FCCMCOG[A-T]")) %>% rowSums(!is.na(.)),
    vocab_14 = ifelse(vocab_14_obs >= 1, vocab_14, NA)
  ) %>%
  select(MCSID, CNUM00 = ECNUM00, verbal_11, vocab_14)

## g. Cohort Member BMI ----
mcs_raw$anthro_3y <- load_mcs("3y/mcs2_cm_interview.dta",
                              c("BCWTKC00", "BCWTGC00", "BCHCMC00", "BCHMMC00"))

load_cm_bmi <- function(sweep){
  fup <- mcs_fups[sweep]
  
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_interview.dta") %>%
    read_dta(col_select = matches("MCSID|CNUM|(W|H)TCM(A|0)0")) %>%
    rename(CNUM00 = matches("^.CNUM00"),
           height_raw = matches("^.CHTCM(A|0)0"),
           weight_raw = matches("^.CWTCM(A|0)0")) %>%
    mutate(sweep = !!sweep) 
}

mcs_raw$anthro_5y_17y <- map_dfr(3:7, load_cm_bmi)

mcs_clean$anthro <- mcs_raw$anthro_3y %>%
  mutate(
    across(where(is.labelled), negative_to_na),
    
    height = (BCHCMC00 + BCHMMC00/10) / 100,
    weight = (BCWTKC00 + BCWTGC00*10),
    
    sweep = 2
  ) %>%
  select(MCSID, CNUM00 = BCNUM00, sweep, height, weight) %>%
  bind_rows(
    mcs_raw$anthro_5y_17y %>%
      mutate(height = negative_to_na(height_raw) / 100,
             weight = negative_to_na(weight_raw)) %>%
      select(-matches("_raw$"))
  ) %>%
  mutate(bmi = weight / (height ^ 2),
         obese = ifelse(bmi >= 30, "Obese", "Not Obese") %>%
           factor(c("Not Obese", "Obese")))

# f. Combine Cleaned Data ----
df_mcs <- mcs_clean$xwave %>%
  uncount(length(mcs_fups), .id = "sweep") %>% # To ensure 1 row per sweep for every cohort member
  left_join(mcs_clean$cm_sex, by = c("MCSID", "CNUM00")) %>%
  left_join(mcs_clean$cm_ethnicity, by = c("MCSID", "CNUM00")) %>%
  left_join(mcs_clean$cog, by = c("MCSID", "CNUM00")) %>%
  left_join(mcs_clean$parent_char, by = "MCSID") %>%
  left_join(mcs_clean$anthro, by = c("MCSID", "CNUM00", "sweep")) %>%
  mutate(iid = glue("{MCSID}_{CNUM00}"),
         fup = mcs_fups[sweep], .after = sweep) %>%
  filter(sweep %in% mcs_clean$anthro$sweep) %>%
  select(-c(MCSID:sweep)) %>%
  zap_labels() %>%
  zap_formats() %>%
  set_variable_labels(
    iid = "Individual ID",
    fup = "Follow-Up Age",
    sex = "Sex",
    ethnic_group = "Ethnic Group",
    father_edu_level = "Father's Highest Qualification",
    mother_edu_level = "Mother's Highest Qualification",
    father_bmi = "Father's BMI",
    mother_bmi = "Mother's BMI",
    verbal_11 = "Verbal Score @ Age 11y",
    vocab_14 = "Vocabulary Score @ Age 14y",
    height = "Height (m)",
    weight = "Weight (kg)",
    bmi = "Body Mass Index (kg/m^2^)",
    obese = "Obese (BMI ≥ 30 kg/m^2^)"
  )

saveRDS(df_mcs, "data/mcs_clean.Rds")

## The Cleaned Datasets ----
lookfor(df_ncds)
lookfor(df_mcs)

