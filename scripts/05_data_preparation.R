# Preparing Data for Analysis {#sec-data_munging} ----
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(haven)
library(glue)
library(labelled)
mcs_fld <- Sys.getenv("mcs_fld")
ncds_fld <- Sys.getenv("ncds_fld")

mcs_17y <- glue("{mcs_fld}/17y/mcs7_cm_interview.dta") %>%
  read_dta()
## Advanced Functions for Data Cleaning ----
### `tidyselect` Helpers ----
mcs_17y %>%
  select(matches("^GCW")) %>%
  select(GCWESM00:last_col()) %>%
  select(where(is.labelled)) %>%
  select(all_of(c("GCWWOP00", "GCWWUS00", "GCWWRE00"))) %>%
  select(any_of(c("GCWWOP00", "GCWWRE00", "not_a_variable"))) %>%
  select(!matches("^GCWWO"))
### `dplyr::rename_with()` ----

mcs_17y %>%
  rename_with(str_to_lower) %>% # Equivalent to ~ str_to_lower(.x)
  rename_with(~ str_remove(.x, "^g")) %>%  # Removes sweep-specific prefix "G"
  rename_with(~ glue("{.x}_17"),
              .cols = -c(mcsid, cnum00))
### Task I ----

ncds_50y <- glue("{ncds_fld}/50y/ncds_2008_followup.dta") %>% 
  read_dta() 
ncds_62y <- glue("{ncds_fld}/62y/ncds10_age62_main_interview.dta") %>% 
  read_dta()

lookfor(ncds_50y, "derived")
lookfor(ncds_62y, "derived")

ncds_50y %>%
  select(NCDSID, matches("ND8"))

ncds_62y %>%
  select(ncdsid, matches("nd10"))

names(ncds_50y)
names(ncds_62y)

harmonise_names <- function(df){
  df %>%
    rename_with(str_to_lower) %>%
    rename_with(~ str_remove(.x, "^nd(8|10)") %>%
                  paste0("_derived"),
                .cols = matches("^nd(8|10)")) %>%
    rename_with(~ str_remove(.x, "^n(8|10)"),
                .cols = -matches("_derived$"))
}

harmonise_names(ncds_50y)
harmonise_names(ncds_62y)
### `dplyr::across()` ----
negative_to_na <- function(x){
  na_range(x) <- c(-Inf, -1)
  user_na_to_na(x)
}

mcs_17y %>%
  select(MCSID, GCNUM00, matches("^GCHT(CM|FT|IN)00")) %>%
  mutate(across(where(is.labelled), ~ negative_to_na(.x)))
mcs_17y %>%
  select(matches("^GCHT(CM|FT|IN)00")) %>%
  mutate(across(where(is.labelled), ~ negative_to_na(.x))) %>%
  summarise(across(where(is.numeric),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))
mcs_17y %>%
  mutate(across(where(is.labelled), ~ negative_to_na(.x))) %>%
  count(across(matches("^GCWW")))
### Some Helpful Functions for Labelled Data ----
values_to_na <- function(x, values){
  na_values(x) <- values
  user_na_to_na(x)
}

mcs_17y %>%
  mutate(self_rated_health = negative_to_na(GCCGHE00) %>%
           values_to_na(6:8) %>%
           as_factor()) %>%
  count(GCCGHE00, self_rated_health)
fix_labels <- function(x){
  if (is.null(val_labels(x))){
    clean <- zap_labels(x)
  } else{
    clean <- as_factor(x)
  }
  return(clean)
}

mcs_17y %>%
  select(GCSDQH00, GCHTCM00) %>%
  mutate(across(everything(), 
                ~ negative_to_na(.x) %>%
                  fix_labels()))
mcs_17y %>%
  mutate(weight = negative_to_na(GCWTCM00),
         height = negative_to_na(GCHTCM00) / 100,
         bmi = weight / (height ^ 2)) %>%
  select(MCSID, cnum = GCNUM00, 
         height, weight, bmi) %>%
  set_variable_labels(
    MCSID = "Family ID",
    cnum = "Cohort Member Number",
    height = "Height (cm)",
    weight = "Weight (kg)",
    bmi = "Body Mass Index (kg/m^2)"
  )
### `dplyr::if_any()` / `dplyr::if_all()` ----
mcs_17y %>%
  mutate(across(where(is.labelled), ~ negative_to_na(.x))) %>%
  count(across(matches("^GCWW"))) %>%
  filter(if_any(matches("^GCWW"), ~ .x == 1))

mcs_17y %>%
  mutate(across(where(is.labelled), ~ negative_to_na(.x))) %>%
  count(across(matches("^GCWW"))) %>%
  filter(if_all(matches("^GCWW"), ~ .x == 1))
### `dplyr::pick()` ----
mcs_17y %>%
  select(matches("^GCWW")) %>%
  mutate(across(everything(), ~ negative_to_na(.x)),
         swemwbs_total = pick(matches("^GCWW")) %>% rowSums())
### Task II ----

ncds_55y <- glue("{ncds_fld}/55y/ncds_2013_flatfile.dta") %>% 
  read_dta()

lookfor(ncds_55y, "casp")

ncds_55y %>%
  mutate(across(matches("N9CASP"), negative_to_na)) %>%
  lookfor("casp")

ncds_55y %>%
  select(matches("N9CASP")) %>%
  mutate(across(matches("N9CASP"), negative_to_na),
         across(matches("N9CASP[4-6])"), ~ 5 - .x),
         casp6_mean = rowMeans(pick(matches("N9CASP[1-6]")), na.rm = TRUE), 
         casp6_missing = rowSums(is.na(pick(matches("N9CASP[1-6]")))),
         casp6_mean = ifelse(casp6_missing >= 4, NA, casp6_mean)) %>%
  group_by(casp6_missing) %>%
  slice(1) %>%
  ungroup()

mcs_14y_cog <- glue("{mcs_fld}/14y/mcs6_cm_cognitive_assessment.dta") %>%
  read_dta()

lookfor(mcs_14y_cog)
lookfor(mcs_14y_cog, "FCCMCOG[A-T]")

val_labels(mcs_14y_cog$FCCMCOGA)
val_labels(mcs_14y_cog$FCCMCOGT)

clean_vocab_item <- function(item){
  value_labels <- val_labels(item)
  correct_answer <- value_labels[str_detect(names(value_labels), "\\(correct answer\\)")]
  
  case_when(item == correct_answer ~ 1,
            item %in% 1:5 ~ 0)
}

mcs_14y_cog %>%
  select(MCSID, FCNUM00, matches("FCCMCOG[A-T]")) %>%
  mutate(across(matches("FCCMCOG[A-T]"), clean_vocab_item),
         cog_score = pick(matches("FCCMCOG[A-T]")) %>% rowSums(na.rm = TRUE),
         cog_obs = pick(matches("FCCMCOG[A-T]")) %>% rowSums(!is.na(.)),
         cog_score = ifelse(cog_obs >= 1, cog_score, NA)) %>%
  select(MCSID, FCNUM00, cog_score)
## Writing Functions to Clean Data ----
mcs_fups <- c(0, 3, 5, 7, 11, 14, 17)

clean_bmi <- function(sweep){
  sweep_letter <- letters[sweep]
  fup <- mcs_fups[sweep]
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_interview.dta") %>%
    read_dta(col_select = c("MCSID",
                            matches("^.(CNUM|C(W|H)TCM)(A|0)0"))) %>%
    rename(cnum = matches(".CNUM00"),
           height_raw = matches(".CHTCM(A|0)0"),
           weight_raw = matches(".CWTCM(A|0)0")) %>%
    mutate(height = negative_to_na(height_raw) %>%
             as.numeric(),
           weight = negative_to_na(weight_raw) %>%
             as.numeric(),
           bmi = weight / ( (height / 100) ^ 2),
           fup = !!fup) %>%
    select(MCSID, cnum, fup, height, weight)
}

map(3:7, ~ clean_bmi(.x))
## Combining `tibbles` ----
load_mcs_mini <- function(sweep){
  sweep_letter <- letters[sweep]
  fup <- mcs_fups[sweep]
  
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_interview.dta") %>%
    read_dta(col_select = c("MCSID", matches("^.(CNUM|C(W|H)TCM)(A|0)0"))) %>%
    filter(MCSID %in% c("M10020R", "M10025W", "M10002P")) %>%
    arrange(MCSID)
}

mcs_14y_mini <- load_mcs_mini(6)
mcs_14y_mini

mcs_17y_mini <- load_mcs_mini(7)
mcs_17y_mini
### Merging `tibbles` with `dplyr::*_join()` ----
full_join(mcs_14y_mini,
          mcs_17y_mini,
          by = c("MCSID", FCNUM00 = "GCNUM00"))

left_join(mcs_14y_mini,
          mcs_17y_mini,
          by = c("MCSID", FCNUM00 = "GCNUM00"))

right_join(mcs_14y_mini,
           mcs_17y_mini,
           by = c("MCSID", FCNUM00 = "GCNUM00"))

inner_join(mcs_14y_mini,
           mcs_17y_mini,
           by = c("MCSID", FCNUM00 = "GCNUM00"))
#### Merging 2+ `tibbles` with `purrr::reduce()` ----
mcs_11y_mini <- load_mcs_mini(5) 

list(mcs_11y_mini,
     mcs_14y_mini, 
     mcs_17y_mini) %>%
  reduce(
    ~ inner_join(
      .x %>% rename(CNUM = matches("CNUM")),
      .y %>% rename(CNUM = matches("CNUM")) ,
      by = c("MCSID", "CNUM")
    )
  )
#### Filtering Joins ----
semi_join(mcs_14y_mini,
          mcs_17y_mini,
          by = c("MCSID", FCNUM00 = "GCNUM00"))

anti_join(mcs_14y_mini,
          mcs_17y_mini,
          by = c("MCSID", FCNUM00 = "GCNUM00"))
### Appending `tibbles` with `dplyr::bind_rows()` ----
# Two equivalent approaches.
# list(
#   `11` = mcs_11y_mini,
#   `14` = mcs_14y_mini,
#   `17` = mcs_17y_mini
# ) %>%
#   bind_rows(.id = "fup")

bind_rows(
  `11` = mcs_11y_mini,
  `14` = mcs_14y_mini,
  `17` = mcs_17y_mini, 
  .id = "fup"
)
list(
  `11` = mcs_11y_mini ,
  `14` = mcs_14y_mini,
  `17` = mcs_17y_mini
) %>%
  map(
    ~ .x %>% 
      rename_with(~ str_sub(.x, 2),
                  .cols = -MCSID)
  ) %>%
  bind_rows(.id = "fup")

list(
  `11` = mcs_11y_mini ,
  `14` = mcs_14y_mini,
  `17` = mcs_17y_mini
) %>%
  map_dfr(~ .x %>% rename_with(~ str_sub(.x, 2), .cols = -MCSID),
          .id = "fup") %>%
  mutate(height = coalesce(CHTCMA0, CHTCM00), 
         weight = coalesce(CWTCMA0, CWTCM00),
         .after = CNUM00)
### Task III ----

ncds_list <- list( # Just to focus on main ID variable
  ncds_50y %>% select(NCDSID),
  ncds_55y %>% select(NCDSID),
  ncds_62y %>% select(NCDSID = ncdsid)
)

reduce(ncds_list, ~ full_join(.x, .y, by = "NCDSID")) %>%
  nrow()
reduce(ncds_list, ~ inner_join(.x, .y, by = "NCDSID")) %>%
  nrow()
reduce(ncds_list, ~ right_join(.x, .y, by = "NCDSID")) %>%
  nrow()

load_anthro <- function(sweep){
  sweep_letter <- letters[sweep]
  fup <- mcs_fups[sweep]
  
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_interview.dta") %>%
    read_dta(col_select = c("MCSID",
                            matches("^.(CNUM|C(W|H)TCM)(A|0)0"))) %>%
    rename(cnum = matches(".CNUM00"),
           height_raw = matches(".CHTCM(A|0)0"),
           weight_raw = matches(".CWTCM(A|0)0")) %>%
    mutate(sweep = !!sweep)
}

task3_solution <- map_dfr(
  3:7, 
  ~ load_anthro(.x) %>%
    mutate(cnum = as.integer(cnum),
           height = negative_to_na(height_raw) %>%
             as.numeric(),
           weight = negative_to_na(weight_raw) %>%
             as.numeric(),
           bmi = weight / ( (height / 100) ^ 2)) %>%
    select(MCSID, cnum, sweep, height, weight, bmi)
)

task3_solution
## Reshaping `tibbles` with `tidyr::pivot_*()` ----
### Wide-to-Long Transformations with `tidyr::pivot_longer()` ----
df_mini_wide <- list(mcs_11y_mini,
                     mcs_14y_mini, 
                     mcs_17y_mini) %>%
  reduce(
    ~ full_join(
      .x %>% rename(CNUM = matches("CNUM")),
      .y %>% rename(CNUM = matches("CNUM")) ,
      by = c("MCSID", "CNUM")
    )
  ) %>%
  rename_with(~ str_replace(.x, "A0$", "00"))

df_mini_wide
df_mini_wide %>%
  pivot_longer(
    cols = -c(MCSID, CNUM), # Columns to pivot (everything but MCSID and CNUM)
    names_to = "variable",
    values_to = "value"
  )
df_mini_wide %>%
  pivot_longer(
    cols = matches("^.C(W|H)TCM00"), # The columns to pivot.
    names_pattern = "(.)(.*)", # Two groups: first letter, and subsequent letters
    names_to = c("sweep", ".value")) # Names for the new groups
df_mini_wide %>%
  pivot_longer(
    cols = matches("^.C(W|H)TCM00"), # The columns to pivot.
    names_pattern = "(.)C(..)CM00", # Two groups: first letter, and third and fourth letters
    names_to = c("sweep_letter", ".value"))  %>%
  mutate(sweep = match(sweep_letter, LETTERS),
         fup = mcs_fups[sweep],
         .before = sweep_letter)
rename_col <- function(var_name){
  var_dict <- c("CHTCM00" = "height", "CWTCM00" = "weight")
  sweep_letter <- str_sub(var_name, 1, 1)
  clean_var <- var_dict[str_sub(var_name, 2)]
  glue("{clean_var}_{mcs_fups[match(sweep_letter, LETTERS)]}")
}

df_mini_wide %>%
  mutate(across(where(is.labelled), negative_to_na)) %>%
  rename_with(rename_col, .cols = -c(MCSID, CNUM)) %>%
  pivot_longer(
    cols = -c(MCSID, CNUM),
    names_pattern = "(.*)_(.*)", 
    names_to = c(".value", "fup"),
    names_transform = list(fup = as.integer) # To change `fup` column from character to integer
  )
#### Task IV ----

df_mini_wide %>%
  pivot_longer(
    cols = matches("^.C(W|H)TCM00"), # The columns to pivot.
    names_pattern = "(.)C(.*)CM00", # Two groups: first letter, and subsequent letters
    names_to = c(".value", "variable")) %>%
  group_by(variable) %>%
  summarise(rho = cor(E, G, use = "complete.obs"))
### Long-to-Wide Transformations with `tidyr::pivot_wider()` ----

df_long_mini <- list(
  `11` = mcs_11y_mini %>% rename_with(~ str_replace(.x, "A0$", "00")),
  `14` = mcs_14y_mini,
  `17` = mcs_17y_mini
) %>%
  map_dfr(~ .x %>% rename_with(~ str_sub(.x, 2), .cols = -MCSID),
          .id = "fup") %>%
  mutate(fup = as.integer(fup)) %>%
  arrange(MCSID, CNUM00, fup) %>%
  relocate(fup, .after = CNUM00)

df_long_mini
df_long_mini %>%
  pivot_wider(
    id_cols = c(MCSID, CNUM00),
    names_from = fup,
    values_from = CHTCM00
  )
df_long_mini %>%
  pivot_wider(
    id_cols = c(MCSID, CNUM00),
    names_from = fup,
    values_from = CHTCM00,
    names_glue = "{.value}_{fup}"
  )
df_long_mini %>%
  pivot_wider(
    id_cols = c(MCSID, CNUM00),
    names_from = fup,
    values_from = matches("C(W|H)TCM00"),
    names_glue = "{.value}_{fup}",
    names_vary = "slowest" # Places variables from the same sweep side-by-side
  )
#### Task V ----

task3_solution %>%
  add_count(MCSID, cnum) %>%
  filter(n == 5) %>%
  pivot_wider(
    id_cols = c(MCSID, cnum),
    names_from = sweep,
    values_from = c(weight, bmi),
    names_glue = "{.value}_{sweep}",
  )
## Some Helpful Functions for Cleaning Long Data ----
### `tidyr::complete()` ----
df_long_mini %>%
  unite("iid", MCSID, CNUM00, sep = "_") %>%
  complete(iid, fup) %>%
  separate(iid, 
           into = c("MCSID", "CNUM00"), 
           sep = "_",
           convert = TRUE)
### `tidyr::fill()` ----
df_long_mini %>%
  complete(MCSID, CNUM00, fup) %>% # Simplified as CNUM == 1 always
  group_by(MCSID, CNUM00) %>%
  fill(CHTCM00, .direction = "down") %>% 
  ungroup()
### Some `dplyr` Helpers ----
df_long_mini %>%
  select(-CHTCM00, -CWTCM00) %>%
  group_by(MCSID, CNUM00) %>%
  mutate(
    prev_fup = lag(fup), 
    next_fup = lead(fup),
    observation = row_number()
  ) %>%
  ungroup()

df_long_mini %>%
  group_by(MCSID, CNUM00) %>%
  summarise(
    first_fup = first(fup), 
    last_fup = last(fup), 
    second_fup = nth(fup, 2),
    n_observations = n(), 
    n_distinct_fups = n_distinct(fup),
    .groups = "drop"
  )
### Task VI ----

ncds_44y <- glue("{ncds_fld}/42y-44y Biomedical/ncds42-4_biomedical_eul.dta") %>%
  read_dta(col_select = c(ncdsid, htres))

lookfor(ncds_44y, "htres")
lookfor(ncds_50y, "DVHT50")

ncds_44y %>%
  mutate(fup = 44,
         height = negative_to_na(htres) / 100) %>%
  select(ncdsid, fup, height) %>%
  bind_rows(
    ncds_50y %>%
      mutate(fup = 50) %>%
      select(ncdsid = NCDSID,
             fup,
             height = DVHT50)
  ) %>%
  complete(ncdsid, fup) %>%
  arrange(ncdsid, fup) %>%
  group_by(ncdsid) %>%
  fill(height, .direction = "downup") %>%
  ungroup() %>%
  pivot_wider(names_from = fup,
              values_from = height,
              names_glue = "height_{fup}")
