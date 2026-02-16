# Data Discovery  {#sec-data_discovery} ----

library(tidyverse)
library(haven)
library(glue)
library(labelled)
library(codebookr)
library(summarytools)

mcs_fld <- Sys.getenv("mcs_fld")
ncds_fld <- Sys.getenv("ncds_fld")

mcs_cm_derived_17y <- glue("{mcs_fld}/17y/mcs7_cm_derived.dta") %>%
  read_dta()
## Creating Codebooks using `codebookr::codebook()` ----
mcs_17y_codebook <- codebook(mcs_cm_derived_17y)
print(mcs_17y_codebook, "outputs/mcs7_cm_derived_codebook.docx")
### Task I ----
ncds_55y <- glue("{ncds_fld}/55y/ncds_2013_derived.dta") %>%
  read_dta()
ncds_55y_codebook <- codebook(ncds_55y)
print(ncds_55y_codebook, "outputs/ncds_55y_derived_codebook.docx")
## Searching Datasets with `labelled::lookfor()`  ----
lookfor(mcs_cm_derived_17y, "overweight")
lookfor(mcs_cm_derived_17y, "overweight") %>%
  as_tibble() %>%
  filter(str_detect(label, "IOTF"))
### Task II ----
lookfor(mcs_cm_derived_17y, "obese|mass|bmi") # Multiple terms to capture variations
ncds_55y_flatfile <- glue("{ncds_fld}/55y/ncds_2013_flatfile.dta") %>%
  read_dta()
lookfor(ncds_55y_flatfile, "weight|height")
## Data Overviews with `summarytools::dfSummary()` ----
# OUTPUT NOT SHOWN
mcs_17y_summary <- dfSummary(mcs_cm_derived_17y)
stview(mcs_17y_summary)
mcs_cm_derived_17y %>%
  count(GCOBFLG7)
# OUTPUT NOT SHOWN
negative_to_na <- function(x){
  na_range(x) <- c(-Inf, -1)
  user_na_to_na(x)
}

mcs_cm_derived_17y %>%
  mutate(bmi_cat_ifelse = ifelse(GCOBFLG7 >= 0, GCOBFLG7, NA),
         bmi_cat_cleaned = negative_to_na(GCOBFLG7)) %>%
  select(GCOBFLG7, bmi_cat_ifelse, bmi_cat_cleaned) %>%
  dfSummary() %>%
  stview()
## Creating Comprehensive Data Dictionaries ----
tibble(file = list.files(path = mcs_fld, 
                         pattern = "\\.dta$", 
                         recursive = TRUE))
mcs_data_dict <- tibble(path = list.files(path = mcs_fld,
                                          pattern = "\\.dta$",
                                          recursive = TRUE)) %>%
  filter(!str_detect(path, "^UKDS")) %>%
  mutate(
    ret = map(
      path, 
      ~ glue("{mcs_fld}/{.x}") %>%
        read_dta(n_max = 1, encoding = "latin1") %>%
        lookfor() %>%
        as_tibble()
    )
  )

mcs_data_dict
mcs_data_dict %>%
  unnest(ret)
mcs_dict_clean <- mcs_data_dict %>%
  unnest(ret) %>%
  separate(path, into = c("fld", "file"), sep = "/") %>%
  mutate(variable_low = str_to_lower(variable),
         label_low = str_to_lower(label))

mcs_dict_clean %>%
  filter(str_detect(label_low, "bmi|body"))
### Task III ----

mcs_dict_clean %>%
  filter(str_detect(label_low, "weight"),
         str_detect(file, "parent"))

ncds_data_dict <- tibble(path = list.files(path = ncds_fld,
                                          pattern = "\\.dta$",
                                          recursive = TRUE)) %>%
  filter(!str_detect(path, "^UKDS")) %>%
  mutate(
    ret = map(
      path, 
      ~ glue("{ncds_fld}/{.x}") %>%
        read_dta(n_max = 1, encoding = "latin1") %>%
        lookfor() %>%
        as_tibble()
    )
  ) %>%
  unnest(ret) %>%
  separate(path, into = c("fld", "file"), sep = "/") %>%
  mutate(variable_low = str_to_lower(variable),
         label_low = str_to_lower(label))

ncds_data_dict %>%
  filter(str_detect(label_low, "diabetes")) # Only looked in variable label!
## Loading Data from Data Dictionaries ----
mcs_dict_clean %>%
  filter(str_detect(label_low, "iotf")) %>% 
  select(fld, file, variable) %>% 
  chop(variable)
load_from_dict <- function(fld, file, variables){
  glue("{mcs_fld}/{fld}/{file}") %>%
    read_dta(col_select = c("MCSID", matches("^.CNUM00$"), all_of(variables)))
}

mcs_dict_clean %>%
  filter(str_detect(label_low, "iotf")) %>% 
  select(fld, file, variable) %>% 
  chop(variable) %>%
  mutate(data = pmap(list(fld, file, variable), load_from_dict))
### Task IV ----

load_from_dict_v2 <- function(fld, file, variables, 
                                study_fld = ncds_fld,
                                id_regex = "NCDSID|ncdsid"){
  glue("{study_fld}/{fld}/{file}") %>%
    read_dta(col_select = c(matches(id_regex), all_of(variables)))
}

ncds_data_dict %>%
  filter(fld == "55y",
         str_detect(label_low, "height")) %>% 
  select(fld, file, variable) %>% 
  chop(variable) %>%
  mutate(data = pmap(list(fld, file, variable), load_from_dict_v2))
