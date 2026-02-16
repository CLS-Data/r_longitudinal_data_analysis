# An Introduction to the `tidyverse` {#sec-intro_tidyverse} ----
knitr::opts_chunk$set(echo = TRUE)

## The `tidyverse` Philosophy ----
# install.packages("tidyverse")
library(tidyverse)

### Incrementalism (and the `%>%` pipe) ----
c(1, 2, 3) %>% mean()

c(1, 2, 3) %>% 
  mean() %>% 
  paste("is the mean value")

paste(mean(c(1,2,3)), "is the mean value")

c(1, 2, 3) %>% 
  mean() %>%
  paste("The mean value is", .)

### Tidy Data ----
### A Warning: Continuous Development ----
### Task I ----
paste("The sum of unique values in c(1, 3, 3, 1) is", 
      sum(unique(c(1, 3, 3, 1))))

c(1, 3, 3, 1) %>%
  unique() %>%
  sum() %>%
  paste("The sum of unique values in c(1, 3, 3, 1) is", .)

## Some Useful `tidyverse` Functions ----
### `glue::glue()` ----
library(glue)
glue("The mean of 1 to 10 is {mean(1:10)}")

### `haven::read_dta()` ----
mcs_fld <- Sys.getenv("mcs_fld")
ncds_fld <- Sys.getenv("ncds_fld")

library(haven)
mcs_17y <- glue("{mcs_fld}/17y/mcs7_cm_derived.dta") %>%
  read_dta()
mcs_17y

### `tibble::tibble()` ----
tibble(
  x = 1:3,
  y = x^2,
  z = y + 2
)

### `dplyr::select()` ----
mcs_17y_mini <- mcs_17y %>%
  select(MCSID, GCNUM00, GCMCS7AG, GCBMIN7, GCOBFLG7)
mcs_17y_mini

mcs_17y_mini %>%
  select(-GCMCS7AG)


mcs_17y %>%
  select(fid = MCSID, pid = GCNUM00, GCMCS7AG, GCBMIN7, GCOBFLG7) %>%
  rename(age = GCMCS7AG, bmi = GCBMIN7, bmi_cat_iotf = GCOBFLG7) %>%
  relocate(pid, .before = 1) %>% # To put pid as first column
  relocate(age, .after = last_col()) %>% # To put age as last column
  pull(bmi) # To extract a single column 

### `dplyr::filter()` ----
mcs_17y_mini %>%
  filter(GCMCS7AG >= 18)

mcs_17y %>%
  filter(GCMCS7AG >= 18,
         GCBMIN7 > mean(GCBMIN7))

mcs_17y %>%
  filter(GCBMIN7 > mean(GCBMIN7),
         GCMCS7AG >= 18)

mcs_17y %>%
  filter(GCBMIN7 > mean(GCBMIN7) |
           GCMCS7AG >= 18)

mcs_17y %>%
  filter(when_any(GCBMIN7 > mean(GCBMIN7),
                  GCMCS7AG >= 18))

#### Task II ----
ncds_55y_mini <- glue("{ncds_fld}/55y/ncds_2013_derived.dta") %>%
  read_dta() %>%
  select(id = NCDSID, region = ND9GOR, bmi = ND9BMI)

ncds_55y_mini %>%
  filter(region %in% 7:8) %>%
  filter(bmi >= 0) %>%
  filter(bmi > mean(bmi)) %>%
  nrow() # Counts rows

### `dplyr::mutate()` ----
mcs_17y_mini %>%
  mutate(age_sq = GCMCS7AG^2,
         bmi_x_age_sq = GCBMIN7*age_sq)

mcs_17y_mini %>%
  mutate(bmi = ifelse(bmi >= 0, GCBMIN7, NA),
         obese = ifelse(bmi >= 30, 1, 0)) %>%
  select(GCBMIN7, bmi, obese)

mcs_17y_mini %>%
  mutate(
    bmi_category = case_when(
      GCBMIN7 < 0 ~ NA,
      GCBMIN7 < 18.5 ~ "Underweight",
      GCBMIN7 >= 18.5 & GCBMIN7 < 25 ~ "Normal weight",
      GCBMIN7 >= 25 & GCBMIN7 < 30 ~ "Overweight",
      GCBMIN7 >= 30 ~ "Obese",
      TRUE ~ NA
    )
  ) %>%
  select(GCBMIN7, bmi_category)

### `dplyr::summarise()` ----
mcs_17y_mini %>%
  summarise(mean_bmi = ifelse(GCBMIN7 >= 0, GCBMIN7, NA) %>%
              mean(na.rm = TRUE))

mcs_17y_mini %>%
  mutate(bmi = ifelse(GCBMIN7 >= 0, GCBMIN7, NA)) %>%
  drop_na(bmi) %>%
  summarise(mean_bmi = mean(bmi),
            min_bmi = min(bmi),
            max_bmi = max(bmi))

### `dplyr::group_by()` ----
mcs_17y_mini %>%
  group_by(GCOBFLG7) %>%
  summarise(mean_bmi= ifelse(GCBMIN7 >= 0, GCBMIN7, NA) %>%
              mean())

mcs_17y_mini %>%
  group_by(MCSID) %>%
  mutate(
    bmi = ifelse(GCBMIN7 >= 0, GCBMIN7, NA),
    bmi_between = mean(bmi, na.rm = TRUE),
    bmi_within = bmi - bmi_between
  ) %>%
  ungroup() %>%
  select(MCSID, GCBMIN7, bmi, bmi_within, bmi_between)

### `dplyr::count()` ----
mcs_17y_mini %>%
  mutate(age_int = floor(GCMCS7AG)) %>%
  count(age_int, GCOBFLG7, name = "n_obs")

mcs_17y_mini %>%
  distinct(GCOBFLG7)

### `dplyr::arrange()` ----
mcs_17y_mini %>%
  transmute(bmi = ifelse(GCBMIN7 >= 0, GCBMIN7, NA),
            age = ifelse(GCMCS7AG >=0, GCMCS7AG, NA)) %>%
  arrange(age, bmi)

mcs_17y_mini %>%
  transmute(bmi = ifelse(GCBMIN7 >= 0, GCBMIN7, NA)) %>%
  arrange(desc(bmi))

mcs_17y_mini %>%
  mutate(bmi = ifelse(GCBMIN7 >= 0, GCBMIN7, NA)) %>%
  drop_na(bmi) %>%
  group_by(GCOBFLG7) %>%
  arrange(desc(bmi)) %>%
  slice(1:3) %>%
  ungroup() %>%
  select(GCOBFLG7, bmi)

#### Task III ----
ncds_55y_mini %>%
  mutate(bmi = ifelse(bmi >= 0, bmi, NA)) %>%
  drop_na(bmi) %>%
  group_by(region) %>%
  summarise(mean_bmi = mean(bmi),
            sd_bmi = sd(bmi)) %>%
  ungroup()


ncds_55y_mini %>%
  mutate(bmi = ifelse(bmi >= 0, bmi, NA)) %>%
  drop_na(bmi) %>%
  group_by(region) %>%
  mutate(
    mean_bmi = mean(bmi),
    sd_bmi = sd(bmi),
    outlier = ifelse(bmi < (mean_bmi - 3*sd_bmi) |
                       bmi > (mean_bmi + 3*sd_bmi), 1, 0)
  ) %>%
  summarise(
    n_total = n(),
    n_outliers = sum(outlier),
    prop_outliers = n_outliers / n_total
  ) %>%
  arrange(desc(prop_outliers)) %>%
  slice(3)

### `purrr::map()` ----
c("mcs1_cm_derived.dta",
  "mcs1_family_derived.dta",
  "mcs1_parent_derived.dta") %>%
  map(
    ~ glue("{mcs_fld}/0y/{.x}") %>%
      read_dta() %>%
      nrow()
  )

#### Task IV ----
mcs_fups <- c(0, 3, 5, 7, 11, 14, 17)


map_int(
  1:7,
  ~ glue("{mcs_fld}/{mcs_fups[.x]}y/mcs{.x}_cm_interview.dta") %>%
    read_dta() %>%
    nrow()
)

## Putting It All Together ----
map_dfr(
  1:7,
  ~ glue("{mcs_fld}/{mcs_fups[.x]}y/mcs{.x}_cm_derived.dta") %>%
    read_dta(col_select = c("MCSID", matches("^([A-G]CNUM00|GCBMIN7|FCBMIN6)$"))) %>%
    rename(CNUM = matches("^[A-G]CNUM00$")),
  .id = "wave"
) %>%
  add_count(MCSID, CNUM) %>%
  filter(n == 7) %>%
  mutate(bmi = case_when(
    FCBMIN6 >= 0 ~ FCBMIN6,
    GCBMIN7 >= 0 ~ GCBMIN7
  )
  ) %>%
  drop_na(bmi) %>%
  group_by(wave) %>%
  summarise(mean_bmi = mean(bmi)) %>%
  mutate(diff_mean_bmi = mean_bmi - lag(mean_bmi))

## Further Reading ----
