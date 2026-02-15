

# Chapter 5 ----
mcs_fups <- c(0, 3, 5, 7, 11, 14, 17)

negative_to_na <- function(x){
  na_range(x) <- c(-Inf, -1)
  user_na_to_na(x)
}

get_data <- function(sweep){
  sweep_letter <- letters[sweep]
  fup <- mcs_fups[sweep]
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_interview.dta") %>%
    read_dta(col_select = c("MCSID", matches("^.(CNUM|C(W|H)TCM)(A|0)0")))
  
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_interview.dta") %>%
    read_dta(col_select = c("MCSID", matches("^.(CNUM|C(W|H)TCM)(A|0)0"))) %>%
    rename_with(~ str_sub(.x, 2), .cols = -MCSID) %>%
    mutate(sweep = !!sweep)
}

bind_rows(
  get_data(6),
  get_data(7)
) %>%
  mutate(across(where(is.labelled), negative_to_na)) %>%
  drop_na() %>%
  group_by(MCSID) %>%
  filter(max(CNUM00) == 1) %>%
  summarise(min_sweep = min(sweep),
            max_sweep = max(sweep)) %>%
  group_by(min_sweep, max_sweep) %>%
  slice(1) %>%
  ungroup() %>%
  pull(MCSID)
c("M10020R", "M10002P", "M10025W")

load_mcs_mini <- function(sweep){
  sweep_letter <- letters[sweep]
  fup <- mcs_fups[sweep]
  
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_interview.dta") %>%
    read_dta(col_select = c("MCSID", matches("^.(CNUM|C(W|H)TCM)(A|0)0"))) %>%
    filter(MCSID %in% c("M10020R", "M10025W", "M10002P")) %>%
    arrange(MCSID)
}

mcs_11y_mini <- load_mcs_mini(5) 
mcs_14y_mini <- load_mcs_mini(6)
mcs_17y_mini <- load_mcs_mini(7)






# Chapter 7 ----




# Chapter 8 ----
mcs_fups <- c(0, 3, 5, 7, 11, 14, 17)

clean_bmi <- function(sweep){
  sweep_letter <- letters[sweep]
  fup <- mcs_fups[sweep]
  glue("{mcs_fld}/{fup}y/mcs{sweep}_cm_interview.dta") %>%
    read_dta(col_select = c("MCSID", matches("^.(CNUM|C(W|H)TCM)(A|0)0"))) %>%
    rename(cnum = matches(".CNUM00"),
           height_raw = matches(".CHTCM(A|0)0"),
           weight_raw = matches(".CWTCM(A|0)0")) %>%
    mutate(cnum = as.numeric(cnum),
           height = negative_to_na(height_raw) %>%
             as.numeric(),
           weight = negative_to_na(weight_raw) %>%
             as.numeric(),
           bmi = weight / ( (height / 100) ^ 2),
           fup = !!fup) %>%
    select(MCSID, cnum, fup, height, weight, bmi)
}

df_bmi <- map_dfr(3:7, ~ clean_bmi(.x))

df_sdq <- glue("{mcs_fld}/3y/mcs2_cm_derived.dta") %>%
  read_dta(col_select = c("MCSID", "BCNUM00", "BEBDTOT")) %>%
  mutate(cnum = as.numeric(BCNUM00),
         sdq_03 = negative_to_na(BEBDTOT) %>% scale() %>% as.numeric()) %>%
  select(MCSID, cnum, sdq_03)

df <- inner_join(df_sdq, df_bmi, by = c("MCSID", "cnum")) %>%
  group_by(MCSID, cnum) %>%
  mutate(iid = cur_group_id()) %>%
  ungroup() %>%
  drop_na()

run_xreg <- function(fup){
  data <- df %>%
    filter(fup == !!fup)
  
  lm(bmi ~ sdq_03, data = data) %>%
    tidy() %>%
    filter(term == "sdq_03") %>%
    select(estimate, conf.low, conf.high)
}

spec_grid <- expand_grid(fup = unique(df$fup),
                         sex = c("male", "female", "all"))

# Issue if requirments change

spec_grid_with_id <- spec_grid %>%
  mutate(spec_id = row_number(),
         .before = 1)

run_xreg_with_id <- function(spec_id){
  spec <- spec_grid_with_id %>%
    filter(spec_id == !!spec_id)
  
  data <- df %>%
    filter(fup == !!spec$fup)
  
  lm(bmi ~ sdq_03, data = data) %>%
    tidy() %>%
    filter(term == "sdq_03") %>%
    select(estimate, conf.low, conf.high)
}


library(lme4)
library(marginaleffects)
library(broom)
library(splines)

model <- lmer(bmi ~ sdq_03*ns(fup, df = 2) + (1 | iid), data = df)
summary(model)
comparisons(model,
            variables = list("sdq_03" = c(1, 0)),
            newdata = expand_grid(iid = NA,
                                  sdq_03 = 0,
                                  fup = seq(min(df$fup), max(df$fup), by = 0.5)),
            re.form = NA) %>%
  as_tibble() %>%
  ggplot() +
  aes(x = fup, y = estimate, ymin = conf.low, ymax = conf.high) +
  geom_line()



## OFFCUTS
## Unit Tests

## Putting It All Together

XX-OUTLINE:
  
  -   SELECT AND TIDYSELECT
-   RENAME_WITH
-   ADVICE ON NAMING VARIABLES.
-   MUTATE
-   CASE_WHEN()
-   AS_FACTOR(); NEGATIVE_TO_NA(); NA_IF() \# IFELSE() DOESN'T RETAIN METADATA
-   FCT\_\*()
-   ACROSS(); IF_ANY(); IF_ALL()
-   C_ACROSS(); rowwise()
-   PICK()
-   COALESCE()
-   ROWWISE()
-   SET_VARIABLE_LABELS
-   NEST() AND UNNEST(), CHOP AND UNCHOP()
-   PIVOT\_\*()
-   \*\_JOIN FUNCTIONS (IMPLICIT TO EXPLICIT NA);
-   bind_rows();
-   COMPLETE()
-   WHEN TO CLEAN; WHEN TO JOIN.
-   REDUCE()
-   KEEP(); DISCARD()
-   
  
  ## Further Functionality from the `labelled` Package
  
  The `labelled` package provides several other functions that are useful when working with labelled data. These include:
  
  -   `var_label()`: Gets/sets variable labels from variables in a `tibble` or a supplied vector
-   `val_labels()`: Gets/sets value labels from variables in a `tibble` or a supplied vector
-   `val_label(x, value)`: Gets/set value label for a specific value in variables in a `tibble` or for a supplied vector
-   `set_var_label()`: Sets variable labels for variables in a dataset

VAR-LABELS; VAL-LABELS; NEGATIVW_TO_NA

MENTION BY THE END WE WILL HAVE PREPARED OUR DATASETS FOR ANALYSIS

Now we know how to find data and load specific variables, we need to prepare it for analysis. In this chapter, XX. Again, our focus is on automating repetitive tasks and doing things in such a way that they are automatable.

cs_cm_derived_17y %\>% select(GCMCS7AG) %\>% mutate(age = negative_to_na(GCMCS7AG)) %\>% mutate(across(where(\~ !is.null(val_labels(.x))), \~ 1))

1.  Open a dataset
2.  Select and clean variables of interest
3.  across() to make this more efficient (w/ negative_to_na)

-   list(mean = .x, ...), .names = {.col}\_{.fn}"

3.  as_factor() (where is.labelled; see above issue though)
4.  pick() to make Likert scales
5.  Repeat in another sweep but note it can be made more efficient with a function
6.  Mention we can clean and then join or join and then clean
7.  Join the datasets together in wide
8.  Join the datasets together in long
9.  Talk about explicit NAs in the wide and long formats and then note fill()

-   complete() ??
-   Dragging data through? first() time-varying to time-invariant

9.  Talk about longitudinal files which can help find appropriate samples (not everyone followed), so open this and then join in.
10. Do joining programmatically with reduce()
11. Reshaping with pivot_longer() and pivot_wider()

-   Doing this with names_glue()