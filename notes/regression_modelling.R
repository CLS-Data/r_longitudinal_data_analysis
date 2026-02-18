library(tidyverse)
library(broom)
library(lme4)
library(broom.mixed)
library(summarytools)
library(splines)
library(marginaleffects)

rm(list = ls())

df_ncds <- readRDS("data/ncds_updated.Rds")
df_mcs <- readRDS("data/mcs_clean.Rds")

# 1. Repeated Regression Models ----
## A Single Regression Model ----
glimpse(df_ncds)
glimpse(df_mcs)

mod_lm <- lm(bmi ~ verbal_11 + sex + ethnic_group + mother_edu_years + mother_bmi,
             data = df_ncds %>% filter(fup == 23))
summary(mod_lm)

tidy(mod_lm, conf.int = TRUE)
glance(mod_lm)
augment(mod_lm)

tidy(mod_lm, conf.int = TRUE) %>%
  filter(term == "verbal_11") %>%
  ggplot() +
  aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high) +
  geom_hline(yintercept = 0) +
  geom_pointrange()

## A Preliminary Function ----
run_lm <- function(fup){
  df <- df_ncds %>%
    filter(fup == !!fup)
  
  lm(bmi ~ verbal_11 + sex + father_edu_years + mother_bmi, 
     data = df) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "verbal_11") %>%
    mutate(fup = !!fup)
}

lm_res <- unique(df_mcs$fup) %>%
  map_dfr(run_lm)
lm_res

## Plotting the Results ----
ggplot(lm_res) +
  aes(x = fup, y = estimate, ymin = conf.low, ymax = conf.high) +
  geom_hline(yintercept = 0) +
  geom_pointrange()

ggplot(lm_res) +
  aes(x = fup, y = estimate, ymin = conf.low, ymax = conf.high) +
  geom_hline(yintercept = 0) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line() +
  geom_point()

### Task 1: ----
# 1. Create a similar function for the MCS.
run_lm_mcs <- function(fup){
  df <- df_mcs %>%
    filter(fup == !!fup)
  
  lm(bmi ~ verbal_11 + sex + father_edu_level + mother_bmi, 
     data = df) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "verbal_11") %>%
    mutate(fup = !!fup)
}

lm_res_mcs <- unique(df_mcs$fup) %>%
  map_dfr(run_lm_mcs)
lm_res_mcs

## Adding More Complexity ----
run_lm_v2 <- function(fup, cog_var){
  df <- df_ncds %>%
    filter(fup == !!fup) %>%
    rename(cog = all_of(!!cog_var))
  
  lm(bmi ~ cog + sex + father_edu_years + mother_bmi, df) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "cog") %>%
    mutate(fup = !!fup,
           cog_var = !!cog_var)
}

expand_grid(
  fup = unique(df_ncds$fup),
  cog_var = c("verbal_11", "vocab_16")
) %>%
  pmap_dfr(run_lm_v2)

expand_grid(
  fup = unique(df_ncds$fup),
  cog_var = c("verbal_11", "vocab_16")
) %>%
  mutate(res = map2(fup, cog_var, run_lm_v2))

expand_grid(
  fup = unique(df_ncds$fup),
  cog_var = c("verbal_11", "vocab_16")
) %>%
  mutate(res = map2(fup, cog_var, run_lm_v2)) %>%
  unnest(res)


## Accounting for mutate() ----
run_lm_v3 <- function(fup, cog_var){
  df <- df_ncds %>%
    filter(fup == !!fup) %>%
    rename(cog = all_of(!!cog_var))
  
  lm(bmi ~ cog + sex + father_edu_years + mother_bmi, df) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "cog")
}

expand_grid(
  fup = unique(df_ncds$fup),
  cog_var = c("verbal_11", "vocab_16")
) %>%
  mutate(res = map2(fup, cog_var, run_lm_v3)) %>%
  unnest(res)

## Spec ID for Arbitrary Complexity ----
ncds_spec_grid <- expand_grid(
  fup = unique(df_ncds$fup),
  cog_var = c("verbal_11", "vocab_16")
) %>%
  mutate(spec_id = row_number(),
         .before = 1)

run_lm_with_id <- function(spec_id){
  spec <- ncds_spec_grid %>%
    filter(spec_id == !!spec_id)
  
  df <- df_ncds %>%
    filter(fup == !!spec$fup) %>%
    rename(cog = all_of(!!spec$cog_var))
  
  lm(bmi ~ cog + sex + father_edu_years + mother_bmi, df) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "cog")
}

ncds_spec_grid %>%
  mutate(res = map(spec_id, run_lm_with_id)) %>%
  unnest(res)

### Task 2 ----
### 1. Create a function that achieves the same as run_lm_with_id but for the cleaned MCS data


## Adding Control Variables ----
ncds_mod_covars <- lst(
  basic = c("sex", "ethnic_group"),
  parent_bmi = c(basic, "father_bmi", "mother_bmi"),
  parent_edu = c(basic, "father_edu_years", "mother_edu_years"),
  full = c(parent_bmi, parent_edu) %>% unique()
)

ncds_spec_grid_v2 <- expand_grid(
  fup = unique(df_ncds$fup),
  cog_var = c("verbal_11", "vocab_16"),
  mod = names(ncds_mod_covars)
) %>%
  mutate(spec_id = row_number(),
         .before = 1)

run_lm_with_id_v2 <- function(spec_id){
  spec <- ncds_spec_grid_v2 %>%
    filter(spec_id == !!spec_id)
  
  df <- df_ncds %>%
    filter(fup == !!spec$fup) %>%
    rename(cog = all_of(!!spec$cog_var))
  
  mod_form <- glue_collapse(c("bmi ~ cog", ncds_mod_covars[[spec$mod]]), sep = " + ") %>%
    as.formula()
  
  lm(mod_form, df) %>%
    tidy(conf.int = TRUE) %>%
    filter(term == "cog")
}

ncds_spec_grid_v2 %>%
  mutate(res = map(spec_id, run_lm_with_id_v2)) %>%
  unnest(res)

### Task 3 ----
# 1. Create a function that achieves the same as run_lm_with_id_v2 but for the cleaned MCS data
# 2. Amend so that it returns the mode R^2 and observations for the model too
# 3. Come up with a neat way to plot the results


# 2. Growth Curve Modelling ----
## Simple Growth Curve Models ----
mod_lmer <- lmer(bmi ~ verbal_11 + fup + verbal_11:fup + sex + (1 | iid),
                 data = df_ncds)
summary(mod_lmer)
tidy(mod_lmer, conf.int = TRUE)

df_ncds %>%
  select(verbal_11, vocab_16) %>%
  descr()

df_ncds_scaled <- df_ncds %>%
  mutate(
    verbal_11 = scale(verbal_11) %>% as.numeric(),
    vocab_16 = scale(vocab_16) %>% as.numeric(),
    fup_23 = fup - 23
  )

mod_lmer_v2 <- lmer(bmi ~ verbal_11 + fup_23 + verbal_11:fup_23 + sex + (1 | iid),
                    data = df_ncds_scaled)
tidy(mod_lmer_v2, conf.int = TRUE)

## Marginal Effects Package ----
mod_lmer_v3 <- lmer(bmi ~ verbal_11 + fup + verbal_11:fup + sex + (1 | iid),
                    data = df_ncds_scaled)
tidy(mod_lmer_v3, conf.int = TRUE)

lmer_preds <- predictions(
  mod_lmer_v3,
  newdata = datagrid(iid = NA,
                     verbal_11 = -2:2,
                     fup = seq(from = min(df_ncds$fup),
                               to = max(df_ncds$fup),
                               by = 1)),
  re.form = NA) %>%
  as_tibble()

ggplot(lmer_preds) +
  aes(x = fup, y = estimate, ymin = conf.low,
      ymax = conf.high, 
      color = verbal_11, fill = verbal_11,
      group = verbal_11) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line()

lmer_coefs <- fixef(mod_lmer_v3)
1*lmer_coefs["verbal_11"] + 1*7*lmer_coefs["verbal_11:fup"]
1*lmer_coefs["verbal_11"] + 1*55*lmer_coefs["verbal_11:fup"]

lmer_comps <- comparisons(
  mod_lmer_v3, 
  variables = list(verbal_11 = c(1, 0)), 
  newdata = datagrid(iid = NA,
                     fup = seq(from = min(df_ncds$fup),
                               to = max(df_ncds$fup),
                               by = 1)),
  re.form = NA
) %>%
  as_tibble() %>%
  select(fup, estimate, conf.low, conf.high)

lmer_comps %>%
  slice(c(1, n()))

ggplot(lmer_comps) +
  aes(x = fup, y = estimate, ymin = conf.low,
      ymax = conf.high) +
  geom_hline(yintercept = 0) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line()

bind_rows(
  lm = lm_res,
  lmer = lmer_comps,
  .id = "model"
)  %>%
  ggplot() +
  aes(x = fup, y = estimate, ymin = conf.low,
      ymax = conf.high, color = model, fill = model) +
  geom_hline(yintercept = 0) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line()


### Non-Linear Interactions ----
mod_lmer_v4 <- lmer(bmi ~ verbal_11 + fup + I(fup^2) +
                      verbal_11:fup + verbal_11:I(fup^2) +
                      sex + (1 | iid),
                    data = df_ncds_scaled)
tidy(mod_lmer_v4, conf.int = TRUE)

lmer_comps_v4 <- comparisons(mod_lmer_v4, 
                             variables = list(verbal_11 = c(1, 0)), 
                             newdata = datagrid(iid = NA,
                                                fup = seq(from = min(df_ncds$fup),
                                                          to = max(df_ncds$fup),
                                                          by = 1)),
                             re.form = NA) %>%
  as_tibble() %>%
  select(fup, estimate, conf.low, conf.high)

bind_rows(
  lmer = lmer_comps,
  lmer_quad = lmer_comps_v4,
  .id = "model"
)  %>%
  ggplot() +
  aes(x = fup, y = estimate, ymin = conf.low,
      ymax = conf.high, color = model, fill = model) +
  geom_hline(yintercept = 0) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line()


df_ncds %>%
  drop_na(fup) %>%
  mutate(ns(fup, 2) %>%
           as_tibble() %>%
           rename(fup_ns1 = 1, fup_ns2 = 2)) %>%
  distinct(across(matches("fup"))) %>%
  ggplot() +
  aes(x = fup) +
  geom_line(aes(y = fup_ns1), color = "red") +
  geom_line(aes(y = fup_ns2), color = "blue")


mod_lmer_v5 <- lmer(bmi ~ verbal_11*ns(fup, 2) +
                      sex + (1 | iid),
                    data = df_ncds_scaled)
tidy(mod_lmer_v5, conf.int = TRUE)

predictions(
  mod_lmer_v5,
  newdata = datagrid(iid = NA,
                     verbal_11 = -2:2,
                     fup = seq(from = min(df_ncds$fup),
                               to = max(df_ncds$fup),
                               by = 1)),
  re.form = NA) %>%
  as_tibble() %>%
  ggplot() +
  aes(x = fup, y = estimate, ymin = conf.low,
      ymax = conf.high, 
      color = verbal_11, fill = verbal_11,
      group = verbal_11) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line()


lmer_comps_v5 <- comparisons(mod_lmer_v5, 
                             variables = list(verbal_11 = c(1, 0)), 
                             newdata = datagrid(iid = NA,
                                                fup = seq(from = min(df_ncds$fup),
                                                          to = max(df_ncds$fup),
                                                          by = 1)),
                             re.form = NA) %>%
  as_tibble() %>%
  select(fup, estimate, conf.low, conf.high)

bind_rows(
  lmer = lmer_comps,
  lmer_quad = lmer_comps_v4,
  lmer_splines = lmer_comps_v5,
  .id = "model"
)  %>%
  ggplot() +
  aes(x = fup, y = estimate, ymin = conf.low,
      ymax = conf.high, color = model, fill = model) +
  geom_hline(yintercept = 0) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line()

mod_slopes_v5 <- slopes(mod_lmer_v5,
                        variables = "verbal_11",
                        newdata = datagrid(iid = NA,
                                           fup = seq(from = min(df_ncds$fup),
                                                     to = max(df_ncds$fup),
                                                     by = 1)),
                        re.form = NA) %>%
  as_tibble()

ggplot(mod_slopes_v5) +
  aes(x = fup, y = estimate, ymin = conf.low,
      ymax = conf.high) +
  geom_hline(yintercept = 0) +
  geom_ribbon(alpha = 0.3, color = NA) +
  geom_line()
