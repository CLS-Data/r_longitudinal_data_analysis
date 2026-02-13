library(tidyverse)
library(labelled)
library(naniar)
library(flextable)
library(officer)
library(gtsummary)
library(gt)
library(glue)
library(scico)
library(broom)

rm(list = ls())

df_mcs <- readRDS("data/mcs_clean.Rds")
lookfor(df_mcs)

df_ncds <- readRDS("data/ncds_clean.Rds") %>%
  mutate(bmi = ifelse(between(bmi, 13, 60), bmi, NA),
         obese = ifelse(bmi >= 30, "Obese", "Not Obese") %>%
           factor(c("Not Obese", "Obese"))) %>%
  set_variable_labels(obese = "Obese (BMI ≥ 30 kg/m^2^)") %>%
  filter(fup >= 20)

lookfor(df_ncds)

# 1. Descriptive Tables ----
## a. Pre-Made ----
df_ncds %>%
  drop_na(bmi) %>%
  select(-iid) %>%
  tbl_summary(by = "fup")

# ADD SOME MORE FORMATTING - NOTE IT USES VALUE LABELS
# REARRANGE COLUMNS
# PIPE INTO GT() FOR MORE FORMATTING


## b. Manual ----
tv_vars <- c("height", "weight", "bmi", "obese")

ncds_wide <- df_ncds %>%
  arrange(fup) %>%
  mutate(fup = str_pad(fup, width = 2, side = "left", pad = "0")) %>%
  pivot_wider(names_from = fup,
              values_from = any_of(tv_vars),
              names_glue = "{.value}_{fup}")

desc_num <- ncds_wide %>%
  select(iid, where(is.numeric)) %>%
  pivot_longer(-iid,
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>%
  summarise(stat_01 = mean(value, na.rm = TRUE),
            stat_02 = sd(value, na.rm = TRUE))

desc_fct <- ncds_wide %>%
  select(iid, where(is.factor)) %>%
  pivot_longer(-iid,
               names_to = "variable",
               values_to = "levels") %>%
  drop_na() %>%
  group_by(variable, levels) %>%
  summarise(stat_01 = n(),
            .groups = "drop_last") %>%
  mutate(stat_02 = stat_01 / sum(stat_01)) %>%
  ungroup()

desc_miss <- miss_var_summary(ncds_wide)

bind_rows(desc_num, desc_fct) %>%
  left_join(desc_miss, by = "variable")

lookfor(ncds_wide)

bind_rows(desc_num, desc_fct) %>%
  left_join(desc_miss, by = "variable")

clean_stats <- bind_rows(desc_num, desc_fct) %>%
  left_join(desc_miss, by = "variable") %>%
  mutate(
    stat_01_clean = ifelse(!is.na(levels),
                           as.integer(stat_01) %>% format(big.mark = ","),
                           round(stat_01, 2) %>% format(nsmall = 2)) %>%
      str_squish(),
    stat_02_clean = ifelse(!is.na(levels),
                           round(100 * stat_02, 1) %>% format(nsmall = 1) %>% paste0("%"),
                           round(stat_02, 2) %>% format(nsmall = 2)) %>%
      str_squish(),
    
    stat_clean = glue("{stat_01_clean} ({stat_02_clean})"),
    miss_clean = round(pct_miss, 1) %>% format(nsmall = 1) %>% str_trim() %>% paste0("%")
  ) %>%
  select(variable, levels, stat_clean, miss_clean)

tv_regex <- glue_collapse(tv_vars, "|") %>%
  paste0("^(", ., ")_\\d\\d$")

tbl_desc <- ncds_wide %>%
  select(matches("^bmi_\\d\\d$"), 
         matches("^obese_\\d\\d$"),
         # matches("^weight_\\d\\d$"), 
         # matches("^height_\\d\\d$"),
         verbal_11, vocab_16,
         sex, ethnic_group,
         father_bmi, mother_bmi,
         father_edu_years, mother_edu_years) %>%
  lookfor() %>%
  as_tibble() %>%
  mutate(levels = map(levels, ~ .x %||% NA_character_)) %>%
  unchop(levels) %>%
  select(pos, variable, col_type, label, levels) %>%
  mutate(
    fup = ifelse(str_detect(variable, tv_regex),
                 str_sub(variable, -2),
                 NA) %>%
      as.integer(),
    label = ifelse(str_detect(variable, tv_regex),
                   glue("{label} @ {fup}y"),
                   label)
  ) %>%
  left_join(clean_stats, by = c("variable", "levels")) %>%
  mutate(variable_clean = ifelse(col_type == "fct", label, NA),
         levels_clean = ifelse(col_type == "fct", levels, label)) %>%
  select(variable_clean, levels_clean, stat_clean, miss_clean)

merge_pos <- tbl_desc %>%
  mutate(row = row_number()) %>%
  drop_na(variable_clean) %>%
  select(variable_clean, row) %>%
  chop(row) %>%
  pull(row)

tbl_desc %>%
  flextable() %>%
  set_header_labels(variable_clean = "",
                    levels_clean = "Variable",
                    stat_clean = "Mean (SD) / N (%)",
                    miss_clean = "% Missing") %>%
  border_remove() %>% 
  reduce(merge_pos,
         ~ merge_at(.x, i = .y, j = 1) %>%
           merge_at(i = .y, j = 4),
         .init = .) %>%
  border_inner_h(border = fp_border(color = "grey50", width = 1, style = "dashed"), part = "body") %>% 
  hline_top(border = fp_border(color = "black", width = 2), part = "all") %>% 
  hline_bottom(border = fp_border(color = "black", width = 2), part = "all") %>% 
  fix_border_issues(part = "all") %>% 
  align(j = 1:2, align = "right", part = "all") %>% 
  align(j = 3:ncol(tbl_desc), align = "center", part = "all") %>%
  valign(valign = "top") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all") %>%
  autofit()

# 2. Plotting Descriptives ----
ncds_wide %>%
  count(obese_50) %>%
  drop_na() %>%
  mutate(prop = n / sum(n)) %>%
  ggplot() +
  aes(x = obese_50, y = prop) +
  geom_col()

ggplot(ncds_wide) +
  aes(x = bmi_50) +
  geom_density()

ggplot(df_ncds) +
  aes(x = bmi, fill = fup, group = fup) +
  geom_density()


scico::scico_palette_show()
ggplot(df_ncds) +
  aes(x = bmi, fill = fup, group = fup) +
  geom_density(alpha = 0.3) +
  scale_fill_scico(palette = "imola", begin = 0.1, end = 0.9) +
  theme_bw() +
  theme(legend.position = c(.95, .95),
        legend.justification = c(1, 1)) +
  labs(x = "Body Mass Index",
       y = "Density",
       fill = "Follow-Up Age")

ggplot(df_ncds) +
  aes(x = bmi, fill = fup, group = fup) +
  facet_wrap(~ fup) +
  geom_density(aes(group = fup_f),
               data = df_ncds %>% rename(fup_f = fup),
               fill = "grey",
               alpha = 0.2) +
  geom_density() +
  scale_fill_scico(palette = "imola", begin = 0.1, end = 0.9) +
  theme_bw() +
  theme(legend.position = "off") +
  labs(x = "Body Mass Index",
       y = "Density")

# NOTES: LOOK THROUGH LECTURE NOTES ON GGPLOT2 TO SHOW IT OFF.

# 3. Attrition ----
ncds_miss <- ncds_wide %>%
  mutate(
    across(matches("^bmi_\\d\\d$"),
           list(scaled = ~ scale(.x) %>% as.numeric(),
                miss = ~ ifelse(is.na(.x), 1, 0)))
  ) %>%
  select(matches("_(scaled|miss)$"))

attrit_mod <- lm(bmi_55_miss ~ bmi_23_scaled, ncds_miss)
summary(attrit_mod)
tidy(attrit_mod, conf.int = TRUE) 

tidy(attrit_mod, conf.int = TRUE) %>%
  filter(term == "bmi_23_scaled") %>%
  ggplot() +
  aes(x = term, y = estimate,
      ymin = conf.low, ymax = conf.high) +
  geom_hline(yintercept = 0) +
  geom_pointrange()

# TODO: REPEAT FOR MORE WITH BIND ROWS.
