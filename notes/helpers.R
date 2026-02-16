library(tidyverse)
library(knitr)
library(glue)

rm(list = ls())

convert_qmd <- function(file){
  
  qmd_script <- read_lines(file) %>%
    tibble(line = .) %>%
    mutate(row = row_number(),
           heading = str_detect(line, "^\\#"),
           chunk_option = str_detect(line, "^\\#\\|"),
           start_chunk = ifelse(str_detect(line, "^\\`\\`\\`\\{r"), 1, 0),
           end_chunk = ifelse(line == "```", 1, 0)) %>%
    mutate(chunk_start = cumsum(start_chunk),
           chunk_end = cumsum(end_chunk))
  
  chunks <- qmd_script %>%
    group_by(chunk_start, chunk_end) %>%
    mutate(chunk_id = cur_group_id()) %>%
    ungroup() %>%
    filter(chunk_start == chunk_end + 1,
           !(str_sub(line, 1, 3) == "```"),
           !str_detect(line, "^\\#\\|")) %>%
    group_by(chunk_id) %>%
    summarise(first_row = first(row),
              last_row = last(row),
              line = glue_collapse(line, "\n")) %>%
    mutate(line = glue("{line}\n"))
  
  chunk_row <- chunks %>%
    select(first_row, last_row) %>% 
    uncount(last_row - first_row + 1, .id = "row") %>%
    mutate(row = first_row + row - 1) %>% pull(row)
  
  headers <- qmd_script %>%
    filter(str_detect(line, "^\\#")) %>%
    select(line, row) %>%
    filter(!(row %in% chunk_row),
           !str_detect(line, "^\\#\\|")) %>%
    mutate(line = glue("\n{line} ----"))
  
  out_script <- bind_rows(headers, 
                          chunks %>% rename(row = first_row)) %>%
    arrange(row) %>%
    select(line) %>%
    pull(line) 
  
  out_file <- glue("scripts/{str_replace(file, 'qmd$', 'R')}")
  
  write_lines(out_script, out_file)
}

list.files(pattern = "\\.qmd$") %>%
  map(convert_qmd)



purl_qmd <row_number()purl_qmd <- function(file){
  output <- glue("scripts/{str_replace(file, 'qmd$', 'R')}")
  
  purl(file, output = output, documentation = 1L)
}

list.files(pattern = "\\.qmd$") %>%
  map(purl_qmd)

list.files(pattern = "\\.qmd$") %>%
  map(~ purl(.x, output = "scripts"))

purl("01_introduction_course.qmd")
file
