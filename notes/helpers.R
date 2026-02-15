library(tidyverse)
library(knitr)
library(glue)

purl_qmd <- function(file){
  output <- glue("scripts/{str_replace(file, 'qmd$', 'R')}")
  
  purl(file, output = output, documentation = 1L)
}

list.files(pattern = "\\.qmd$") %>%
  map(purl_qmd)

list.files(pattern = "\\.qmd$") %>%
  map(~ purl(.x, output = "scripts"))

purl("01_introduction_course.qmd")
file


# qmd_purl.R
# Utility: extract R chunks from a Quarto .qmd into scripts/<name>.R
# - Removes chunk option lines (```{r ...})
# - Removes Quarto option lines inside chunks that start with "#|"
# - Writes Markdown section headers as comment headers

qmd_purl <- function(input, output = NULL, include_chunk_labels = FALSE, scripts_dir = "scripts") {
  stopifnot(is.character(input), length(input) == 1, nzchar(input))
  if (!file.exists(input)) stop("Input file does not exist: ", input)
  
  # Determine output path (default: scripts/ next to input)
  if (is.null(output) || !nzchar(output)) {
    base <- sub("\\.qmd$", "", basename(input), ignore.case = TRUE)
    if (identical(base, basename(input))) base <- paste0(base, "_purl")
    out_dir <- file.path(dirname(input), scripts_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    output <- file.path(out_dir, paste0(base, ".R"))
  } else {
    # If user provided output, still ensure parent folder exists
    dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  }
  
  lines <- readLines(input, warn = FALSE)
  
  # Helpers
  trim <- function(x) sub("^\\s+|\\s+$", "", x)
  is_header <- function(x) grepl("^\\s*#{1,6}\\s+\\S", x)
  header_text <- function(x) sub("^\\s*#{1,6}\\s+", "", trim(x))
  
  # Start of an R chunk: ```{r ...}
  is_r_chunk_start <- function(x) grepl("^\\s*```\\s*\\{\\s*[rR]\\b[^}]*\\}\\s*$", x)
  
  # Start of any chunk: ```{...}
  is_any_chunk_start <- function(x) grepl("^\\s*```\\s*\\{[^}]*\\}\\s*$", x)
  
  # End fence: ```
  is_chunk_end <- function(x) grepl("^\\s*```\\s*$", x)
  
  # Quarto in-chunk option lines: "#| ..."
  is_quarto_opt_line <- function(x) grepl("^\\s*#\\|", x)
  
  # Extract label from chunk header, e.g. ```{r mylabel, echo=FALSE}
  extract_chunk_label <- function(x) {
    inside <- sub("^\\s*```\\s*\\{|\\}\\s*$", "", trim(x)) # "r mylabel, echo=FALSE"
    inside <- trim(inside)
    
    # Remove leading "r" or "R"
    inside <- sub("^[rR]\\b", "", inside)
    inside <- trim(inside)
    if (!nzchar(inside)) return(NA_character_)
    
    # Label is typically the first token before a comma
    first <- strsplit(inside, ",", fixed = TRUE)[[1]][1]
    first <- trim(first)
    
    # If it looks like an option assignment (e.g. echo=TRUE), treat as no label
    if (!nzchar(first) || grepl("=", first)) return(NA_character_)
    
    # If multiple tokens remain (e.g. "label otherstuff"), take the first token
    tok <- strsplit(first, "\\s+")[[1]][1]
    if (!nzchar(tok)) return(NA_character_)
    tok
  }
  
  con <- file(output, open = "wb")
  on.exit(close(con), add = TRUE)
  
  in_r_chunk <- FALSE
  in_other_chunk <- FALSE
  chunk_label <- NA_character_
  wrote_any <- FALSE
  
  write_out <- function(x) {
    writeLines(x, con = con, sep = "\n")
    wrote_any <<- TRUE
  }
  
  for (ln in lines) {
    if (!in_r_chunk && !in_other_chunk) {
      # Markdown headers -> comment headers
      if (is_header(ln)) {
        txt <- header_text(ln)
        if (wrote_any) write_out("") # blank line between sections
        write_out(paste0("# ---- ", txt, " ----"))
        next
      }
      
      # Start of an R chunk
      if (is_r_chunk_start(ln)) {
        in_r_chunk <- TRUE
        in_other_chunk <- FALSE
        chunk_label <- extract_chunk_label(ln)
        if (include_chunk_labels && !is.na(chunk_label)) {
          write_out(paste0("# [chunk] ", chunk_label))
        }
        next
      }
      
      # Start of some other chunk (python, bash, etc.) -> skip until closing fence
      if (is_any_chunk_start(ln)) {
        in_other_chunk <- TRUE
        next
      }
      
      next
    }
    
    # We are inside a non-R chunk: ignore until closing fence
    if (in_other_chunk) {
      if (is_chunk_end(ln)) in_other_chunk <- FALSE
      next
    }
    
    # We are inside an R chunk: write code lines until closing fence
    if (in_r_chunk) {
      if (is_chunk_end(ln)) {
        in_r_chunk <- FALSE
        chunk_label <- NA_character_
        write_out("") # blank line after each chunk for readability
        next
      }
      
      # Drop Quarto option/comment lines beginning with "#|"
      if (is_quarto_opt_line(ln)) next
      
      write_out(ln)
      next
    }
  }
  
  invisible(normalizePath(output, winslash = "/", mustWork = FALSE))
}