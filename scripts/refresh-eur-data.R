#!/usr/bin/env Rscript

# Run from any working directory. Configuration is an operator-owned R file.
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || any(!args[-1] %in% "--check")) {
  stop("Usage: Rscript scripts/refresh-eur-data.R CONFIG.R [--check]", call. = FALSE)
}
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
project_root <- normalizePath(file.path(dirname(script), ".."))
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  devtools::load_all(project_root, quiet = TRUE)
})
source(file.path(project_root, "scripts", "lib", "refresh-workflow.R"))
settings <- new.env(parent = globalenv())
sys.source(normalizePath(args[[1]], mustWork = TRUE), envir = settings)
if (!exists("config", envir = settings, inherits = FALSE)) stop("Configuration must define config.")
run_pbwg_refresh(settings$config, project_root, check_only = "--check" %in% args)
