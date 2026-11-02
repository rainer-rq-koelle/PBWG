#!/usr/bin/env Rscript
# Usage: Rscript scripts/cache-aviation-data.R CACHE apdf YEAR ICAO,ICAO
#        Rscript scripts/cache-aviation-data.R CACHE nm-flt YEAR
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L || length(args) > 4L) stop("Supply CACHE DATASET YEAR [AIRPORTS].")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
source(file.path(dirname(script), "lib", "aviation-r2.R"))
airports <- if (length(args) == 4L) strsplit(args[[4]], ",", fixed = TRUE)[[1]] else NULL
cache_aviation_files(args[[1]], args[[2]], as.integer(args[[3]]), airports)
