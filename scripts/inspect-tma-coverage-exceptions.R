#!/usr/bin/env Rscript
# Focused follow-up for the sparse-reference exposure found in the 2024-2026 review.
# Rscript scripts/inspect-tma-coverage-exceptions.R CACHE REFERENCE_RUN REVIEW_OUTPUT
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=3L) stop("Supply CACHE REFERENCE_RUN REVIEW_OUTPUT.")
script<-sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[[1]])
root<-normalizePath(file.path(dirname(script),".."))
suppressPackageStartupMessages({library(dplyr);library(readr);devtools::load_all(root,quiet=TRUE)})
source(file.path(root,"scripts/lib/refresh-workflow.R"))
source(file.path(root,"scripts/lib/aviation-r2.R"))
source(file.path(root,"scripts/lib/tma-coverage.R"))
refs<-read_tma_coverage_references(args[[2]])
sectors<-read_approved_tma_sector_definitions() |> rename(ICAO=AIRPORT)
selected<-tibble::tibble(YEAR=c(2025L,2025L,2026L),ICAO=c("LFPG","LTFM","LTFM"))
result<-list();warnings_seen<-list()
for(i in seq_len(nrow(selected))){
 a<-selected$ICAO[i];y<-selected$YEAR[i]
 message("Inspecting ",a," ",y)
 withCallingHandlers({
  raw<-read_refresh_apdf(aviation_refresh_sources(args[[1]],"apdf",y,a))
  s<-tma_coverage_samples(raw,a,filter(sectors,ICAO==a),filter(refs,ICAO==a)) |>
   filter(!is.na(OBS_DATE),OBS_DATE>=as.Date(paste0(y,"-01-01")),
     OBS_DATE<=as.Date(paste0(y,if(y==2026) "-09-30" else "-12-31")),SMALL_REF_20) |>
   mutate(YEAR=y,MONTH=format(OBS_DATE,"%Y-%m"))
  result[[i]]<-s |> summarise(N_MATCHED=n(),FIRST_DATE=min(OBS_DATE),LAST_DATE=max(OBS_DATE),
    .by=c(ICAO,YEAR,MONTH,PHASE,RANGE_NM,CLASS,RWY,SECTOR,REF_N,IDENTIFIED))
 },warning=function(w){
  warnings_seen[[length(warnings_seen)+1L]]<<-tibble::tibble(ICAO=a,YEAR=y,WARNING=conditionMessage(w))
  invokeRestart("muffleWarning")
 })
 rm(raw,s);gc(verbose=FALSE)
}
write_csv(bind_rows(result),file.path(args[[3]],"sparse-reference-exception-groups.csv"))
write_csv(bind_rows(tibble::tibble(ICAO=character(),YEAR=integer(),WARNING=character()),warnings_seen),
 file.path(args[[3]],"focused-inspection-warnings.csv"))

# The full-period review also exposed a concentrated September 2024 crossing gap.
raw<-read_refresh_apdf(aviation_refresh_sources(args[[1]],"apdf",2024,"LTFM")) |>
 mutate(DATE=coalesce(as.Date(MVT_TIME_UTC,tz="UTC"),as.Date(BLOCK_TIME_UTC,tz="UTC"))) |>
 filter(DATE>=as.Date("2024-01-01"),DATE<=as.Date("2024-12-31"),SRC_PHASE %in% c("ARR","DEP"))
daily<-raw |> summarise(N_REPORTED=n(),N_MISSING_40=sum(is.na(C40_CROSS_TIME)),
 N_MISSING_100=sum(is.na(C100_CROSS_TIME)),.by=c(DATE,SRC_PHASE)) |>
 mutate(PCT_MISSING_40=100*N_MISSING_40/N_REPORTED,PCT_MISSING_100=100*N_MISSING_100/N_REPORTED) |>
 arrange(DATE,SRC_PHASE)
write_csv(daily,file.path(args[[3]],"ltfm-2024-crossing-gap-daily.csv"))
