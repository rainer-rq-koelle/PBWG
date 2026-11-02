#!/usr/bin/env Rscript
# Diagnostic review of fixed 2024 references and the 2024-2026 archive snapshot.
# Usage: Rscript scripts/build-tma-coverage-review.R CACHE REFERENCE_RUN OUTPUT
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("Supply CACHE REFERENCE_RUN OUTPUT.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
project_root <- normalizePath(file.path(dirname(script), ".."))
suppressPackageStartupMessages({library(dplyr);library(readr);library(tibble);devtools::load_all(project_root,quiet=TRUE)})
source(file.path(project_root,"scripts/lib/refresh-workflow.R"))
source(file.path(project_root,"scripts/lib/aviation-r2.R"))
source(file.path(project_root,"scripts/lib/tma-coverage.R"))
cache <- normalizePath(args[[1]], mustWork=TRUE)
reference_run <- normalizePath(args[[2]], mustWork=TRUE)
output <- args[[3]]
if (dir.exists(output) && length(list.files(output,all.files=TRUE,no..=TRUE))) stop("Output must be new or empty.")
dir.create(output,recursive=TRUE,showWarnings=FALSE)
writeLines("RUNNING",file.path(output,"RUN-STATUS.txt"))
reference <- read_tma_coverage_references(reference_run)
icao_reference <- read_tma_coverage_references(reference_run,"icao_ganp_p20")
keys <- c("ICAO","PHASE","RANGE_NM","CLASS","RWY","SECTOR")
stopifnot(!anyDuplicated(reference[keys]),
  isTRUE(all.equal(arrange(select(reference,-REF_TMA),across(all_of(keys))),
                   arrange(select(icao_reference,-REF_TMA),across(all_of(keys))))))
airports <- pbwg_study_airports()
baseline <- read_csv(file.path(reference_run,"sources.csv"),show_col_types=FALSE,
                     col_types=cols(.default=col_character(),YEAR=col_integer())) |>
  filter(YEAR==2024,ICAO %in% airports) |> mutate(COHORT="REFERENCE_INPUT_2024")
archive <- bind_rows(lapply(2024:2026,function(year) aviation_refresh_sources(cache,"apdf",year,airports))) |>
  mutate(COHORT="ARCHIVE_SNAPSHOT")
sources <- bind_rows(baseline,archive)
paths <- unique(sources$ARCHIVE)
hashes <- tools::md5sum(paths)
sources$OBSERVED_MD5 <- unname(hashes[sources$ARCHIVE])
expected <- coalesce(sources$SOURCE_MD5,sources$ARCHIVE_MD5)
if (any(is.na(expected) | sources$OBSERVED_MD5!=expected)) stop("Input checksum mismatch.")
write_csv(sources,file.path(output,"sources.csv"))
file.copy(file.path(cache,"apdf-manifest.csv"),file.path(output,"upstream-apdf-manifest.csv"))
file.copy(file.path(reference_run,"manifest.csv"),file.path(output,"reference-run-manifest.csv"))
sectors <- read_approved_tma_sector_definitions(airports=airports) |> rename(ICAO=AIRPORT)
monthly<-list(); inventory<-list(); miss_groups<-list(); class_monthly<-list(); daily_misses<-list()
for(i in seq_len(nrow(sources))){
 src<-sources[i,]; message("Reviewing ",src$COHORT," ",src$ICAO," ",src$YEAR)
 raw<-read_refresh_apdf(src)
 if(!is.na(src$SOURCE_ROWS) && nrow(raw)!=src$SOURCE_ROWS) stop("Source row count mismatch.")
 obs<-coalesce(as.Date(raw$MVT_TIME_UTC,tz="UTC"),as.Date(raw$BLOCK_TIME_UTC,tz="UTC"))
 start<-as.Date(paste0(src$YEAR,"-01-01"))
 end<-as.Date(paste0(src$YEAR,if(src$YEAR==2026) "-09-30" else "-12-31"))
 inside<-!is.na(obs)&obs>=start&obs<=end
 inventory[[i]]<-tibble(COHORT=src$COHORT,YEAR=src$YEAR,ICAO=src$ICAO,
   N_SOURCE_ROWS=nrow(raw),N_DATE_UNAVAILABLE=sum(is.na(obs)),N_OUTSIDE_WINDOW=sum(!is.na(obs)&!inside),
   N_REPORTED_IN_WINDOW=sum(inside),N_UNKNOWN_PHASE=sum(inside & !raw$SRC_PHASE %in% c("ARR","DEP")),
   WINDOW_START=as.character(start),WINDOW_END=as.character(end))
 s<-tma_coverage_samples(raw,src$ICAO,filter(sectors,ICAO==src$ICAO),filter(reference,ICAO==src$ICAO)) |>
   filter(!is.na(OBS_DATE),OBS_DATE>=start,OBS_DATE<=end) |>
   mutate(COHORT=src$COHORT,YEAR=src$YEAR,MONTH=format(OBS_DATE,"%Y-%m"))
 monthly[[i]]<-tma_coverage_counts(s,c("COHORT","YEAR","ICAO","PHASE","RANGE_NM","MONTH"))
 class_monthly[[i]]<-s |> mutate(CLASS_DIAGNOSTIC=case_when(HELICOPTER~"HELICOPTER_EXCLUDED",
   CLASS_UNIDENTIFIED~"UNIDENTIFIED",TRUE~CLASS)) |>
   summarise(N_REPORTED=n(),N_MISSING_CROSSING=sum(MISSING_CROSSING),N_VALID=sum(ALGORITHM_VALID),
     N_MISS=sum(MISS),.by=c(COHORT,YEAR,ICAO,PHASE,RANGE_NM,MONTH,CLASS_DIAGNOSTIC))
 miss_groups[[i]]<-s |> filter(MISS) |>
   summarise(N_MISS=n(),FIRST_DATE=if(n()) min(OBS_DATE) else as.Date(NA),
     LAST_DATE=if(n()) max(OBS_DATE) else as.Date(NA),DAYS=n_distinct(OBS_DATE),
     .by=c(COHORT,YEAR,ICAO,PHASE,RANGE_NM,MONTH,CLASS,RWY,SECTOR,IDENTIFIED))
 daily_misses[[i]]<-s |> summarise(N_VALID=sum(ALGORITHM_VALID),N_MISS=sum(MISS),
   .by=c(COHORT,YEAR,ICAO,PHASE,RANGE_NM,OBS_DATE))
 rm(raw,s);gc(verbose=FALSE)
}
monthly<-bind_rows(monthly)
sum_counts <- function(x,groups) x |> summarise(across(starts_with("N_"),sum),.by=all_of(groups))
group_keys<-c("COHORT","YEAR","ICAO","PHASE","RANGE_NM")
annual<-sum_counts(monthly,group_keys) |> mutate(PERIOD="AVAILABLE_YEAR",
  WINDOW_START=paste0(YEAR,"-01-01"),WINDOW_END=paste0(YEAR,ifelse(YEAR==2026,"-09-30","-12-31")))
ytd<-monthly |> filter(substr(MONTH,6,7)<="09") |> sum_counts(group_keys) |>
  mutate(PERIOD="JAN_SEP",WINDOW_START=paste0(YEAR,"-01-01"),WINDOW_END=paste0(YEAR,"-09-30"))
periods<-bind_rows(annual,ytd)
reference_cells<-reference |> mutate(IDENTIFIED=!tma_missing_label(RWY)&!tma_missing_label(CLASS)) |>
  summarise(N_REFERENCE_GROUPS=n(),N_DIAGNOSTIC_GROUPS=sum(!IDENTIFIED),
    N_GROUPS_LT5=sum(REF_N<5),N_GROUPS_LT20=sum(REF_N<20),
    N_IDENTIFIED_GROUPS_LT5=sum(IDENTIFIED&REF_N<5),N_IDENTIFIED_GROUPS_LT20=sum(IDENTIFIED&REF_N<20),
    N_REFERENCE_OBSERVATIONS=sum(REF_N),.by=all_of(c("ICAO","PHASE","RANGE_NM")))
release<-annual |> filter(COHORT=="REFERENCE_INPUT_2024") |>
  left_join(reference_cells,by=c("ICAO","PHASE","RANGE_NM"),relationship="one-to-one") |>
  tma_coverage_rates() |> mutate(RELEASE_STATUS="COVERAGE_REVIEW_PENDING")
stopifnot(all(release$N_MISS==0),all(release$N_VALID==release$N_REFERENCE_OBSERVATIONS))
write_csv(release,file.path(output,"reference-release-detail.csv"))
compact_ref<-sum_counts(release,"ICAO") |> tma_coverage_rates() |>
  mutate(REFERENCE_YEAR=2024L,RELEASE_STATUS="COVERAGE_REVIEW_PENDING")
write_csv(compact_ref,file.path(output,"reference-release-summary.csv"))
write_csv(tma_coverage_rates(periods),file.path(output,"coverage-period-detail.csv"))
compact<-sum_counts(periods,c("COHORT","YEAR","ICAO","PERIOD","WINDOW_START","WINDOW_END")) |>
  tma_coverage_rates()
write_csv(compact,file.path(output,"application-coverage-summary.csv"))
write_csv(tma_coverage_rates(monthly),file.path(output,"coverage-monthly.csv"))
write_csv(bind_rows(inventory),file.path(output,"input-inventory.csv"))
write_csv(bind_rows(miss_groups),file.path(output,"missing-reference-groups.csv"))
write_csv(bind_rows(class_monthly),file.path(output,"coverage-by-class-month.csv"))
write_csv(bind_rows(daily_misses),file.path(output,"missing-reference-daily.csv"))
write_csv(reference_cells,file.path(output,"reference-cell-support.csv"))
writeLines(capture.output(sessionInfo()),file.path(output,"session-info.txt"))
code<-c(script,file.path(project_root,c("scripts/lib/tma-coverage.R","R/apdf_tma.R","R/apdf.R","R/decode.R")))
write_csv(tibble(FILE=code,MD5=unname(tools::md5sum(code))),file.path(output,"code-and-method.csv"))
writeLines("COMPLETE",file.path(output,"RUN-STATUS.txt"))
message("Coverage computation complete: ",output)
