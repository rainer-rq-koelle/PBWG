#!/usr/bin/env Rscript
# Isolated sensitivity study. Never overwrites the approved reference or products.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2)
study <- normalizePath(args[1]); out <- args[2]
if(dir.exists(out) && length(list.files(out))) stop("Use a new output directory")
dir.create(out,recursive=TRUE)
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr);devtools::load_all(quiet=TRUE)})
source("scripts/lib/aviation-r2.R");source("scripts/lib/refresh-workflow.R");source("scripts/lib/tma-coverage.R")
sources <- read_csv(file.path(study,"sources.csv"),show_col_types=FALSE)
airports <- c("EGKK","EDDF","EDDM","LFPG","LTFM","LPPT")
keys <- c("ICAO","PHASE","RWY","STND")
all_months <- list(); all_groups <- list(); supplements <- list(); results <- list(); checks <- list()
for(a in airports){
 message("Maintenance scenario: ",a)
 ss <- list()
 for(y in 2024:2026){
  src <- filter(sources,ICAO==a,YEAR==y)
  stopifnot(nrow(src)==1,unname(tools::md5sum(src$ARCHIVE))==src$SOURCE_MD5)
  raw <- read_refresh_apdf(src) |> mutate(across(where(~inherits(.x,"POSIXt")),~lubridate::with_tz(.x,"UTC")))
  ss[[as.character(y)]] <- raw |> prepare_apdf_txxt_input() |> prepare_txxt_reference_input() |>
   mutate(Q_DATE=coalesce(as.Date(BLOCK_TIME,tz="UTC"),as.Date(MVT_TIME,tz="UTC")),
    YEAR=y,MONTH=format(Q_DATE,"%Y-%m"),UNIDENTIFIED=tma_missing_label(RWY)|tma_missing_label(STND)) |>
   filter(ICAO==a,!is.na(Q_DATE),Q_DATE>=as.Date(paste0(y,"-01-01")),Q_DATE<=as.Date(paste0(y,if(y==2026) "-09-30" else "-12-31")))
 }
 base <- read_csv(file.path(study,"taxi-reference",paste0("PBWG-EUR-",a,"-ref-txxt-2024-pbwg_avg_p05_p15-n5.csv")),show_col_types=FALSE,
  col_types=cols(RWY=col_character(),STND=col_character()),na="")
 basekeys <- base |> select(all_of(keys),BASE_N=N,BASE_ELIGIBLE=IS_VALID_SAMPLE,BASE_REF_PBWG=REF_TXXT)
 pool <- bind_rows(ss[["2024"]],ss[["2025"]])
 # Build genuine candidate values for both methods, but only for identified gaps.
 for(v in txxt_reference_variants()$REF_VARIANT){
  rr <- build_txxt_reference(pool,as.POSIXct("2024-01-01",tz="UTC"),as.POSIXct("2025-12-31 23:59:59",tz="UTC"),variant=v,min_n=5,keep_below_threshold=TRUE) |>
   left_join(basekeys,by=keys) |>
   filter(!tma_missing_label(RWY),!tma_missing_label(STND),!coalesce(BASE_ELIGIBLE,FALSE)) |>
   mutate(SCENARIO_ONLY=TRUE)
  supplements[[length(supplements)+1]] <- rr
  if(v=="pbwg_avg_p05_p15") candidate <- rr |> select(all_of(keys),POOL_N=N,POOL_REF=REF_TXXT)
 }
 z <- bind_rows(ss) |> left_join(basekeys,by=keys) |> left_join(candidate,by=keys) |>
  mutate(BASE_MATCH=VALID_TXXT & coalesce(BASE_ELIGIBLE,FALSE),
   MISS=VALID_TXXT & !BASE_MATCH,
   REASON=case_when(!MISS~"MATCHED_OR_INVALID",UNIDENTIFIED~"UNIDENTIFIED_INPUT",is.na(BASE_N)~"NO_REFERENCE_GROUP",TRUE~"REFERENCE_N_LT5"))
 gm <- z |> filter(VALID_TXXT) |> summarise(N_VALID=n(),N_MISS=sum(MISS),
  N_RECOVERABLE_N5=sum(MISS & !UNIDENTIFIED & coalesce(POOL_N>=5,FALSE)),
  .by=c(ICAO,PHASE,YEAR,MONTH,RWY,STND,REASON))
 all_groups[[length(all_groups)+1]] <- gm
 counts <- z |> summarise(N_VALID=sum(VALID_TXXT),N_MISS=sum(MISS),.by=c(ICAO,PHASE,YEAR,MONTH))
 existing <- read_csv(file.path(study,"taxi-coverage-monthly.csv"),show_col_types=FALSE) |>
  filter(ICAO==a,REF_VARIANT=="pbwg_avg_p05_p15") |> select(ICAO,PHASE,YEAR,MONTH,N_VALID,N_MISS)
 ck <- full_join(counts,existing,by=c("ICAO","PHASE","YEAR","MONTH"),suffix=c("","_SAVED"))
 stopifnot(all(ck$N_VALID==ck$N_VALID_SAVED),all(ck$N_MISS==ck$N_MISS_SAVED))
 checks[[length(checks)+1]] <- tibble(ICAO=a,CHECK="All monthly denominators and baseline misses reproduce saved run",PASS=TRUE)
 for(threshold in c(5L,20L)){
  zz <- z |> mutate(RECOVERED=MISS & !UNIDENTIFIED & coalesce(POOL_N>=threshold,FALSE),
   REMAIN=MISS & !RECOVERED,NEW_ADD=if_else(RECOVERED,TXXT-POOL_REF,NA_real_))
  mm <- zz |> summarise(N_VALID=sum(VALID_TXXT),N_BASE_MISS=sum(MISS),N_RECOVERED=sum(RECOVERED),N_REMAIN=sum(REMAIN),
   N_UNIDENTIFIED=sum(MISS&UNIDENTIFIED),N_REMAIN_NO_GROUP=sum(REMAIN&REASON=="NO_REFERENCE_GROUP"),
   N_REMAIN_SPARSE=sum(REMAIN&REASON=="REFERENCE_N_LT5"),
   NEW_ADD_SUM=sum(NEW_ADD,na.rm=TRUE),.by=c(ICAO,PHASE,YEAR,MONTH)) |> mutate(SUPPLEMENT_MIN_N=threshold)
  all_months[[length(all_months)+1]] <- mm
  for(period in c("AVAILABLE_YEAR","JAN_SEP")){
   m <- if(period=="JAN_SEP") filter(mm,substr(MONTH,6,7)<="09") else mm
   results[[length(results)+1]] <- m |> summarise(across(starts_with("N_"),sum),NEW_ADD_SUM=sum(NEW_ADD_SUM),
    .by=c(ICAO,PHASE,YEAR,SUPPLEMENT_MIN_N)) |> mutate(PERIOD=period,
     BASE_MISS_PCT=100*N_BASE_MISS/N_VALID,SCENARIO_MISS_PCT=100*N_REMAIN/N_VALID,
     EVALUATION=ifelse(YEAR<=2025,"RETROSPECTIVE_IN_SAMPLE","HELD_OUT_2026"))
  }
 }
 rm(ss,pool,z,raw);gc()
}
write_csv(bind_rows(results),file.path(out,"scenario-period.csv"))
write_csv(bind_rows(all_months),file.path(out,"scenario-monthly.csv"))
write_csv(bind_rows(all_groups),file.path(out,"group-monthly.csv"))
write_csv(bind_rows(supplements),file.path(out,"candidate-supplements.csv"))
write_csv(bind_rows(checks),file.path(out,"validation-checks.csv"))
write_csv(filter(sources,ICAO %in% airports),file.path(out,"sources.csv"))
writeLines(c("SCENARIO_ONLY_COMPLETE","2024 eligible references unchanged. Only identified missing/ineligible groups are candidates.",
 "Candidates pool 2024 and 2025. 2024/2025 recovery is in-sample; 2026 is held out.",
 "N>=20 is a sensitivity criterion for supplements, not a changed baseline eligibility rule.",
 "Coverage recovery is not validation of reference values, subgroup equivalence or operational applicability."),file.path(out,"RUN-STATUS.txt"))
writeLines(capture.output(sessionInfo()),file.path(out,"session-info.txt"))
