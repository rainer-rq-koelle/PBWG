#!/usr/bin/env Rscript
# Rscript scripts/backfill-study-products.R CACHE REFERENCE_RUN STUDY_OUTPUT
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=3L) stop("Supply CACHE REFERENCE_RUN STUDY_OUTPUT.")
script<-sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[[1]])
root<-normalizePath(file.path(dirname(script),".."))
suppressPackageStartupMessages({library(dplyr);library(readr);devtools::load_all(root,quiet=TRUE)})
source(file.path(root,"scripts/lib/aviation-r2.R"));source(file.path(root,"scripts/lib/refresh-workflow.R"))
source(file.path(root,"scripts/lib/tma-coverage.R"))
out<-normalizePath(args[3],mustWork=TRUE);dest<-file.path(out,"historical-completion")
if(dir.exists(dest)&&length(list.files(dest))) stop("Use a new historical-completion directory.")
dir.create(dest);dir.create(file.path(dest,"airport-tma"));dir.create(file.path(dest,"airport-traffic"))
writeLines("RUNNING",file.path(dest,"RUN-STATUS.txt"))
airports<-pbwg_study_airports();variants<-tma_reference_variants()$REF_VARIANT
sources<-bind_rows(lapply(2024:2025,function(y) aviation_refresh_sources(args[1],"apdf",y,airports)))
stopifnot(all(unname(tools::md5sum(sources$ARCHIVE))==sources$SOURCE_MD5))
write_csv(sources,file.path(dest,"sources.csv"))
refs<-setNames(lapply(variants,function(v) read_tma_coverage_references(args[2],v) |>
 rename(N=REF_N) |> mutate(REF_VARIANT=v,REF_PERIOD="2024")),variants)
sectors<-read_approved_tma_sector_definitions() |> rename(ICAO=AIRPORT)
expected<-read_csv(file.path(root,"outputs/tma-coverage-review-2024-2026-20261102/coverage-period-detail.csv"),show_col_types=FALSE) |>
 filter(COHORT=="ARCHIVE_SNAPSHOT",PERIOD=="AVAILABLE_YEAR",YEAR %in% 2024:2025)
combined<-list();checks<-list()
for(i in seq_len(nrow(sources))){
 src<-sources[i,];a<-src$ICAO;y<-src$YEAR;message("Completing historical products: ",a," ",y)
 raw<-read_refresh_apdf(src) |> mutate(across(where(~inherits(.x,"POSIXt")),~lubridate::with_tz(.x,"UTC")))
 stopifnot(nrow(raw)==src$SOURCE_ROWS)
 s<-raw |> prepare_apdf_tma_input() |> prepare_tma_reference_input() |>
  filter(ICAO==a,lubridate::year(MVT_TIME)==y) |> assign_tma_sector(filter(sectors,ICAO==a))
 for(v in variants){
  z<-apply_tma_reference(s,filter(refs[[v]],ICAO==a))
  daily<-summarise_pbwg_tma_daily(z,year=y)
  write_pbwg_tma(daily,y,2024,v,file.path(dest,"airport-tma"),airport=a)
  counts<-daily |> summarise(N_VALID=sum(MVTS),N_MISS=sum(TMA_NA),.by=c(ICAO,PHASE,RANGE_NM)) |>
   inner_join(filter(expected,ICAO==a,YEAR==y),by=c("ICAO","PHASE","RANGE_NM"),suffix=c("_NEW","_EXPECTED"))
  ok<-nrow(counts)==4&&all(counts$N_VALID_NEW==counts$N_VALID_EXPECTED & counts$N_MISS_NEW==counts$N_MISS_EXPECTED)
  matched<-z$VALID_TMA&z$HAS_REFERENCE
  arithmetic<-isTRUE(all.equal(sum(daily$TOT_ADD_TIME),sum(z$TMA_TIME[matched])-sum(z$REF_TMA[matched]),tolerance=1e-9))
  stopifnot(ok,arithmetic)
  checks[[length(checks)+1L]]<-tibble(ICAO=a,YEAR=y,VARIANT=v,COVERAGE_MATCHES_REVIEW=ok,ARITHMETIC=arithmetic)
  combined[[length(combined)+1L]]<-mutate(daily,YEAR=y,VARIANT=v)
 }
 if((a=="LGAV"&&y==2024)||(a=="LPPT"&&y==2025)){
  input<-prepare_apdf_traffic_input(raw) |> filter(ICAO==a,!is.na(DATE),lubridate::year(DATE)==y)
  daily_traffic<-prepare_apdf_daily_traffic(input,year=y)
  stopifnot(sum(daily_traffic$ARRS+daily_traffic$DEPS)==sum(input$PHASE %in% c("ARR","DEP")))
  write_pbwg_airport_traffic(daily_traffic,a,y,file.path(dest,"airport-traffic"))
 }
 rm(raw,s,z);gc(verbose=FALSE)
}
combined<-bind_rows(combined)
for(y in 2024:2025) for(v in variants) write_pbwg_tma(filter(combined,YEAR==y,VARIANT==v) |> select(-YEAR,-VARIANT),
 y,2024,v,file.path(dest,"airport-tma"))
write_csv(bind_rows(checks),file.path(dest,"validation-checks.csv"))
writeLines("COMPLETE",file.path(dest,"RUN-STATUS.txt"))
