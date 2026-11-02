#!/usr/bin/env Rscript
# Fixed-2024 taxi references, 2024-2026 applications, and observed hourly throughput.
# Rscript scripts/complete-study-airports.R CACHE OUTPUT [AIRPORTS]
args<-commandArgs(trailingOnly=TRUE)
if(!length(args)%in%c(2L,3L)) stop("Supply CACHE OUTPUT [comma-separated AIRPORTS].")
script<-sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[[1]])
root<-normalizePath(file.path(dirname(script),".."))
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr);devtools::load_all(root,quiet=TRUE)})
source(file.path(root,"scripts/lib/aviation-r2.R"))
source(file.path(root,"scripts/lib/refresh-workflow.R"))
source(file.path(root,"scripts/lib/tma-coverage.R"))
airports<-if(length(args)==3) strsplit(args[3],",")[[1]] else pbwg_study_airports()
stopifnot(length(airports)>0,!anyDuplicated(airports),all(airports %in% pbwg_study_airports()))
out<-args[2]
if(dir.exists(out)&&length(list.files(out,all.files=TRUE,no..=TRUE))) stop("Use a new output directory.")
dir.create(out,recursive=TRUE)
for(d in c("taxi-reference","airport-taxi","hourly-throughput","validation")) dir.create(file.path(out,d))
writeLines("RUNNING",file.path(out,"RUN-STATUS.txt"))
options(warn=1)
sources<-bind_rows(lapply(2024:2026,function(y) aviation_refresh_sources(args[1],"apdf",y,airports)))
sources$OBSERVED_MD5<-unname(tools::md5sum(sources$ARCHIVE))
stopifnot(all(sources$OBSERVED_MD5==sources$SOURCE_MD5))
write_csv(sources,file.path(out,"sources.csv"))
file.copy(file.path(args[1],"apdf-manifest.csv"),file.path(out,"upstream-apdf-manifest.csv"))
variants<-txxt_reference_variants()$REF_VARIANT
inventory<-list(); coverage<-list(); missed<-list(); support<-list(); checks<-list(); daily_all<-list()
check<-function(airport,year,variant,what,ok){
 checks[[length(checks)+1L]]<<-tibble(ICAO=airport,YEAR=year,VARIANT=variant,CHECK=what,PASS=isTRUE(ok))
 if(!isTRUE(ok)) stop("Failed: ",airport," ",year," ",variant," ",what)
}
for(a in airports){
 refs<-list()
 for(y in 2024:2026){
  message("Preparing ",a," ",y)
  src<-filter(sources,ICAO==a,YEAR==y)
  raw<-read_refresh_apdf(src) |> mutate(across(where(~inherits(.x,"POSIXt")),~lubridate::with_tz(.x,"UTC")))
  stopifnot(nrow(raw)==src$SOURCE_ROWS)
  start<-as.Date(paste0(y,"-01-01"));end<-as.Date(paste0(y,if(y==2026) "-09-30" else "-12-31"))
  s<-raw |> prepare_apdf_txxt_input() |> prepare_txxt_reference_input() |>
   mutate(DATE_BLOCK=as.Date(BLOCK_TIME,tz="UTC"),
    Q_DATE=coalesce(DATE_BLOCK,as.Date(MVT_TIME,tz="UTC")),
    UNIDENTIFIED=tma_missing_label(RWY)|tma_missing_label(STND))
  inventory[[length(inventory)+1L]]<-tibble(ICAO=a,YEAR=y,N_SOURCE=nrow(raw),
   N_UNKNOWN_PHASE=sum(!raw$SRC_PHASE %in% c("ARR","DEP")),N_UNKNOWN_AIRPORT=sum(is.na(s$ICAO)),
   N_UNDATED=sum(is.na(s$Q_DATE)),N_OUTSIDE_WINDOW=sum(!is.na(s$Q_DATE)&(s$Q_DATE<start|s$Q_DATE>end)))
  s<-s |> filter(ICAO==a,!is.na(Q_DATE),Q_DATE>=start,Q_DATE<=end)
  if(y==2024){
   for(v in variants){
    r<-build_txxt_reference(s,as.POSIXct("2024-01-01 00:00:00",tz="UTC"),
      as.POSIXct("2024-12-31 23:59:59",tz="UTC"),variant=v,min_n=5,
      keep_below_threshold=TRUE,include_unknown=FALSE)
    refs[[v]]<-r
    write_pbwg_txxt_reference(r,a,2024,v,5,file.path(out,"taxi-reference"))
    support[[length(support)+1L]]<-r |> mutate(UNIDENTIFIED=tma_missing_label(RWY)|tma_missing_label(STND)) |>
     summarise(N_GROUPS=n(),N_GROUPS_LT5=sum(N<5),N_GROUPS_LT20=sum(N<20),
       N_DIAGNOSTIC_GROUPS=sum(UNIDENTIFIED),N_REFERENCE_OBS=sum(N),
       N_ELIGIBLE_REFERENCE_OBS=sum(N[IS_VALID_SAMPLE]),.by=c(ICAO,PHASE,REF_VARIANT))
   }
   keys<-c("ICAO","PHASE","RWY","STND","N","IS_VALID_SAMPLE")
   check(a,y,"both","Reference keys and support identical",identical(select(refs[[1]],all_of(keys)),select(refs[[2]],all_of(keys))))
  }
  previous<-NULL
  for(v in variants){
   r<-refs[[v]]
   z<-apply_txxt_reference(s,r,valid_reference_only=TRUE) |>
    left_join(select(r,ICAO,PHASE,RWY,STND,REF_N=N),by=c("ICAO","PHASE","RWY","STND"),relationship="many-to-one") |>
    mutate(MONTH=format(Q_DATE,"%Y-%m"),
      MISS=VALID_TXXT&!HAS_REFERENCE,
      MISS_UNIDENTIFIED=MISS&UNIDENTIFIED,
      MISS_NO_GROUP=MISS&!UNIDENTIFIED&is.na(REF_N),
      MISS_SPARSE=MISS&!UNIDENTIFIED&!is.na(REF_N)&REF_N<5,
      MATCHED=VALID_TXXT&HAS_REFERENCE)
   daily<-summarise_pbwg_txxt_daily(z,year=y)
   write_pbwg_txxt(daily,if(y==2026) "2026p" else y,2024,v,file.path(out,"airport-taxi"),a)
   daily_all[[length(daily_all)+1L]]<-mutate(daily,YEAR=y,REF_VARIANT=v)
   coverage[[length(coverage)+1L]]<-z |>
    summarise(N_REPORTED=n(),N_VALID=sum(VALID_TXXT),N_MATCHED=sum(MATCHED),N_MISS=sum(MISS),
      N_MISS_UNIDENTIFIED=sum(MISS_UNIDENTIFIED),N_MISS_NO_GROUP=sum(MISS_NO_GROUP),N_MISS_SPARSE=sum(MISS_SPARSE),
      N_UNIDENTIFIED=sum(UNIDENTIFIED),N_MATCHED_UNIDENTIFIED=sum(MATCHED&UNIDENTIFIED),
      N_MATCHED_REF_LT20=sum(MATCHED&!is.na(REF_N)&REF_N<20),
      N_MISSING_BLOCK=sum(is.na(BLOCK_TIME)),N_MISSING_MVT=sum(is.na(MVT_TIME)),
      N_INVALID_TIME=sum(!VALID_TXXT),N_EXTREME=sum(EXTREME_TXXT),
      .by=c(ICAO,PHASE,MONTH)) |> mutate(YEAR=y,REF_VARIANT=v)
   if(any(z$MISS)) missed[[length(missed)+1L]]<-z |> filter(MISS) |>
    mutate(REASON=case_when(MISS_UNIDENTIFIED~"UNIDENTIFIED_INPUT",MISS_SPARSE~"REFERENCE_N_LT5",TRUE~"NO_REFERENCE_GROUP")) |>
    summarise(N_MISS=n(),FIRST_DATE=min(Q_DATE),LAST_DATE=max(Q_DATE),.by=c(ICAO,PHASE,RWY,STND,REF_N,REASON)) |>
    mutate(YEAR=y,REF_VARIANT=v)
   check(a,y,v,"Daily count reconciliation",sum(daily$MVTS)==sum(z$VALID_TXXT)&&sum(daily$TX_NA)==sum(z$MISS))
   check(a,y,v,"Miss reasons partition",sum(z$MISS)==sum(z$MISS_UNIDENTIFIED)+sum(z$MISS_NO_GROUP)+sum(z$MISS_SPARSE))
   check(a,y,v,"Additional time arithmetic",isTRUE(all.equal(sum(daily$TOT_ADD_TIME),sum(z$TXXT[z$MATCHED])-sum(z$REF_TXXT[z$MATCHED]),tolerance=1e-9)))
   if(!is.null(previous)) check(a,y,v,"Variant coverage identical",identical(previous,select(daily,ICAO,PHASE,DATE,MVTS,TX_NA)))
   previous<-select(daily,ICAO,PHASE,DATE,MVTS,TX_NA)
  }
  # Observed hours only: no silent zero-filling of gaps or change to the historical BLI/PLI denominator.
  h<-raw |> prepare_apdf_traffic_input() |>
   filter(ICAO==a,PHASE %in% c("ARR","DEP"),!is.na(MVT_TIME),as.Date(MVT_TIME)>=start,as.Date(MVT_TIME)<=end) |>
   calc_throughput(unit="hour")
  write_csv(h,file.path(out,"hourly-throughput",paste0("EUR-",a,"-hourly-throughput-",y,".csv")))
  check(a,y,"hourly","Hourly movement count",sum(h$FLTS)==sum(raw$SRC_PHASE %in% c("ARR","DEP") &
    !is.na(raw$MVT_TIME_UTC) & as.Date(raw$MVT_TIME_UTC)>=start & as.Date(raw$MVT_TIME_UTC)<=end &
    ifelse(raw$SRC_PHASE=="ARR",!is.na(raw$ADES_ICAO)&raw$ADES_ICAO==a,!is.na(raw$ADEP_ICAO)&raw$ADEP_ICAO==a),na.rm=TRUE))
  rm(raw,s,z,h);gc(verbose=FALSE)
 }
}
daily_all<-bind_rows(daily_all)
for(y in 2024:2026) for(v in variants) write_pbwg_txxt(
 filter(daily_all,YEAR==y,REF_VARIANT==v) |> select(-YEAR,-REF_VARIANT),
 if(y==2026) "2026p" else y,2024,v,file.path(out,"airport-taxi"))
monthly<-bind_rows(coverage)
rates<-function(x) x |> mutate(PCT_VALID=100*N_VALID/N_REPORTED,
 PCT_MISS_VALID=ifelse(N_VALID>0,100*N_MISS/N_VALID,NA_real_),
 PCT_UNIDENTIFIED=100*N_UNIDENTIFIED/N_REPORTED,
 PCT_MATCHED_REF_LT20=ifelse(N_MATCHED>0,100*N_MATCHED_REF_LT20/N_MATCHED,NA_real_))
write_csv(rates(monthly),file.path(out,"taxi-coverage-monthly.csv"))
annual<-monthly |> summarise(across(starts_with("N_"),sum),.by=c(ICAO,YEAR,PHASE,REF_VARIANT)) |>
 mutate(PERIOD="AVAILABLE_YEAR")
ytd<-monthly |> filter(substr(MONTH,6,7)<="09") |>
 summarise(across(starts_with("N_"),sum),.by=c(ICAO,YEAR,PHASE,REF_VARIANT)) |> mutate(PERIOD="JAN_SEP")
write_csv(rates(bind_rows(annual,ytd)),file.path(out,"taxi-coverage-period.csv"))
write_csv(bind_rows(missed),file.path(out,"taxi-missing-reference-groups.csv"))
write_csv(bind_rows(support),file.path(out,"taxi-reference-support.csv"))
write_csv(bind_rows(inventory),file.path(out,"input-inventory.csv"))
write_csv(bind_rows(checks),file.path(out,"validation","checks.csv"))
writeLines(capture.output(sessionInfo()),file.path(out,"session-info.txt"))
code<-c(script,file.path(root,c("R/apdf_txxt.R","R/apdf_throughput.R","R/decode.R","scripts/lib/aviation-r2.R")))
write_csv(tibble(FILE=code,MD5=unname(tools::md5sum(code))),file.path(out,"code-and-method.csv"))
files<-list.files(out,recursive=TRUE,full.names=TRUE)
write_csv(tibble(FILE=substring(files,nchar(out)+2),BYTES=file.info(files)$size,MD5=unname(tools::md5sum(files))) |>
 filter(FILE!="RUN-STATUS.txt"),file.path(out,"manifest.csv"))
writeLines("TAXI_AND_HOURLY_COMPLETE",file.path(out,"RUN-STATUS.txt"))
