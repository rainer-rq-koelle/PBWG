#!/usr/bin/env Rscript
# Rscript scripts/build-study-capacity-loads.R STUDY_OUTPUT [CAPACITY_OVERRIDES_CSV]
# Overrides must supply ICAO,YEAR,MAX_CAP,SOURCE,STATUS; no implicit carry-forward.
args<-commandArgs(trailingOnly=TRUE)
if(!length(args)%in%c(1L,2L)) stop("Supply STUDY_OUTPUT [CAPACITY_OVERRIDES_CSV].")
script<-sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[[1]])
root<-normalizePath(file.path(dirname(script),".."))
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr);devtools::load_all(root,quiet=TRUE)})
source(file.path(root,"scripts/lib/refresh-workflow.R"))
out<-normalizePath(args[1],mustWork=TRUE)
dest<-file.path(out,"capacity-loads")
if(dir.exists(dest)&&length(list.files(dest))) stop("Use a new capacity-loads directory; preserve previous runs.")
dir.create(dest,showWarnings=FALSE)
historical<-file.path(root,"scripts/build-bra-eur-bli-pli-report-inputs.R")
# Evaluate only the two reviewed data-definition assignments, never the report-writing script.
env<-new.env(parent=globalenv())
for(e in parse(historical)) if(is.call(e)&&identical(e[[1]],as.name("<-"))&&
 as.character(e[[2]]) %in% c("carry_forward_capacity","eur_cap_history")) eval(e,envir=env)
history<-env$eur_cap_history |> rename(ICAO=APT_ICAO) |> mutate(SOURCE=historical,
 SOURCE_VALUE_YEAR=ifelse(YEAR %in% c(2023,2024),2022L,YEAR),
 STATUS=ifelse(YEAR %in% c(2023,2024),"EXISTING_REPORT_CARRY_FORWARD","EXISTING_REPORT_VALUE"))
lirf_source<-normalizePath(file.path(root,"../BRA-EUR-2024/R/05-capacity_and_throughput-graphs.R"),mustWork=TRUE)
lines<-readLines(lirf_source)
stopifnot(any(grepl('"LIRF" , 2022 , 90',lines,fixed=TRUE)),
 any(grepl('eur_cap <- eur_cap |> no_change_cap(2023) |> no_change_cap(2024)',lines,fixed=TRUE)))
history<-bind_rows(history,tibble(ICAO="LIRF",YEAR=2024L,MAX_CAP=90,SOURCE=lirf_source,
 SOURCE_VALUE_YEAR=2022L,STATUS="EXISTING_REPORT_CARRY_FORWARD"))
airports<-sort(unique(read_csv(file.path(out,"sources.csv"),show_col_types=FALSE)$ICAO))
capacity<-expand_grid(ICAO=airports,YEAR=2024:2026) |>
 left_join(history,by=c("ICAO","YEAR"),relationship="one-to-one") |>
 mutate(STATUS=coalesce(STATUS,"MISSING_PERIOD_CAPACITY"))
candidate<-bind_rows(lapply(seq_len(nrow(capacity)),function(i){
 row<-capacity[i,];prior<-history |> filter(ICAO==row$ICAO,YEAR<=row$YEAR) |> arrange(desc(YEAR)) |> slice_head(n=1)
 tibble(ICAO=row$ICAO,YEAR=row$YEAR,CANDIDATE_MAX_CAP=if(nrow(prior)) prior$MAX_CAP else NA_real_,
 CANDIDATE_YEAR=if(nrow(prior)) prior$YEAR else NA_integer_)
}))
capacity<-left_join(capacity,candidate,by=c("ICAO","YEAR"))
if(length(args)==2){
 override<-read_csv(args[2],show_col_types=FALSE)
 if("STATUS" %in% names(override) && any(grepl("CROSS_CHECK_ONLY|CURRENT_SNAPSHOT_PERIOD_UNCONFIRMED",override$STATUS)))
  stop("Cross-check evidence is not a selected study capacity input. Supply explicitly selected study values with provenance.")
 stopifnot(all(c("ICAO","YEAR","MAX_CAP","SOURCE","STATUS") %in% names(override)),
  !anyDuplicated(override[c("ICAO","YEAR")]),all(override$MAX_CAP>0),!anyNA(override$MAX_CAP),
  all(nzchar(override$SOURCE)),all(nzchar(override$STATUS)),all(override$ICAO %in% airports),all(override$YEAR %in% 2024:2026))
 capacity<-bind_rows(anti_join(capacity,override,by=c("ICAO","YEAR")),override) |> arrange(ICAO,YEAR)
 file.copy(args[2],file.path(dest,"capacity-overrides.csv"))
}
write_csv(history,file.path(dest,"historical-capacity-evidence.csv"))
write_csv(capacity,file.path(dest,"capacity-inventory.csv"))
paths<-unique(c(historical,lirf_source,if(length(args)==2) normalizePath(args[2])))
write_csv(tibble(FILE=paths,MD5=unname(tools::md5sum(paths))),file.path(dest,"capacity-sources.csv"))
summaries<-list();hour_counts<-list();loads_all<-list()
for(i in seq_len(nrow(capacity))){
 row<-capacity[i,];a<-row$ICAO;y<-row$YEAR
 h<-read_csv(file.path(out,"hourly-throughput",paste0("EUR-",a,"-hourly-throughput-",y,".csv")),show_col_types=FALSE)
 stopifnot(nrow(h)>0,!anyDuplicated(h$BIN),all(h$FLTS==h$ARRS+h$DEPS),all(h$FLTS>0))
 loads<-prepare_throughput_load_characteristics(h,transmute(row,APT_ICAO=ICAO,YEAR,MAX_CAP)) |>
  mutate(CAPACITY_STATUS=row$STATUS,CAPACITY_SOURCE=row$SOURCE)
 loads_all[[i]]<-loads
 for(p in c("AVAILABLE_YEAR","JAN_SEP")){
  d<-if(p=="JAN_SEP") filter(loads,lubridate::month(BIN)<=9) else loads
  summary<-summarise_load_indices(d) |> mutate(PERIOD=p,WINDOW_START=paste0(y,"-01-01"),
   WINDOW_END=paste0(y,if(y==2026||p=="JAN_SEP") "-09-30" else "-12-31"),
   CAPACITY_STATUS=row$STATUS,CAPACITY_SOURCE=row$SOURCE,
   N_HOURS_WITH_CAPACITY=sum(!is.na(d$MAX_CAP)),DENOMINATOR="OBSERVED_HOURS_WITH_MOVEMENTS",
   N_CALENDAR_HOURS=24*(as.integer(as.Date(WINDOW_END)-as.Date(WINDOW_START))+1),
   N_UNOBSERVED_HOURS=N_CALENDAR_HOURS-N)
  stopifnot(all(is.na(summary$BLI)|(summary$BLI>=0&summary$BLI<=1)),
    all(is.na(summary$PLI)|(summary$PLI>=0&summary$PLI<=summary$BLI)))
  if(is.na(row$MAX_CAP)) stopifnot(is.na(summary$BLI),is.na(summary$PLI))
  summaries[[length(summaries)+1L]]<-summary
 }
 hour_counts[[i]]<-loads |> mutate(DATE=as.Date(BIN,tz="UTC")) |>
  summarise(N_OBSERVED_HOURS=n(),MOVEMENTS=sum(TOT_THRU),BLI_HOURS=if(all(is.na(BLI))) NA_integer_ else sum(BLI),
    PLI_HOURS=if(all(is.na(PLI))) NA_integer_ else sum(PLI),.by=c(ICAO,DATE)) |>
  mutate(YEAR=y,MAX_CAP=row$MAX_CAP,CAPACITY_STATUS=row$STATUS)
}
write_csv(bind_rows(summaries),file.path(dest,"EUR-bli-pli-2024-2026.csv"))
write_csv(bind_rows(hour_counts),file.path(dest,"EUR-load-counts-daily-2024-2026.csv"))
write_csv(bind_rows(loads_all) |> prepare_ordered_throughput(),file.path(dest,"EUR-ordered-throughput-2024-2026.csv"))
writeLines(if(anyNA(capacity$MAX_CAP)) "PARTIAL_AWAITING_CAPACITY" else "CALCULATED_REVIEW_SOURCE_STATUS",file.path(dest,"RUN-STATUS.txt"))
message("Capacity rows available: ",sum(!is.na(capacity$MAX_CAP))," / ",nrow(capacity))
