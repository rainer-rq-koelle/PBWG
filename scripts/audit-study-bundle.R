#!/usr/bin/env Rscript
# Inventory the 2024-2026 study without rewriting existing analytical products.
# Rscript scripts/audit-study-bundle.R STUDY_OUTPUT
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Supply STUDY_OUTPUT.")
script<-sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[[1]])
root<-normalizePath(file.path(dirname(script),".."))
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr)})
out<-normalizePath(args[1],mustWork=TRUE)
legacy<-normalizePath(file.path(root,"../PBWG-data"))
refresh<-file.path(root,"outputs/pbwg-refresh-2026-20261102")
airports<-sort(unique(read_csv(file.path(out,"sources.csv"),show_col_types=FALSE)$ICAO))
variants<-c("icao_ganp_p20","pbwg_avg_p05_p15")
candidates<-list();cache<-new.env()
add<-function(product,variant,year,path,origin){
 if(!file.exists(path)) return(invisible(NULL))
 path<-normalizePath(path)
 if(!exists(path,envir=cache,inherits=FALSE)){
  header<-names(read_csv(path,n_max=0,show_col_types=FALSE))
  wanted<-intersect(c("ICAO","DATE","BIN","PHASE","RANGE_NM","REG","REGION","D","A","I","O"),header)
  x<-read_csv(path,col_select=all_of(wanted),show_col_types=FALSE)
  if(!"ICAO" %in% names(x)) x$ICAO<-"EUR"
  if(!"PHASE" %in% names(x)) x$PHASE<-"ALL"
  if(!"RANGE_NM" %in% names(x)) x$RANGE_NM<-0L
  if("REG" %in% names(x)) x<-filter(x,REG=="EUR")
  if("REGION" %in% names(x)) x<-filter(x,REGION=="EUR")
  x$DATE<-as.Date(if("DATE" %in% names(x)) x$DATE else x$BIN,tz="UTC")
  x<-x |> filter(!is.na(DATE)) |> mutate(YEAR=as.integer(format(DATE,"%Y")))
  assign(path,x,envir=cache)
 }
 x<-get(path,envir=cache)
 ceiling<-as.Date(paste0(year,if(year==2026&&product=="network") "-10-28" else if(year==2026) "-09-30" else "-12-31"))
 x<-x |> filter(YEAR==year,DATE<=ceiling)
 if(!nrow(x)) return(invisible(NULL))
 x$BREAKDOWN_MISSING<-if(product=="network"&&all(c("D","A","I","O") %in% names(x)))
   rowSums(is.na(x[c("D","A","I","O")]))>0 else FALSE
 detail<-x |> summarise(N_GROUP_DAYS=n_distinct(DATE),.by=c(ICAO,PHASE,RANGE_NM)) |>
  summarise(MIN_SUBGROUP_DAYS=min(N_GROUP_DAYS),N_PHASE_RANGE_GROUPS=n(),.by=ICAO)
 candidates[[length(candidates)+1L]]<<-x |> summarise(N_ROWS=n(),N_DAYS=n_distinct(DATE),
  N_DAYS_MISSING_BREAKDOWN=n_distinct(DATE[BREAKDOWN_MISSING]),FIRST_DATE=min(DATE),LAST_DATE=max(DATE),.by=ICAO) |>
  left_join(detail,by="ICAO") |> mutate(YEAR=year,PRODUCT=product,VARIANT=variant,FILE=path,ORIGIN=origin,
    EXPECTED_DAYS=as.integer(ceiling-as.Date(paste0(year,"-01-01")))+1L,
    MD5=unname(tools::md5sum(path)))
}
for(y in 2024:2026){
 if(y==2026){
  add("traffic","none",y,file.path(refresh,"apdf/airport-traffic/PBWG-EUR-airport-traffic-2026p.csv"),"VALIDATED_REFRESH")
  add("punctuality","none",y,file.path(refresh,"apdf/airport-punctuality/PBWG-EUR-punc-2026p.csv"),"VALIDATED_REFRESH")
  add("throughput_15min","none",y,file.path(refresh,"apdf/airport-throughput/PBWG-EUR-thru-analytic-2026p.csv"),"VALIDATED_REFRESH")
  add("network","none",y,file.path(refresh,"network/network-traffic/PBWG-EUR-network-traffic-2026p.csv"),"VALIDATED_REFRESH")
 } else {
  for(f in if(y==2024) c("EUR-airport-tfc-2022-2024.csv","EUR-airport-tfc.csv") else "PBWG-EUR-airport-traffic-2025.csv")
   add("traffic","none",y,file.path(legacy,"airport-traffic",f),"LEGACY_NOT_REVALIDATED")
  for(f in if(y==2024) c("PBWG-EUR-PUNC-2024.csv","PBWG-EUR-PUNC-LPPT-2019-2024.csv","PBWG-EUR-LGAV-punc-2024.csv") else "PBWG-EUR-punc-2025.csv")
   add("punctuality","none",y,file.path(legacy,"airport-punctuality",f),"LEGACY_NOT_REVALIDATED")
  add("throughput_15min","none",y,file.path(legacy,"airport-throughput",paste0("PBWG-EUR-thru-analytic-",y,".csv")),"LEGACY_NOT_REVALIDATED")
  add("network","none",y,file.path(legacy,"network-traffic",paste0("PBWG-EUR-network-traffic-",y,".csv")),"LEGACY_NOT_REVALIDATED")
  add("network","none",y,file.path(root,"../international-PBWG-2026/data/processed/network-traffic/EUR-network-traffic-2019-2025.csv"),"LEGACY_NOT_REVALIDATED")
  for(a in airports) add("traffic","none",y,file.path(out,"historical-completion/airport-traffic",paste0("PBWG-EUR-",a,"-tfc-",y,".csv")),"VALIDATED_CURRENT_RUN")
 }
 for(v in variants){
  add("taxi",v,y,file.path(out,"airport-taxi",paste0("PBWG-EUR-txxt-analytic-",if(y==2026) "2026p" else y,"-ref2024-",v,".csv")),"VALIDATED_CURRENT_RUN")
  if(y==2026) add("tma",v,y,file.path(refresh,"apdf/airport-tma",paste0("PBWG-EUR-tma-analytic-2026p-ref2024-",v,".csv")),"VALIDATED_REFRESH") else
   add("tma",v,y,file.path(legacy,"airport-tma",paste0("PBWG-EUR-tma-analytic-2023-2025-ref2024-",v,".csv")),"LEGACY_NOT_REVALIDATED")
  if(y<2026) add("tma",v,y,file.path(out,"historical-completion/airport-tma",paste0("PBWG-EUR-tma-analytic-",y,"-ref2024-",v,".csv")),"VALIDATED_CURRENT_RUN")
 }
}
catalog<-bind_rows(candidates)
write_csv(catalog,file.path(out,"bundle-candidate-files.csv"))
selected<-catalog |> arrange(desc(N_DAYS),desc(N_ROWS)) |> distinct(ICAO,YEAR,PRODUCT,VARIANT,.keep_all=TRUE)
products<-bind_rows(tibble(PRODUCT=c("traffic","punctuality","throughput_15min"),VARIANT="none"),
 expand_grid(PRODUCT=c("tma","taxi"),VARIANT=variants))
expected<-bind_rows(crossing(ICAO=airports,YEAR=2024:2026,products),
 tibble(ICAO="EUR",YEAR=2024:2026,PRODUCT="network",VARIANT="none"))
inventory<-left_join(expected,selected,by=c("ICAO","YEAR","PRODUCT","VARIANT")) |>
 mutate(STATUS=case_when(is.na(FILE)~"MISSING",
  N_DAYS<EXPECTED_DAYS|MIN_SUBGROUP_DAYS<EXPECTED_DAYS~"OBSERVED_DATE_GAPS",
  N_DAYS_MISSING_BREAKDOWN>0~"TOTALS_COMPLETE_BREAKDOWN_GAPS",
  PRODUCT=="tma"&N_PHASE_RANGE_GROUPS!=4~"MISSING_PHASE_RANGE",
  PRODUCT %in% c("taxi","punctuality")&N_PHASE_RANGE_GROUPS!=2~"MISSING_PHASE",
  TRUE~ORIGIN))
cap<-read_csv(file.path(out,"capacity-loads/capacity-inventory.csv"),show_col_types=FALSE)
caprows<-crossing(cap,PRODUCT=c("capacity","bli_pli")) |>
 transmute(ICAO,YEAR,PRODUCT,VARIANT="none",STATUS=ifelse(is.na(MAX_CAP),"MISSING_CAPACITY",STATUS),
 FILE=file.path(out,"capacity-loads",ifelse(PRODUCT=="capacity","capacity-inventory.csv","EUR-bli-pli-2024-2026.csv")))
inventory<-bind_rows(inventory,caprows) |> arrange(PRODUCT,VARIANT,YEAR,ICAO)
write_csv(inventory,file.path(out,"study-bundle-inventory.csv"))
write_csv(inventory |> count(PRODUCT,VARIANT,YEAR,STATUS),file.path(out,"study-bundle-summary.csv"))
write_csv(inventory |> filter(STATUS %in% c("MISSING","MISSING_CAPACITY","OBSERVED_DATE_GAPS","MISSING_PHASE_RANGE","MISSING_PHASE","TOTALS_COMPLETE_BREAKDOWN_GAPS")),file.path(out,"study-bundle-gaps.csv"))
print(inventory |> count(PRODUCT,YEAR,STATUS),n=80)
