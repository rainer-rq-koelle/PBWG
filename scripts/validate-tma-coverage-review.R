#!/usr/bin/env Rscript
# Rscript scripts/validate-tma-coverage-review.R REVIEW_OUTPUT PRODUCTION_APDF_OUTPUT
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop("Supply REVIEW_OUTPUT PRODUCTION_APDF_OUTPUT.")
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr)})
out<-normalizePath(args[[1]],mustWork=TRUE)
production<-normalizePath(args[[2]],mustWork=TRUE)
read<-function(name) read_csv(file.path(out,name),show_col_types=FALSE)
monthly<-read("coverage-monthly.csv");periods<-read("coverage-period-detail.csv")
compact<-read("application-coverage-summary.csv");release<-read("reference-release-detail.csv")
checks<-list()
check<-function(label,condition,detail){
 checks[[length(checks)+1]]<<-tibble(CHECK=label,PASS=isTRUE(condition),DETAIL=detail)
}
check("Original 2024 reference reconstruction", all(release$N_MISS==0 &
 release$N_VALID==release$N_REFERENCE_OBSERVATIONS),"All 52 airport/phase/range cells reconcile with original reference counts.")
check("Counts reconcile",all(monthly$N_VALID==monthly$N_MATCHED+monthly$N_MISS &
 monthly$N_MISS==monthly$N_MISS_IDENTIFIED+monthly$N_MISS_UNIDENTIFIED &
 monthly$N_MATCHED==monthly$N_MATCHED_IDENTIFIED+monthly$N_MATCHED_UNIDENTIFIED),
 "Valid = matched + missed; identified and unidentified components sum to totals.")
check("Counts complete and bounded",!anyNA(select(monthly,starts_with("N_"))) &&
 all(monthly$N_VALID<=monthly$N_REPORTED & monthly$N_DATA_ATTENTION<=monthly$N_REPORTED &
 monthly$N_MISSING_CROSSING<=monthly$N_REPORTED & monthly$N_SMALL_REF_5<=monthly$N_SMALL_REF_20 &
 monthly$N_SMALL_REF_20<=monthly$N_MATCHED),"No missing count; nested samples and percentages have valid denominators.")
expected<-bind_rows(expand_grid(COHORT="REFERENCE_INPUT_2024",YEAR=2024L,MONTH=sprintf("2024-%02d",1:12)),
 bind_rows(lapply(2024:2026,function(y) tibble(COHORT="ARCHIVE_SNAPSHOT",YEAR=y,
 MONTH=sprintf("%d-%02d",y,1:if(y==2026) 9 else 12))))) |>
 crossing(ICAO=sort(unique(monthly$ICAO)),PHASE=c("ARR","DEP"),RANGE_NM=c(40L,100L))
keys<-c("COHORT","YEAR","MONTH","ICAO","PHASE","RANGE_NM")
check("Expected monthly cells",!anyDuplicated(monthly[keys]) && nrow(monthly)==nrow(expected) &&
 nrow(anti_join(expected,monthly,by=keys))==0,"13 airports × two phases × two ranges in every covered month; 2,340 rows.")
group<-c("COHORT","YEAR","ICAO","PHASE","RANGE_NM")
year_counts<-monthly |> summarise(across(starts_with("N_"),sum),.by=all_of(group))
saved<-periods |> filter(PERIOD=="AVAILABLE_YEAR") |> select(all_of(group),starts_with("N_"))
check("Monthly counts reconcile with annual",isTRUE(all.equal(arrange(year_counts,across(all_of(group))),
 arrange(saved,across(all_of(group))),check.attributes=FALSE)),"Aggregation uses summed counts.")
ytd<-monthly |> filter(substr(MONTH,6,7)<="09") |> summarise(across(starts_with("N_"),sum),.by=all_of(group))
saved_ytd<-periods |> filter(PERIOD=="JAN_SEP") |> select(all_of(group),starts_with("N_"))
check("Common January–September window",isTRUE(all.equal(arrange(ytd,across(all_of(group))),
 arrange(saved_ytd,across(all_of(group))),check.attributes=FALSE)),"Same nine calendar months in each year.")
review<-periods |> filter(COHORT=="ARCHIVE_SNAPSHOT",YEAR==2026,PERIOD=="AVAILABLE_YEAR")
for(v in c("pbwg_avg_p05_p15","icao_ganp_p20")){
 p<-read_csv(file.path(production,"airport-tma",paste0("PBWG-EUR-tma-analytic-2026p-ref2024-",v,".csv")),
  show_col_types=FALSE) |> summarise(P_VALID=sum(MVTS),P_MISS=sum(TMA_NA),.by=c(ICAO,PHASE,RANGE_NM))
 z<-left_join(review,p,by=c("ICAO","PHASE","RANGE_NM"),relationship="one-to-one")
 check(paste("2026 production counts",v),all(z$N_VALID==z$P_VALID & z$N_MISS==z$P_MISS),
  paste(format(sum(z$N_VALID),big.mark=","),"valid observations and",sum(z$N_MISS),"misses across 52 cells."))
}
original<-periods |> filter(COHORT=="REFERENCE_INPUT_2024",PERIOD=="AVAILABLE_YEAR")
archived<-periods |> filter(COHORT=="ARCHIVE_SNAPSHOT",YEAR==2024,PERIOD=="AVAILABLE_YEAR")
fields<-c("N_REPORTED","N_VALID","N_MATCHED","N_MISS","N_MISSING_CROSSING","N_DATA_ATTENTION")
vintage<-inner_join(select(original,ICAO,PHASE,RANGE_NM,all_of(fields)),
 select(archived,ICAO,PHASE,RANGE_NM,all_of(fields)),by=c("ICAO","PHASE","RANGE_NM"),
 suffix=c("_ORIGINAL","_ARCHIVED"),relationship="one-to-one")
for(f in fields) vintage[[paste0(f,"_DELTA")]]<-vintage[[paste0(f,"_ARCHIVED")]]-vintage[[paste0(f,"_ORIGINAL")]]
write_csv(vintage,file.path(out,"reference-source-vintage-comparison.csv"))
hotspots<-monthly |> filter(COHORT=="ARCHIVE_SNAPSHOT") |>
 select(ICAO,YEAR,MONTH,PHASE,RANGE_NM,N_REPORTED,N_VALID,N_MISSING_CROSSING,N_DATA_ATTENTION,N_MISS,
 PCT_MISSING_CROSSING,PCT_DATA_ATTENTION,PCT_MISS_VALID) |>
 group_by(ICAO,YEAR) |> slice_max(PCT_MISSING_CROSSING,n=1,with_ties=FALSE) |> ungroup()
write_csv(hotspots,file.path(out,"highest-monthly-crossing-gap.csv"))
result<-bind_rows(checks)
write_csv(result,file.path(out,"validation-checks.csv"))
print(result,width=Inf)
if(!all(result$PASS)) stop("Coverage validation failed; inspect validation-checks.csv.")
