# PBWG Metric Output Schema

This note defines the standard column contract for daily and derived
additional-time metric outputs. It applies to taxi-in/taxi-out and should also
be used for ASMA/DSMA outputs when those pipelines are promoted into the
package.

## Count Columns

- `MVTS_VALID`: movements that enter the metric calculation. These movements
  have the required observed-time inputs and a valid reference value for the
  relevant grouping.
- `MVTS_NA`: movements that cannot enter the metric calculation because a
  required observed-time input is invalid or missing, or because no valid
  reference value is available.

The total phase movement count represented in the analytic file is:

```r
MVTS_TOTAL <- MVTS_VALID + MVTS_NA
```

Large or changing `MVTS_NA` values should be treated as a data-quality signal.
They can indicate missing input fields, changed operational patterns, or a
reference dataset that needs to be augmented.

## Time Columns

Observed, reference, and additional-time totals are calculated over
`MVTS_VALID`.

- `TOT_TXXT`: observed taxi time for taxi-in/taxi-out outputs.
- `TOT_ASMA_TIME`: observed ASMA time for arrival terminal-airspace outputs.
- `TOT_DSMA_TIME`: observed DSMA time for departure terminal-airspace outputs.
- `TOT_REF`: reference time for the metric-ready sample.
- `TOT_ADD_TIME`: additional time for the metric-ready sample.

The standard averages are:

```r
AVG_OBS_TIME <- TOT_<METRIC>_TIME / MVTS_VALID
AVG_REF_TIME <- TOT_REF / MVTS_VALID
AVG_ADD_TIME <- TOT_ADD_TIME / MVTS_VALID
```

For taxi-in/taxi-out, `TOT_TXXT` is the observed-time column and replaces the
generic `TOT_<METRIC>_TIME` placeholder.

## Grouping Columns

All metric outputs should keep the grouping fields required to interpret the
aggregation. Daily airport-level outputs should include at least:

- `ICAO`
- `PHASE`
- `DATE`

More detailed ASMA/DSMA or taxi outputs may add grouping fields such as
`DIST_NM`, `SECTOR`, `RWY`, `STND`, `CLASS`, or reference metadata. The count
and time-column semantics above remain unchanged at each grouping level.
