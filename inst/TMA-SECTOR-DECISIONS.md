# Approved TMA sector decisions

Approved definitions cover ARR and DEP at 40/100 NM for EDDF, EDDM, EGKK, EGLL,
EHAM, LEBL, LEMD, LFPG, LGAV, LIRF, LPPT, LSZH, and LTFM. The reference/review year
is 2024. Sector counts vary; the earlier blanket six-sector statement is obsolete.

## Authoritative files

- `extdata/arr-sectors-approved-2024.csv`: reviewed arrival boundaries.
- `dsma-sector-decisions-2024.csv`: departure register; APPROVED rows generate
  `extdata/dep-sectors-approved-2024.csv`.
- The corresponding `extdata/*.parquet` files are the package-readable copies.
- `TN-XSMA.qmd`: methodology and airport-level review evidence.
- `dsma-sector-review-2024.rds`: saved DSMA evidence used by the technical note.

`scripts/export-approved-tma-sectors.R` maintains the package-readable copies.
Ordinary refreshes read those copies; they do not regenerate decisions or propose
new boundaries.

Use `scripts/refresh-eur-data.R` for both reference variants and ASMA/DSMA outputs.
Sparse cells remain available with sample counts. Retired `-n5` references and
12-airport automatic proposals are not the approved production reference set.
See the [operator guide](process-guide/data-preparation-process.md).
