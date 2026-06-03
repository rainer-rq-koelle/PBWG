# Contributor-Facing Output Names

Context: the `international-PBWG-2026` project will use the PBWG package and
related local outputs as preparation workflows, but contributors should receive
and submit harmonised analytical files directly in the project exchange format.

## Principle

The PBWG package should be able to write contributor-facing harmonised outputs
directly. The report/application project should not depend on a separate
post-processing script that renames package outputs after they are produced.

## Current Contributor-Facing Convention

First-batch exchange files use:

- REGION file key: `EUR`, `USA`, `CAN`, `BRA`, `CHN`, `SIN`, `THA`, `JPN`.
- Daily analytical granularity by default.
- `DATE` in `YYYY-MM-DD` format.
- Machine-readable CSV/TXT by default, parquet acceptable for larger data.
- Integer counts/source quantities rather than pre-calculated percentages.
- File naming pattern: `{REGION}-{dataset}-{years}.csv`.

First-batch dataset families:

- `{REGION}-network-traffic-{years}.csv`
- `{REGION}-airport-traffic-{years}.csv`
- `{REGION}-airport-punc-{years}.csv`
- `{REGION}-airport-thru-{years}.csv`
- `{REGION}-airport-meta-{years}.csv`

## Package Alignment To Consider

The package currently writes many useful canonical analytical files, but some
file names are still package/internal or historical:

- Prefixes such as `PBWG-{REGION}-...`.
- Dataset slugs such as `tfc`, `punc`, and `thru-analytic`.
- Package output columns such as `REG` where contributor-facing files may rely
  on the REGION encoded in the file name.
- WTC columns may appear as `H`, `M`, `L`, while the contributor-facing request
  asks for `HEAVY`, `MED`, `LIGHT`.

The package should support options to produce the exchange names directly, for
example:

- optional filename prefix, defaulting to no prefix for contributor-facing
  exchange outputs;
- optional filename postfix/variant suffix where analytical variants must be
  explicit;
- explicit dataset slug choices for exchange outputs versus internal/debug
  outputs;
- column-name normalisation for exchange outputs where needed.

This keeps the package as the place where harmonisation happens, rather than
moving harmonisation into project-specific intake/renaming scripts.
