# PBWG

`PBWG` is a tidyverse-oriented R package scaffold for preparing extracted data
used in international operational ANS performance benchmarking work.

The current first-phase workflow starts from files extracted from the data
warehouse and follows a simple pipeline:

`extract file -> read -> decode source columns -> harmonise -> prepare outputs`

The package currently includes:

- reading helpers for NM flight table and APDF extracts
- zipped NM flight table helpers driven by user-supplied paths
- a decode layer to map source-specific names to harmonised column names
- first-stage preparation helpers for network and airport data
- output file naming helpers
- a basic `testthat` setup
- a process guide skeleton for future operators and collaborators

The initial development focus is on Heathrow (`EGLL`) and Athens (`LGAV`).
