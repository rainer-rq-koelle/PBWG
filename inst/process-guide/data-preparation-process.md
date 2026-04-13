# Data Preparation Process Guide

## Purpose

This guide is for the person running or updating the PBWG data preparation
workflow. It complements the package documentation with a practical view of how
to move from data warehouse extracts to project-ready output files.

## Scope

The initial workflow starts from files extracted outside the package from the
current data warehouse. A later phase may replace the file interface with SQL
queries or data platform connectors without changing the downstream preparation
logic.

## Core Input Sources

- NM flight table extracts for network data products
- APDF extracts for airport-focused data products

## Current Development Focus

- Aerodromes: `EGLL` and `LGAV`
- Initial workflow: read extract, decode source fields, harmonise structure,
  apply project-specific preparation, write output files

## Implemented Outputs

The package currently supports these output families:

- NM network traffic
- APDF airport traffic
- APDF punctuality
- APDF throughput / capacity analytics

The next planned airport-focused outputs are:

- taxi-in and taxi-out additional time
- arrival additional time in terminal airspace (`ASMA`)
- departure additional time in terminal airspace (`DSMA`)

## Naming Principles

- Tibble column names are kept in upper case, for example `ADEP`, `ADES`,
  `PHASE`
- R objects use lower case names, for example `nm_flights`, `apdf`,
  `reference_times`
- Output file names should encode the product family, relevant year or year
  range, and optional variant information

## Canonical and Derived Files

The package follows a two-stage output strategy:

- canonical annual files are written first
- project files are derived by combining the canonical files that are needed

This reduces reprocessing and makes it easier to handle changing airport sets
across projects.

Current naming patterns include:

- network annual: `PBWG-EUR-network-traffic-YYYY.csv`
- network multi-year: `PBWG-EUR-network-traffic-YYYY-YYYY.csv`
- airport traffic annual: `PBWG-EUR-ICAO-tfc-YYYY.csv`
- airport traffic multi-year: `PBWG-EUR-ICAO-tfc-YYYY-YYYY.csv`
- punctuality annual: `PBWG-EUR-ICAO-punc-YYYY.csv`
- punctuality project aggregate: `PBWG-EUR-punc-YYYY.csv`
- throughput annual: `PBWG-EUR-ICAO-thru-analytic-YYYY.csv`
- throughput project aggregate: `PBWG-EUR-thru-analytic-YYYY.csv`

## Data Dictionary Layer

The first explicit transformation step after reading an extract is to map
warehouse-specific field names to harmonised package field names. This serves
two purposes:

- stable downstream processing functions
- a transparent data dictionary for discussion with project groups

## Recommended Workflow

1. Confirm the source extract version, coverage period, and aerodromes.
2. Read the extracted files into R.
3. Decode source column names to harmonised names.
4. Build canonical annual output files for the relevant product family.
5. Validate row counts, date ranges, and key fields.
6. Combine canonical files into the project-level outputs that are needed.
7. Record noteworthy assumptions, anomalies, or manual decisions.

## Operational Notes

- Do not store credentials, connection strings, or secrets in the repository.
- Keep raw operational data outside the package repository.
- Use small representative sample data for tests and examples.
- Use larger local-only datasets for full development and validation runs.

## To Be Expanded

- input file inventory and expected formats
- agreed harmonised field definitions
- quality checks by product family
- reference-time dataset versioning and naming rules
- project-specific output inventories
- examples of canonical-to-project build chains
