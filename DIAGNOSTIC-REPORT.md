# Diagnostic Report - PBWG Development Environment
**Date:** 2026-09-02  
**Machine:** Current Windows development machine

## Executive Summary

**Status:** Partial functionality - R scripts work, Quarto rendering fails  
**Root Cause:** Arrow package memory-mapped file access issues  
**Impact:** Cannot generate HTML documentation, but can process data and run R scripts

---

## What WORKS ✅

### 1. R Package Development
- ✅ `devtools::load_all()` works
- ✅ Package functions load correctly
- ✅ All package dependencies available
- ✅ Git operations work (commit, push)

### 2. Data Access
- ✅ Can read APDF parquet files from zip archives
- ✅ Can process millions of rows (1.2M+ DEP movements tested)
- ✅ Arrow `read_parquet()` works in R scripts
- ✅ Data in both locations accessible:
  - `../xx-test-gotcha/data/apdf-annual/` ✓
  - Network drives (EUROCONTROL paths) - NOT TESTED

### 3. Algorithm Execution
- ✅ `prepare_tma_bearing_density()` works
- ✅ `identify_tma_bearing_extrema()` works
- ✅ `propose_tma_sector_definitions()` works
- ✅ Successfully processed EDDF, EDDM, EGLL for both ARR and DEP
- ✅ Sector proposals generated correctly

### 4. Interactive R Execution
- ✅ `Rscript` command works
- ✅ R console output displays correctly
- ✅ Scripts can read/write files
- ✅ Scripts can create plots (not tested but likely works)

---

## What FAILS ❌

### 1. Quarto Rendering (CRITICAL)
**Error Pattern:**
```
Error in arrow:::io___MemoryMappedFile__Open(path, mode)
IOError: Failed to open local file '...' Detail: [Windows error 2] 
The system cannot find the file specified.
```

**Affected:**
- ❌ `quarto render inst/TN-XSMA.qmd`
- ❌ `quarto render inst/DSMA-PREVIEW.qmd`

**Where it fails:**
- EGLL arrival bearings section (line 73-103 in TN-XSMA.qmd)
- Data preparation chunks in DSMA-PREVIEW.qmd

**Analysis:**
- NOT a data access issue (same files work in Rscript)
- NOT a permissions issue (files are readable)
- LIKELY: Quarto's R session has different file path handling
- LIKELY: Temporary directory extraction not working in Quarto environment

**Why this matters:**
- Cannot generate HTML documentation for visual review
- Cannot share rendered reports
- User cannot judge DSMA results visually

### 2. Batch Processing Edge Cases
**Symptom:** Some batch scripts fail inconsistently

**Example failures:**
- `dsma-all-airports-final.R` - data frame creation issues
- Earlier batch attempts - variable scoping issues

**Analysis:**
- NOT R crashes or segmentation faults
- ARE script logic errors (NULL values, wrong variable scope)
- FIXABLE with better error handling

**Why this matters:**
- Slows down processing all 12 airports
- Requires manual intervention

---

## What's UNCERTAIN ⚠️

### 1. Network Drive Access
**Status:** Not tested in this session  
**Paths:** `\\sky.corp.eurocontrol.int\DFSRoot\Groups\...`

**Test needed:**
```r
# Can we read/write to network locations?
test_path <- "\\\\sky.corp.eurocontrol.int\\DFSRoot\\Groups\\HQ\\dgof-pru\\Data\\DataProcessing\\ICAO_Template\\2026 - ICAO - template\\prefil"
dir.exists(test_path)
list.files(test_path)
```

### 2. Large Dataset Processing
**Tested:** 1.2M rows successfully  
**Not tested:** All 12 airports simultaneously (4.4M rows)

**Risk:** May hit memory limits with full batch

### 3. Plot Generation
**Status:** Not tested in this session  
**Expected:** Should work (ggplot2 available)

---

## Root Cause Analysis

### The Arrow/Quarto Issue

**Technical details:**
1. Quarto runs R code in a subprocess
2. Subprocess may have different working directory
3. `tempfile()` and `tempdir()` behave differently in subprocess
4. `unzip(..., exdir = dirname(tempfile()))` creates paths that don't persist
5. Arrow tries to memory-map files that no longer exist at expected paths

**Evidence:**
- Same code works in `Rscript` (direct R execution)
- Same code fails in Quarto (subprocess R execution)
- Error is specifically about file paths, not file content
- Error occurs at arrow memory-mapping stage

**Why it's hard to fix:**
- Quarto environment is isolated
- File extraction timing issues
- Can't easily debug subprocess environment

---

## Remedial Plan

### Immediate Actions (Do Now)

#### 1. Test Network Drive Access (2 minutes)
```r
# Run this in R console
test_paths <- c(
  "\\\\sky.corp.eurocontrol.int\\DFSRoot\\Groups\\HQ\\dgof-pru\\Data\\DataProcessing\\ICAO_Template\\2026 - ICAO - template\\prefil",
  "C:\\Users\\rkoelle\\EUROCONTROL\\ANS Performance Benchmarking - data-2026\\PBWG-data"
)

for (path in test_paths) {
  exists <- dir.exists(path)
  if (exists) {
    cat(sprintf("✓ %s - %d files\n", path, length(list.files(path))))
  } else {
    cat(sprintf("✗ %s - NOT ACCESSIBLE\n", path))
  }
}
```

#### 2. Extract Data Permanently (10 minutes)
Instead of extracting from zip each time:
```bash
# Extract all 2024 APDF to a permanent location
mkdir -p ~/PBWG-data/apdf-2024
cd ~/PBWG-data/apdf-2024
unzip ../../xx-test-gotcha/data/apdf-annual/apdf-2024.zip
```

Then update scripts to read from this permanent location. This will:
- Fix Quarto rendering (no tempfile issues)
- Speed up repeated processing
- More reliable

#### 3. Create Simple Reference Table Script (5 minutes)
- Use approved ARR sector definitions
- Process ASMA references only (DEP pending)
- Save to PBWG-data
- This is KNOWN to work (no Quarto needed)

### Short-term Fixes (This Week)

#### 1. Alternative Documentation Approach
**Instead of Quarto:**
- Run R scripts directly
- Save plots as PNG files
- Create simple markdown with embedded images
- Use R Markdown instead of Quarto (may work better)

**Or:**
- Generate results on other machine (as you mentioned)
- Transfer plots/tables back here for documentation

#### 2. Sequential DSMA Processing
**Instead of batch:**
- Process airports one at a time
- Save intermediate results
- Combine at the end
- More reliable, easier to debug

#### 3. Sector Definition Export
**What you need:**
- Export approved ARR sectors to CSV/parquet
- Save to PBWG-data for other machines
- Document format clearly

### Long-term Solutions (Future)

#### 1. Docker/Container Environment
- Standardized R environment
- Consistent file paths
- No Quarto subprocess issues
- Reproducible builds

#### 2. Separate Data Processing from Documentation
- Process data → save results → document results
- Never process in documentation (Quarto/Rmd)
- Documentation just loads pre-computed results

#### 3. CI/CD Pipeline
- Automated testing
- Catch errors early
- Version-controlled outputs

---

## Recommended Next Steps (Priority Order)

### Priority 1: Enable Other Machine
**Goal:** Transfer approved ARR sectors and scripts to working machine

**Actions:**
1. Commit current scripts to GitHub ✓ (already done)
2. Export approved ARR sector definitions:
   ```r
   arr_sectors <- read_approved_tma_sector_definitions(phase = "ARR")
   write_parquet(arr_sectors, "arr-sectors-approved.parquet")
   write.csv(arr_sectors, "arr-sectors-approved.csv", row.names = FALSE)
   ```
3. Push to PBWG-data repository
4. Process DSMA on other machine
5. Transfer results back

### Priority 2: Test This Machine's Capabilities
**Goal:** Understand exactly what works

**Actions:**
1. Test network drive access (2 min)
2. Extract APDF data permanently (10 min)
3. Retry Quarto with permanent data paths (5 min)
4. Generate ASMA reference tables (15 min)

### Priority 3: Documentation Workaround
**Goal:** Get DSMA results visible

**Actions:**
1. Process DSMA on working machine
2. Generate plots as PNG
3. Create simple markdown documentation
4. Commit plots + markdown to GitHub

---

## Machine Capability Assessment

**This machine CAN:**
- ✅ Develop R package code
- ✅ Process data with R scripts
- ✅ Run algorithms and generate results
- ✅ Export data to CSV/parquet
- ✅ Git operations
- ✅ Generate reference tables (likely)

**This machine CANNOT:**
- ❌ Render Quarto documents reliably
- ❌ Generate HTML documentation automatically
- ⚠️ Process large batches reliably (needs testing)

**Verdict:** Useful for development, not ideal for production processing

---

## Questions to Answer

1. **Network drives:** Can this machine access EUROCONTROL network shares?
2. **PBWG-data location:** Where should reference tables be saved? Local or network?
3. **Other machine setup:** What's different about the working machine's environment?
4. **Workflow preference:** Process here + document elsewhere, or all on other machine?

---

## Files to Transfer (if using other machine)

**Essential:**
- ✅ `scripts/dsma-*` scripts (already on GitHub)
- ⚠️ Approved ARR sector definitions (need to export)
- ⚠️ APDF 2024 data (4.6GB - already have on other machine?)
- ⚠️ Package code (already on GitHub via git)

**Nice to have:**
- TN-XSMA.qmd source
- Decision tracking documents
- Validation scripts
