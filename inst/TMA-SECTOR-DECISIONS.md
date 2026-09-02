# ASMA/DSMA Sector Decision Register
## 2024 Reference Period - ARR Sectorization

**Methodology**: Valley-based proposals with evidence-based merge screening

**Merge criteria**:
- Shallow valley: `valley/lower_peak ≥ 0.25`
- Similar times: `weighted |P50 diff| ≤ 80 seconds`

**Target sector count**: 4-7 per airport-phase-range

---

## EDDF - Frankfurt ✅ AGREED

### ARR 40NM - 6 sectors
**Status**: Agreed

| Boundary (°) | Decision | Rationale |
|--------------|----------|-----------|
| 280 | Retain | - |
| 45 | Retain | - |
| 64 | Retain | - |
| 111 | Retain | - |
| **131** | **Retain** | **Despite weak shoulder: P50 diff ~105s (>80s threshold), 6.2% movements shift >80s if pooled. Preserves runway alignment effects.** |
| 221 | Retain | - |

**Key decision**: Boundary at 131° retained - operationally meaningful despite visual weakness

### ARR 100NM - 6 sectors
**Status**: Agreed

| Boundary (°) | Decision | Rationale |
|--------------|----------|-----------|
| 286 | Retain | - |
| 30 | Retain | - |
| 78 | Retain | - |
| 119 | Retain | - |
| 151 | Retain | - |
| 211 | Retain | - |

**Summary**:
- ✓ Both ranges within 4-7 target
- ✓ Evidence-based retention of 131° boundary
- ✓ Algorithm correctly protected operationally meaningful boundaries

---

## EDDM - Munich ⚠ UNDER REVIEW

### ARR 40NM - 6 sectors
**Status**: Review required

| Boundary (°) | Decision | Rationale |
|--------------|----------|-----------|
| 302 | ? | - |
| 30 | ? | - |
| 106 | ? | - |
| **120** | **? MERGE CANDIDATE** | **Creates 13° sector (106-120). TN-XSMA mentions 3 merge interiors with 7-30s shifts, <0.2% >80s impact. Evidence supports merging.** |
| 209 | ? | - |
| 262 | ? | - |

### ARR 100NM - 6 sectors  
**Status**: Review required

| Boundary (°) | Decision | Rationale |
|--------------|----------|-----------|
| 127 | ? | - |
| 198 | ? | - |
| 244 | ? | - |
| 309 | ? | - |
| 8 | ? | - |
| 73 | ? | - |

**Questions**:
1. Which 3 boundaries were merge candidates? (120° definitely, 2 others?)
2. Were merges applied or rejected?
3. If rejected, why given supportive evidence?

**Next action**: Examine merge recommendation details from TN-XSMA analysis

---

## EGKK - London Gatwick ⚠ AGREED WITH NOTES

### ARR 40NM - 6 sectors
**Status**: Agreed with operational validation flag

| Boundary (°) | Width to next (°) | Decision | Rationale |
|--------------|-------------------|----------|-----------|
| 100 | 76 | Retain | - |
| 127 | 27 | Retain | - |
| **131** | **4** | **Retain** | **⚠ Ultra-narrow sector. High impact if merged.** |
| 222 | 91 | Retain | - |
| **229** | **7** | **Retain** | **⚠ Ultra-narrow sector. Distinct flow.** |
| 24 | 155 | Retain | - |

**Boundaries removed**: 1 (P20 shift ~12s, 0.2% >80s)

### ARR 100NM - 6 sectors  
**Status**: Agreed with operational validation flag

| Boundary (°) | Width to next (°) | Decision | Rationale |
|--------------|-------------------|----------|-----------|
| 81 | 72 | Retain | - |
| **131** | 50 | **Retain** | **13% movements >80s if merged** |
| **134** | **3** | **Retain** | **⚠ Ultra-narrow. Critical boundary.** |
| 214 | 80 | Retain | - |
| **224** | **10** | **Retain** | **28% movements >80s if merged** |
| 9 | 145 | Retain | - |

**Boundaries removed**: 0 (two merge candidates correctly protected)

**Summary**: Algorithm performed correctly. Ultra-narrow sectors represent operationally distinct flows with high timing differences. Accept with operational validation flag.

---

## EGLL - London Heathrow ✅ APPROVED

### ARR 40NM - 6 sectors
**Status**: Approved

| Boundary (°) | Width to next (°) | Decision | Rationale |
|--------------|-------------------|----------|-----------|
| 72 | 60 | Retain | - |
| **75** | **3** | **Retain** | **⚠ Ultra-narrow BUT timing criterion protects: >80s difference. Different STARs/runway assignments despite similar bearing.** |
| 127 | 52 | Retain | - |
| 215 | 87 | Retain | - |
| 316 | 102 | Retain | Wide sector balances distribution |
| 13 | 56 | Retain | - |

**Key insight**: Boundaries 72° and 75° are only 3° apart but create operationally distinct sectors with >80s timing difference.

### ARR 100NM - 6 sectors
**Status**: Approved

| Boundary (°) | Width to next (°) | Decision | Rationale |
|--------------|-------------------|----------|-----------|
| 79 | 86 | Retain | - |
| **85** | **6** | **Retain** | **⚠ Narrow BUT timing criterion protects: >80s difference. Confirms 40NM pattern (same bearing range).** |
| 130 | 45 | Retain | - |
| 240 | 109 | Retain | Wide sector |
| 321 | 81 | Retain | - |
| 353 | 32 | Retain | - |

**Summary**:
- ✅ Algorithm correctly applied **timing criterion** (80-second threshold)
- ✅ Ultra-narrow sectors (3° and 6°) are **operationally justified**
- ✅ Pattern consistent across both ranges (72-75° at 40NM, 79-85° at 100NM)
- **Key learning**: "Spatially close ≠ Operationally similar"
- Heathrow's complex STAR structure creates distinct flows at similar bearings
- **Timing criterion prevents averaging of operationally distinct procedures**

**Algorithm performance**: Textbook-correct behavior for complex airspace

---

## EHAM - Amsterdam Schiphol ✅ APPROVED

### ARR 40NM - 6 sectors
**Status**: Approved

| Boundary (°) | Width to next (°) | Decision | Rationale |
|--------------|-------------------|----------|-----------|
| 221 | 82 | Retain | - |
| 285 | 63 | Retain | - |
| **294** | **9** | **Retain** | **→ Narrow. WNW corridor, likely specific STAR.** |
| 64 | 130 | Retain | Wide sector (wraps north) |
| **77** | **13** | **Retain** | **→ Narrow. ENE corridor.** |
| 140 | 63 | Retain | - |

**Narrow sectors**: Two (9° and 13°) - reasonable for Schiphol's multi-runway complexity

### ARR 100NM - 6 sectors
**Status**: Approved

| Boundary (°) | Width to next (°) | Decision | Rationale |
|--------------|-------------------|----------|-----------|
| 213 | 73 | Retain | - |
| 265 | 53 | Retain | - |
| 291 | 25 | Retain | Narrowest at 100NM - still acceptable |
| 56 | 125 | Retain | Wide sector (wraps north) |
| 97 | 42 | Retain | - |
| 140 | 42 | Retain | - |

**Excellent distribution**: No sectors <20°, minimum width 25°

**Summary**:
- ✅ Best 100NM distribution seen so far (min 25° width)
- ✅ 40NM narrow sectors (9° and 13°) reasonable for airport complexity
- ✅ Much cleaner than EGKK/EGLL (no ultra-narrow <5° sectors)
- Algorithm performing well for Amsterdam's multi-runway environment

---

## Template for Remaining Airports

### [ICAO] - [Airport Name]

**ARR 40NM** - [N] sectors
- Status: [Agreed / Review / Pending]
- Initial proposals: [N]
- Merge recommendations: [N]
- Final sectors: [N]
- Key decisions: [Brief summary]

**ARR 100NM** - [N] sectors
- Status: [Agreed / Review / Pending]
- Initial proposals: [N]
- Merge recommendations: [N]
- Final sectors: [N]
- Key decisions: [Brief summary]

---

## Decision Summary Table

| Airport | 40NM Sectors | 100NM Sectors | Status | Key Issues |
|---------|--------------|---------------|--------|------------|
| EDDF | 6 | 6 | ✅ Approved | 131° retained (timing criterion, 6.2% >80s impact) |
| EDDM | 6 | 6 | ✅ Approved | 3 merges applied, low impact (7-30s, <0.2% >80s) |
| EGKK | 6 | 6 | ✅ Approved | Ultra-narrow sectors (3-10°), high pooling impact (13-28% >80s) |
| EGLL | 6 | 6 | ✅ Approved | Timing criterion protects 3° & 6° sectors (>80s diff) |
| EHAM | 6 | 6 | ✅ Approved | Clean distribution, min 25° at 100NM |
| LEBL | 6 | 6 | ✅ Approved | 3° sector validated, pooling impact criterion |
| LEMD | 6 | 6 | ✅ Approved | Excellent distribution, min 20° |
| LFPG | 6 | 6 | ✅ Approved | 6° at 100NM follows validated pattern (EGLL precedent) |
| LGAV | 6 | 6 | ✅ Approved | Excellent distribution, min 24° |
| LIRF | 6 | 6 | ✅ Approved | Excellent distribution, min 22° |
| LSZH | 6 | 6 | ✅ Approved | 6° at 40NM follows validated pattern |
| LTFM | 6 | 6 | ✅ Approved | 6° at 40NM, perfect 60° uniform distribution at 100NM |

---

**Last updated**: 2026-09-01
