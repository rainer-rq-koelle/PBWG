"""Independent CSV-level checks. Run with a Python environment containing pandas."""
import sys
from pathlib import Path
import pandas as pd
import numpy as np

out = Path(sys.argv[1])
root = Path(__file__).resolve().parents[1]
checks = []

def check(name, passed, detail=""):
    checks.append(dict(CHECK=name, PASS=bool(passed), DETAIL=detail))
    if not passed:
        raise AssertionError(name + ": " + detail)

coverage = pd.read_csv(out / "taxi-coverage-period.csv")
coverage = coverage[coverage.PERIOD == "AVAILABLE_YEAR"]
check("All taxi periods and variants", len(coverage) == 156)
check("Taxi counts partition", (coverage.N_VALID == coverage.N_MATCHED + coverage.N_MISS).all())
check("Taxi miss reasons partition", (coverage.N_MISS == coverage.N_MISS_UNIDENTIFIED + coverage.N_MISS_NO_GROUP + coverage.N_MISS_SPARSE).all())
check("Taxi diagnostics bounded", (coverage.N_VALID <= coverage.N_REPORTED).all())
for (year, variant), c in coverage.groupby(["YEAR", "REF_VARIANT"]):
    label = "2026p" if year == 2026 else str(year)
    d = pd.read_csv(out / "airport-taxi" / f"PBWG-EUR-txxt-analytic-{label}-ref2024-{variant}.csv")
    got = d.groupby(["ICAO", "PHASE"])[["MVTS", "TX_NA"]].sum()
    expected = c.set_index(["ICAO", "PHASE"])
    check(f"Taxi daily counts {year} {variant}", (got.MVTS == expected.N_VALID).all() and (got.TX_NA == expected.N_MISS).all())
    check(f"Taxi daily keys {year} {variant}", not d.duplicated(["ICAO", "PHASE", "DATE"]).any())
    check(f"Taxi both phases {year} {variant}", d.groupby("ICAO").PHASE.nunique().eq(2).all())
for f in (out / "taxi-reference").glob("*.csv"):
    r = pd.read_csv(f, keep_default_na=False)
    check(f"Reference support {f.name}", (r.IS_VALID_SAMPLE == (r.N >= 5)).all() and not r.duplicated(["ICAO", "PHASE", "RWY", "STND"]).any())

loads = pd.read_csv(out / "capacity-loads/EUR-bli-pli-2024-2026.csv")
for row in loads.itertuples():
    h = pd.read_csv(out / "hourly-throughput" / f"EUR-{row.ICAO}-hourly-throughput-{row.YEAR}.csv")
    if row.PERIOD == "JAN_SEP":
        h = h[h.BIN.str[5:7] <= "09"]
    check(f"Observed-hour denominator {row.ICAO} {row.YEAR} {row.PERIOD}", len(h) == row.N and not h.BIN.duplicated().any())
    if pd.isna(row.MAX_CAP):
        check(f"Unknown capacity stays unknown {row.ICAO} {row.YEAR} {row.PERIOD}", pd.isna(row.BLI) and pd.isna(row.PLI))
    else:
        b = (h.FLTS >= .2 * row.MAX_CAP).sum()
        p = (h.FLTS >= .8 * row.MAX_CAP).sum()
        check(f"Independent load calculation {row.ICAO} {row.YEAR} {row.PERIOD}", b == row.BLI_HRS and p == row.PLI_HRS and np.isclose(b / len(h), row.BLI) and np.isclose(p / len(h), row.PLI))

old_checks = []
for year in (2024, 2025):
    new = pd.read_csv(out / "airport-taxi" / f"PBWG-EUR-txxt-analytic-{year}-ref2024-icao_ganp_p20.csv")
    old = pd.read_csv(root.parent / "BRA-EUR-2025/data" / f"PBWG-EUR-txxt-analytic-{year}-ref2024-icao_ganp_p20.csv")
    new = new[new.ICAO == "LSZH"]
    old = old[old.ICAO == "LSZH"]
    z = new.merge(old, on=["ICAO", "PHASE", "DATE"], suffixes=("_NEW", "_OLD"), validate="one_to_one")
    same = len(z) == len(new) == len(old) and (z.MVTS - z.TX_NA == z.MVTS_VALID).all() and np.allclose(z.TOT_REF_NEW, z.TOT_REF_OLD, atol=1e-7, rtol=0) and np.allclose(z.TOT_ADD_TIME_NEW, z.TOT_ADD_TIME_OLD, atol=1e-7, rtol=0)
    check(f"Historical LSZH taxi {year}", same, "Matched counts and daily reference/additional-time sums; missing-count definitions differ.")
    old_checks.append(dict(YEAR=year, N_ROWS=len(z), PASS=same))
pd.DataFrame(old_checks).to_csv(out / "validation/historical-lszh-comparison.csv", index=False)

through = pd.read_csv(root / "outputs/pbwg-refresh-2026-20261102/apdf/airport-throughput/PBWG-EUR-thru-analytic-2026p.csv")
through["HOUR"] = pd.to_datetime(through.BIN, utc=True).dt.floor("h")
expected = through.groupby(["ICAO", "HOUR"])[["ARRS", "DEPS", "FLTS"]].sum().sort_index()
hourly = pd.concat([pd.read_csv(f) for f in (out / "hourly-throughput").glob("*-2026.csv")])
hourly["HOUR"] = pd.to_datetime(hourly.BIN, utc=True)
got = hourly.set_index(["ICAO", "HOUR"])[["ARRS", "DEPS", "FLTS"]].sort_index()
check("2026 hourly throughput agrees with validated 15-minute output", expected.equals(got))

pd.DataFrame(checks).to_csv(out / "validation/independent-checks.csv", index=False)
print(f"{len(checks)} independent checks passed.")
