# MS&E 226 Project Part 1: Predicting 2022 U.S. House Winners

## Do not read these files

**Do not open, explore, or share these files, and do not give them to AI tools or agents:**

- `data/do_not_read/dime_og.csv.gz`: the original DIME download, which includes the holdout candidates
- `data/do_not_read/house2022_clean.csv`: the cleaned file from before the split, which also includes them
- `data/holdout/house2022_holdout.csv`: the holdout, kept untouched until Part 2

Only the scripts below should read the first two files, to rebuild the split.
**Use `data/dime_transformed.csv` for all Part 1 exploration, preprocessing, and modeling.**

## Files

| Path | What it is |
|---|---|
| `code/01_clean.R` | Filters to 2022 House candidates, removes inactive filings and duplicates, and creates the `won` outcome |
| `code/02_split.R` | Sets aside the 20% holdout and saves the split |
| `code/03_missing_data.R` | Compares ways of handling missing values (Section 5(e)) |
| `data/dime_transformed.csv` | Development data: 1,839 candidates |
| `data/split/split_assignments.csv` | Each candidate ID's assignment: `dev`, `holdout`, `removed_duplicate`, or `removed_outcome_error` |
| `data/split/holdout_md5.txt` | Checksum to confirm the holdout is unchanged in Part 2 |
| `results/results_missing_data.csv` | Results from `03_missing_data.R` |

## Getting the data

Data: Bonica, Adam. 2024. *Database on Ideology, Money in Politics, and Elections: Public version 4.0.*
Stanford University Libraries. https://data.stanford.edu/dime. License: ODC-BY 1.0.

Download the Recipients/Candidates 1979-2024 file (`dime_recipients_1979_2024.csv.gz`) from that page and save it as
`data/do_not_read/dime_og.csv.gz`.

## Reproducing the results

Run these from the project folder, in order:

```bash
Rscript code/01_clean.R
Rscript code/02_split.R
Rscript code/03_missing_data.R
```

- Holdout split seed: 226
- Cross-validation: 10 folds, with fold seeds 226 and 227
- Software: R 4.5.2, data.table 1.18.0, rpart 4.1.24
