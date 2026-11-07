# MS&E 226 Project Part 1: Predicting 2022 U.S. House Winners

## The holdout is not in this repository

The holdout, the original DIME download, and the cleaned file from before the split all contain holdout
candidates. They are kept outside this repository and outside every folder an AI tool works in.
**Do not open, explore, or share them, and do not give them to AI tools or agents.**

**Use `data/dime_transformed.csv` for all Part 1 exploration, preprocessing, and modeling.**

In Part 2, one team member runs the saved pipeline on the holdout once, with no AI tool open in that
folder, after checking that the holdout's MD5 checksum matches `data/split/holdout_md5.txt`. Only summary
metrics are shared.

## Files

| Path | What it is |
|---|---|
| `code/01_clean.R` | Filters to 2022 House candidates, removes inactive filings and duplicates, and creates the `won` outcome (Section 3) |
| `code/02_split.R` | Sets aside the 20% holdout and saves the split (Section 4) |
| `code/03_missing_data.R` | Compares ways of handling missing values (Section 5(e)) |
| `code/05_exploration` | Exploratory analysis of which predictors are associated with winning (Section 5) |
| `code/pipeline_functions.R` | Feature building, preprocessing, and model fitting used by `04_prediction.R` |
| `code/04_prediction.R` | Trains, tunes, and compares the prediction models with cross-validation, and saves the final pipeline (Section 6) |
| `code/06_prediction_figures.R` | Makes Figure 6.1 from the saved Section 6 results |
| `data/dime_transformed.csv` | Development data: 1,839 candidates |
| `data/split/split_assignments.csv` | Each candidate ID's assignment: `dev`, `holdout`, `removed_duplicate`, or `removed_outcome_error` |
| `data/split/holdout_md5.txt` | Checksum to confirm the holdout is unchanged in Part 2 |
| `results/results_missing_data.csv` | Results from `03_missing_data.R` |
| `results/*_prediction*`, `results/fig_*` | Results and figures from `04_prediction.R` and `06_prediction_figures.R` |
| `results/final_pipeline.rds` | Saved final pipeline, to be run once on the holdout in Part 2 |
| `results/full_cycle_benchmark_pipeline.rds` | Saved benchmark pipeline that uses full-cycle fundraising (retrospective only) |

## Getting the data

Data: Bonica, Adam. 2024. *Database on Ideology, Money in Politics, and Elections: Public version 4.0.*
Stanford University Libraries. https://data.stanford.edu/dime. License: ODC-BY 1.0.

Download the Recipients/Candidates 1979-2024 file (`dime_recipients_1979_2024.csv.gz`) from that page and save it as
`data/do_not_read/dime_og.csv.gz`.

## Reproducing the results

The development data and saved split are already in the repository, so reproducing the analysis only
needs these scripts. Run them from the project folder, in this order:

```bash
Rscript code/03_missing_data.R
Rscript code/05_exploration
Rscript code/04_prediction.R
Rscript code/06_prediction_figures.R
```

`05_exploration` writes its figures and tables to the project folder; the other scripts write to `results/`.

`01_clean.R` and `02_split.R` rebuild the development data from the original DIME file. They
also recreate the holdout in `data/holdout/`, so only run them if necessary, and move the holdout and
`data/do_not_read/` back out of the project afterward.

- Holdout split seed: 226
- Cross-validation: 10 folds, with fold seed 226 for model selection and 227 as a robustness check
- Software: R 4.5.2 (data preparation and missing-data comparison) and R 4.4.3 (prediction);
  data.table 1.18, rpart 4.1, glmnet 4.1-10, ranger 0.18.0. `05_exploration` uses base R only.
