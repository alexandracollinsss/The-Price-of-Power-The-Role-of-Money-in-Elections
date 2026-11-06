# MS&E 226 Project Part 1 -- Section 4: holdout set.
# Randomly sets aside 20% of the de-duplicated data as a holdout and saves the split.
# Run after 01_clean.R and before any exploration, preprocessing, or modeling.
# This script prints only sample sizes, never holdout values.

library(data.table)

SPLIT_SEED <- 226
HOLDOUT_FRACTION <- 0.20

h <- fread("data/do_not_read/house2022_clean.csv", showProgress = FALSE)

# Sort by ID first so the split does not depend on row order in the input file.
setorder(h, bonica.rid)

set.seed(SPLIT_SEED)
n_holdout <- round(HOLDOUT_FRACTION * nrow(h))
holdout_rows <- sample(nrow(h), n_holdout)

holdout <- h[holdout_rows]
dev <- h[-holdout_rows]

# Candidates sharing a last name, first name, and state may be one person filed in two
# districts. An audit found these after the split, so they are resolved in a way that keeps
# the split and the holdout file unchanged: a development record matching a holdout record is
# removed from development; among development records the one with larger receipts is kept.
name_key <- function(x) paste(tolower(trimws(x$lname)), tolower(trimws(x$fname)), x$state)
n_dev_before <- nrow(dev)
all_dev_ids <- dev$bonica.rid
dev <- dev[!name_key(dev) %in% name_key(holdout)]
setorder(dev, -total.receipts)
dev <- dev[!duplicated(name_key(dev))]
removed_ids <- setdiff(all_dev_ids, dev$bonica.rid)
setorder(dev, bonica.rid)
cat("Possible duplicates removed from development:", n_dev_before - nrow(dev), "\n")

# Records DIME marks as general-election winners although someone else won the seat; they
# appear to be record-linkage errors. Found during modeling, so removed from development only.
LABEL_ERROR_IDS <- c("cand153090",  # Luke Letlow, LA05 (winner: Julia Letlow)
                     "cand155886",  # Karen Carter Peterson, LA02 (winner: Troy Carter)
                     "cand153606",  # Lee Griffin, PA12 (winner: Summer Lee)
                     "cand212491")  # Lindsey Williams, NY13 (winner: Adriano Espaillat)
stopifnot(all(LABEL_ERROR_IDS %in% dev$bonica.rid))
dev <- dev[!bonica.rid %in% LABEL_ERROR_IDS]
cat("Outcome errors removed from development:", length(LABEL_ERROR_IDS), "\n")

stopifnot(nrow(dev) + nrow(holdout) + length(removed_ids) + length(LABEL_ERROR_IDS) == nrow(h),
          length(intersect(dev$bonica.rid, holdout$bonica.rid)) == 0,
          !any(name_key(dev) %in% name_key(holdout)))

dir.create("data/holdout", showWarnings = FALSE)
dir.create("data/split", showWarnings = FALSE)
fwrite(dev, "data/dime_transformed.csv")
# The holdout is saved read-only below, so allow a rerun to replace it.
if (file.exists("data/holdout/house2022_holdout.csv")) {
  Sys.chmod("data/holdout/house2022_holdout.csv", mode = "0600")
}
fwrite(holdout, "data/holdout/house2022_holdout.csv")

# Save the split itself, plus a checksum to show in Part 2 that the holdout is unchanged.
fwrite(data.table(bonica.rid = h$bonica.rid,
                  split = ifelse(seq_len(nrow(h)) %in% holdout_rows, "holdout",
                                ifelse(h$bonica.rid %in% removed_ids, "removed_duplicate",
                                       ifelse(h$bonica.rid %in% LABEL_ERROR_IDS,
                                              "removed_outcome_error", "dev")))),
       "data/split/split_assignments.csv")
checksum <- unname(tools::md5sum("data/holdout/house2022_holdout.csv"))
writeLines(checksum, "data/split/holdout_md5.txt")

# Read-only, so a stray script cannot overwrite it.
Sys.chmod("data/holdout/house2022_holdout.csv", mode = "0400")

cat("R version:", R.version.string, "\n")
cat("Split seed:", SPLIT_SEED, "\n")
cat("Total rows:", nrow(h), "\n")
cat("Development rows:", nrow(dev), "\n")
cat("Holdout rows:", nrow(holdout), "\n")
cat("Holdout md5:", checksum, "\n")
