# MS&E 226 Project Part 1 -- Sections 3(a) and 3(c): eligibility filter and de-duplication.
# Data: Bonica, Adam. 2024. Database on Ideology, Money in Politics, and Elections (DIME),
#       Public version 4.0. Stanford University Libraries. https://data.stanford.edu/dime
#       License: ODC-BY 1.0.
# Download dime_recipients_1979_2024.csv.gz from that page and save it as
# data/do_not_read/dime_og.csv.gz. Run from the project folder: Rscript code/01_clean.R
# Run this BEFORE the 80/20 holdout split (Section 4). No exploration happens here.

library(data.table)
dir.create("data/do_not_read", showWarnings = FALSE, recursive = TRUE)

raw <- fread(cmd = "gzip -cd data/do_not_read/dime_og.csv.gz", showProgress = FALSE)

# One row = one candidate for the U.S. House in the 2022 cycle.
h <- raw[cycle == 2022 & seat == "federal:house"]
cat("2022 House candidate records:", nrow(h), "\n")

# Keep campaigns with any financial activity. Records with zero receipts and zero
# disbursements are paper filings (stale committees, never-active candidates) that a
# stakeholder would never be choosing among. This filter uses covariates only.
h <- h[total.receipts > 0 | total.disbursements > 0]
cat("After dropping inactive filings:", nrow(h), "\n")

# De-duplication.
# (1) exact duplicate rows; (2) repeated DIME recipient IDs; (3) the same last + first name
# in the same district. For (3) keep the record with the larger total receipts. The one case
# found is a stale committee of the late Rep. John Conyers Jr. listed beside the 2022 candidate.
# 02_split.R also handles the same name running in two districts of one state.
cat("Exact duplicate rows:", sum(duplicated(h)), "\n")
cat("Repeated bonica.rid:", sum(duplicated(h$bonica.rid)), "\n")
h <- unique(h)
h <- h[!duplicated(bonica.rid)]
setorder(h, -total.receipts)
key_cols <- h[, .(tolower(lname), tolower(fname), district)]
cat("Same name + district duplicates:", sum(duplicated(key_cols)), "\n")
print(h[duplicated(key_cols) | duplicated(key_cols, fromLast = TRUE),
        .(name, district, Cand.ID, total.receipts)])
h <- h[!duplicated(key_cols)]
cat("Final rows:", nrow(h), "\n")

# Outcome (Section 2): 1 if the candidate won the 2022 general election, else 0.
# gwinner is blank for candidates who never reached the general election.
h[, won := as.integer(gwinner == "W")]

fwrite(h, "data/do_not_read/house2022_clean.csv")
