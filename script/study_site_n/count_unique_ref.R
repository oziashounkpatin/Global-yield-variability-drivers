# ===== Deduplicate references by normalized title (and DOI if present) =====
# Packages -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(stringr)
})

# ---- Configuration ----------------------------------------------------------
input_path  <- "./input/data/all_dis_cov2_last.xlsx"                 # <- change if needed
sheet       <- 1                             # sheet index or name
# Try these column names (case-insensitive). If not found, we pick a heuristic.
title_candidates <- c("references", "title", "article title", "document title")
doi_candidates   <- c("doi", "document object identifier")

out_dedup  <- "references_deduplicated1.xlsx"
out_dupes  <- "duplicates_report1.xlsx"
out_groups <- "duplicate_groups_summary1.xlsx"

# ---- Helpers ----------------------------------------------------------------
normalize_title <- function(x) {
  if (is.null(x)) return(character())
  x <- tolower(trimws(as.character(x)))
  x <- str_replace_all(x, "\\s+", " ")  # collapse spaces/tabs/newlines
  x <- str_replace_all(
    x,
    "[\\.,:;!?\"'’”“`´\\-–—_/\\\\\\(\\)\\[\\]{}<>*^~#\\|+•…]",
    ""                                    # drop punctuation & odd dashes/quotes
  )
  x <- trimws(str_replace_all(x, "\\s+", " "))
  x
}

normalize_doi <- function(x) {
  if (is.null(x)) return(character())
  x <- tolower(trimws(as.character(x)))
  x <- str_remove(x, "^https?://doi\\.org/")
  x <- str_remove(x, "^doi:\\s*")
  trimws(x)
}

pick_col <- function(cols, candidates, fallback_contains) {
  # direct match (case-insensitive)
  low <- tolower(cols)
  hit <- match(tolower(candidates), low, nomatch = 0)
  if (any(hit > 0)) return(cols[hit[hit > 0][1]])
  # heuristic: first column whose name contains fallback string
  w <- grepl(fallback_contains, low, fixed = TRUE)
  if (any(w)) return(cols[which(w)[1]])
  # final fallback: first column
  cols[1]
}

# ---- Read data --------------------------------------------------------------
df <- read_excel(input_path, sheet = sheet) %>%
    filter(!Crop_Group %in% c("Grass"))%>%
                  drop_na(Crop_Group)
cols <- names(df)

title_col <- pick_col(cols, title_candidates, "title")
doi_col   <- pick_col(cols, doi_candidates,   "doi")
if (!"doi" %in% tolower(doi_col)) doi_col <- NA_character_  # no real DOI column

message("Using columns -> Title: '", title_col, "' | DOI: ",
        ifelse(is.na(doi_col), "(none)", paste0("'", doi_col, "'")))

# ---- Build normalized keys (base indexing; no `.data`) ----------------------
norm_title <- normalize_title(df[[title_col]])
norm_doi   <- if (!is.na(doi_col)) normalize_doi(df[[doi_col]]) else rep("", nrow(df))

dedupe_key <- ifelse(nchar(norm_doi) > 0, norm_doi, norm_title)

# ---- Deduplicate & reports --------------------------------------------------
original_rows <- nrow(df)

# keep first occurrence of each key
keep_idx <- !duplicated(dedupe_key)
dedup_df <- df[keep_idx, , drop = FALSE]

# all duplicates (any key with freq > 1)
dupe_flag <- duplicated(dedupe_key) | duplicated(dedupe_key, fromLast = TRUE)
dupes_df  <- cbind(df, .dedupe_key = dedupe_key)[dupe_flag, , drop = FALSE]
dupes_df  <- dupes_df[order(dupes_df$.dedupe_key), , drop = FALSE]

# group summary
group_tab <- as.data.frame(table(dedupe_key), stringsAsFactors = FALSE)
group_tab <- group_tab[group_tab$Freq > 1, , drop = FALSE]
group_tab <- group_tab[order(-group_tab$Freq), , drop = FALSE]
if (nrow(group_tab) > 0) {
  # add a readable example reference for each group
  # match returns first index of each key in the original df
  ex_idx <- match(group_tab$dedupe_key, dedupe_key)
  group_tab$example_reference <- df[[title_col]][ex_idx]
  group_summary <- group_tab[, c("Freq", "example_reference")]
  names(group_summary) <- c("count", "example_reference")
} else {
  group_summary <- data.frame(count = integer(), example_reference = character())
}

# ---- Write files ------------------------------------------------------------
write_xlsx(dedup_df, out_dedup)
if (nrow(dupes_df) > 0) write_xlsx(dupes_df, out_dupes)
if (nrow(group_summary) > 0) write_xlsx(group_summary, out_groups)

# ---- Print summary ----------------------------------------------------------
cat(
  "\n===== Dedup Summary =====\n",
  "Total rows: ", original_rows, "\n",
  "Unique references: ", nrow(dedup_df), "\n",
  "Duplicates removed: ", original_rows - nrow(dedup_df), "\n",
  "Title column: ", title_col, "\n",
  "DOI column: ", ifelse(is.na(doi_col), "(none)", doi_col), "\n",
  "Outputs: \n  - ", out_dedup,
  if (nrow(dupes_df) > 0) paste0("\n  - ", out_dupes) else "",
  if (nrow(group_summary) > 0) paste0("\n  - ", out_groups) else "",
  "\n==========================\n",
  sep = ""
)
