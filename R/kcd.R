#' Reshape a wide clean master to one row per diagnosis code
#'
#' Melts the diagnosis columns (`kcd0..kcd4`) to long form: one row per diagnosis
#' code, carrying every other column. `sub_kcd` is `0` for the main diagnosis
#' (`kcd0`) and `1` for sub-diagnoses.
#'
#' @param clean A cleansed wide master from [clean_icis()].
#' @param kcd_cols Character vector of diagnosis-code column names
#'   (default `kcd0..kcd4`).
#' @return A long `data.table` with columns `kcd`, `sub_kcd`, and the carried
#'   claim columns.
#' @seealso [map_disease()].
#' @export
melt_kcd <- function(clean, kcd_cols = paste0("kcd", 0:4)) {
  clean <- as.data.table(clean)
  carry <- setdiff(names(clean), kcd_cols)
  long <- melt(clean, id.vars = carry, measure.vars = kcd_cols,
               variable.name = "ord", value.name = "kcd", na.rm = TRUE)
  long[, sub_kcd := as.integer(as.integer(sub("kcd", "", ord)) > 0L)]   # 0 = main
  long[, ord := NULL]
  long[]
}

#' Normalize a diagnosis-code cell to the flat KCD form
#'
#' Denoise then parse: uppercase and keep only code characters (`[A-Z0-9]`, dot,
#' comma), take the first comma-separated code, drop the dot, roll to `len`
#' characters, and validate the KCD shape (one letter followed by two or more
#' digits). Fully vectorized, so it runs at data.table speed over millions of
#' rows and returns the first code of each cell.
#'
#' @param x A character vector of raw diagnosis-code cells.
#' @param len Integer length to roll each flat code to (default `4L`).
#' @return A character vector of normalized codes; entries that do not match the
#'   KCD shape become `NA`.
#' @seealso [split_kcd()] to keep every code in a cell.
#' @examples
#' normalize_kcd(c("m00.0", "M51.13", "K63.5, S33"))
#' @export
normalize_kcd <- function(x, len = 4L) {
  denoised <- .denoise_kcd(x)
  first    <- sub(",.*$", "", denoised)   # first comma-separated code
  .flatten_kcd(first, len)
}

#' Split a multi-code cell into normalized codes
#'
#' Like [normalize_kcd()] but keeps every comma-separated code in the cell, not
#' just the first. Used when a single cell holds several diagnoses that should be
#' redistributed (first -> main diagnosis, the rest -> sub-diagnoses).
#'
#' @param cell A single diagnosis-code cell (length-1 character).
#' @param len Integer length to roll each flat code to (default `4L`).
#' @return A character vector of the cell's normalized codes, `NA` entries
#'   dropped.
#' @examples
#' split_kcd("K63.5, S33, junk")
#' @export
split_kcd <- function(cell, len = 4L) {
  tokens <- strsplit(.denoise_kcd(cell), ",", fixed = TRUE)[[1]]
  tokens <- .flatten_kcd(tokens[nzchar(tokens)], len)
  tokens[!is.na(tokens)]
}

# normalize_kcd / split_kcd share a denoise-then-parse pipeline. Target flat
# form: one uppercase letter + 2-3 digits, no dot -- "m00.0" -> "M000",
# "M51.13" -> "M511" (len = 4), "K63.5, S33" -> "K635".

# Denoise: uppercase, then keep ONLY the code characters -- [A-Z0-9], the decimal
# point, and the comma separating several codes in one cell. Everything else (any
# non-Latin script, symbol, whitespace) is dropped by the whitelist, not by
# naming specific characters to remove.
.denoise_kcd <- function(x) {
  x <- toupper(as.character(x))
  gsub("[^A-Z0-9.,]", "", x)
}

# Flatten one already-denoised single code: drop dot, roll to len, validate.
.flatten_kcd <- function(x, len) {
  x <- gsub("\\.", "", x)
  if (!is.null(len) && is.finite(len)) x <- substr(x, 1L, len)
  x[!grepl("^[A-Z][0-9]{2,}$", x)] <- NA_character_
  x
}
