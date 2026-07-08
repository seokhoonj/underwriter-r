# KCD diagnosis-code normalization by denoise-then-parse.
# Step 1 (denoise): uppercase, then keep ONLY the code characters -- [A-Z0-9],
#   the decimal point, and the comma that separates several codes in one cell.
#   Everything else (any non-Latin script, symbols, whitespace) is dropped by the
#   whitelist, not by naming specific characters to remove.
# Step 2 (parse): take the first comma-separated code, drop the dot to get the
#   flat form, roll to `len` characters, and validate the KCD shape.
#
# Target flat form: one uppercase letter + 2-3 digits, no dot. For example
#   "m00.0" -> "M000", "M51.13" -> "M511" (len = 4), "K63.5, S33" -> "K635".

# Denoise: keep only code characters and the comma separator.
.denoise_disease_code <- function(x) {
  x <- toupper(as.character(x))
  gsub("[^A-Z0-9.,]", "", x)
}

# Flatten one already-denoised single code: drop dot, roll to len, validate.
.flatten_disease_code <- function(x, len) {
  x <- gsub("\\.", "", x)
  if (!is.null(len) && is.finite(len)) x <- substr(x, 1L, len)
  x[!grepl("^[A-Z][0-9]{2,}$", x)] <- NA_character_
  x
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
#' @seealso [split_disease_code()] to keep every code in a cell.
#' @examples
#' normalize_disease_code(c("m00.0", "M51.13", "K63.5, S33"))
#' @export
normalize_disease_code <- function(x, len = 4L) {
  denoised <- .denoise_disease_code(x)
  first    <- sub(",.*$", "", denoised)   # first comma-separated code
  .flatten_disease_code(first, len)
}

#' Split a multi-code cell into normalized codes
#'
#' Like [normalize_disease_code()] but keeps every comma-separated code in the
#' cell, not just the first. Used when a single cell holds several diagnoses that
#' should be redistributed (first -> main diagnosis, the rest -> sub-diagnoses).
#'
#' @param cell A single diagnosis-code cell (length-1 character).
#' @param len Integer length to roll each flat code to (default `4L`).
#' @return A character vector of the cell's normalized codes, `NA` entries
#'   dropped.
#' @examples
#' split_disease_code("K63.5, S33, junk")
#' @export
split_disease_code <- function(cell, len = 4L) {
  tokens <- strsplit(.denoise_disease_code(cell), ",", fixed = TRUE)[[1]]
  tokens <- .flatten_disease_code(tokens[nzchar(tokens)], len)
  tokens[!is.na(tokens)]
}
