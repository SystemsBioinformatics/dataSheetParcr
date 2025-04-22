#' @import parcr
NULL

# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("comment_pattern", "header_pattern", "empty_line_pattern"))

#' Regular expression for comment lines
#'
#' This pattern matches lines that start with a `#` followed by any text.
#' @noRd
comment_pattern <- "^#(.+)"

#' Regular expression for header lines
#'
#' This pattern matches lines that start with a `>` followed by any text.
#' @noRd
header_pattern <- "^>(.+)"

#' Regular expression for empty lines
#'
#' This pattern matches lines that contain only whitespace.
#' @noRd
empty_line_pattern <- "^\\s*$"

#' Match Comment-Like Lines
#'
#' Matches either empty lines or comment lines and returns `NULL`.
#' @return `NULL` if the line matches an empty line or a comment.
#' @noRd
CommentLike <- function() {
  (EmptyLine() %or% Comment()) %ret% NULL
}

#' Match Comment Lines
#'
#' Matches lines that are comments based on the `comment_pattern`.
#' @return A parsed comment or `NULL` if no match is found.
#' @noRd
Comment <- function() {
  match_s(parse_comment)
}

#' Parse a Datasheet
#'
#' Parses a datasheet containing multiple data blocks.
#' @return A parsed datasheet as a structured object.
#' @export
DataSheet <- function() {
  zero_or_more(CommentLike()) %then%
    one_or_more(DataBlock()) %then%
    eof()
}

#' Parse a Data Block
#'
#' Parses a data block consisting of a header, optional comments, and a data table.
#' @return A named list where the header is the name and the data table is the value.
#' @noRd
DataBlock <- function() {
  (Header() %then%
    zero_or_more(CommentLike()) %then%
    DataTable() %then%
    zero_or_more(CommentLike())) %using%
  function(x) {
    stats::setNames(x[2], x[[1]])
  }
}

#' Parse a Data Table
#'
#' Parses a data table consisting of multiple data lines.
#' @return A tibble representing the parsed data table.
#' @noRd
DataTable <- function() {
  one_or_more(DataLine()) %using%
  function(x) {
    do.call(rbind, x) |>
      matrix_to_df()
  }
}

#' Parse a Data Line
#'
#' Matches and parses a single data line.
#' @return A parsed data line as a list or `NULL` if no match is found.
#' @noRd
DataLine <- function() {
  match_s(parse_tsv_line)
}

#' Parse a Header
#'
#' Matches and parses a header line based on the `header_pattern`.
#' @return A list containing the parsed header.
#' @noRd
Header <- function() {
  match_s(parse_header) %using%
    function(x) list(dataname = unlist(x))
}

#' Parse a Comment Line
#'
#' Parses a comment line based on the `comment_pattern`.
#' @param line A character string representing a line from the datasheet.
#' @return A parsed comment as a string or an empty list if parsing fails.
#' @noRd
parse_comment <- function(line) {
  m <- stringr::str_match(line, comment_pattern)
  if (is.na(m[1])) {
    return(list()) # signal failure
  } else {
    return(m[2])
  }
}

#' Parse a Header Line
#'
#' Parses a header line based on the `header_pattern`.
#' @param line A character string representing a line from the datasheet.
#' @return A parsed header as a string or an empty list if parsing fails.
#' @noRd
parse_header <- function(line) {
  m <- stringr::str_match(line, header_pattern)
  if (is.na(m[1])) {
    return(list()) # signal failure
  } else {
    return(stringr::str_trim(m[2], side = "both"))
  }
}

#' Parse a Tab-Separated Line
#'
#' Parses a tab-separated line and validates its content.
#' @param line A character string representing a line from the datasheet.
#' @return A parsed line as a list or an empty list if parsing fails.
#' @noRd
parse_tsv_line <- function(line) {
  m <- stringr::str_split_1(line, "\t")
  if (length(m) == 1 && m[1] == "" ||
      stringr::str_detect(m[1], header_pattern) ||
      stringr::str_detect(m[1], comment_pattern) ||
      stringr::str_detect(m[1], empty_line_pattern)) {
    return(list()) # signal failure
  } else {
    return(m)
  }
}

#' Convert a Matrix to a Tibble
#'
#' Converts a character matrix to a tibble. The first row is treated as column headers.
#' Columns with empty names are removed, and empty values are replaced with `NA`.
#' @param m A character matrix where the first row contains column headers.
#' @return A tibble representing the data in the matrix.
#' @export
matrix_to_df <- function(m) {
  colnames <- m[1, ]
  non_empty_cols <- colnames != ""
  values <- if (nrow(m) > 1) m[-1, non_empty_cols, drop = FALSE] else matrix(NA, nrow = 0, ncol = sum(non_empty_cols))
  colnames(values) <- colnames[non_empty_cols]
  values[values == ""] <- NA
  dplyr::as_tibble(values)
}
