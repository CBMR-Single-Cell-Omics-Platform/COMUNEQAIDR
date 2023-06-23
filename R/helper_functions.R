#' Inverse Value Matching
#' @name notin
#' @param x vector or \code{NULL}: the values to be matched.
#' @param table vector or \code{NULL}: the values to be matched against.
#'
#' @return A logical vector, indicating if a match was nor located for each
#' element of \code{x}: thus the values are \code{TRUE} or \code{FALSE} and
#' never \code{NA}.
#' @seealso \link[base]{match}
#' @export
#'
#' @examples
#' c("a", "b", "c") %!in% c("c", "d")
`%!in%` <- function(x, table) { # nolint: object_name_linter
  !(x %in% table)
}


#' Extract BCL pin from folder name
#'
#' @param x string, BCL folder name
#'
#' @return string containing the four digits in the pin
#' @export
#'
#' @examples
#' extract_pin("900101_A00001_0001_ABCDEFGHI1")
extract_pin <- function(x) {
  gsub(
    pattern = "^[[:digit:]]{6}_[[:alpha:]]+[[:digit:]]+_([[:digit:]]{4})_[[:alnum:]]{10}$", # nolint: line_length_linter
    replacement = "\\1",
    x = x
  )
}

#' Extract sequencing date from folder name
#'
#' @param x string, BCL folder name
#'
#' @return string containing the sequencing date
#' @export
#'
#' @examples
#' extract_sequencing_date("900101_A00001_0001_ABCDEFGHI1")
extract_sequencing_date <- function(x) {
  gsub(
    pattern = "^([[:digit:]]{6})_[[:alpha:]]+[[:digit:]]+_[[:digit:]]{4}_[[:alnum:]]{10}$", # nolint: line_length_linter
    replacement = "\\1",
    x = x
  )
}

#' Extract flowcell ID from folder name
#'
#' @param x string, BCL folder name
#'
#' @return string containing the flowcell ID
#' @export
#'
#' @examples
#' extract_flowcell_id("900101_A00001_0001_ABCDEFGHI1")
extract_flowcell_id <- function(x) {
  gsub(
    pattern = "^[[:digit:]]{6}_[[:alpha:]]+[[:digit:]]+_[[:digit:]]{4}_[A|B]([[:alnum:]]{9})$", # nolint: line_length_linter
    replacement = "\\1",
    x = x
  )
}
