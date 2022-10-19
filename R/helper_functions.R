#' Write message to log
#'
#' @param message string, text to be printed
#' @param section logical, format message as section header
#' @param title logical, format message as title
#' @param log_number optional integer, denoting current log number. Ignored
#' unless title = TRUE
#' @param total_logs optional integer, denoting total number of logs. Ignored
#' unless title = TRUE
#' @param horizontal_line logical, ignore message and print horizontal line
#' @param width integer, line width in characters
#'
#' @return a formatted message is printed to standard out, \code{NULL} is
#' invisibly returned
#' @export
#'
#' @examples
#' write_log(c("foo", "bar"))
#' write_log(c("foo", "bar"), section = TRUE)
#' write_log(c("foo", "bar"), title = TRUE)
#' write_log(c("foo", "bar"), title = TRUE, log_number = 4, total_logs = 6)
#' write_log(horizontal_line = TRUE)
#' write_log("a very long line of characters", width = 25, title = TRUE)
write_log <- function(message = NULL, section = FALSE, title = FALSE,
                      log_number = NULL, total_logs = NULL,
                      horizontal_line = FALSE, width = 80L) {
  hline <- stringr::str_dup(string = "#", times = width)
  split_message <- function(x, width) {
    unlist(stringr::str_split(stringr::str_wrap(x, width = width), "\n"))
  }

  if (title) {
    decor_top <- c(
      hline,
      stringr::str_c(
        stringr::str_dup(
          string = c("#", " ", "#"),
          times = c(5L, width - 10L, 5L)
        ),
        collapse = ""
      )
    )

    if (!is.null(log_number) && !is.null(total_logs)) {
      log_counter <- stringr::str_c(
        stringr::str_dup(
          string = c("*", " / ", "*"),
          times = c(log_number, 1L, total_logs)
        ),
        collapse = ""
      )
    } else {
      log_counter <- ""
    }

    decor_bottom <- c(
      stringr::str_c(
        c(
          stringr::str_dup(
            string = c("#", " "),
            times = c(5L, width - stringr::str_length(log_counter) - 15L)
          ),
          log_counter,
          stringr::str_dup(string = c(" ", "#"), times = c(5L, 5L))
        ),
        collapse = ""
      ),
      hline
    )

    available_space <- width - 12L # five '#' and a whitespace in both ends
    # title always has a decor above and below
    # title is centered

    message <- stringr::str_c(
      "##### ",
      stringr::str_pad(
        string = split_message(message, available_space),
        width = available_space, side = "both"
      ),
      " #####"
    )
  } else if (section) {
    decor <- stringr::str_c(
      stringr::str_dup(
        string = c("#", " "),
        times = c(5L, width - 5L)
      ),
      collapse = ""
    )
    decor_top <- c(
      decor,
      stringr::str_pad("##", width, side = "right")
    )
    decor_bottom <- c(
      stringr::str_pad("##", width, side = "right"),
      decor
    )
    available_space <- width - 3L

    message <- stringr::str_pad(
      stringr::str_c("## ", string = split_message(message, available_space)),
      width = width,
      side = "right"
    )
  } else if (horizontal_line) {
    decor_top <- NULL
    decor_bottom <- NULL
    message <- hline
  } else {
    decor_top <- NULL
    decor_bottom <- NULL
    available_space <- width - 3L
    message <- stringr::str_pad(
      stringr::str_c("## ", string = split_message(message, available_space)),
      width = width,
      side = "right"
    )
  }

  out <- stringr::str_c(
    c(
      decor_top,
      message,
      decor_bottom
    ),
    collapse = "\n"
  )
  cat(out)
}

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
    pattern = "^[[:digit:]]{6}_[[:alpha:]]+[[:digit:]]+_[[:digit:]]{4}_([[:alnum:]]{10})$", # nolint: line_length_linter
    replacement = "\\1",
    x = x
  )
}
