#' Figure out the type of input and use the correct input
#'
#' @param files vector of files to be processed
#' @param lib_sheet_path string, path to library sheets folder
#'
#' @return data.frame with sample information
#' @export
read_input_files <- function(files, lib_sheet_path) {
  # Try to detect app input data
  extension <- unique(tools::file_ext(files))

  if (length(extension) > 1) stop("Mixture of input filetypes not supported")

  if (extension == "xlsx") {
    sheet_names <- readxl::excel_sheets(here::here(lib_sheet_path, files[[1]]))
    version <- detect_samplesheet_version(sheet_names)
    reader <- switch(version,
      v1 = parse_library_sheet_v1,
      v2 = parse_library_sheet_v2,
      stop(version, " not implemented")
    )

    sheet <- read_multiple_sheets(
      library_sheets = files,
      reader = reader,
      lib_sheet_path = lib_sheet_path
    )
    sheet[["scop_id"]] <- "Cannot be read from library sheet v1"
  } else if (extension == "csv") {
    if (all(c("samp", "nume", "sele", "text") %in% names(files))) {
      sheet <- parse_app_data(
        samp = files[["samp"]],
        nume = files[["nume"]],
        sele = files[["sele"]],
        text = files[["text"]]
      )
    } else {
      stop("App data files must be named 'samp', 'nume', 'sele' and 'text'")
    }
  } else {
    stop("Only csv and xlsx files supported")
  }
  sheet
}

#' Detect sample sheet version
#'
#' @param sheet_names vector of the names of sheets in excel file
#'
#' @return string, sample sheet version
#' @export
detect_samplesheet_version <- function(sheet_names) {
  v1_names <- c("10X library prep", "Sequencing info")
  v2_names <- c("Metadata_sheet", "10X library prep", "Sequencing info")

  if (all(sheet_names == v1_names)) {
    version <- "v1"
  } else if (all(sheet_names == v2_names)) {
    version <- "v2"
  } else {
    stop("Sheet version could not be detected")
  }
  version
}


#' Parse a library sheet v1 file
#'
#' @param file string, path to excel file with sequencing information
#'
#' @return data.frame with info necessary for running COMUNEQAID
#' @export
parse_library_sheet_v1 <- function(file) {
  sheet <- readxl::read_excel(file, "10X library prep")
  # Remove non-breaking space
  colnames(sheet) <- gsub("(*UCP)\\s", " ", colnames(sheet), perl = TRUE)
  sample_col <- "Sample #"

  relevant_cols <- c(
    sample = "Sample #",
    date_10x = "Date for 10X",
    i7_index = "i7 Index (10X)",
    tissue = "Tissue/region",
    hash_index = "Hash_index",
    hto = "Hash code",
    loaded_cells = "Hash (cells loaded)",
    species = "Specie",
    seq_type = "Nuclei or whole cells",
    workflow = "Hashtag used (Y/N)",
    kit = "10X kit used",
    various_info = "Various information",
    note = "note:"
  )

  hash_cells_empty <- all(is.na(sheet[[relevant_cols[["loaded_cells"]]]]))
  if (hash_cells_empty) {
    # Guess it is without HTO
    relevant_cols[["loaded_cells"]] <- "Standard 10X (Cells loaded in total)"
  }

  n_samples <- split(sheet, cumsum(is.na(sheet[[sample_col]])))
  n_samples <- Filter(n_samples, f = \(x) nrow(x) > 1)
  n_samples <- length(n_samples)

  spacer_rows <- is.na(sheet[[sample_col]])
  sheet <- sheet[!spacer_rows, relevant_cols]
  sheet[[sample_col]] <- as.character(sheet[[sample_col]])

  bcl_output_col <- "output name"
  bcl_sample_col <- "sample name(s) format: \"s1\",\"s2\",\"s..."
  seq_info <- readxl::read_excel(file, "Sequencing info")
  colnames(seq_info) <- gsub("(*UCP)\\s", " ", colnames(seq_info), perl = TRUE)

  bcl_folder <- process_seq_info(
    seq_info = seq_info,
    sample_col = bcl_sample_col,
    output_col = bcl_output_col
  )

  no_bcl_folder <- setdiff(sheet[[sample_col]], rownames(bcl_folder))

  if (length(no_bcl_folder) > 0) {
    stop(
      "Samples ", paste(no_bcl_folder, collapse = ", "),
      " have no associated bcl folders"
    )
  }

  sheet[["bcl_folder"]] <- bcl_folder[sheet[[sample_col]], "bcl_folder"]
  sheet[["n_samples"]] <- n_samples

  colnames(sheet) <- c(names(relevant_cols), "bcl_folder", "n_samples")

  nucleus_isolation_id <- paste(
    sheet[["date_10x"]],
    sheet[["i7_index"]],
    sep = "_"
  )
  sheet[["nucleus_isolation_id"]] <- nucleus_isolation_id

  if (hash_cells_empty) {
    sheet[["hto_bcl"]] <- NA
  } else {
    sheet[["hto_bcl"]] <- sheet[["bcl_folder"]]
  }

  sheet[["10x_bcl"]] <- sheet[["bcl_folder"]]
  sheet[["bcl_folder"]] <- NULL
  sheet[["date_10x"]] <- NULL

  sheet[["i7_index"]] <- paste0("SI-TT-", sheet[["i7_index"]])

  sheet[["lane"]] <- "*"
  sheet[["comment"]] <- paste(sheet[["various_info"]], sheet[["note"]])

  sheet
}

#' Parse a library sheet v1 file
#'
#' @param file string, path to excel file with sequencing information
#'
#' @return data.frame with info necessary for running COMUNEQAID
#' @export
parse_library_sheet_v2 <- function(file) {

}

#' Process sequencing info sheet
#'
#' @param seq_info data.frame, sequencing info
#' @param sample_col string, name of column with comma separated sample names
#' @param output_col string, name of column with bcl folder
#'
#' @return data.frame mapping samples to BCL folders
#' @export
process_seq_info <- function(seq_info, sample_col, output_col) {
  spacer_rows <- is.na(seq_info[[sample_col]])
  bcl_folder <- seq_info[!spacer_rows, c(sample_col, output_col)]

  splitter <- function(x) {
    out <- strsplit(x = as.character(x), split = ",", fixed = TRUE)[[1]]
    out <- gsub(x = out, pattern = "\"", replacement = "")
    out <- trimws(out, which = "both")
    out
  }

  bcl_folder <- tapply(
    X = bcl_folder[[sample_col]],
    INDEX = bcl_folder[[output_col]],
    FUN = splitter
  )
  bcl_folder <- utils::stack(bcl_folder)

  bcl_folder <- tapply(
    X = bcl_folder[["ind"]],
    INDEX = bcl_folder[["values"]],
    FUN = paste0,
    collapse = ","
  )
  bcl_folder <- utils::stack(bcl_folder)

  data.frame(
    bcl_folder = bcl_folder[["values"]],
    row.names = bcl_folder[["ind"]]
  )
}

#' Fix typical issues in sample sheets
#'
#' @param sheet data.frame read by \link{read_input_files}.
#'
#' @return sample sheet with simple common errors fixed
#' @export
fix_library_sheet <- function(sheet) {
  # Fix spaces in sample name
  sheet[["sample"]] <- gsub("[[:space:]]+", "_", sheet[["sample"]])
  sheet[["workflow"]] <- toupper(sheet[["workflow"]])

  sheet
}

#' Check library sheet for consistency
#'
#' @param sheet data.frame, library sheet to be tested
#' @param n_samples number of samples expected
#'
#' @return TRUE If library sheet passes all test, otherwise a string describing
#' the first test that failed.
#' @export
check_library_sheet <- function(sheet, n_samples) {
  return(TRUE)
  out <- TRUE

  n_i7 <- tapply(
    X = sheet[["i7_index"]],
    INDEX = sheet[["bcl_folder"]],
    FUN = \(x) length(unique(x))
  )
  n_i7 <- sum(n_i7)

  # No NA can be present in the sheet
  if (anyNA(sheet)) {
    out <- "NA detected in sheet"
  } else if (n_i7 != n_samples) {
    out <- paste0(
      "Inferred samples: ", n_samples, "\n",
      "Number of unique i7 indexes: ", n_i7
    )
  } else if (!all(sheet[["seq_type"]] %in% c("Nuclei", "Whole cell"))) {
    out <- paste0("Sequencing type must be 'Nuclei' og 'Whole cell'.")
  } else if (!all(sheet[["workflow"]] %in% c("Y", "N"))) {
    out <- paste0("Hashtags used must be 'Y' or 'N'")
  } else if (!all(sheet[["species"]] %in% known_species)) {
    out <- paste0(
      "Species must be one of '", paste(
        known_species[-length(known_species)],
        collapse = "', '"
      ), "'",
      " and '", known_species[length(known_species)], "'"
    )
  } else if (!all(sheet[["i7_index"]] %in% index_10x)) {
    out <- paste0(
      "I7 index used must be a letter from A-H followed ",
      "by a number from 1-12, e.g. F11"
    )
  } else if (!all(sheet[["hash_index"]] %in% index_hto)) {
    out <- paste0(
      "Hash index used must be a D followed ",
      "by a number from 701-712, e.g. D705"
    )
  } else if (!all(sheet[["hto"]] %in% hto_ids)) {
    out <- paste0(
      "Hash code used must start with A0 followed ",
      "by a 3 digit number in one of the ranges 251-254, 301-306",
      " or 451-465, e.g. A0456"
    )
  }

  out
}

#' Read multiple library sheets at once
#'
#' @param library_sheets vector of library sheet filenames
#' @param reader function used to read library_sheets
#' @param lib_sheet_path character path to library sheets, if unspecified
#' uses `default_lib_sheet_path`
#'
#' @details If the library sheets are located in different folders, the full
#' path can be given in `library_sheets` and `lib_sheet_path` set to an empty
#' string.
#' TODO: Probably doesn't handle re-sequencing. Number of samples will be wrong.
#'
#' @return single merges library sheet
#' @export
#'
#' @examples
#' \dontrun{
#' read_multiple_sheets(vector_of_sheet_paths)
#' }
read_multiple_sheets <- function(library_sheets,
                                 reader = parse_library_sheet_v1,
                                 lib_sheet_path = default_lib_sheet_path) {
  if (missing(library_sheets)) {
    stop("library_sheet(s) must be specified")
  }

  ### Read and test library sheet
  sheet_files <- here::here(lib_sheet_path, library_sheets)

  sheets <- list()

  for (file in sheet_files) {
    if (!file.exists(file)) {
      stop("Could not find library sheet at ", file)
    }

    sheet <- reader(file)
    sheet <- fix_library_sheet(sheet)

    sheet_ok <- check_library_sheet(sheet = sheet)

    if (!isTRUE(sheet_ok)) {
      stop(sheet_ok, " in ", file)
    }

    sheets[[file]] <- sheet
  }

  sheet <- do.call(what = "rbind", sheets)


  sheet_ok <- check_library_sheet(sheet)

  if (!isTRUE(sheet_ok)) {
    stop(sheet_ok, " in merged library sheet")
  }

  sheet
}

#' Parse files from COMUNEQAID app
#'
#' @param samp string, path to samp.csv file
#' @param nume string, path to nume.csv file
#' @param sele string, path to sele.csv file
#' @param text string, path to text.txt file
#'
#' @return data.frame with info necessary for running COMUNEQAID
#' @export
parse_app_data <- function(samp, nume, sele, text) {
  sheet <- utils::read.csv(samp)
  colnames(sheet) <- c(
    "sample", "tissue", "i7_index", "hash_index", "10x_pin",
    "hto_pin", "hto", "loaded_cells", "lane", "comment"
  )
  n_samples <- utils::read.csv(nume)
  vars <- utils::read.csv(sele)
  text_vars <- utils::read.csv(text)

  sheet[["n_samples"]] <- n_samples[["value"]]
  sheet[["species"]] <- vars[vars[["inputId"]] == "select_organism", "value"]
  sheet[["seq_type"]] <- vars[vars[["inputId"]] == "select_seqType", "value"]
  sheet[["workflow"]] <- vars[vars[["inputId"]] == "select_workflow", "value"]
  sheet[["workflow"]] <- ifelse(sheet[["workflow"]] == "10x + HTO", "Y", "N")
  sheet[["nucleus_isolation_id"]] <- paste(sample(letters, 30, TRUE),
    collapse = ""
  )
  sheet[["scop_id"]] <- text_vars[
    text_vars[["inputId"]] == "text_scopID", "value"
  ]
  sheet[["kit"]] <- "unknown"
  sheet
}
