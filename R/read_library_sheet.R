#' Detect sample sheet version
#'
#' @param sheet_names vector of the names of sheets in excel file
#'
#' @return string, sample sheet version
#' @export
detect_samplesheet_version <- function(sheet_names) {
  v1_names <- c("10X library prep", "Sequencing info")
  v2_names <- c("sample_sheet", "sample2reaction", "reaction_sheet",
                "reaction2library", "library_sheet", "library2sequencing",
                "sequencing_sheet")

  if (all(v1_names %in% sheet_names)) {
    version <- "v1"
  } else if (all(v2_names %in% sheet_names)) {
    version <- "v2"
  } else {
    stop("Sheet version could not be detected")
  }
  version
}

#' Process sequencing info sheet
#'
#' @param seq_info data.frame, sequencing info
#' @param sample_col string, name of column with comma separated sample names
#' @param date_col string, name of column with sequencing date
#' @param output_col string, name of column with bcl folder
#'
#' @return data.frame mapping samples to BCL folders
#' @export
process_seq_info <- function(seq_info, sample_col, date_col, output_col) {
  spacer_rows <- is.na(seq_info[[sample_col]])
  bcl_folder <- seq_info[!spacer_rows, c(sample_col, date_col, output_col)]

  splitter <- function(x) {
    out <- strsplit(x = as.character(x), split = ",", fixed = TRUE)[[1]]
    out <- gsub(x = out, pattern = "\"", replacement = "")
    out <- trimws(out, which = "both")
    out
  }

  splitter <- function(x) {
    x <- unname(x)
    sample <- strsplit(x = as.character(x[1]), split = ",", fixed = TRUE)[[1]]
    sample <- gsub(x = sample, pattern = "\"", replacement = "")
    sample <- trimws(sample, which = "both")

    out <- data.frame(sample_id = sample, date = x[2], bcl_folder = x[3])
  }

  bcl_data <- apply(bcl_folder, 1, splitter)
  bcl_data <- do.call("rbind", bcl_data)

  bcl_data <- split(bcl_data, bcl_data[["date"]])

  for (i in names(bcl_data)) {
    bcl_data[[i]][["sequencing_id"]] <- paste0(
      bcl_data[[i]][["date"]], "_",
      LETTERS[as.integer(as.factor(bcl_data[[i]][["bcl_folder"]]))]
    )
  }
  bcl_data <- do.call("rbind", bcl_data)
  bcl_data <- bcl_data[, c("sample_id", "sequencing_id", "bcl_folder")]

  bcl_data
}


#' Parse a library sheet v1 file
#'
#' @param file string, path to excel file with sequencing information
#'
#' @return list of data.frames with info tables
#' @export
parse_library_sheet_v1 <- function(file) {
  sheet <- readxl::read_excel(file, "10X library prep")
  # Remove non-breaking space
  colnames(sheet) <- gsub("(*UCP)\\s", " ", colnames(sheet), perl = TRUE)
  sample_col <- "Sample #"

  relevant_cols <- c(
    sample_id = "Sample #",
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

  uses_hashtags <- sheet[[relevant_cols[["workflow"]]]] |>
    na.omit() |>
    tolower()
  uses_hashtags <- all(uses_hashtags == "y")

  if (!uses_hashtags) {
    # Guess it is without HTO
    relevant_cols[["loaded_cells"]] <- "Standard 10X (Cells loaded in total)"
  }

  spacer_rows <- is.na(sheet[[sample_col]])
  sheet <- sheet[!spacer_rows, relevant_cols]
  sheet[[sample_col]] <- as.character(sheet[[sample_col]])

  colnames(sheet) <- names(relevant_cols)

  ##### Process BCL folder
  bcl_output_col <- "output name"
  bcl_sample_col <- "sample name(s) format: \"s1\",\"s2\",\"s..."
  bcl_date_col <- "seq date (YYYYMMDD)"
  seq_info <- readxl::read_excel(file, "Sequencing info")
  colnames(seq_info) <- gsub("(*UCP)\\s", " ", colnames(seq_info), perl = TRUE)

  bcl_data <- process_seq_info(
    seq_info = seq_info,
    sample_col = bcl_sample_col,
    date_col = bcl_date_col,
    output_col = bcl_output_col
  )

  no_bcl_folder <- setdiff(sheet[["sample_id"]], bcl_data[["sample_id"]])

  if (length(no_bcl_folder) > 0) {
    stop(
      "Samples ", paste(no_bcl_folder, collapse = ", "),
      " have no associated bcl folders"
    )
  }

  sheet <- merge(sheet, bcl_data, by = "sample_id")
  sheet[["reaction_id"]] <- paste0(
    sheet[["date_10x"]], "_", sheet[["i7_index"]]
    )

  ##### Sample Sheet
  sample_sheet <- unique(sheet[, c("sample_id", "species", "hto")])
  rownames(sample_sheet) <- NULL
  sample_sheet[["sample_id"]] <- gsub(
    pattern = "(*UCP)[\\s\\p{L}]+|\\W+$", # See https://stackoverflow.com/questions/43734293/remove-non-breaking-space-character-in-string
    replacement = "",
    x = sample_sheet[["sample_id"]])

  ##### Sample to Reaction
  sample2reaction <- unique(
    sheet[, c("sample_id", "reaction_id", "loaded_cells")]
    )

  ##### Reaction Sheet
  reaction_sheet <- unique(
    sheet[, c("reaction_id", "seq_type")]
  )
  reaction_sheet[["cutoff"]] <- "auto"

  # Only allow one alignment target per reaction
  species_per_reaction <- Reduce(
    merge, list(sample_sheet, sample2reaction, reaction_sheet)
    )
  align_to <- tapply(
    X = species_per_reaction[["species"]],
    INDEX = species_per_reaction[["reaction_id"]],
    FUN = \(x) guess_species(x) |> unique(),
    simplify = FALSE
  )
  align_targets <- lapply(align_to, length) |> unlist()
  if (any(align_targets > 1)) {
    problem_reactions <- names(align_targets[align_targets > 1])
    warning("Reactions: ", paste0(problem_reactions, collapse = ", "),
         " contains a mixture of species. Alignment target for these reactions",
         " will not be determined automatically.")
  }
  align_to[align_targets > 1] <- "unknown"
  align_to <- unlist(align_to)
  reaction_sheet[["align"]] <- align_to[reaction_sheet[, "reaction_id"]]


  ##### Reaction to Library
  reaction2library <- unique(sheet[, c("reaction_id"), drop = FALSE])
  reaction2library[["library_id"]] <- paste0(
    reaction2library[["reaction_id"]], "_10x"
  )
  reaction2library[["library_type"]] <- "10x"
  if (uses_hashtags) {
    tmp <- reaction2library
    tmp[["library_id"]] <- paste0(
      reaction2library[["reaction_id"]], "_hto"
    )
    tmp[["library_type"]] <- "hto"
    reaction2library <- rbind(reaction2library, tmp)
  }

  ##### Library Sheet
  library_sheet <- unique(sheet[, c("reaction_id", "i7_index")])
  library_sheet[["library_id"]] <- paste0(
    library_sheet[["reaction_id"]], "_10x"
  )
  library_sheet[["reaction_id"]] <- NULL
  colnames(library_sheet) <- c("index", "library_id")
  library_sheet[["index"]] <- paste0("SI-TT-", library_sheet[["index"]])
  if (uses_hashtags) {
    tmp <- unique(sheet[, c("reaction_id", "hash_index")])
    tmp[["library_id"]] <- paste0(
      tmp[["reaction_id"]], "_hto"
    )
    tmp[["reaction_id"]] <- NULL
    colnames(tmp) <- c("index", "library_id")
    library_sheet <- rbind(library_sheet, tmp)
  }
  library_sheet <- library_sheet[, c("library_id", "index")]

  ##### Library to Sequencing
  library2sequencing <- sheet[, c("reaction_id", "sequencing_id")]
  library2sequencing <- merge(reaction2library, library2sequencing)
  library2sequencing[["reaction_id"]] <- NULL
  library2sequencing[["library_type"]] <- NULL
  library2sequencing <- unique(library2sequencing)
  rownames(library2sequencing) <- NULL
  library2sequencing[["lane"]] <- "*"

  ##### Sequencing Sheet
  sequencing_sheet <- unique(bcl_data[, c("sequencing_id", "bcl_folder")])
  rownames(sequencing_sheet) <- NULL
  sequencing_sheet[["override_cycles"]] <- ""

  list(
    sample_sheet = sample_sheet,
    sample2reaction = sample2reaction,
    reaction_sheet = reaction_sheet,
    reaction2library = reaction2library,
    library_sheet = library_sheet,
    library2sequencing = library2sequencing,
    sequencing_sheet = sequencing_sheet
  )
}

#' Parse a library sheet v2 file
#'
#' @param file string, path to excel file with sequencing information
#'
#' @return list of data.frames with info tables
#' @export
parse_library_sheet_v2 <- function(file) {
  sheet_names <- readxl::excel_sheets(file)
  names(sheet_names) <- sheet_names

  out <- lapply(sheet_names, readxl::read_excel, path = file)
  lapply(out, \(x) {x[is.na(x)] <- ""; x})
}

#' Parse a config file
#'
#' @param file string, path to config file with sequencing information
#'
#' @return list of data.frames with info tables
#' @export
parse_config <- function(file) {
  config <- yaml::read_yaml(file)
  sheets <- c("sample_sheet", "sample2reaction", "reaction_sheet",
              "reaction2library", "library_sheet", "library2sequencing",
              "sequencing_sheet")
  for (i in sheets) {
    config[[i]] <- as.data.frame(config[[i]])
  }
  config
}
