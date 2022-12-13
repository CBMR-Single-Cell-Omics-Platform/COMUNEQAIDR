known_species <- c("Human", "Mouse", "Rat", "Rhesus", "Mouse - optimized")

translate_index_10x_dual <- utils::read.csv(
  file = here::here("data-raw/Dual_Index_Kit_TT_Set_A.csv"),
  skip = 3
)
rownames(translate_index_10x_dual) <- gsub(
  pattern = "SI-TT-",
  replacement = "",
  x = translate_index_10x_dual[["index_name"]]
)
translate_index_10x_dual[["index_name"]] <- NULL

translate_index_10x_single <- utils::read.csv(
  file = here::here("data-raw/Single_Index_Kit_T_Set_A.csv"),
  header = FALSE
)
rownames(translate_index_10x_single) <- gsub(
  pattern = "SI-GA-",
  replacement = "",
  x = translate_index_10x_single[[1]]
)
translate_index_10x_single[[1]] <- NULL

translate_index_hto <- utils::read.csv(
  file = here::here("data-raw/TruSeq_I7_indexes.csv"),
  header = FALSE
)
rownames(translate_index_hto) <- translate_index_hto$V1
translate_index_hto[[1]] <- NULL

translate_bc_hto <- utils::read.csv(
  file = here::here("data-raw/totalseq-a-hashtags.csv"),
  sep = "\t",
  colClasses = c("Barcode" = "character")
)
rownames(translate_bc_hto) <- paste0("A", translate_bc_hto[["Barcode"]])
translate_bc_hto <- translate_bc_hto[, "Sequence", drop = FALSE]


index_10x <- rownames(translate_index_10x_dual)
index_hto <- rownames(translate_index_hto)
hto_ids <- rownames(translate_bc_hto)

default_qc_path <- "scRNAseq/00_QC"
default_bcl_path <- "scRNAseq/01_BCL"
default_fastq_path <- "scRNAseq/02_FASTQ"
default_out_path <- "scRNAseq/03_PipelineOut"
default_log_path <- "scRNAseq/04_Log"
default_lib_sheet_path <- "scRNAseq/05_LabData/01_Experiments/I_Library_sheet"
default_config_path <- "scRNAseq/"

samplesheet_dualindex_header <-
  "[Header]
EMFileVersion,4

[Reads]
28
90

[Data]
Lane,Sample_ID,Sample_Name,index,index2,Sample_Project,Original_Sample_ID
"

usethis::use_data(known_species,
  index_10x,
  index_hto,
  hto_ids,
  translate_index_10x_dual,
  translate_index_hto,
  translate_bc_hto,
  default_qc_path,
  default_bcl_path,
  default_fastq_path,
  default_out_path,
  default_log_path,
  default_lib_sheet_path,
  default_config_path,
  samplesheet_dualindex_header,
  internal = TRUE,
  overwrite = TRUE
)
