# stats <- jsonlite::read_json("scRNAseq/02_FASTQ/221025_A00642_0321_AHG3Y7DMXY/fastq-path/Stats/Stats.json")
#
# demux_stats <- function(stats, lane = "*") {
#   # Lane can be a vector of lane numbers
#
#   if (length(lane) > 1 || lane != "*") {
#     # It is called "Lane" in UnknownBarcodes and "LaneNumber" in
#     # ConversionResults, we use partial matching
#     filter_fn <- function(x) x$Lane %in% lane
#     stats$UnknownBarcodes <- Filter(filter_fn, stats$UnknownBarcodes)
#     stats$ConversionResults <- Filter(filter_fn, stats$ConversionResults)
#   }
#
#   sample_stats <- lapply(stats$ConversionResults, lane_demux_stats)
#   sample_stats <- Reduce(merge_stats, sample_stats)
#
#   unknown_barcodes <- lapply(stats$UnknownBarcodes, `[[`, "Barcodes")
#   unknown_barcodes <- Reduce(merge_stats, unknown_barcodes)
#   unknown_barcodes <- unlist(unknown_barcodes)
#   unknown_barcodes <- sort(unknown_barcodes, decreasing = TRUE)
#
#   list(sample_stats = sample_stats,
#        unknown_barcodes = unknown_barcodes)
# }
#
# lane_demux_stats <- function(conversion_results) {
#   reads <- lapply(conversion_results$DemuxResults, `[[`, "NumberReads")
#   reads <- lapply(reads, as.numeric)
#   names(reads) <- lapply(conversion_results$DemuxResults, `[[`, "SampleName")
#   reads[["Undetermined"]] <-
#     as.numeric(conversion_results$Undetermined$NumberReads)
#   reads[["TotalClustersRaw"]] <- as.numeric(conversion_results$TotalClustersRaw)
#   reads[["TotalClustersPF"]] <- as.numeric(conversion_results$TotalClustersPF)
#   reads
# }
#
# merge_stats <- function(x, y) {
#   all_barcodes <- union(names(x), names(y))
#   Map(`+`, x[all_barcodes], y[all_barcodes])
# }
