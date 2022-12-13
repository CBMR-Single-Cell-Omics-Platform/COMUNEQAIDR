# fastq_paths <- "scRNAseq/02_FASTQ/221025_A00642_0321_AHG3Y7DMXY/fastq-path/Stats/Stats.json"
# pool_table_path <- "scRNAseq/04_Log/test/poolTable.csv"
#
# fastq_paths <- c(fastq_paths, fastq_paths)
#
# stats_list <- lapply(fastq_paths, jsonlite::read_json)
# names(stats_list) <- c("220707_A00642_0293_AHCV5HDMXY",
#                        "220707_A00642_0294_AHCV5HDABC")
# #names(stats_list) <- lapply(stats_list, `[[`, "RunId")
# stats_list <- lapply(stats_list, demux_stats)
#
# plot_read_distribution(stats_list)
# plot_top_n_undetermined(stats_list)
#
# update_pool_table(pool_table_path, fastq_paths)
#
#
# update_pool_table <- function(pool_table_path, fastq_paths) {
#   pool_table <- read.csv(pool_table_path)
#   stats_list <- lapply(fastq_paths, jsonlite::read_json)
#   names(stats_list) <- lapply(stats_list, `[[`, "RunId")
#   stats_list <- lapply(stats_list, demux_stats)
#
#   sample_reads <- lapply(stats_list, `[[`, "sample_stats")
#   for (i in names(sample_reads)) {
#     sample_reads[[i]] <- data.frame(index = names(sample_reads[[i]]),
#                                     reads = unlist(sample_reads[[i]]),
#                                     run_id = i)
#   }
#   sample_reads <- do.call("rbind", sample_reads)
#   rownames(sample_reads) <- NULL
#
#   sum_reads <- function(index, run_ids) {
#     sub_df <- subset(sample_reads, index == index & run_id %in% run_ids)
#     sum(sub_df[["reads"]])
#   }
#
#   pool_table[["READS (10x)"]] <- NA
#   if ("Index..HTO." %in% colnames(pool_table)) {
#     pool_table[["READS (HTO)"]] <- NA
#   }
#
#   for (i in seq_len(nrow(pool_table))) {
#     index_10x <- pool_table[i, "Index..10x."]
#     folders_10x <- strsplit(pool_table[i, "BCL.PIN..10x."], ",")[[1]]
#
#     pool_table[i, "READS (HTO)"] <- sum_reads(index_10x, folders_10x)
#     if ("Index..HTO." %in% colnames(pool_table)) {
#       index_hto <- pool_table[i, "Index..HTO."]
#       folders_hto <- strsplit(pool_table[i, "BCL.PIN..HTO."], ",")[[1]]
#
#       pool_table[i, "READS (HTO)"] <- sum_reads(index_hto, folders_hto)
#     }
#   }
#
#   pool_table
# }
#
#
# plot_top_n_undetermined <- function(stats_list, n = 10) {
#   undetermined <- lapply(stats_list, `[[`, "unknown_barcodes")
#   top_n <- lapply(undetermined, `[`, seq_len(n))
#
#   pD <- data.frame(barcode = character(),
#                    count = integer(),
#                    run_id = character())
#   for (i in names(top_n)) {
#     this_df <- data.frame(
#       barcode = names(top_n[[i]]),
#       count = top_n[[i]],
#       run_id = i
#     )
#     rownames(this_df) <- NULL
#     pD <- rbind(pD, this_df)
#   }
#
#   # For plotting we sort by barcode frequency
#   label_order <- unique(pD[["barcode"]][order(pD[["count"]])])
#   pD[["barcode"]] <- factor(x = pD[["barcode"]],
#                             levels = label_order)
#
#   ggplot2::ggplot(pD, ggplot2::aes(x = .data[["count"]] / 10^6,
#                                    y = .data[["barcode"]])) +
#     ggplot2::geom_bar(stat = "identity") +
#     ggplot2::facet_wrap("run_id", scales = "free", ncol = 1) +
#     ggplot2::ylab('Barcode') +
#     ggplot2::xlab('Counts [millions]') +
#     ggplot2::theme_minimal(base_size = 20)
# }
#
# plot_read_distribution <- function(stats_list) {
#   pD <- lapply(stats_list, `[[`, "sample_stats")
#   pD <- lapply(pD, function(x) data.frame(sample = names(x),
#                                           reads = unlist(x)))
#
#   for (i in names(pD)) {
#     frac_pf <- round(stats_list[[c(i, "frac_pf")]], digits = 3) * 100
#     title <- paste0(i, " - PF: ", frac_pf, "%")
#     pD[[i]]$run_id <- title
#   }
#
#   pD <- do.call("rbind", pD)
#   pD[["sample"]] <- factor(pD[["sample"]])
#   pD[["sample"]] <- relevel(pD[["sample"]], "Undetermined")
#   rownames(pD) <- NULL
#
#   ggplot2::ggplot(pD, ggplot2::aes(x = .data[["reads"]] / 10^6,
#                                    y = .data[["sample"]])) +
#     ggplot2::geom_bar(stat = "identity") +
#     ggplot2::facet_wrap("run_id", scales = "free", ncol = 1) +
#     ggplot2::ylab('Index ID') +
#     ggplot2::xlab('Counts [millions]') +
#     ggplot2::theme_minimal(base_size = 20)
# }
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
#   sample_stats <- lapply(stats[["ConversionResults"]], lane_demux_stats)
#   sample_stats <- Reduce(merge_stats, sample_stats)
#
#   sum_clusters <- function(idx) {
#     counts <- lapply(stats[["ConversionResults"]], `[[`, idx)
#     counts <- lapply(counts, as.numeric)
#     sum(unlist(counts))
#   }
#
#   frac_pf <- sum_clusters("TotalClustersPF") / sum_clusters("TotalClustersRaw")
#
#   unknown_barcodes <- lapply(stats[["UnknownBarcodes"]], `[[`, "Barcodes")
#   unknown_barcodes <- Reduce(merge_stats, unknown_barcodes)
#   unknown_barcodes <- unlist(unknown_barcodes)
#   unknown_barcodes <- sort(unknown_barcodes, decreasing = TRUE)
#
#   list(sample_stats = sample_stats,
#        frac_pf = frac_pf,
#        unknown_barcodes = unknown_barcodes)
# }
#
# lane_demux_stats <- function(conversion_results) {
#   reads <- lapply(conversion_results[["DemuxResults"]], `[[`, "NumberReads")
#   reads <- lapply(reads, as.numeric)
#   names(reads) <- lapply(conversion_results[["DemuxResults"]],
#                          `[[`, "SampleName")
#   reads[["Undetermined"]] <-
#     as.numeric(conversion_results[[c("Undetermined", "NumberReads")]])
#   reads
# }
#
# merge_stats <- function(x, y) {
#   all_barcodes <- union(names(x), names(y))
#   Map(`+`, x[all_barcodes], y[all_barcodes])
# }
