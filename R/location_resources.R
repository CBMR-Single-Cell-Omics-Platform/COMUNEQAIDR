resource_base <- list(
  computerome = "/home/projects/ku_00016/data/resources",
  yggdrasil = "/projects/SCOP/resources",
  nidhogg = "/projects/SCOP/resources"
)

whitelist <- "/non-species-specific/whitelist/3M-february-2018.txt"

species_specific <- list(
  "Mouse" = list(
    path_ref     = "mus-musculus/sc-ref/mouse_splici",
    salmon_index = "af_tutorial_splici/mm10_splici_idx",
    T2G          = "refdata-gex-mm10-2020-A/t2g.tsv",
    T3G          = "refdata-gex-mm10-2020-A/t2g_3col.tsv"
  ),
  "Human" = list(
    path_ref     = "homo-sapiens/sc-ref/human_splici",
    salmon_index = "af_tutorial_splici/grch38_splici_idx",
    T2G          = "refdata-gex-GRCh38-2020-A/t2g.tsv",
    T3G          = "refdata-gex-GRCh38-2020-A/t2g_3col.tsv"
  ),
  "Rat" = list(
    path_ref     = "rattus-norvegicus/sc-ref/rat_splici",
    salmon_index = "af_tutorial_splici/rnor_splici_idx",
    T2G          = "Rnor_6.0/t2g.tsv",
    T3G          = "Rnor_6.0/t2g_3col.tsv"
  ),
  "Rhesus" = list(
    path_ref     = "macaca-mulatta/sc-ref/rhesus_splici",
    salmon_index = "af_tutorial_splici/mmul_10_splici_idx",
    T2G          = "Mmul_10/t2g.tsv",
    T3G          = "Mmul_10/t2g_3col.tsv"
  ),
  "Mouse - optimized" = list(
    path_ref     = "mus-musculus/sc-ref/mouse_pool",
    salmon_index = "af_tutorial_splici/mouse-optimized_idx",
    T2G          = "mouse_mm10_optimized_v1/t2g.tsv",
    T3G          = "mouse_mm10_optimized_v1/t2g_3col.tsv"
  )
)


#' Get location specific resources
#'
#' @param location string, name of computing location.
#' @param species string, name of species investigated.
#'
#' @return list of resource locations
#' @export
get_resources <- function(location, species) {
  full_path <- function(x) {
    file.path(
      resource_base[[location]],
      species_specific[[species]][["path_ref"]],
      species_specific[[species]][[x]]
    )
  }
  list(
    salmon_index = full_path("salmon_index"),
    T2G = full_path("T2G"),
    T3G = full_path("T3G"),
    whitelist = file.path(resource_base[[location]], whitelist)
  )
}
