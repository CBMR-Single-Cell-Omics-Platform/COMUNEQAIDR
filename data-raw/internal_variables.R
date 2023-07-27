project_base <- list(
  computerome = "/home/projects/ku_00016/data",
  esrum = "/maps/projects/scop/data"
)

bcl_base <- list(
  computerome = "/home/projects/ku_00016/scratch",
  esrum = "/maps/projects/scop/scratch"
)

resource_base <- list(
  computerome = "/home/projects/ku_00016/data/resources",
  esrum = "/maps/projects/scop/data/resources"
)

whitelist = "non-species-specific/whitelist/3M-february-2018.txt"

species_specific <- list(
  "mouse" = list(
    path_ref     = "mus-musculus/sc-ref/mouse_splici",
    salmon_index = "af_tutorial_splici/mm10_splici_idx",
    T2G          = "refdata-gex-mm10-2020-A/t2g.tsv",
    T3G          = "refdata-gex-mm10-2020-A/t2g_3col.tsv"
  ),
  "human" = list(
    path_ref     = "homo-sapiens/sc-ref/human_splici",
    salmon_index = "af_tutorial_splici/grch38_splici_idx",
    T2G          = "refdata-gex-GRCh38-2020-A/t2g.tsv",
    T3G          = "refdata-gex-GRCh38-2020-A/t2g_3col.tsv"
  ),
  "rat" = list(
    path_ref     = "rattus-norvegicus/sc-ref/rat_splici",
    salmon_index = "af_tutorial_splici/rnor_splici_idx",
    T2G          = "Rnor_6.0/t2g.tsv",
    T3G          = "Rnor_6.0/t2g_3col.tsv"
  ),
  "rhesus" = list(
    path_ref     = "macaca-mulatta/sc-ref/rhesus_splici",
    salmon_index = "af_tutorial_splici/mmul_10_splici_idx",
    T2G          = "Mmul_10/t2g.tsv",
    T3G          = "Mmul_10/t2g_3col.tsv"
  ),
  "unknown" = list(
    path_ref     = "",
    salmon_index = "",
    T2G          = "",
    T3G          = ""
  )
)


usethis::use_data(
  project_base,
  bcl_base,
  resource_base,
  whitelist,
  species_specific,
  overwrite = TRUE
)
