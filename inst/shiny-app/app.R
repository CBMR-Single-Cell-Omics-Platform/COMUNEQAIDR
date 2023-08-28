#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(rhandsontable)
library(readxl)
library(writexl)
library(yaml)
library(COMUNEQAIDR)

# Define UI for application that draws a histogram
ui <- function(id){
  shinyWidgets::useSweetAlert()
  fluidPage(
    shinyjs::useShinyjs(),
    # Application title
    titlePanel("Metadata to Config"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "metadatafile", label = "Metadata file (.xlsx)"),
        textInput(inputId = "scop_id", label = "SCOP ID", value = ""),
        selectInput(inputId = "location", label = "Location", choices = c("esrum", "computerome")),
        textInput(inputId = "com_id", label = "COMUNEQAID ID", value = ""),
        textInput(inputId = "resource_path", label = "Resource Path", value = "/maps/projects/scop/data/resources"),
        textInput(inputId = "project_path", label = "Project Path", value = "/maps/projects/scop/data"),
        textInput(inputId = "bcl_path", label = "BCL Path", value = "/maps/projects/scop/scratch"),
        textInput(inputId = "qc_path", label = "QC Path", value = "scRNAseq/dry-lab/QC"),
        textInput(inputId = "fastq_path", label = "FASTQ Path", value = "scRNAseq/dry-lab/FASTQ"),
        textInput(inputId = "out_path", label = "Out Path", value = "scRNAseq/dry-lab/PipelineOut"),
        textInput(inputId = "log_path", label = "Log Path", value = "scRNAseq/dry-lab/Log"),
        actionButton(inputId = "make_config", label = "Make Config"),
        downloadButton(outputId = "download_excel", label = "Download Excel"),
        downloadButton(outputId = "download_config", label = "Download Config")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Metadata", tabsetPanel(
            tabPanel("Sample Sheet", rHandsontableOutput("sample_sheet")),
            tabPanel("Sample 2 Reaction", rHandsontableOutput("sample2reaction")),
            tabPanel("Reaction Sheet", rHandsontableOutput("reaction_sheet")),
            tabPanel("Reaction 2 Library", rHandsontableOutput("reaction2library")),
            tabPanel("Library Sheet", rHandsontableOutput("library_sheet")),
            tabPanel("Library 2 Sequencing", rHandsontableOutput("library2sequencing")),
            tabPanel("Sequencing Sheet", rHandsontableOutput("sequencing_sheet"))
          )),
          tabPanel("Config", verbatimTextOutput("config"))
        )
      )
    )
  )
}
# Define server logic required to draw a histogram
server <- function(input, output) {
  shinyjs::disable("download_config")
  shinyjs::disable("download_excel")

  constants <- c("com_id", "scop_id", "bcl_path", "project_path",
                 "qc_path", "fastq_path", "out_path", "log_path")
  names(constants) <- constants
  sheets <- c("sample_sheet", "sample2reaction", "reaction_sheet",
              "reaction2library", "library_sheet", "library2sequencing",
              "sequencing_sheet")
  names(sheets) <- sheets


  config_list <- reactiveValues()
  out_config <- reactiveVal()

  observeEvent(input$metadatafile, {
    req(input$metadatafile$datapath)

    file <- input$metadatafile$datapath
    filetype <- tools::file_ext(file)
    if (filetype == "config" || filetype == "yaml") {
      reader <- parse_config
    } else if (filetype == "xlsx") {
      sheet_names <- excel_sheets(file)
      version <- detect_samplesheet_version(sheet_names)

      reader <- switch(version,
                       v1 = parse_library_sheet_v1,
                       v2 = parse_library_sheet_v2,
                       stop(version, " not implemented")
      )
    } else {
      stop("Only .xlsx and .config/.yaml files are supported")
    }

    config <- reader(file)

    for (i in names(config)) {
      config_list[[i]] <- config[[i]]
    }

    for (i in constants) {
      if (i %in% names(config_list)) {
        updateTextInput(inputId = i, value = config_list[[i]])
      }
    }

    # Creating tables with lapply makes the tables render correctly
    render <- function(x) renderRHandsontable(rhandsontable(config_list[[x]]))
    output_tables <- lapply(sheets, render)
    for (i in names(output_tables)) {
      output[[i]] <- output_tables[[i]]
    }
    shinyjs::enable("download_excel")
    shinyjs::disable("download_config")
  })

  observeEvent(input$location, {
    updateTextInput(inputId = "project_path", value = project_base[[input$location]])
    updateTextInput(inputId = "bcl_path", value = bcl_base[[input$location]])
    updateTextInput(inputId = "resource_path", value = resource_base[[input$location]])
  })

  observeEvent(input$make_config, {
    for (i in constants) {
      config_list[[i]] <- input[[i]]
    }

    species <- guess_species(unique(config_list[["reaction_sheet"]][["align"]]))
    base <- sapply(species_specific[species], `[[`, "path_ref")

    for (i in c("salmon_index", "T2G", "T3G")) {
      path <- sapply(species_specific[species], `[[`, i)
      full_path <- file.path(input$resource_path, base, path)
      names(full_path) <- names(base)
      config_list[[i]] <- as.list(full_path)
    }

    config_list[["whitelist"]] <- file.path(input$resource_path, whitelist)

    config_order <- c(
      "com_id", "scop_id", "bcl_path", "project_path", "qc_path",
      "fastq_path", "out_path", "log_path", "salmon_index", "T2G", "T3G",
      "whitelist", "sample_sheet", "sample2reaction", "reaction_sheet",
      "reaction2library", "library_sheet", "library2sequencing",
      "sequencing_sheet")

    out_config_list <- reactiveValuesToList(config_list)[config_order]
    output$config <- renderText(as.yaml(out_config_list))
    out_config(out_config_list)

    shinyjs::enable("download_config")
  })

  output$download_excel <- downloadHandler(
    filename = function() paste(input[["scop_id"]], "Library_sheet.xlsx", sep = "_"),
    content = function(file) writexl::write_xlsx(x = reactiveValuesToList(config_list)[sheets], path = file)
  )

  output$download_config <- downloadHandler(
    filename = function() paste0(config_list[["com_id"]], ".yaml"),
    content = function(file) yaml::write_yaml(x = out_config(), file = file)
  )

}

# Run the application
shinyApp(ui = ui, server = server)
