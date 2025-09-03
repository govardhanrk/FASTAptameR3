# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

seqEnrichTab <- tabPanel(
  "Sequence Enrichment",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for 1st input file
      fileInput(
        "enrichInput1",
        label = strong("Input data 1:"),
        multiple = FALSE,
        placeholder = "FASTA file",
        accept = c(".fasta")
      ),
      
      # ask for 2nd input file
      fileInput(
        "enrichInput2",
        label = strong("Input data 2:"),
        multiple = FALSE,
        placeholder = "FASTA file",
        accept = c(".fasta")
      ),
      
      # radiobutton for whether to keep missing sequences
      radioButtons("enrichKeepNA", label = strong("Keep missing sequences?"), choices = c("Yes", "No"), selected = "No", inline = TRUE),
      shinyBS::bsTooltip("enrichKeepNA", "If No, only keep sequences found in all files."),
      
      # start button
      actionButton("enrichStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("enrichDownload", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for enrichment histogram
      radioButtons(
        "enrich_hist_custom",
        label = strong("Adjust default enrichment histogram?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("enrich_hist_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.enrich_hist_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("enrich_hist_xaxis", label = strong("X-axis - represents bins of log2(enrichment) values."), value = "log2(enrichment)"),
        textAreaInput("enrich_hist_yaxis", label = strong("Y-axis - represents unique sequence count."), value = "Unique sequence count"),
        textAreaInput("enrich_hist_title", label = strong("Plot title."), value = "log2E histogram"),
        colourpicker::colourInput("enrich_hist_baroutline", "Color for bar outline.", "black"),
        colourpicker::colourInput("enrich_hist_barfill", "Color for bar fill.", "skyblue")
      ),
      
      # fold-change histogram
      actionButton("fcHistStart", label = h5("log2(enrichment) histogram"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("fcHistStart", "Histogram of fold-changes for every population comparison"),
      shinyBS::bsModal(
        id = "fcHistWindow",
        title = "log2(Enrichment) Histogram",
        trigger = "fcHistStart",
        size = "large",
        shinycssloaders::withSpinner(plotlyOutput("fcHistOutput"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for RPU scatter plot
      radioButtons(
        "enrich_rpuScatter_custom",
        label = strong("Adjust default RPU scatter plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("enrich_rpuScatter_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.enrich_rpuScatter_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("enrich_rpuScatter_xaxis", label = strong("X-axis - represents RPU in 1st population."), value = "RPU.a"),
        textAreaInput("enrich_rpuScatter_yaxis", label = strong("Y-axis - represents RPU in 2nd population."), value = "RPU.b"),
        textAreaInput("enrich_rpuScatter_title", label = strong("Plot title."), value = "RPU.a vs RPU.b"),
        colourpicker::colourInput("enrich_rpuScatter_pointcolour", "Color for points.", "skyblue")
      ),
      
      # RPU scatter plot
      actionButton("rpuScatterStart", label = h5("RPU scatter plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("rpuScatterStart", "Scatter plot of RPUs from each supplied population"),
      shinyBS::bsModal(
        id = "rpuScatterWindow",
        title = "RPU scatter plot",
        trigger = "rpuScatterStart",
        size = "large",
        shinycssloaders::withSpinner(plotlyOutput("rpuScatterOutput"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for RA plot
      radioButtons(
        "enrich_raScatter_custom",
        label = strong("Adjust default RA scatter plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("enrich_raScatter_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.enrich_raScatter_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("enrich_raScatter_xaxis", label = strong("X-axis - represents mean log2(RPU)."), value = "Average log2(RPU)"),
        textAreaInput("enrich_raScatter_yaxis", label = strong("Y-axis - represents fold change."), value = "Fold change"),
        textAreaInput("enrich_raScatter_title", label = strong("Plot title."), value = "Enrichment RA plot"),
        colourpicker::colourInput("enrich_raScatter_pointcolour", "Color for points.", "skyblue")
      ),
      
      # RA plot
      actionButton("raStart", label = h5("RA plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("raStart", "RA plot to compare across input files."),
      shinyBS::bsModal(
        id = "raWindow",
        title = "RA plot",
        trigger = "raStart",
        size = "large",
        shinycssloaders::withSpinner(plotlyOutput("raOutput"))
      )
    ),
    
    # display count output as data.table
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("enrichOutput")))
  )
)
