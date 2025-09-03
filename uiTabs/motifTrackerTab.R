# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

motifTrackerTab <- tabPanel(
  "Tracker",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input files
      shinyBS::tipify(
        fileInput(
          "motifTrackerInput",
          label = strong("Input data:"),
          multiple = TRUE,
          placeholder = "FASTA files",
          accept = c('.fasta')
        ),
        "Holding ctrl (Windows) or command (Mac) will allow you to click multiple files."
      ),
      
      # reorder files because fileInput keeps them in alphabetical order
      selectizeInput("motifTracker_selectInput", label = strong("Select file order."), choices = "*", multiple = TRUE),
      
      # ask for motif
      textAreaInput("motifInput_query", label = strong("Motif or sequence list:"), height = '100px'),
      shinyBS::bsTooltip("motifInput_query", "Please supply one motif or sequence of interest per line."),
      
      # ask for aliases
      textAreaInput("motifInput_alias", label = strong("Alias list (1 per line):"), height = '100px'),
      shinyBS::bsTooltip("motifInput_alias", "User-defined IDs for queries."),
      
      # radio buttons for nt or aa searching
      radioButtons(
        "motifTrackerButton_queryType",
        label = strong("Search for motifs or whole sequences?"),
        choices = c("Motif", "Sequence"),
        selected = "Motif",
        inline = TRUE
      ),
      shinyBS::bsTooltip("motifTrackerButton_queryType", "Are you searching for motifs or sequences?"),
      
      conditionalPanel(
        condition = "input.motifTrackerButton_queryType == 'Motif'",
        
        # radio buttons for nt or aa searching
        radioButtons(
          "motifTrackerButton_motifType",
          label = strong("Type of pattern?"),
          choices = c("Nucleotide", "AminoAcid", "String"),
          selected = "Nucleotide",
          inline = TRUE
        ),
        shinyBS::bsTooltip("motifTrackerButton_motifType", "Nucleotide option considers degenerate codes; other options do not.")
      ),
      
      # start button
      actionButton("motifTrackerStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button for summary
      downloadButton("motifTrackerDownload_summary", label = h5("Download summary"), style='padding:2px; font-size:80%'),
      
      # download button for enrich
      downloadButton("motifTrackerDownload_enrich", label = h5("Download enrichment"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for line plot
      radioButtons(
        "motifTracker_line_custom",
        label = strong("Adjust default line plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("motifTracker_line_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.motifTracker_line_custom == 'Yes'",
        
        # text input for x-axis, y-axis, and plot title
        textAreaInput("motifTracker_line_xaxis", label = strong("X-axis - represents uploaded populations."), value = "Population"),
        textAreaInput("motifTracker_line_yaxis", label = strong("Y-axis - represents percentage for motifs or total RPU for sequences."), value = "Percentage / RPU"),
        textAreaInput("motifTracker_line_title", label = strong("Plot title."), value = "Motif / sequence tracker"),
        
        # select input for colour palette
        selectInput(
          "motifTracker_line_palette",
          label = strong("Color palette."),
          choices = c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2"),
          selected = "Dark2",
          multiple = FALSE
        )
      ),
      
      # start button for motif tracker plot
      actionButton("motifTracker_plotStart", label = h5("Tracker plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("motifTracker_plotStart", "Shows a line plot of motif percentage / sequence RPU per population."),
      shinyBS::bsModal(
        id = "motifTracker_plotWindow",
        title = "Tracker plot",
        trigger = "motifTracker_plotStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("motifTrackerPlot", height = "650px"))
      ),
    ),
    
    # display distance output as datatable
    mainPanel(
      shinycssloaders::withSpinner(DT::dataTableOutput("motifTrackerOutput")),
      tags$hr(style="border-color: black;"),
      shinycssloaders::withSpinner(DT::dataTableOutput("motifTrackerOutput_enrichmentTable"))
    )
  )
)
