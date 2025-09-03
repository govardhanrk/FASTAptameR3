# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

motifOmitTab <- tabPanel(
  "Omit",
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "motifOmitInput",
        label = strong("Input data:"),
        multiple = FALSE,
        placeholder = "FASTA file",
        accept = c('.fasta')
      ),
      
      # ask for motif
      textInput("motifInput_omit", label = strong("Comma-separated patterns:")),
      shinyBS::bsTooltip("motifInput_omit", "E.g., aaa,gtg"),
      
      # radio buttons for type of filtering
      radioButtons(
        "motifOmitButton_partial",
        label = strong("If multiple patterns, return partial matches?"),
        choices = c("Yes","No"),
        selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("motifOmitButton_partial", "'Yes' is an OR operation, whereas 'No' is an AND operation."),
      
      # radio buttons for nt or aa searching
      radioButtons(
        "motifOmitButton_motifType",
        label = strong("Type of pattern?"),
        choices = c("Nucleotide","AminoAcid", "String"),
        selected = "Nucleotide",
        inline = TRUE
      ),
      shinyBS::bsTooltip("motifOmitButton_motifType", "Nucleotide option considers degenerate codes; other options do not."),
      
      # select file type for download
      radioButtons(
        "motifOmitDownloadType",
        label = strong("FASTA or CSV download?"),
        choices = c("FASTA", "CSV"),
        selected = "FASTA",
        inline = TRUE
      ),
      shinyBS::bsTooltip("motifOmitDownloadType", "FASTA is required for subsequent modules; CSV retains all features from data table"),
      
      # start button
      actionButton("motifOmitStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("motifOmitDownload", label = h5("Download"), style='padding:2px; font-size:80%')
    ),
    
    # display distance output as datatable
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("motifOmitOutput")))
  )
)

