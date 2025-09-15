# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## imports
library(shiny)
library(shinyBS)
library(colourpicker)
library(shinyalert)
library(DT)
library(shinycssloaders)

library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(plotly)
library(UpSetR)
library(msa)
library(LaF)
library(janitor)

library(future)
library(promises)
plan(multisession)

## source files for support functions
source("./R/functions_support.R")

## source files for analytical functions
source("./R/preprocess_analyses.R")
source("./R/count_analyses.R")
source("./R/recount_analyses.R")
source("./R/translate_analyses.R")
source("./R/motif_support.R")
source("./R/motifSearch_analyses.R")
source("./R/motifOmit_analyses.R")
source("./R/motifTracker_analyses.R")
source("./R/motifDiscovery_analyses.R")
source("./R/mutationNetwork_analyses.R")
source("./R/distance_analyses.R")
source("./R/dataMerge_analyses.R")
source("./R/seqEnrich_analyses.R")
source("./R/edgeR_analyses.R")
source("./R/cluster_analyses.R")
source("./R/clusterDiversity_analyses.R")
source("./R/clusterMSA_analyses.R")
source("./R/clusterPHMM_analyses.R")
source("./R/recluster_analyses.R")
source("./R/posEnrich_analyses.R")

## source files for visualization functions
source("./R/count_plots.R")
source("./R/motifTracker_plots.R")
source("./R/motifDiscovery_plots.R")
source("./R/distance_plots.R")
source("./R/dataMerge_plots.R")
source("./R/seqEnrich_plots.R")
source("./R/posEnrich_plots.R")
source("./R/edgeR_plots.R")
source("./R/clusterDiversity_plots.R")
source("./R/clusterMSA_plots.R")
source("./R/recluster_plots.R")
source("./R/posEnrich_plots.R")

## source files for tabs
source("./uiTabs/startTab.R")
source("./uiTabs/translateTab.R")
source("./uiTabs/motifTab.R")
source("./uiTabs/distanceTab.R")
source("./uiTabs/mutationNetworkTab.R")
source("./uiTabs/dataMergeTab.R")
source("./uiTabs/seqEnrichTab.R")
source("./uiTabs/diffAnalysisTab.R")
source("./uiTabs/clusterTab.R")
source("./uiTabs/aboutTab.R")

## source files for server
source("./serverFunctions/inputChecks.R")
source("./serverFunctions/preprocessServer.R")
source("./serverFunctions/countServer.R")
source("./serverFunctions/recountServer.R")
source("./serverFunctions/translateServer.R")
source("./serverFunctions/motifSearchServer.R")
source("./serverFunctions/motifOmitServer.R")
source("./serverFunctions/motifTrackerServer.R")
source("./serverFunctions/motifDiscoveryServer.R")
source("./serverFunctions/distanceServer.R")
source("./serverFunctions/mutationNetworkServer.R")
source("./serverFunctions/dataMergeServer.R")
source("./serverFunctions/seqEnrichServer.R")
source("./serverFunctions/diffAnalysisServer.R")
source("./serverFunctions/clusterLEDServer.R")
source("./serverFunctions/clusterDiversityServer.R")
source("./serverFunctions/clusterMSAServer.R")
source("./serverFunctions/clusterPHMMServer.R")
source("./serverFunctions/reclusterServer.R")
source("./serverFunctions/posEnrichServer.R")
source("./serverFunctions/aboutServer.R")

## change limit for file sizes
options(shiny.maxRequestSize=2000*1024^2)

## sanitize error messages
options(shiny.sanitize.errors = TRUE)

## define ui
ui <- navbarPage(
  "FASTAptameR 3.0",
  
  ## application theme
  theme = shinythemes::shinytheme("cosmo"),
  
  ## UI tabs
  startTab,
  translateTab,
  motifTab,
  mutationNetworkTab,
  distanceTab,
  dataMergeTab,
  seqEnrichTab,
  diffAnalysisTab,
  clusterTab,
  aboutTab,
  
  ## favicon
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  
  ## source HTML code for GA if file is found (only found if running through web app)
  if(file.exists("google-analytics.html")){
    tags$head(includeHTML(("google-analytics.html")))
  }
)

server <- function(input, output, session) {
  
  # content of the shiny alert
  shinyalert::shinyalert(
    "Welcome to FASTAptameR 3.0!",
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
    type = "info"
  )
  
  # sub-server functions
  preprocessServer(input, output, session)
  countServer(input, output, session)
  recountServer(input, output, session)
  translateServer(input, output, session)
  motifSearchServer(input, output, session)
  motifOmitServer(input, output, session)
  motifTrackerServer(input, output, session)
  motifDiscoveryServer(input, output, session)
  distanceServer(input, output, session)
  mutationNetworkServer(input, output, session)
  dataMergeServer(input, output, session)
  seqEnrichServer(input, output, session)
  diffAnalysisServer(input, output, session)
  clusterLEDServer(input, output, session)
  clusterDiversityServer(input, output, session)
  clusterMSAServer(input, output, session)
  clusterPHMMServer(input, output, session)
  reclusterServer(input, output, session)
  posEnrichServer(input, output, session)
  aboutServer(input, output, session)
  
  # close connection when session ends
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
