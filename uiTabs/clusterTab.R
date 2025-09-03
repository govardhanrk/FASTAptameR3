# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

# source the sub-tabs
source("./uiTabs/clusterLEDTab.R")
source("./uiTabs/clusterDiversityTab.R")
source("./uiTabs/clusterMSATab.R")
source("./uiTabs/clusterPHMMTab.R")
source("./uiTabs/reclusterTab.R")
source("./uiTabs/posEnrichTab.R")

# merge the sub-tabs
clusterTab <- tabPanel(
  "Cluster",
  tabsetPanel(
    clusterLEDTab,
    clusterDiversityTab,
    clusterMSATab,
    clusterPHMMTab,
    reclusterTab,
    posEnrichTab,
    type = "pills"
  )
)
