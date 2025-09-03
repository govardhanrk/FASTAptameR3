High-level file details are in the commit messages. In brief:
- R/: functions for R package
- man/: manuals for R functions
- serverFunctions/: code for Shiny app server
- trans/: DEPRECATED; holds JSON files for alternative genetic codes
- uiTabs/: code for Shiny app user interface
- www/: change log between FA2 and FA3, FA2 logo, FA2 favicon
- app.R: links UI and server
- LICENSE.txt: FA2 license
- UserGuide.pdf: FA2 user guide

TODO:
- Deploy R package to GitHub (easiest option), CRAN (longest wait), or BioConductor (needs full rewrite to be compatible)
- Write package vignettes
- Call R package from server
- Develop unit tests for application stability
- Dockerize Shiny application; not sure where this will be hosted
- Document all FA3 updates in user guide

Final documentation:
1. R manual for users based in R; explicitly documents the codebase, including all function arguments, outputs, etc.
2. R vignettes for users based in R; shows examples of how to use the codebase, interpret outputs, etc.
3. User guide for users based in Docker or the web app; shows examples of how to use the user interface
