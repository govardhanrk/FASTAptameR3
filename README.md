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
- DOCKER_GUIDE.md: step-by-step Docker instructions (Windows/macOS)

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

Run app in you local:
1. Modify the project_path in launch.R to your own local path （project_path <- "C:/Users/yqzn9/Documents/GitHub/FASTAptameR3" ）
2. Then run script: launch.R 

Prefer a no-install experience? Run the app in Docker.

- Quick start guide: see `DOCKER_GUIDE.md` (Windows & macOS)
- Published image: `yongfangqin/fastaptamer3:latest`