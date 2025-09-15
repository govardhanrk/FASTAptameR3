## === Configure your project path (use forward slashes / or double backslashes \\) ===
project_path <- "C:/Users/yqzn9/Documents/GitHub/FASTAptameR3" 

## === Basic setup ===
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")

options(install.packages.check.source = "no")
options(BioC_mirror = "https://bioconductor.org")

## === Install dependencies via DESCRIPTION ===
remotes::install_deps(project_path, dependencies = TRUE, upgrade = "never")

## === Parse DESCRIPTION to ensure all dependencies are installed ===
desc_file <- file.path(project_path, "DESCRIPTION")
stopifnot(file.exists(desc_file))
desc <- read.dcf(desc_file)

get_pkgs <- function(field) {
  x <- desc[, field, drop = TRUE]
  if (is.na(x)) return(character())
  x <- gsub("\\n", " ", x)
  x <- unlist(strsplit(x, ","))
  x <- trimws(gsub("\\s*\\(.*?\\)", "", x))
  x[x != "" & x != "R"]
}

deps <- unique(c(get_pkgs("Imports"), get_pkgs("Depends")))

## === Check and install missing packages ===
inst <- rownames(installed.packages())
missing <- setdiff(deps, inst)

if (length(missing)) {
  repos_all <- c(getOption("repos"), BiocManager::repositories())
  ap_all <- available.packages(repos = repos_all)
  is_available <- missing %in% rownames(ap_all)
  
  if (any(!is_available)) {
    message("These packages were not found in standard repositories: ",
            paste(missing[!is_available], collapse = ", "))
  }
  
  ap_bioc <- available.packages(repos = BiocManager::repositories())
  bioc_pkgs <- intersect(missing, rownames(ap_bioc))
  cran_pkgs <- setdiff(intersect(missing, rownames(ap_all)), bioc_pkgs)
  
  if (length(bioc_pkgs)) BiocManager::install(bioc_pkgs, update = FALSE, ask = FALSE)
  if (length(cran_pkgs)) install.packages(cran_pkgs, repos = getOption("repos"), Ncpus = max(1, parallel::detectCores()-1))
}

## === Ensure key Bioconductor packages are installed ===
must_bioc <- c("msa","Biostrings","edgeR")
still_missing <- setdiff(must_bioc, rownames(installed.packages()))
if (length(still_missing)) BiocManager::install(still_missing, update = FALSE, ask = FALSE)

## === Verify all packages can be loaded ===
need <- unique(c(deps, must_bioc))
failed <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(failed)) stop("These dependencies failed to load: ", paste(failed, collapse = ", "))

## === Launch Shiny app ===
app_file <- file.path(project_path, "app.R")
txt <- readLines(app_file, warn = FALSE)
txt <- gsub('source\\("\\./functions/', 'source("./R/', txt)  # global replace
writeLines(txt, app_file)

# launch from folder so relative paths resolve
options(shiny.maxRequestSize = 2000*1024^2)
future::plan("multisession")
shiny::runApp(project_path, launch.browser = TRUE)
