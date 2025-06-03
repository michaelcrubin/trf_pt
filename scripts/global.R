
## --------------- LIBRARIES ------------------
# renv::status()
# renv::init(repos = "https://cran.r-project.org")
# renv::init()
# renv::snapshot()
# Install the RnData package from the GitLab repository
# install.packages("remotes")
# remotes::install_git("http://gitlab11.hagel.local/rnd/rnd_packages/RnData.git")
# 

# options(pkgType = "binary")
# remotes::install_git("http://gitlab11.hagel.local/rnd/rnd_packages/RnData.git", upgrade = "never")
library(brms)
# renv::init()
# renv::hydrate()
# renv::snapshot()
library(here)
library(magrittr)
library(dplyr)
#library(RnData)
library(tidyr)
library(purrr)
library(jsonlite)
library(sf)
library(ggplot2)
library(jsonlite)
library(plotly)
library(readr)
library(ggplot2)
library(patchwork)
library(flextable)

# output_path <- here("DB", "cache")
# SET_storage_env(user_path = output_path)
# Sys.getenv("env_ok")
# # Set up the storage environment. Must be a local
# Sys.getenv("mm_user") # should return schweizer_hagel
# GET_mm_stats() # should return a dataframe with some statisics about the Meteomatics API
# 
