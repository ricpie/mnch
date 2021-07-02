if (!require("pacman")) install.packages("pacman")

devtools::install_github("cardiomoon/ggiraphExtra")

pacman::p_load(lubridate,plyr,dplyr,reshape2,devtools,shiny,shinydashboard,dygraphs,DT,shinyjs,tools,data.table,writexl,zoo,readxl
,gmailr,mailR,cronR,miniUI,shinyFiles,ggplot2,stringr,chron,doParallel,foreach,openxlsx,gridExtra,egg,cowplot,corrgram,
factoextra,scales,htmlwidgets,tidyfast,ggmap,leaflet,plotly,tidyverse,skimr,janitor,ggalt,car,ggiraphExtra,GGally)


getmode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

registerDoParallel(cores=detectCores()-2)

