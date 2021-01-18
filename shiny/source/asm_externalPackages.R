if("shiny" %in% row.names(installed.packages())==FALSE){
  install.packages("shiny",dependencies=TRUE);
  require("shiny");
} else {
  require(shiny)
}

if("shinyjs" %in% rownames(installed.packages())==FALSE){
  install.packages("shinyjs")
  library(shinyjs)
} else {
  library(shinyjs)
}

if("shinythemes" %in% rownames(installed.packages()) == FALSE){
  install.packages("shinythemes")
  library(shinythemes)
} else {
  library(shinythemes)
}

if("data.table" %in% rownames(installed.packages()) == FALSE) {
  install.packages("data.table")
  library(data.table)
} else {
  library(data.table)
}

if("devtools" %in% row.names(installed.packages())==FALSE){
  install.packages("devtools",dependencies=TRUE)
  library(devtools)
} else{
  library(devtools)
}

if("DT" %in% row.names(installed.packages())==FALSE){
  devtools::install_github('rstudio/DT');
  library(DT);
} else {
  library(DT);
}

if("httr" %in% row.names(installed.packages())==FALSE){
  install.packages("httr",dependecies=TRUE);
  require("httr")
} else {
  require("httr")
}

if("plotly" %in% row.names(installed.packages())==FALSE){
  install.packages("plotly",dependencies=TRUE);
  require("plotly")
} else {
  require("plotly")
}

if("ggplot2" %in% row.names(installed.packages())==FALSE){
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}