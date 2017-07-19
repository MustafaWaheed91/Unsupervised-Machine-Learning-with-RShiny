################################################################################################
################################################################################################
###############################   Exploring Root Cause Analysis     ############################
###############################      Author: Mustafa Waheed         ############################
################################################################################################
################################################################################################
require(shiny)
require(shinyjs)
require(shinyBS)
require(shinyAce)
require(sqldf)
require(DT)
require(fmsb)
require(cluster)
require(pdist)
require(doParallel)
require(foreach)
require(MASS)
require(shinythemes)
require(plotly)
require(devtools)
require(highcharter)
require(ggvis)
require(shinydashboard)
require(stringr)
require(rhandsontable)
require(rpart)
require(ppls)
require(data.table)
require(reshape2)

if(!"parcoords" %in% installed.packages()) devtools::install_github("timelyportfolio/parcoords")
library(parcoords)