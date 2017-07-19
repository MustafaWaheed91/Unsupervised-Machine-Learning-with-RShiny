shinyUI(
  navbarPage("Unsupervised Machine Learning App"
    ,collapsable = TRUE
    ,tabPanel("Feature Investigation"
            ,fluidPage(
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "../css/custom.css"))
                ,theme = shinytheme("flatly")
                ,sidebarLayout(
                  sidebarPanel(
                    conditionalPanel(
                      condition = "input.tab_selected2 == 10 || input.tab_selected2 == 30"
                     ,h3(strong("Filter Occupancy Data "))
                     ,hr()
                     ,selectInput(inputId = "selected_cols", label = "Choose Numeric features", choices = names(sample_data[,-1]), selected = names(sample_data[,-1]), multiple = TRUE)
                     ,hr()
                     ,dateRangeInput(
                       inputId = "selected_drange", label =  "Choose Date Range", start = min(sample_data$date), end =  max(sample_data$date)
                       ,min = min(sample_data$date), max =  max(sample_data$date)
                      )
                     ,hr()
                     ,actionButton(inputId = "do_corr", label = "Update Correlation Plot")
                    )
                  ,conditionalPanel(
                    condition = "input.tab_selected2 == 20"
                   ,h3(strong("Assess Feature Distribution"))
                   ,hr()
                   ,uiOutput("col")
                  )
                )
                ,mainPanel(
                  tabsetPanel(
                    tabPanel("Check Correlation", value = 10, plotlyOutput("heat"), hr(), plotlyOutput("scatterplot"))
                   ,tabPanel("Check Distributions", value = 20, plotlyOutput("dist_plot"))
                   ,tabPanel("View Data", value = 30, dataTableOutput("cluster_data"))
                   ,id = "tab_selected2")
                )
              )
            )
          )
    # ,navbarMenu("Hierarchical Clustering"
    #   ,tabPanel("Select Clusters"
    #            ,fluidPage(
    #              tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "../css/custom.css"))
    #              ,theme = shinytheme("flatly")
    #              ,sidebarLayout(
    #              sidebarPanel(
    #                selectInput(inputId = "clustering_method", label = "Select Hierarchical Clustering method",choices = c("complete","average","ward.D"),selected = "ward.D")
    #                ,selectInput(inputId = "dist_method",label = "Choose the Type of Distance Calculation" ,choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), selected = "euclidean")
    #                ,actionButton(inputId = "do_cluster",label = "Cluster")
    #                ,hr()
    #                ,p("Choose Ranges of Clusters to Check:")
    #                ,div(style="display:inline-block",textInput(inputId = "cluster_start", label = "START:", value = 1))
    #                ,div(style = "display:inline-block",textInput(inputId = "cluster_end", label = "END:", value = 14))
    #                ,actionButton(inputId ="do_scree",label = "Update Scree Plot")
    #                ,hr()
    #                ,div(style = "display:inline-block",textInput(inputId = "hist_tree",label = "Enter Number of Clusters for Histogram:"))
    #                ,div(style = "display:inline-block",actionButton(inputId = "do_histogram",label = "Update Histogram"))
    #              ),
    #              mainPanel( tabsetPanel(tabPanel("EDD Clustering"
    #                                              ,h3(strong("Cluster Selection"), align = "center")
    #                                              ,fluidRow(splitLayout(cellWidths = c("50%", "50%"),highchartOutput("scree_plot"),highchartOutput("hist_plot")))
    #                                              ,hr()
    #                                              ,h3(strong("Size of Prize"), align = "center")
    #                                              ,div(style="display:inline-block",numericInput("edd_miss", "Overall EDD Missed %",value = 2))
    #                                              ,rHandsontableOutput("prize_size")
    #              )
    #              ))
    #            ))
    #   )
    #   ,tabPanel("Clustering Results"
    #            ,fluidPage(theme = shinytheme("flatly"),sidebarLayout(
    #              sidebarPanel(
    #                conditionalPanel(condition = "input.tab_selected == 1"
    #                                 ,uiOutput("cSel")
    #                                 # ,selectInput(inputId = "select_radar_clus",label = "Select Clusters to Display:", choices = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),multiple = T,selected =c(1,2,3,4))
    #                                 ,checkboxInput(inputId = "show_hist",label = "Show Distribution of Clusters",value = FALSE )
    #                                 ,br()
    #                                 ,checkboxInput(inputId = "show_rename",label = "Rename Cluster Descriptions",value = FALSE)
    #                                 ,uiOutput("looped_input")
    #                                 ,br()
    #                                 ,downloadButton(outputId ="download_means" ,label = "Download Cluster Averages CSV  ")
    #                                 ,hr()
    #                                 ,downloadButton(outputId ="download_members" ,label = "Download Cluster Members CSV")
    #                )
    #                ,conditionalPanel(condition = "input.tab_selected == 3"
    #                                  ,h4(strong("Root Cause Clusters were defined on:"))
    #                                  ,uiOutput("train_data")
    #                                  ,uiOutput("train_features")
    #                                  ,hr()
    #                                  ,checkboxInput(inputId = "override",label = "Check to Override Selected Data ",value = FALSE )
    #                                  ,uiOutput("show_upload")
    #                                  ,hr()
    #                                  ,uiOutput("select_dt_range")
    #                                  ,hr()
    #                                  ,radioButtons(inputId = "assign_method",label = "Choose Method to Assign Clusters:",choices = c("Classification Tree","Average Distance"))
    #                                  ,actionButton(inputId = "do_assign",label = "Assign Clusters To New Season Data")
    #                                  ,checkboxInput(inputId = "add_plot",label ="Show Additional Plots", value = FALSE)
    #                                  ,hr()
    #                                  ,downloadButton(outputId ="download_ass" ,label = "Download New Cluster Assignments CSV")
    #                )
    #              ),
    #              mainPanel( tabsetPanel(tabPanel("Cluster Profiling",value = 1
    #                                              ,h3(strong("Cluster Distribution Over Time"), align = "center")
    #                                              ,highchartOutput("stacked_plot")
    #                                              ,hr()
    #                                              ,h3(strong("Cluster Averages"), align = "center")
    #                                              ,uiOutput("hidden_hist")
    #              )
    #              ,tabPanel("Assign Clusters to Orders",value = 3
    #                        ,h3(strong("Assigned Cluster Distribution Over Time"), align = "center")
    #                        ,highchartOutput("stacked_plot2")
    #                        ,hr()
    #                        ,uiOutput("show_assign_plots")
    #
    #              )
    #              ,id = "tab_selected"
    #              ))
    #            ))
    #   )
    )
  )
