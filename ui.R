shinyUI(
  navbarPage("Unsupervised Machine Learning"
    ,collapsable = TRUE
    ,tabPanel("Feature Investigation"
            ,fluidPage(
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "../css/custom.css"))
                ,theme = shinytheme("flatly")
                ,sidebarLayout(
                  sidebarPanel(
                    conditionalPanel(
                      condition = "input.tab_selected2 == 10 || input.tab_selected2 == 30"
                     ,h3("Filter Occupancy Data ")
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
                   ,h3("Assess Feature Distribution")
                   ,hr()
                   ,uiOutput("col")
                  )
                )
                ,mainPanel(
                  tabsetPanel(
                    tabPanel("Check Correlation", value = 10, plotlyOutput("heat"), hr(), plotlyOutput("scatterplot"))
                   ,tabPanel("Check Distributions", value = 20, plotlyOutput("dist_plot"))
                   ,tabPanel("View Data", value = 30, dataTableOutput("cluster_data"))
                   ,id = "tab_selected2"
                  )
                )
              )
          )
        )
    ,navbarMenu("Hierarchical Clustering"
      ,tabPanel("Select Clusters"
               ,fluidPage(
                 tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "../css/custom.css"))
                 ,theme = shinytheme("flatly")
                 ,sidebarLayout(
                   sidebarPanel(
                     selectInput(inputId = "clustering_method", label = "Select Hierarchical Clustering method",choices = c("complete","average","ward.D"),selected = "average")
                     ,selectInput(inputId = "dist_method",label = "Choose the Type of Distance Calculation" ,choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), selected = "euclidean")
                     ,actionButton(inputId = "do_cluster",label = "Cluster")
                     ,hr()
                     ,p("Choose Ranges of Clusters to Check:")
                     ,fluidRow(div(style="display:inline-block",textInput(inputId = "cluster_start", label = "START:", value = 1))
                               ,div(style = "display:inline-block",textInput(inputId = "cluster_end", label = "END:", value = 12))
                     )
                     ,actionButton(inputId ="do_scree",label = "Update Scree Plot")
                     ,hr()
                     ,div(style = "display:inline-block",textInput(inputId = "hist_tree",label = "Enter Number of Clusters for Histogram:"))
                     ,div(style = "display:inline-block",actionButton(inputId = "do_histogram",label = "Update Histogram"))
                    )
                   ,mainPanel(
                     tabsetPanel(
                       tabPanel("Occupancy Clustering"
                                ,h3("Cluster Selection", align = "center")
                                ,hr()
                                ,splitLayout(cellWidths = c("50%", "50%")
                                              ,highchartOutput("scree_plot", height = 500)
                                              ,highchartOutput("hist_plot", height = 500)
                                              )
                                )
                       )
                     )
                   )
                 )
              )
      ,tabPanel("Clustering Results"
               ,fluidPage(theme = shinytheme("flatly")
                          ,sidebarLayout(
                            sidebarPanel(
                              conditionalPanel(condition = "input.tab_selected == 1 || input.tab_selected == 2 || input.tab_selected == 3"
                                    ,uiOutput("cSel")
                                    ,checkboxInput(inputId = "show_hist",label = "Show Distribution of Clusters",value = FALSE )
                                    ,br()
                                    ,checkboxInput(inputId = "show_rename",label = "Rename Cluster Descriptions",value = FALSE)
                                    ,uiOutput("looped_input")
                                    ,br()
                                    ,downloadButton(outputId ="download_means" ,label = "Download Cluster Averages CSV  ")
                                    ,hr()
                                    ,downloadButton(outputId ="download_members" ,label = "Download Cluster Members CSV")
                                    )
                              )
                 ,mainPanel(
                   tabsetPanel(
                     tabPanel("Profiling Averages", value = 1
                             ,h3("Radar Chart of Cluster Averages", align = "center")
                             ,uiOutput("hidden_hist")
                             )
                     ,tabPanel("Profiling Over Time", value = 2
                              ,h3("Cluster Distribution Over Time", align = "center")
                              ,highchartOutput("stacked_plot")
                     )
                     ,tabPanel("Profiling Across Features", value = 3
                               ,h3("Parallel Coordinates Plot over features", align = "center")
                               ,parcoordsOutput("parcoor", height = 480)
                     )
                     ,id = "tab_selected"
                     )
                   )

               )
            )
      )
    )
  )
)
