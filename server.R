shinyServer(
  function(input, output) {

   ### PAGE 1 TAB 1

    key_data1 <- reactive({
      sample_data[sample_data$date >= input$selected_drange[1] &  sample_data$date <= input$selected_drange[2] , 1]
    })

    sample_data1 <- reactive({
      sample_data[ sample_data$date >= input$selected_drange[1] &  sample_data$date <= input$selected_drange[2] , input$selected_cols ]
    })

    output$heat <- renderPlotly({
      input$do_corr
      if(input$do_corr == 0){
        return()
      } else{
        isolate({
          ################ This is the only place I need to add data for this plot
          df1 <- sample_data1()
          df1 <- df1[,order(colnames(df1))]
          correlation <- round(cor(df1),2)
          nms <- names(df1)
          #########################################################
          f2 <- list(family = "Old Standard TT, serif",size = 6,color = "lightgrey")
          a <- list(title = "",showticklabels = TRUE,tickangle = 85,tickfont = f2,exponentformat = "E")
          m <- list(l = 180,b = 80)
          plot_ly(x = nms, y = nms, z = correlation
                  ,zmax = 1 ,zmin = -1
                  ,colors = colorRamp(c("#FE5432","#36D7B7","#FE5432"))
                  ,key = correlation
                  , type = "heatmap", source = "CORR_MATRIX") %>%
            layout(xaxis = list(title = ""),
                   yaxis = list(title = ""),margin = m)

        })
      }

    })

    output$scatterplot <- renderPlotly({
        s <- event_data("plotly_click", source = "CORR_MATRIX")

        if (length(s)) {
          vars <- c(s[["x"]], s[["y"]])
          ################ This is the only place I need to add data
          df1 <- sample_data1()
          df1 <- df1[,order(colnames(df1))]
          d <- setNames(df1[vars], c("x", "y"))
          #########################################################
          yhat <- fitted(lm(y ~ x, data = d))
          plot_ly(d, x = ~ x) %>%
            add_markers(y = ~ y) %>%
            add_lines(y = ~ yhat) %>%
            layout(xaxis = list(title = s[["x"]]),
                   yaxis = list(title = s[["y"]]),
                   showlegend = FALSE)

        } else {
          plotly_empty()
        }

    })




    #### PAGE 1 TAB 2


    output$col <- renderUI({
      df <- sample_data1()
        nm <- names(df)
      selectInput(inputId = "feature_col",label = "Choose features to Check Distribution"
                  ,choices = nm, multiple = F)
    })


    output$dist_plot <- renderPlotly({
      df <- sample_data1()
      x <- df[,input$feature_col]
      fit <- density(x)
      plt <- plot_ly(x = x) %>%
        add_histogram(name = "Histogram") %>%
        add_lines(x = fit$x, y = fit$y ,mode = "lines", yaxis = "y2", fill = "tozeroy", name = "Density") %>%
        layout(
          xaxis = list(title = "Value",showgrid = F),
          yaxis = list( title = "Number of Records",showgrid = F),
          yaxis2 = list(overlaying = "y",  title = "",
                        zeroline = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE)
        )

      plt
    })






    ### PAGE 1 TAB 3

    output$cluster_data <- renderDataTable({
      Date <- key_data1()
      as.data.frame(cbind(Date, sample_data1()))
      },options = list(pageLength = 10, scrollX=TRUE)
      )








    ### PAGE 2 TAB 1


    clust_result <- reactive({
      input$do_cluster
      if(input$do_cluster == 0){
        return()
        } else {
        isolate({
          x <- standardizeData(sample_data1())
          d <- dist(x, method = as.character(input$dist_method))
          Sys.sleep(1)
          clust_result <- hclust(d,method = as.character(input$clustering_method))
          clust_result
        })

      }
    })

    output$scree_plot <- renderHighchart({
      input$do_scree
      withProgress(message = 'Plotting In Progress ...', value = 5, {
      if(input$do_scree == 0 | input$do_cluster == 0){
        return()
      }else{
        isolate({
          incProgress(amount = 0.1, message = NULL, detail = NULL, session = getDefaultReactiveDomain())
          plt <- updateScreePlot(start = input$cluster_start, end = input$cluster_end, cl = clust_result(), data = sample_data1())
          plt
        })
      }
      })
    })

    memberSet <- reactive({
      memberSet <- clusterMembers(clust_result(),sample_data1(),tree_num = input$hist_tree)
      as.data.frame(memberSet)
    })

    output$hist_plot <- renderHighchart({
      input$do_histogram
      if(input$do_histogram==0){
        return()
      }else{isolate({
        histo <- updateHistogram(memberSet(), "% of Records")
        histo
      })
      }

    })



    ### PAGE 3 TAB 1


    output$cSel <- renderUI({
      selectInput(inputId = "select_radar_clus", label = "Select Clusters to Display:"
                  , choices = 1:input$hist_tree, multiple = T, selected = 1:input$hist_tree)
    })

    tabMembers <- reactive({
      kk <- memberSet()
      kk$order_date <- key_data1()
      return(kk)
    })

    output$stacked_plot <- renderHighchart({
      updateStacked(tabMembers(),input$select_radar_clus)
    })

    means <- reactive({
      means <- clusterMeans(hc.c = clust_result(),c2dc_trimmed2 = sample_data1(),tree_num = input$hist_tree)$org_means
      as.data.frame(means)
    })

    norm_means <- reactive({
      l <- means()[,-1]
      ll <- apply(l,2,normalize.vector)
      Group.1 <- means()[,1]
      out <- cbind(Group.1,as.data.frame(ll))
      as.data.frame(out)
    })

    output$radar_plot <- renderHighchart({
      input$do_cluster
      if(input$do_cluster == 0){
        return()
      } else {
        chrt <- updateRadarChart(norm_means()[norm_means()$Group.1 %in% input$select_radar_clus,])
        chrt
      }
    })

    output$hist_plot_scam <- renderHighchart({
      input$do_histogram
      if(input$do_histogram==0){
        return()
      }else{isolate({
        histo <- updateHistogram(memberSet(), "% of Records")
        histo
      })
      }

    })

    output$hidden_hist <- renderUI({
      input$show_hist
      if(input$show_hist == TRUE ){
        return(fluidRow(splitLayout(cellWidths = c("50%","50%"),highchartOutput("radar_plot", height = 500),highchartOutput("hist_plot_scam", height = 500))))
      }else{
        return(fluidRow(splitLayout(cellWidths = c("100%"),highchartOutput("radar_plot", height = 500))))
      }
    })





    ### PAGE 3 TAB 2

    output$parcoor <- renderParcoords({
      df <- memberSet()
      df$Cluster <- df$`member.c`
      df$`member.c` <- NULL
      df <- df[ df$`Cluster` %in% input$select_radar_clus,]
      # df <- df[,-c(2,2)]
      plt <- parcoords(df
                       , rownames = F # turn off rownames from the data.frame
                       , brushMode = "2D-strums"
                       , reorderable = T
                       , queue = T
                       , color = list(
                         colorBy = "Cluster"
                         , colorScale  = htmlwidgets::JS(sprintf('d3.scale.ordinal().range(["#96e2de","#6a5fa3","#B44B86","#008685","#a6daff","#FE5432"
                                                                 ,"#ffc9c9","#570B3A","#4C4281","#d9d9d9","#144e8d","#36D7B7","#b5abe8"])'
                         ))
                       )
      )

    })


    output$looped_input  <-  renderUI({
      if(input$show_rename == TRUE){
        lapply(1:as.numeric(input$hist_tree), function(i) {
          textInput(paste0('clus_name_',i), paste0('Enter Cluster ',i, " Description"),value = i)
        })
      }else{return()}
    })


    newNames <- reactive({
      res <- lapply(1:as.numeric(input$hist_tree), function(i) input[[paste0('clus_name_', i)]])
      df <- data.frame(list(member.name = unlist(res), member.c = 1:as.numeric(input$hist_tree) ))
      df
    })

    output$download_means <- downloadHandler(
      filename = function(){
        as.character( paste("Occupancy Clustering"," ",input$hist_tree," Cluster Averages",'.csv', sep='') )
      },
      content = function(file){
        mns <- means()
        mns$Group.1 <- as.numeric(mns$Group.1)
        cc <- merge(x=mns,y =combo(), by.x = "Group.1", by.y = "member.c",all.x = TRUE)
        cc$cluster <- cc$Group.1
        cc$Group.1 <- NULL
        cc$cluster_name <- cc$member.name
        cc$member.name <- NULL
        cc <- as.data.frame(cc)
        write.csv(cc,file,row.names = FALSE)

      }
    )


    output$download_members <- downloadHandler(
      filename = function(){
        as.character(paste("Occupancy Clustering","_",input$hist_tree," Cluster Members",'.csv', sep=''))
      },
      content = function(file){
        mns <- tabMembers()
        mns$member.c <- as.numeric(mns$member.c)
        cc <- merge(x=mns,y =combo(), by.x = "member.c", by.y = "member.c",all.x = TRUE)
        cc$cluster <- cc$member.c
        cc$member.c <- NULL
        cc$cluster_name <- cc$member.name
        cc$member.name <- NULL
        cc$size_of_prize <- NULL
        cc <- as.data.frame(cc)
        write.csv(cc,file, row.names = FALSE)
      }
    )



    # # REACTIVE OBJECTS
    # cutoff_val <- reactive({
    #   switch(input$select_data,
    #          "Click to DC USA" = 12,
    #          "Click to DC EU" = 12,
    #          "BRD DC to Ship" =  16,
    #          "SD1 DC to Ship"= 16,
    #          "NALC DC to Ship"= 16,
    #          "Shelby DC to Ship"= 16,
    #          "Other DC to Ship"= 16
    #   )
    # })
    #
    #
    # var <- reactive({
    #   switch(input$select_data,
    #          "Click to DC USA" = names(c2dc_dedup),
    #          "Click to DC EU" = names(c2dc_eu_dedup),
    #          "BRD DC to Ship" =  names(dc2s_dedup_brd),
    #          "SD1 DC to Ship"= names(dc2s_dedup_sd1),
    #          "NALC DC to Ship"= names(dc2s_dedup_NALC),
    #          "Shelby DC to Ship"= names(dc2s_dedup_Shelby),
    #          "Other DC to Ship"= names(dc2s_dedup_other_dcs)
    #
    #   )
    # })
    #
    #
    # sea <- reactive({
    #   switch(input$select_data,
    #          "Click to DC USA" = as.character(unique(c2dc_dedup$season)),
    #          "Click to DC EU" = unique(c2dc_eu_dedup$season),
    #          "BRD DC to Ship" =  unique(dc2s_dedup_brd$season),
    #          "SD1 DC to Ship"= unique(dc2s_dedup_sd1$season),
    #          "NALC DC to Ship"= unique(dc2s_dedup_NALC$season),
    #          "Shelby DC to Ship"= unique(dc2s_dedup_Shelby$season),
    #          "Other DC to Ship"= as.character(unique(dc2s_dedup_other_dcs$season))
    #
    #   )
    # })
    #
    # # This is the object that contains the dataset that the clustering will run on (with the first column being the dummyKey which you can take out )
    # # Please understand that in the reactive object below "c2dc_" is just a placeholder name for the variable that will be used later
    # # this does not necessarily mean the the data this object holds is always going to be c2dc phase data in the fullfilment process
    # c2dc_trimmed2 <- reactive({
    #   if(input$select_data == "Click to DC USA"){
    #     table_data <- filterFeatures(c2dc_dedup,sea = as.character(input$select_season),cutoff = as.numeric(cutoff_val()),
    #                                   feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "Click to DC EU"){
    #     table_data <- filterFeatures(c2dc_eu_dedup,sea = as.character(input$select_season),cutoff = as.numeric(cutoff_val()),
    #                                   feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "BRD DC to Ship" ){
    #     table_data <- filterFeatures(dc2s_dedup_brd,sea = as.character(input$select_season),cutoff = as.numeric(cutoff_val()),
    #                                   feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "SD1 DC to Ship"){
    #     table_data <- filterFeatures(dc2s_dedup_sd1,sea = as.character(input$select_season),cutoff = as.numeric(cutoff_val()),
    #                                   feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "NALC DC to Ship"){
    #     table_data <- filterFeatures(dc2s_dedup_NALC,sea = as.character(input$select_season),cutoff = as.numeric(cutoff_val()),
    #                                   feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "Shelby DC to Ship"){
    #     table_data <- filterFeatures(dc2s_dedup_Shelby,sea = as.character(input$select_season),cutoff = as.numeric(cutoff_val()),
    #                                   feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "Other DC to Ship"){
    #     table_data <- filterFeatures(dc2s_dedup_other_dcs,sea = as.character(input$select_season),cutoff = as.numeric(cutoff_val()),
    #                                   feature_names = input$selected_col_names)
    #   }
    #
    #   as.data.frame(table_data)
    # })
    #
    #
    # c2dc_trimmed5 <- reactive({
    #   if(cutoff_val() == 12){
    #     c2dc_trimmed2()[year(c2dc_trimmed2()$order_date) %in% as.numeric(input$data_date),]
    #   } else {
    #     c2dc_trimmed2()[year(c2dc_trimmed2()$dc_drop_date) %in% as.numeric(input$data_date),]
    #   }
    # })
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    # c2dc_trimmed3 <- reactive({
    #   if(input$select_data == "Click to DC USA"){
    #     table_data <- filterFeatures2(c2dc_dedup,cutoff = as.numeric(cutoff_val()),
    #                                  feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "Click to DC EU"){
    #     table_data <- filterFeatures2(c2dc_eu_dedup,cutoff = as.numeric(cutoff_val()),
    #                                  feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "BRD DC to Ship" ){
    #     table_data <- filterFeatures2(dc2s_dedup_brd,cutoff = as.numeric(cutoff_val()),
    #                                  feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "SD1 DC to Ship"){
    #     table_data <- filterFeatures2(dc2s_dedup_sd1,cutoff = as.numeric(cutoff_val()),
    #                                  feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "NALC DC to Ship"){
    #     table_data <- filterFeatures2(dc2s_dedup_NALC,cutoff = as.numeric(cutoff_val()),
    #                                  feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "Shelby DC to Ship"){
    #     table_data <- filterFeatures2(dc2s_dedup_Shelby,cutoff = as.numeric(cutoff_val()),
    #                                  feature_names = input$selected_col_names)
    #   }
    #   if(input$select_data == "Other DC to Ship"){
    #     table_data <- filterFeatures2(dc2s_dedup_other_dcs,cutoff = as.numeric(cutoff_val()),
    #                                  feature_names = input$selected_col_names)
    #   }
    #
    #   as.data.frame(table_data)
    #
    # })
    #
    #
    #
    # c2dc_trimmed6 <- reactive({
    #   if(cutoff_val() == 12){
    #     c2dc_trimmed3()[c2dc_trimmed3()$order_date >= as.Date(input$dt_range[1]) & c2dc_trimmed3()$order_date <= as.Date(input$dt_range[2]) ,-2]
    #   } else {
    #     c2dc_trimmed3()[c2dc_trimmed3()$dc_drop_date >= as.Date(input$dt_range[1]) & c2dc_trimmed3()$dc_drop_date <= as.Date(input$dt_range[2]),-2]
    #   }
    # })
    #
    #
    #
    #
    # nam <- reactive({
    #   inFile <- input$file1
    #   if (is.null(inFile)){
    #     return(NULL)
    #   } else {
    #     aa <- as.data.frame(read.csv(inFile$datapath))
    #     aa$member.c <- aa$cluster
    #     aa$cluster <- NULL
    #     aa$member.name <- aa$cluster_name
    #     aa$cluster_name <- NULL
    #     bb <- unique(aa[,c("member.name","member.c")])
    #     aa$member.name <- NULL
    #     aa <- joinKeys(aa,cutoff = cutoff_val())
    #     aa <-aa[,c("member.c","dummyKey",input$selected_col_names)]
    #     # return(as.data.frame(aa))
    #     member.c <- aa$member.c
    #     cc <- aggregate(aa[,-c(1,2)],list(member.c),mean)
    #     cc <- as.data.frame(cc)
    #     return(list(memberSet = aa, newNames = bb, means = cc))
    #   }
    # })
    #
    #
    #
    # alt_memberSet <- reactive({
    #   input$do_assign
    #   if(input$do_assign == 0){return()} else{
    #     isolate({
    #       if(is.null(input$file1) | input$override == FALSE){
    #         x <- assign_clus(memberSet(),means(),c2dc_trimmed6(),assign_method = as.character(input$assign_method))
    #         return(as.data.frame(x))
    #       }else {
    #         x <- assign_clus(nam()$memberSet,nam()$means,c2dc_trimmed6(),assign_method = as.character(input$assign_method))
    #         return(as.data.frame(x))
    #       }
    #     })
    #   }
    # })
    #
    #
    #
    # means2 <- reactive({
    #   input$do_assign
    #   if(input$do_assign == 0){return()} else{
    #     alt_means <- altClusterMeans(alt_memberSet())$org_means
    #     as.data.frame(alt_means)
    #   }
    # })
    #
    #
    # norm_means2 <- reactive({
    #   l <- means2()[,-1]
    #   ll <- apply(l,2,normalize.vector)
    #   Group.1 <- means2()[,1]
    #   out <- cbind(Group.1,as.data.frame(ll))
    # })
    #
    #

    #
    #
    #
    # alt_tabMembers <- reactive({
    #   input$do_assign
    #   if(input$do_assign == 0){
    #     return()
    #   } else {
    #     isolate({
    #       tabOut <- prepTab(alt_memberSet(),cutoff_val())
    #       as.data.frame(tabOut)
    #     })
    #   }
    # })
    #
    #
    #
    # newNames <- reactive({
    #   res <- lapply(1:as.numeric(input$hist_tree), function(i) input[[paste0('clus_name_', i)]])
    #   df <- data.frame(list(member.name = unlist(res), member.c = 1:as.numeric(input$hist_tree) ))
    #   df
    # })
    #
    #
    # alt_newNames <- reactive({
    #   if(input$override == FALSE){
    #     newNames()
    #   } else{
    #     xx <- nam()$newNames
    #     as.data.frame(xx)
    #   }
    # })
    #
    #
    # prz <- reactive({
    #   Clusters <- memberSet()$member.c
    #   df2 <- as.data.frame(table(Clusters))
    #   cat <- df2[,"Clusters"]
    #   dat <- round(as.numeric(df2[,"Freq"])/sum(df2[,"Freq"]),2)
    #   bat <- (input$edd_miss/100)*(dat)
    #   bb <- cbind(cat,dat,bat)
    #   bb
    # })
    #
    # combo <- reactive({
    #   ll <- as.data.frame(prz())
    #   lj <- as.data.frame(newNames())
    #   ll$cat <- as.numeric(ll$cat)
    #   lj$member.c <- as.numeric(lj$member.c)
    #   cc <- merge(x=lj,y =ll, by.x = "member.c", by.y = "cat",all.x = TRUE)
    #   cc$size_of_prize <- cc$bat
    #   cc$dat <- NULL
    #   cc$bat <- NULL
    #   cc$cat <- NULL
    #   as.data.frame(cc)
    # })
    #
    #
    #
    #
    #
    #
    #
    # ###########################################################################################################################################################################
    # ###########################################################################################################################################################################
    # # RENDERED OUTPUT VARIABLES
    # ###########################################################################################################################################################################
    # ###########################################################################################################################################################################
    # output$col_names <- renderUI({
    #     selectInput(inputId = "selected_col_names",label = "Choose Numeric features",choices = names(sample_data),multiple = TRUE)
    # })
    #
    #
    #
    #
    #
    #
    #
    #
    #
    # # Create the download handlers
    # # download_means
    # output$download_means <- downloadHandler(
    #   filename = function(){
    #     as.character( paste(input$select_data,"_",input$select_season,input$hist_tree,"_cluster_averages",'.csv', sep='') )
    #   },
    #   content = function(file){
    #     mns <- means()
    #     mns$Group.1 <- as.numeric(mns$Group.1)
    #     cc <- merge(x=mns,y =combo(), by.x = "Group.1", by.y = "member.c",all.x = TRUE)
    #     cc$cluster <- cc$Group.1
    #     cc$Group.1 <- NULL
    #     cc$cluster_name <- cc$member.name
    #     cc$member.name <- NULL
    #     cc <- as.data.frame(cc)
    #     write.csv(cc,file,row.names = FALSE)
    #
    #   }
    # )
    #
    #
    # output$download_members <- downloadHandler(
    #   filename = function(){
    #     as.character(paste(input$select_data,"_",input$select_season,input$hist_tree,"_cluster_members",'.csv', sep=''))
    #   },
    #   content = function(file){
    #     mns <- tabMembers()
    #     mns$member.c <- as.numeric(mns$member.c)
    #     cc <- merge(x=mns,y =combo(), by.x = "member.c", by.y = "member.c",all.x = TRUE)
    #     cc$cluster <- cc$member.c
    #     cc$member.c <- NULL
    #     cc$cluster_name <- cc$member.name
    #     cc$member.name <- NULL
    #     cc$size_of_prize <- NULL
    #     cc <- as.data.frame(cc)
    #     write.csv(cc,file, row.names = FALSE)
    #   }
    # )
    #
    #
    # # #Important DO NOT DELETE
    # #
    # output$parcoor <- renderParcoords({
    #   df <- memberSet()
    #   df <- df[ df$`member.c` %in% input$select_radar_clus,]
    #   df <- df[,-c(2,2)]
    #   plt <- parcoords(df
    #                    ,rownames = F # turn off rownames from the data.frame
    #                    , brushMode = "2D-strums"
    #                    , reorderable = T
    #                    , queue = T
    #                    , color = list(
    #                      colorBy = "member.c"
    #                      ,colorScale = htmlwidgets::JS("d3.scale.category10()")
    #                    )
    #   )
    #
    # })
    #
    #
    # output$train_data <- renderUI({
    #   input$do_cluster
    #   if(input$do_cluster == 0){
    #     return()
    #
    #   } else {
    #     isolate(selectInput(inputId = "selected_train_data",label = "Data Selected:",choices = input$select_data,multiple = FALSE)
    #     )}
    # })
    #
    #
    # output$train_season <- renderUI({
    #   input$do_cluster
    #   if(input$do_cluster == 0){
    #     return()
    #
    #   } else {
    #     isolate(selectInput(inputId = "selected_train_season",label = "Season Selected:",choices = input$select_season,multiple = FALSE)
    #     )}
    # })
    #
    # output$train_features <- renderUI({
    #   textInput("selected_train_cols",label = "Features Selected:",paste(input$selected_col_names, collapse = ","))
    # })
    #
    #
    #
    # output$radar_plot2 <- renderHighchart({
    #   input$do_assign
    #   if(input$do_assign == 0){
    #     return()
    #   } else {
    #     isolate({
    #       dt3 <- norm_means2()
    #       bb <- alt_newNames()[,c("member.name")]
    #       dt3$Group.1 <- bb[as.numeric(dt3$Group.1)]
    #       chrt <- updateRadarChart(dt3)
    #       chrt
    #     })
    #   }
    # })
    #
    #
    #
    # output$hist_plot2 <- renderHighchart({
    #   input$do_assign
    #   if(input$do_assign==0){
    #     return()
    #   }else{isolate({
    #     dd <- merge(x = alt_memberSet(),y = alt_newNames(),by = "member.c", all.x = TRUE)
    #     dd$member.c <- dd$member.name
    #     dd$member.name <- NULL
    #     histo <- updateHistogram(dd, "% of Records")
    #     histo
    #   })
    #   }
    #
    # })
    #
    #
    #
    #
    #
    #
    #

    #
    # output$looped_input  <-  renderUI({
    #   if(input$show_rename == TRUE){
    #     lapply(1:as.numeric(input$hist_tree), function(i) {
    #       textInput(paste0('clus_name_',i), paste0('Enter Cluster ',i, " Description"),value = i)
    #     })
    #   }else{return()}
    # })
    #
    #
    # output$select_dt_range <- renderUI({
    #   dd <- c2dc_trimmed3()
    #   # dd$member.c <- 1
    #   # dd2 <- prepTab(dd,cutoff_val())
    #   dateRangeInput(inputId = "dt_range",label =  "Select the Order Placement Date Range for New Season:",start = min(dd[,2])
    #                  ,end = max(dd[,2]),min = min(dd[,2]),max = max(dd[,2]))
    #
    # })
    #
    #
    # output$stacked_plot2 <- renderHighchart({
    #   if(input$do_assign==0){
    #     return()
    #   }else{
    #     isolate({
    #       dd <- alt_tabMembers()
    #       bb <- alt_newNames()
    #       # (dd$order_date >= as.Date(input$dt_range[1]) & dd$order_date <= as.Date(input$dt_range[2]))
    #       plt <- updateStacked2(prepTab_output = dd,bb[,c("member.name")],data_name = input$select_data)
    #       return(plt)
    #     })
    #
    #   }
    #
    # })
    #
    #
    # output$show_assign_plots <- renderUI({
    #   if(input$add_plot == TRUE){
    #     fluidRow(
    #       h3(strong("Assigned Cluster Profiles"), align = "center"),
    #       splitLayout(cellWidths = c("50%", "50%"),highchartOutput("radar_plot2"),highchartOutput("hist_plot2"))
    #       )
    #   }else{return()}
    # })
    #
    #




    # output$ddt <- renderUI({
    #   input$select_data
    #   input$select_season
    #   isolate({
    #     selectInput(inputId = "data_date",label = "Choose the Date Range for Selected Data:"
    #                 ,choices = min(unique(year(c2dc_trimmed2()[,2]))):max(unique(year(c2dc_trimmed2()[,2])))
    #                 ,multiple = T)
    #   })
    # })
    #
    #
    # output$download_ass <- downloadHandler(
    #   filename = function(){
    #     as.character(paste0("Asssigned_",input$select_data,"_","_cluster_members",'.csv', sep=''))
    #   },
    #   content = function(file){
    #     mns <- alt_tabMembers()
    #     mns$member.c <- as.numeric(mns$member.c)
    #     cc <- merge(x=mns,y =alt_newNames(), by.x = "member.c", by.y = "member.c",all.x = TRUE)
    #     cc$cluster <- cc$member.c
    #     cc$member.c <- NULL
    #     cc$cluster_name <- cc$member.name
    #     cc$member.name <- NULL
    #     # cc$size_of_prize <- NULL
    #     cc <- as.data.frame(cc)
    #     write.csv(cc,file, row.names = FALSE)
    #   }
    # )
    #
    #
    # output$prize_size <- renderRHandsontable({
    #   input$do_histogram
    #   if(input$do_histogram == 0){
    #     return()
    #     }else{
    #       bb <- prz()
    #       colnames(bb) <- c("Cluster", "EDD Failures Selected (%)", "Size of Prize (%)")
    #       bb <- rhandsontable(bb, width = "100%" , stretchH = "all") %>%
    #         hot_col("Cluster", format = "0", halign = "htCenter") %>%
    #         hot_col("EDD Failures Selected (%)", format = "0.00%",  halign = "htCenter") %>%
    #         hot_col("Size of Prize (%)", format = "0.00%",  halign = "htCenter")
    #       return(bb)
    #   }
    #
    # })
    #
    #
    # output$show_upload <- renderUI({
    #   if(input$override == TRUE){
    #     div(fileInput("file1", "Upload Training Data", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    #       p(strong("IMPORTANT: "),
    #       p("Uploaded Data must match the parameters selected above")))
    #   } else {return()}
    # })
    #
    #
    # output$sea_dat <- renderUI({
    #   x <- sea()
    #   selectInput(inputId = "select_season",label = "Select Month:", choices = x, selected = x[1], multiple = T)
    # })
  }
)