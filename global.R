################################################################################################
################################################################################################
###############################   Exploring Root Cause Analysis     ############################
###############################      Author: Mustafa Waheed         ############################
################################################################################################
################################################################################################
rm(list = ls())
source(file ="loadPackages.R")
sample_data <- read.csv("datatraining.txt", sep = ",", stringsAsFactors = F)
sample_data$date <- as.Date(sample_data$date)

co <- c("#96e2de","#6a5fa3","#B44B86","#008685","#a6daff","#FE5432","#ffc9c9","#570B3A","#4C4281","#d9d9d9","#144e8d","#36D7B7","#b5abe8")
cco <- "#36D7B7"
# # This function takes the firs till cutoff cols and creates a key from them assuming
# # you have 1-cutoff cols describing the level or dimensions of the data.
# joinKeys <- function(c2dc_dedup,cutoff){
#   dummyKey <- vector()
#   for(ii in 1:cutoff){
#     if(ii > 1){
#       dummyKey <- paste(dummyKey,"_",c2dc_dedup[,ii])
#     }
#
#     if(ii == 1){
#       dummyKey <- c2dc_dedup[,1]
#     }
#
#   }
#   c2dc_dedup$dummyKey <- dummyKey
#
#   return(c2dc_dedup)
# }
# ########################################################################################
# filterFeatures <- function(c2dc_dedup,sea,feature_names,cutoff){
#   #  Create new variable to act as key
#   c2dc_dedup <- joinKeys(c2dc_dedup,cutoff)
#   if(cutoff == 12){
#     featureList <- c("dummyKey","order_date",feature_names)
#   }
#
#   if(cutoff == 16){
#     featureList <- c("dummyKey","dc_drop_date",feature_names)
#   }
#   c2dc_trimmed2 <- c2dc_dedup[c2dc_dedup$season %in% sea,featureList]
#
#
#   if("fraud_duration"  %in% featureList){
#     #  logic to Impute NAs from fraud check duration variable
#     c2dc_trimmed2$fraud_duration[is.na(c2dc_trimmed2$fraud_duration)] <- 0
#   }
#   if("ship_to_origin"  %in% featureList){
#     c2dc_trimmed2$ship_to_origin[is.na(c2dc_trimmed2$ship_to_origin)] <- 0
#   }
#   if("forecast_variance"  %in% featureList){
#     c2dc_trimmed2[is.na(c2dc_trimmed2$forecast_variance),"forecast_variance"] <- 0
#   }
#   return(c2dc_trimmed2)
# }
#
# filterFeatures2 <- function(c2dc_dedup,feature_names,cutoff){
#   #  Create new variable to act as key
#   c2dc_dedup <- joinKeys(c2dc_dedup,cutoff)
#   if(cutoff == 12){
#     featureList <- c("dummyKey","order_date",feature_names)
#   }
#
#   if(cutoff == 16){
#     featureList <- c("dummyKey","dc_drop_date",feature_names)
#   }
#   c2dc_trimmed2 <- c2dc_dedup[,featureList]
#
#
#   if("fraud_duration"  %in% featureList){
#     #  logic to Impute NAs from fraud check duration variable
#     c2dc_trimmed2$fraud_duration[is.na(c2dc_trimmed2$fraud_duration)] <- 0
#   }
#   if("ship_to_origin"  %in% featureList){
#     c2dc_trimmed2$ship_to_origin[is.na(c2dc_trimmed2$ship_to_origin)] <- 0
#   }
#   if("forecast_variance"  %in% featureList){
#     c2dc_trimmed2[is.na(c2dc_trimmed2$forecast_variance),"forecast_variance"] <- 0
#   }
#   return(c2dc_trimmed2)
# }
#
# ########################################################################################
# # #this returns a plot post feature selection
# # takes in the result of filter features
# # createCorrelogram <- function(c2dc_trimmed2){
# #   # #look at correlation between the variables
# #   z <- c2dc_trimmed2[,-c(1,1)]
# #   M <- cor(z)
# #   plot <- corrplot(M, method="number")
# #   return(plot)
# # }
########################################################################################
# THIS FUNCTION STANDARDIZES THE DATA SET (MEAN CENTERED)
#  AND RETURNS THE DISTANCE MATRIX
# takes in the output from filterFeatures() fucn
standardizeData <- function(z){
  # Normalization
  m <- apply(z,2,mean)
  s <- apply(z,2,sd)
  z <- scale(z,m,s)
  return(z)
}
# ########################################################################################
# This function returns the 1) memebers set with the orginal data that was
# standardized as well as the cutree amt and the hclust() object
clusterMembers <- function(hc.c,c2dc_trimmed2,tree_num){
  # get member set
  member.c <- cutree(hc.c,k=tree_num)
  memberSet.c <- cbind(as.data.frame(member.c),c2dc_trimmed2)
  return(memberSet.c)
}
# ########################################################################################
# # # create histogram from the memberSets
# # createHistogram <- function(memberSet,brk){
# #   Clusters <- memberSet$member.c
# #   H <- hist(Clusters, plot = FALSE,breaks = as.numeric(brk))
# #   H$density <- with(H, 100 * density* diff(breaks)[1])
# #   labs <- paste(round(H$density), "%", sep="")
# #   plot(H, freq = F,labels = labs, ylim=c(0, 1.08*max(H$density)),xlab = "Cluster Number")
# # }
# ########################################################################################
updateHistogram <- function(memberSet, yLb){
  # rownames(memberSet) <- memberSet$member.c
  Clusters <- memberSet$member.c
  df2 <- as.data.frame(table(Clusters))
  cat <- df2[,"Clusters"]
  dat <- round(as.numeric(df2[,"Freq"])*100/sum(df2[,"Freq"]),1)
  highchart() %>%
    hc_colors(cco) %>%
    hc_chart(type = "column") %>%
    # hc_title(text = "Cluster Distribution") %>%
    hc_xAxis(title = list(text = "Cluster"),categories = cat) %>%
    hc_yAxis(title = list(text = yLb)) %>%
    hc_tooltip(formatter = JS("function(){return ('% of Records: ' + this.y + '%' + ' <br> Cluster: ' + this.x)}")) %>%
    hc_add_series(data =dat,name = "Cluster Distribution")
}
# ########################################################################################
# clusterMeans<- function(c2dc_trimmed2,hc.c,tree_num){
#   # Calculate Cluster Means
#   member.c <- cutree(hc.c,tree_num)
#   z <- standardizeData(c2dc_trimmed2)
#   normal_cluster_means <- aggregate(z,list(member.c),mean)
#   cluster_means <- aggregate(c2dc_trimmed2[,-c(1,1)],list(member.c),mean)
#   return(list(org_means = cluster_means,  norm_means = normal_cluster_means ))
# }
# #######################################################################################
# standMeans <- function(x){
#   normalized = (x-min(x))/(max(x)-min(x))
# }
# #######################################################################################
# ## This function will create multidimensional radar plot of the cluster means
# # createRadarChart <- function(data){
# #   colors_border=c(rgb(0.2,0.5,0.5,1),
# #                   rgb(0.8,0.2,0.5,0.8) ,
# #                   rgb(0.7,0.5,0.1,0.65) ,
# #                   rgb(0.140,0.86,0.75,0.55),
# #                   rgb(0.255,0.255,0,0.5),
# #                   rgb(0.255,0.127,0.14,0.5),
# #                   rgb(0.239,0.79,0.1,0.5),
# #                   rgb(0.179,0.213,0.175,0.5))
# #   radarchart( data[,-c(1,1)]  , axistype=1 , maxmin=F,
# #               plwd=2 , plty=1
# #               # ,pfcol = colors_in
# #               , pcol=colors_border,
# #               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(-1,14,0.5), cglwd=1,
# #               vlcex=0.8)
# #   legend(x=2, y=1, legend = rownames(data), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=1.2, pt.cex=3)
# #
# # }
# ########################################################################################
# updateRadarChart <- function(dataset,statement1,statement2){
#   rownames(dataset) <- as.character(dataset$Group.1)
#   dataset <- dataset[,-c(1,1)]
#   seter <- rownames(dataset)
#   catagories <- colnames(dataset)
#   graph_data <- lapply(seter, function(city) {
#     list(name = paste(city)
#          ,data = as.numeric(dataset[city,])
#          ,pointPlacement = 'on')
#   })
#
#   plt <- highchart() %>%
#     hc_colors(co) %>%
#     hc_chart(polar = TRUE, type = "line") %>%
#     # hc_title(text = statement1) %>%
#     # hc_subtitle(text = statement2) %>%
#     hc_xAxis(categories = catagories,
#              tickmarkPlacement = 'on',
#              lineWidth = 1.5) %>%
#     hc_yAxis(gridLineInterpolation = 'polygon',lineWidth = 2,min = min(dataset), max =max(dataset), tickInterval=0.5) %>%
#     hc_add_series_list(graph_data)
#   return(plt)
# }
# ########################################################################################
# #function to plot Parallel Coordinates Plot.
# # createParPlot <- function(memberSet){
# #   par(las = 1, mar = c(4.5,3,3,2)+0.1,cex = 0.8)
# #   parcoord(memberSet[,-c(2,2)],col = as.character(memberSet$member.c), var.label = T, lwd = 1)
# #   title("Parallel Coordinate Plot for Clusters")
# #   par(xpd=T)
# #   par(xpd = NA)
# # }
# ########################################################################################
# # these functions help calculate the WSS for a hclust() object
wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}
wrap <- function(i, hc, x) {
  cl <- cutree(hc, i)
  spl <- split(x, cl)
  wss <- sum(sapply(spl, wss))
  wss
}
updateScreePlot <- function(start,end,cl,data){
  res <- sapply(seq.int(start,end), wrap, h = cl, x = data)
  hc <- highchart(theme = getOption("highcharter.theme")) %>%
    hc_colors(cco) %>%
    # hc_title(text = "Scree Plot") %>%
    hc_xAxis(title = list(text = "Number of Clusters"),crosshairs = TRUE) %>%
    hc_yAxis(title = list(text = "ERROR"),labels = list(enabled = F)) %>%
    hc_add_series(name ="Within-Cluster Sum of Squares" ,data = round(normalize.vector(res),2)) %>%
    hc_tooltip(formatter = JS("function(){return ('Normalized Error: ' + this.y + ' <br> Number of Clusters: ' + this.x)}"))
  return(hc)
}
# #########################################################################################
# assign_clus <- function(memberSet,means,c2dc_trimmed3,assign_method = "Classification Tree"){
#   c2dc_trimmed3$member.c <- NULL
#   rownames(means) <- means$Group.1
#
#   if(assign_method == "Average Distance"){
#
#     assi <- function(x){
#       cluster <- which.min(as.matrix(pdist(means,x)))
#       return(cluster)
#     }
#
#     member.c <- apply(c2dc_trimmed3[,-1],1,assi)
#     out <- cbind(as.data.frame(member.c),c2dc_trimmed3)
#     return(out)
#   }
#
#
#   if(assign_method == "Classification Tree"){
#     m3 <- rpart(member.c ~ ., data = memberSet[,-2], method = "class")
#     member.c <- predict(m3,c2dc_trimmed3[,-1], type = "class")
#     out <- cbind(as.data.frame(member.c),c2dc_trimmed3)
#     return(out)
#   }
#
#
# }
# #########################################################################################
# altClusterMeans <- function(alt_memberSet){
#   member.c <- alt_memberSet$member.c
#   z <- standardizeData(alt_memberSet[,-1])
#   normal_cluster_means <- aggregate(z,list(member.c),mean)
#   cluster_means <- aggregate(alt_memberSet[,-c(1,2)],list(member.c),mean)
#   return(list(org_means = cluster_means, norm_means = normal_cluster_means))
# }
# #########################################################################################
# prepTab <- function(memberSet,cutoff_val){
#   kk <- memberSet
#   kkk <- str_split_fixed(kk$dummyKey, "_", as.numeric(cutoff_val))
#   kkk <- as.data.frame(kkk)
#   if(as.numeric(cutoff_val) == 12){
#     colnames(kkk) <- c("order_header_key","order_line_key","order_no","country","trckgnbr","extn_style_number","extn_color_number","extn_size_description",
#                        "order_date","order_hour","EDD.date","Customer.date")
#     kk$dummyKey <- NULL
#     tabOut <- cbind(kkk,kk)
#     tabOut$order_date <- as.Date(tabOut$order_date)
#     return(tabOut)
#   }
#   if(as.numeric(cutoff_val) == 16){
#     colnames(kkk) <- c("order_header_key","order_line_key","enterprise_key","order_no","extn_style_number","extn_color_number","extn_size_description","contract","trckgnbr","customer_zip_code",
#                        "SHPGMTHDDESC","dc","dc_drop_date","TS_SHIP","OrigScan","EDD.date")
#     kk$dummyKey <- NULL
#     tabOut <- cbind(kkk,kk)
#     tabOut$dc_drop_date <- as.Date(tabOut$dc_drop_date)
#     colnames(tabOut)[grep("dc_drop_date",names(tabOut))] <- "order_date"
#     return(tabOut)
#   }
#
# }
# #########################################################################################
# updateStacked <- function(prepTab_output,data_name,season) {
#   # prepTab_output$order_date <- as.Date(prepTab_output$order_date)
#   df <- prepTab_output[,c("order_date","member.c","trckgnbr","order_header_key","order_line_key")]
#   # weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday')
#   # df$wDay <- factor((weekdays(df$order_date) %in% weekdays1),
#   #                   levels=c(FALSE, TRUE), labels=c('wkend', 'wkday'))
#
#   df$wDay <- factor(substr(weekdays(df$order_date),1,3))
#
#   df$order_date <- paste(as.character(df$order_date),as.character(df$wDay))
#   df$wDay <- NULL
#   dt <- data.table(df)
#   dt1 <- dt[, order_count := .N,by = c("order_date","member.c")]
#   dt2 <- dt1[,list(order_date,member.c,order_count)]
#   dt3 <- dcast(dt2,order_date ~ member.c, value.var = 'order_count')
#   rownames(dt3) <- as.character(dt3$order_date)
#   dt3 <- dt3[,-c(1,1)]
#   seter <- as.factor(rownames(dt3))
#   ds <- lapply(colnames(dt3), function(x){
#     list(data = dt3[,x], name = x ,pointPlacement = 'on')
#   })
#
#   plt <- highchart() %>%
#     hc_colors(co) %>%
#     hc_chart(type = "column") %>%
#     # hc_title(text = paste(as.character(data_name),"EDD Failures", as.character(season))) %>%
#     hc_yAxis(title = list(text = "# of Records")) %>%
#     hc_xAxis(categories = seter,
#              tickmarkPlacement = 'on',
#              lineWidth = 1,
#              label = list(align = "left")) %>%
#     hc_plotOptions(column = list(
#       dataLabels = list(enabled = FALSE),
#       stacking = "normal",
#       enableMouseTracking = TRUE)
#     ) %>%
#     hc_add_series_list(ds)
#
#   return(plt)
# }
# #########################################################################################
# updateStacked2 <- function(prepTab_output,clusNames,data_name) {
#   # prepTab_output$order_date <- as.Date(prepTab_output$order_date)
#   df <- prepTab_output[,c("order_date","member.c","trckgnbr","order_header_key","order_line_key")]
#   # weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday')
#   # df$wDay <- factor((weekdays(df$order_date) %in% weekdays1),
#   #                   levels=c(FALSE, TRUE), labels=c('wkend', 'wkday'))
#
#   df$wDay <- factor(substr(weekdays(df$order_date),1,3))
#
#   df$order_date <- paste(as.character(df$order_date),as.character(df$wDay))
#   df$wDay <- NULL
#   dt <- data.table(df)
#   dt1 <- dt[, order_count := .N,by = c("order_date","member.c")]
#   dt2 <- dt1[,list(order_date,member.c,order_count)]
#   dt3 <- dcast(dt2,order_date ~ member.c, value.var = 'order_count')
#   rownames(dt3) <- as.character(dt3$order_date)
#   dt3 <- dt3[,-c(1,1)]
#   names(dt3) <- clusNames[as.numeric(names(dt3))]
#   seter <- as.factor(rownames(dt3))
#   ds <- lapply(colnames(dt3), function(x){
#     list(data = dt3[,x], name = x ,pointPlacement = 'on')
#   })
#
#   plt <- highchart() %>%
#     hc_colors(co) %>%
#     hc_chart(type = "column") %>%
#     # hc_title(text = paste(as.character(data_name),"EDD Failures for Assigned Date Range")) %>%
#     hc_yAxis(title = list(text = "# of Records")) %>%
#     hc_xAxis(categories = seter,
#              tickmarkPlacement = 'on',
#              lineWidth = 1,
#              maxPadding = 0,
#              minPadding = 0,
#              label = list(align = "center")) %>%
#     hc_plotOptions(column = list(
#       dataLabels = list(enabled = FALSE),
#       stacking = "normal",
#       enableMouseTracking = TRUE)
#     ) %>%
#     hc_add_series_list(ds)
#
#   return(plt)
# }
# #########################################################################################