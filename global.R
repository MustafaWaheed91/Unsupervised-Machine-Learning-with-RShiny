################################################################################################
################################################################################################
###############################   Unsupervised Machine Learning app  ###########################
###############################      Author: Mustafa Waheed         ############################
################################################################################################
################################################################################################
rm(list = ls())
source(file ="loadPackages.R")
sample_data <- read.csv("datatraining.csv", stringsAsFactors = F)
# sample_data <- read.csv("datatraining.txt", sep = ",", stringsAsFactors = F)
# write_data <- sample_data[order(runif(nrow(sample_data))),]
sample_data$date <- as.Date(sample_data$date)
co <- c("#96e2de","#6a5fa3","#B44B86","#008685","#a6daff","#FE5432","#ffc9c9","#570B3A","#4C4281","#d9d9d9","#144e8d","#36D7B7","#b5abe8")
cco <- "#36D7B7"
# THIS FUNCTION STANDARDIZES THE DATA SET (MEAN CENTERED)
#  AND RETURNS THE DISTANCE MATRIX
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
    hc_yAxis(title = list(text = yLb), gridLineWidth = 0) %>%
    hc_tooltip(formatter = JS("function(){return ('% of Records: ' + this.y + '%' + ' <br> Cluster: ' + this.x)}")) %>%
    hc_add_series(data =dat,name = "Cluster Distribution")
}
# ########################################################################################
clusterMeans<- function(c2dc_trimmed2,hc.c,tree_num){
  member.c <- cutree(hc.c,tree_num)
  normal_cluster_means <- aggregate(standardizeData(c2dc_trimmed2),list(member.c),mean)
  cluster_means <- aggregate(c2dc_trimmed2,list(member.c),mean)
  return(list(org_means = cluster_means,  norm_means = normal_cluster_means ))
}

updateRadarChart <- function(dataset){
  rownames(dataset) <- as.character(dataset$Group.1)
  dataset <- dataset[,-c(1,1)]
  seter <- rownames(dataset)
  catagories <- colnames(dataset)
  graph_data <- lapply(seter, function(city) {
    list(name = paste(city)
         ,data = as.numeric(dataset[city,])
         ,pointPlacement = 'on')
  })

  plt <- highchart() %>%
    hc_colors(co) %>%
    hc_chart(polar = TRUE, type = "line") %>%
    # hc_title(text = statement1) %>%
    # hc_subtitle(text = statement2) %>%
    hc_xAxis(categories = catagories,
             tickmarkPlacement = 'on',
             lineWidth = 1.5) %>%
    hc_yAxis(gridLineInterpolation = 'polygon',lineWidth = 2,min = min(dataset), max =max(dataset), tickInterval=0.5) %>%
    hc_add_series_list(graph_data)
  return(plt)
}

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
    hc_yAxis(title = list(text = "ERROR"),labels = list(enabled = F), gridLineWidth = 0) %>%
    hc_add_series(name ="Within-Cluster Sum of Squares" ,data = round(normalize.vector(res),2)) %>%
    hc_tooltip(formatter = JS("function(){return ('Normalized Error: ' + this.y + ' <br> Number of Clusters: ' + this.x)}"))
  return(hc)
}

updateStacked <- function(tabMembers, radar_clusters) {
  df <- tabMembers[tabMembers$member.c %in% radar_clusters,]
  df$wDay <- factor(substr(weekdays(df$order_date),1,3))
  df$order_date <- paste(as.character(df$order_date),as.character(df$wDay))
  df$wDay <- NULL

  dt <- data.table(df)
  dt1 <- dt[, order_count := .N,by = c("order_date","member.c")]
  dt2 <- dt1[,list(order_date,member.c,order_count)]
  dt3 <- dcast(dt2,order_date ~ member.c, value.var = 'order_count')
  rownames(dt3) <- as.character(dt3$order_date)
  dt3 <- dt3[,-c(1,1)]
  seter <- as.factor(rownames(dt3))
  ds <- lapply(colnames(dt3), function(x){
    list(data = dt3[,x], name = x ,pointPlacement = 'on')
  })
  plt <- highchart() %>%
    hc_colors(co) %>%
    hc_chart(type = "column") %>%
    # hc_title(text = paste(as.character(data_name),"EDD Failures", as.character(season))) %>%
    hc_yAxis(title = list(text = "# of Records")) %>%
    hc_xAxis(categories = seter,
             tickmarkPlacement = 'on',
             lineWidth = 1,
             label = list(align = "left")) %>%
    hc_plotOptions(column = list(
      dataLabels = list(enabled = FALSE),
      stacking = "normal",
      enableMouseTracking = TRUE)
    ) %>%
    hc_add_series_list(ds)

  return(plt)
}
# #########################################################################################
