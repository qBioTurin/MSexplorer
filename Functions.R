library(patchwork)
library(ggrepel)
library(ggfortify)
library(dplyr)
library(ggplot2)
library(ggforce)
library(factoextra)
library(tidyr)
library(purrr)
# remotes::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)
library(stats)
library(clusterCrit)
library(concaveman)
library(plotly)
library(survival)
library(survminer)

### functions

statistical_tests <- function(df) {
  results <- data.frame(Variable = character(), Test = character(), P_Value = numeric(), stringsAsFactors = FALSE)
  
  for (var in colnames(df %>% select(-Cluster))) {
    # Identify variable type
    # if (is.numeric(df[[var]])) {
    #   # Check normality using Shapiro-Wilk test
    #   if (shapiro.test(df[[var]])$p.value > 0.05) {
    #     # If normal, apply ANOVA
    #     test_result <- aov(df[[var]] ~ df[["Cluster"]])
    #     p_value <- summary(test_result)[[1]][["Pr(>F)"]][1]
    #     test_name <- "ANOVA"
    #   } else {
    #     # If not normal, apply Kruskal-Wallis test
    #     test_result <- kruskal.test(df[[var]] ~ as.factor(df[["Cluster"]]))
    #     p_value <- test_result$p.value
    #     test_name <- "Kruskal-Wallis"
    #   }
    #   
    # } else {
      # If categorical, use Chi-square test
      contingency_table <- table(df[[var]], df[["Cluster"]])
      
      # Ensure all expected counts > 5 for Chi-square, otherwise use Fisher's exact test
      if (all(chisq.test(contingency_table)$expected > 5)) {
        test_result <- chisq.test(contingency_table)
        test_name <- "Chi-square"
      } else {
        test_result <- fisher.test(contingency_table)
        test_name <- "Fisher's Exact"
      }
      
      p_value <- test_result$p.value
      
    #}
    
    # Append results to dataframe
    results <- rbind(results, data.frame(Variable = var, Test = test_name, P_Value = p_value, stringsAsFactors = FALSE))
  }
  
  results = results %>% mutate(Output = if_else(P_Value < 0.05,"Significant differences","No significant differences"))
  return(results)
}

cluster_indexes <-function(data){
  AllIndexes = do.call(rbind,
                       lapply(2:6,function(k){
                         # Perform the kmeans algorithm
                         set.seed(42)
                         cl <- kmeans(data, k)
                         df = clusterCrit::intCriteria(as.matrix(data),cl$cluster,"all")
                         data.frame(k = k, as.data.frame(df) )
                       }
                       )
  )
  
  
  vals <- vector()
  for(nIndex in names(AllIndexes %>% select(-k))){
    vals = c(vals, clusterCrit::bestCriterion(AllIndexes[[nIndex]],nIndex))
  }
  
  bestK = sort(table(AllIndexes$k[vals]),decreasing = T)
  
  return(list(bestK = bestK, AllIndexes = AllIndexes))
}

cluster.generation <- function(data, Kmeans.before) {
  pca_res <- prcomp(data, scale = T)
  plPCAvar <- factoextra::fviz_eig(pca_res)
  
  plPCA <- factoextra::fviz_pca_biplot(pca_res,
                                       repel = TRUE,
                                       col.var = "#2E9FDF",
                                       col.ind = "#696969")
  
  summ <- summary(pca_res)$importance[2,]
  cumsumm <- cumsum(summ)
  PCmin <- min(which(cumsumm > 0.85))
  pca_data <- pca_res$x[, 1:PCmin] 
  
  if (Kmeans.before) {
    allCl = cluster_indexes(data)
    
    sil_values = rbind(allCl$AllIndexes %>% select(k,silhouette), c(1,0)) %>%
      ggplot() + geom_line(aes(x = k, y = silhouette))+ geom_point(aes(x = k, y = silhouette)) +
      theme_minimal()+labs(x = "Number of Clusters", y = "Silhouette score (to maximise)")

  } else {
    allCl = cluster_indexes(pca_data)
    sil_values = rbind(allCl$AllIndexes %>% select(k,silhouette), c(1,0)) %>%
      ggplot() + geom_line(aes(x = k, y = silhouette))+ geom_point(aes(x = k, y = silhouette)) +
      theme_minimal()+labs(x = "Number of Clusters", y = "Silhouette score (to maximise)")
    
  }
  
  return(list(Data = data, pca_res = pca_res, plPCAvar = plPCAvar, plPCA = plPCA, silhouette = sil_values, AllClusteringIndex = allCl))
}

cluster.plot <- function(data,Kmeans.before, k,palette = NULL) {
  pca_res <- prcomp(data, scale = TRUE)
  plPCAvar <- factoextra::fviz_eig(pca_res)
  
  summ <- summary(pca_res)$importance[2,]
  cumsumm <- cumsum(summ)
  PCmin <- min(which(cumsumm > 0.85))
  pca_data <- pca_res$x[, 1:PCmin] 
  
  plPCA <- factoextra::fviz_pca_biplot(pca_res,
                                       repel = TRUE,
                                       col.var = "#2E9FDF",
                                       col.ind = "#696969")
  
  if (!Kmeans.before) {
    kmeans_result <- kmeans(data, centers = as.numeric(k), nstart = 25)
    
    title <- "K-means Clustering on Initial Data"
  } else {
    
    kmeans_result <- kmeans(pca_data, centers = as.numeric(k), nstart = 25)
    
    title <- paste0("K-means Clustering on PCA-Reduced Data (", PCmin, " components)")
  }
  
  pca_data_df <- as.data.frame(pca_data)
  pca_data_df$cluster <-kmeans_result$cluster
  plPCA$data$cluster <- kmeans_result$cluster
  
  plCL2 <- plPCA + 
    geom_point(size = 3,aes(col = as.factor(cluster))) +
    ggforce::geom_mark_hull(aes(fill = as.factor(cluster), group = as.factor(cluster)), alpha = 0.1) +
    labs(title = title, col = "Cluster",fill = "Cluster",
         x = paste("Dim1 (", summ[1]*100, "%)"),
         y = paste("Dim2 (", summ[2]*100, "%)")) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette)
  
  return(list(pca_data_df = pca_data_df, plPCAvar = plPCAvar, plPCA = plPCA,
              plCL2 = plCL2, dataClustered = data.frame(id = rownames(pca_data_df), Cluster = pca_data_df$cluster)) )
}

test_indipendence<-function(info_tibble,variable_1,variable_2,palette){
  table<-info_tibble%>%select(all_of(c(variable_2,variable_1)))%>%table()
  table<-table[rowSums(table)!=0,colSums(table)!=0]
  if(!is.table(table)){return(NULL)}
  else{
    df<-as.data.frame(table)
    colnames(df)<-c("Var1","Var2","Count")
    mosaicplot<-ggplot(data = df) +
      geom_mosaic(aes(weight = Count, x = product(Var2), fill = Var1),alpha=1)+
      xlab(variable_1)+
      scale_fill_manual(values=palette)+
      scale_color_manual(values=palette)+
      guides(fill=guide_legend(title=variable_2),
             color=guide_legend(title=variable_2))+
      theme(axis.title.y = element_blank(),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            plot.background = element_rect(fill = "transparent",linewidth = 0),
            panel.background= element_rect(fill = "transparent",linewidth = 0),
            legend.box.background =  element_rect(fill = "transparent",linewidth = 0),
            legend.background = element_rect(fill = "transparent",linewidth = 0),
            strip.background=element_rect(fill = "transparent",linewidth = 1))
    chisq_test<-chisq.test(table)
    df_residuals<-as.data.frame.table(chisq_test$residuals)
    colnames(df_residuals)<-c("Var1","Var2","Residual")
    plot<-mosaicplot+
      ggplot() +
      geom_tile(data =df_residuals ,
                aes(x = Var2, y = Var1, fill = Residual,width=0.9, height=0.9),color = "white") +
      geom_text(data =df,aes(x=Var2,y=Var1,label=Count), size = 4, fontface = "bold" ) +
      scale_fill_gradient2(low = "#583E60", high = "#B6A21B", mid = "white",
                           midpoint = 0, limit = c(min(df_residuals$Residual), max(df_residuals$Residual)),
                           space = "Lab", name = "Residuals")+
      xlab(variable_1)+
      theme(axis.title = element_blank(),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            plot.background = element_rect(fill = "transparent",linewidth = 0),
            panel.background= element_rect(fill = "transparent",linewidth = 0),
            legend.box.background =  element_rect(fill = "transparent",linewidth = 0),
            legend.background = element_rect(fill = "transparent",linewidth = 0),
            strip.background=element_rect(fill = "transparent",linewidth = 1))
    
    if(any(chisq_test$expected<5)|all(dim(table)==c(2,2))){
      test_type<-"Fisher"
      test<-fisher.test(table)
      plot<-plot+
        plot_annotation(
          caption = paste("p-value of the Fisher test: ",round(test$p.value,4)),
          theme = theme(plot.background = element_rect(fill = "transparent",linewidth = 0),
                        panel.background= element_rect(fill = "transparent",linewidth = 0),
                        legend.box.background =  element_rect(fill = "transparent",linewidth = 0),
                        legend.background = element_rect(fill = "transparent",linewidth = 0),
                        strip.background=element_rect(fill = "transparent",linewidth = 1))
        )
    }
    else{
      test_type<-"Chi Squared"
      test<-chisq_test
      plot<-plot+
        plot_annotation(
          caption = paste("p-value of the Chi Squared test: ",round(test$p.value,4)),
          theme = theme(plot.background = element_rect(fill = "transparent",linewidth = 0),
                        panel.background= element_rect(fill = "transparent",linewidth = 0),
                        legend.box.background =  element_rect(fill = "transparent",linewidth = 0),
                        legend.background = element_rect(fill = "transparent",linewidth = 0),
                        strip.background=element_rect(fill = "transparent",linewidth = 1))
        )
    }
    return(list(test_type=test_type,test=test,plot=plot,table=table))
  }
}
PCA3dplot = function(pca_res,pca_data_df, palette=NULL){
  # Extract PCA loadings (eigenvectors)
  pca_loadings <- as.data.frame(pca_res$rotation[, 1:3])
  pca_loadings$var <- rownames(pca_loadings)
  
  # Scale loadings for visualization
  scale_factor <- max(abs(pca_data_df$PC1)) / max(abs(pca_loadings$PC1))
  pca_loadings[, 1:3] <- pca_loadings[, 1:3] * scale_factor
  
  # Prepare eigenvectors for plotting (start at 0,0,0 and go to loading values)
  arrows_df <- data.frame(
    x = rep(0, nrow(pca_loadings)), xend = pca_loadings$PC1,
    y = rep(0, nrow(pca_loadings)), yend = pca_loadings$PC2,
    z = rep(0, nrow(pca_loadings)), zend = pca_loadings$PC3,
    text = pca_loadings$var
  )
  
  pca_data_df$color <- palette[paste(pca_data_df$cluster)]
  
  pl = plot_ly() %>%
    add_trace(
      x = pca_data_df$PC1, y = pca_data_df$PC2, z = pca_data_df$PC3,
      color =  paste0("Cluster ", pca_data_df$cluster),
      type = "scatter3d", mode = "markers",
      marker = list(size = 8, color = pca_data_df$color),
      text = rownames(pca_data_df),
      hoverinfo = "text"
    ) 
  
    scale_color_manual(values=palette)
  
  for(tr in seq_along(arrows_df$text) ){
    pl = pl %>% add_trace(
      x = c(rbind(arrows_df$x[tr], arrows_df$xend[tr])),  
      y = c(rbind(arrows_df$y[tr], arrows_df$yend[tr])),  
      z = c(rbind(arrows_df$z[tr], arrows_df$zend[tr])), 
      color = rep(arrows_df$text[tr],  each = 2),
      type = "scatter3d", mode = "lines",
      line = list(width =6),
      text = rep(arrows_df$text[tr], each = 2), 
      hoverinfo = "text"
    )
  }
  pl = pl %>%
    # Labels and layout
    layout(
      title = "3D PCA Plot",
      scene = list(
        xaxis = list(title = "PC1"),
        yaxis = list(title = "PC2"),
        zaxis = list(title = "PC3")
      )
    )
  
  return(pl)
}

chordDiagram = function(metadata,dataClustered){
  df = merge( dataClustered, metadata) %>% select(-id)%>% 
    select(Cluster, EDSS_DIAGNOSI, EDSS_PROGRESSIONE) %>%
    na.omit() %>%
    group_by(Cluster, EDSS_DIAGNOSI, EDSS_PROGRESSIONE) %>%
    count() %>%
    ungroup() %>%
    group_by(EDSS_DIAGNOSI) %>% 
    mutate(Freq1 = n / sum(n) * 100) %>%
    ungroup() %>%
    group_by(EDSS_PROGRESSIONE) %>% 
    mutate(Freq2 = n / sum(n) * 100) %>%
    ungroup()%>%
    arrange(EDSS_DIAGNOSI,EDSS_PROGRESSIONE) %>%
    group_by(EDSS_DIAGNOSI)%>%
    mutate(y2_start = cumsum(Freq1),
           y1_start = lag(y2_start, default = 0)
    )%>%
    ungroup()%>%
    arrange(EDSS_PROGRESSIONE) %>%
    group_by(EDSS_PROGRESSIONE) %>%
    mutate(y2_end = cumsum(Freq2),
           y1_end = lag(y2_end, default = 0)) %>%
    ungroup()%>%
    arrange(EDSS_DIAGNOSI)
  
  EDSS_levels = sort(na.omit(unique(c(metadata$EDSS_DIAGNOSI,metadata$EDSS_PROGRESSIONE))))
  df$Starting_Level = match(df$EDSS_DIAGNOSI,EDSS_levels)
  df$Ending_Level = match(df$EDSS_PROGRESSIONE,EDSS_levels)
  
  data = df %>%  mutate(y1_start = y1_start + (Starting_Level-1)*100,
                        y2_start = y2_start + (Starting_Level-1)*100,
                        y1_end = y1_end + (Ending_Level-1)*100,
                        y2_end = y2_end + (Ending_Level-1)*100)
  
  data$id = 1:nrow(data)
  # construct x-spline grobs
  
  positions_to_flow <- function(x0, x1, ymin0, ymax0, ymin1, ymax1) {
    segments <- 48
    
    curve_fun <- function(x) 3*x^2 - 2*x^3
    i_fore <- seq(0, 1, length.out = segments + 1)
    f_fore <- curve_fun(i_fore)
    x_fore <- x0 + (x1 - x0) * i_fore
    data.frame(
      x = x_fore,
      ymin = ymin0 + (ymin1 - ymin0) * f_fore,
      ymax = rev(ymax1 + (ymax0 - ymax1) * f_fore ) )
  }
  grobs <- lapply(split(data, seq_len(nrow(data))), function(row) {
    # path of spline or unit curve
    f_path <- positions_to_flow(x0 = 1, x1=3,
                                ymin0 = row$y1_start, ymax0 = row$y2_start,
                                ymin1 = row$y1_end,  ymax1 = row$y2_end)
    
    f_path$col = row$Cluster
    f_path$id = row$id
    f_path
  })
  
  flows = do.call(rbind,grobs)
  
  plChord = ggplot(flows) +
    geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax,group = id, fill = as.factor(col), col = as.factor(col)), alpha = 0.6) +
    geom_hline(yintercept = seq(0, max(flows$ymax), 100), linetype = "dashed")+
    scale_y_continuous(breaks = sort(c( seq(0, max(flows$ymax), 100),  seq(101, max(flows$ymax), 100)) ),
                       labels = rep(c("0%","100%"), 8) ) +
    scale_x_continuous(breaks = c(1, 3), labels = c("DIAGNOSIS", "PROGRESSION")) +
    theme_minimal(base_size = 14) +
    labs(title = "EDSS - Ribbon Flow", x = NULL, y = "", fill = "Cluster", color = "Cluster")+
    theme(legend.position = "none",plot.margin = margin(0, 0, 0, 0, "pt"))
  
  head(data)
  EDSS = seq(50, max(data$y2_start),  100 )
  names(EDSS) = unique(data$EDSS_DIAGNOSI)
  data$Freq1Axis = EDSS[paste(data$EDSS_DIAGNOSI)]
  data$Freq2Axis = EDSS[paste(data$EDSS_PROGRESSIONE)]
  
  plright = ggplot(data) +
    geom_bar(aes(y = Freq1, x = Freq1Axis, fill = as.factor(Cluster) ), stat="identity", position = "dodge")+
    lims(y = c(0,100))+
    coord_flip()+
    scale_x_continuous(sec.axis = dup_axis(breaks = rev(EDSS), labels = names(rev(EDSS)))) +
    scale_y_reverse()+
    labs(x = "", y = "% Freq in each EDSS score")+
    theme_minimal(base_size = 14)+
    theme(legend.position = "none", 
          axis.text.y = element_text(hjust = 1),
          panel.grid.major.y = element_blank(), 
          axis.text.y.left = element_blank(),
          axis.text.y.right = element_text(hjust = 1,face = "bold",size = 16),
          plot.margin = margin(0, 0, 0, 0, "pt")) 
  
  plleft = ggplot(data) +
    geom_bar(aes(y = Freq2, x = (Freq2Axis), fill = as.factor(Cluster) ), stat="identity", position = "dodge")+
    coord_flip()+
    scale_x_continuous(breaks = EDSS, labels = names(EDSS))+
    labs(x = "", y = "% Freq in each EDSS score")+
    lims(y = c(0,100))+
    theme_minimal(base_size = 14)+
    theme(legend.position = "none",plot.margin = margin(0, 0, 0, 0, "pt"),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(hjust = 1,face = "bold",size = 16))  
  
  layout <- c(
    area(t = 1, l = 1, b = 3, r = 2),
    area(t = 1, l = 3, b = 3, r = 6),
    area(t = 1, l = 7, b = 3, r = 8)
  )
  
  
  return(
    plright+ plChord + plleft +
      plot_layout(design = layout)
  )
}

Metadata = readRDS("www/metadata.Rds")
DataComplete = readRDS("www/Data.Rds")
