selected_var <- "Shannon"
selected_Method <- "Both"
data_tool <- "individual"
selected_patient <- "MS"
selected_event <- "T12"
selected_perc = "001"

VarsStat <- c(
  "sex", "clinical_presentation", "gc_treatment", "subtentorial_lesions",
  "spinal_cord", "gadolinium_contrast", "lesion_burden",
  "WORSENING", "EDSS_DIAGNOSI", "EDSS_PROGRESSIONE","age"
)
names(VarsStat) <- c(
  "Sex", "Clinical Presentation", "Treatment with Glucocorticoids", "Subtentorial Lesions",
  "Spinal Cord", "Gadolinium Contrast", "Lesion Burden",
  "Worsening", "EDSS Diagnosis", "EDSS Progression","Age"
)
source("Functions.R")

server <- function(input, output, session) {
  dataReact <- reactiveValues(plotLIST = NULL, dataClustered = NULL)
  
  # Data Explorer: Load and display bacterial p-value tables
  output$bacteriaTable <- renderDT({
    # req(input$data_perc)
    # req(data_tool)
    # req(input$data_discriminant)
    
    # Construct file path based on selected percentage
    file_path <- paste0("www/tables/001_bacteria_maaslin.tsv")
    
    # Read the table
    data <- read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    
    cols_to_keep <- "species"
    pattern <- paste0("P.val_", data_tool, "_", input$data_discriminant)
    cols_to_keep <- c(cols_to_keep, grep(pattern, colnames(data), value = TRUE))

    data_filtered <- data[, cols_to_keep, drop = FALSE]
    
    
    # Create more readable column names
    col_names <- colnames(data_filtered)
    col_names <- gsub("P.val_", "", col_names)
    col_names <- gsub("_gadolinium_contrast", " (Gadolinium)", col_names)
    col_names <- gsub("_lesion_burden", " (Lesion Burden)", col_names)
    col_names <- gsub("_spinal_cord", " (Spinal Cord)", col_names)
    col_names <- gsub("_subtentorial", " (Subtentorial)", col_names)
    col_names <- gsub("maaslin", "MaAsLin3", col_names, ignore.case = TRUE)
    col_names <- gsub("limma", "limma", col_names, ignore.case = TRUE)
    col_names <- gsub("lefse", "LEfSe", col_names, ignore.case = TRUE)
    col_names <- gsub("aldex2", "ALDEx2", col_names, ignore.case = TRUE)
    col_names <- gsub("ancombc2", "ANCOM-BC2", col_names, ignore.case = TRUE)
    col_names <- gsub("species", "Species", col_names)
    colnames(data_filtered) <- col_names
    
    # Create caption with filter info
    caption_text <- paste0("Bacterial P-values Table (", input$data_perc, "% abundance threshold")
    caption_text <- paste0(caption_text, ")")
    
    # Create a more user-friendly data table with filtering options
    dt <- datatable(data_filtered,
                    options = list(
                      pageLength = 25,
                      scrollX = TRUE,
                      searchHighlight = TRUE,
                      dom = "Bfrtip",
                      columnDefs = list(
                        list(className = "dt-left", targets = 0),
                        list(className = "dt-center", targets = 1:(ncol(data_filtered) - 1))
                      )
                    ),
                    filter = "top",
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      style = "caption-side: top; text-align: left; color: black; font-size: 120%;",
                      caption_text
                    )
    )
    
    # Apply formatting only to numeric columns (skip first column which is species)
    if (ncol(data_filtered) > 1) {
      dt <- dt %>%
        formatRound(columns = 2:ncol(data_filtered), digits = 4) %>%
        formatStyle(
          columns = 2:ncol(data_filtered),
          backgroundColor = styleInterval(c(0.01, 0.05), c("#ffe6e6", "#fff9e6", "white"))
        )
    }
    
    dt
  })
  
  # Placeholder for other analysis table - you can replace this with your actual data
  output$otherTable <- renderDT({
    # This is a placeholder - replace with your actual table file when ready
    # For example: data <- read.table("www/tables/other_analysis.tsv", header = TRUE, sep = "\t")
    
    placeholder_data <- data.frame(
      Message = "Other analysis table will be loaded here. Please add your table file to www/tables/ folder."
    )
    
    datatable(placeholder_data,
              options = list(
                pageLength = 10,
                dom = "t"
              ),
              rownames = FALSE
    )
  })
  
  observeEvent(input$button_start, {
    # req(input$selected_var)
    # req(selected_perc)
    # req(selected_patient)
    
    # reduce = input$reduce
    reduce <- TRUE
    isolate({
      Data <- DataComplete %>%
        as.data.frame() %>%
        filter(
          Alpha %in% selected_var,
          Method == selected_Method,
          Subset == selected_perc
        ) %>%
        mutate(MetricType = paste0(Discriminant, "_", Alpha)) %>%
        select(-Subset, -Alpha, -Discriminant, -Method) %>%
        tidyr::spread(key = MetricType, value = Value)
      
      
      if (selected_patient != "All") {
        Data[grepl(pattern = selected_patient, x = Data$Patient), ] -> Data
      }
      
      rownames(Data) <- Data$Patient
      Data <- Data %>% select(-Patient)
      
      Data <- Data[, colSums(abs(Data), na.rm = T) > 0]
      Data <- Data %>% na.omit()
      
      # Remove columns with zero variance
      Data <- Data[, sapply(Data, function(x) var(x, na.rm = TRUE) != 0)]
      
      dataReact$plotLIST <- plotLIST <- cluster.generation(data = Data, Kmeans.before = reduce)
      
      output$FeatureComp <- renderDT(
        {
          round(plotLIST$pca_res$rotation, digits = 3)
        },
        rownames = T,
        options = list(dom = "t", scrollX = TRUE)
      )
      
      # Display results
      output$pcaPlotvar <- renderPlot({
        plotLIST$plPCAvar
      })
      output$pcaPlot <- renderPlot({
        plotLIST$plPCA
      })
      
      kbest <- as.numeric(names(plotLIST$AllClusteringIndex$bestK[1]))
      
      output$clustChoicePlot <- renderPlot({
        plotLIST$silhouette +
          geom_vline(aes(
            xintercept = kbest,
            color = paste0(
              "Best k considering ",
              plotLIST$AllClusteringIndex$bestK[1], " indexes over ",
              sum((plotLIST$AllClusteringIndex$bestK))
            )
          ), linetype = "dashed") +
          labs(col = "") + theme(legend.position = "top")
      })
      bestk <- as.data.frame(dataReact$plotLIST$AllClusteringIndex$bestK)
      colnames(bestk) <- c("Number of\n clusters", "Number of indexes\n in accordance")
      output$ClusterIndexesTable <- renderTable(bestk)
      updateSliderInput(session, "clusterSlider", value = kbest, min = 2, max = 6, step = 1)
      # updateSelectInput(session = session, "StatVar",
      #                  choices = c("", colnames(Metadata %>% select(-id,-Event,-EventTime))))
    })
  })
  
  observeEvent(input$clusterStart, {
    shinybusy::show_modal_spinner()
    isolate({
      req(input$clusterSlider != 0)
      req(dataReact$plotLIST)
      Nclust <- input$clusterSlider
      paletteCluster <- RColorBrewer::brewer.pal(Nclust, "Set1")[1:Nclust]
      names(paletteCluster) <- paste0(1:Nclust)
      
      plotLIST <- cluster.plot(
        data = dataReact$plotLIST$Data,
        Kmeans.before = T, # input$reduce,
        k = Nclust,
        palette = paletteCluster
      )
      
      output$PCA3d <- renderPlotly({
        PCA3dplot(
          pca_res = dataReact$plotLIST$pca_res,
          pca_data_df = plotLIST$pca_data_df,
          palette = paletteCluster
        )
      })
      output$clustPlot <- renderPlot({
        plotLIST$plCL2
      })
      
      output$clusterMappingTable <- renderDT({
        req(dataReact$dataClustered)
        datatable(dataReact$dataClustered,
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE,
                  filter = "top"
        )
      })
      
      output$downloadClusterTable <- downloadHandler(
        filename = function() {
          paste0("cluster_mapping_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(dataReact$dataClustered, file, row.names = FALSE)
        }
      )
      
      dataperc <- plotLIST$dataClustered %>%
        mutate(Status = ifelse(grepl("MS", id), "MS", "HD")) %>%
        group_by(Cluster, Status) %>%
        summarise(count = n()) %>%
        group_by(Cluster) %>%
        mutate(perc = count / sum(count))
      
      pl <- dataperc %>% ggplot(aes(x = as.factor(Cluster), y = perc * 100, fill = factor(Status))) +
        geom_bar(stat = "identity", width = 0.7) +
        labs(x = "Cluster", y = "%", fill = "Status") +
        theme_minimal(base_size = 14) +
        geom_text(aes(label = paste0(round(perc * 100, 1), "%")),
                  position = position_stack(vjust = 0.5),
                  col = "white", size = 4, fontface = "bold"
        )
      
      output$percPlot <- renderPlot({
        pl
      })
      
      dataReact$dataClustered <- plotLIST$dataClustered
    })
    shinybusy::remove_modal_spinner()
  })
  
  output$SurvivalPlot <- renderPlot({
    dataClustered <- req(dataReact$dataClustered)
    vardata <- merge(dataClustered, Metadata) %>% select(-id)
    if (selected_event == "T24") {
      vardata <- vardata %>%
        select(-Event, -EventTime) %>%
        rename(Event = Event_T24, EventTime = EventTime_T24)
    }
    
    vardata <- vardata %>%
      filter(!is.na(Event)) %>%
      mutate(Cluster = paste(Cluster))
    list2env(list(vardata = vardata), envir = .GlobalEnv)
    fit <- survfit(Surv(vardata$EventTime, vardata$Event) ~ Cluster, data = vardata)
    list2env(list(fit = fit), envir = .GlobalEnv)
    
    Nclust <- as.numeric(max(dataClustered$Cluster))
    paletteCluster <- RColorBrewer::brewer.pal(Nclust, "Set1")[1:Nclust]
    names(paletteCluster) <- paste0("Cluster=", 1:Nclust)
    
    ggsurv <- survminer::ggsurvplot(
      fit = fit, data = vardata,
      xlab = "Weeks", ylab = "Worsening", palette = paletteCluster,
      size = 2, pval = TRUE, risk.table = TRUE, conf.int = F,
      risk.table.col = "strata", ggtheme = theme_bw(),
      surv.median.line = "hv"
    )
    print(fit)
    
    
    # dataReact$dataClustered -> datatmp
    # datatmp$KS = ifelse(datatmp$Cluster  == "1", "LowRisk", ifelse(datatmp$Cluster  == "2", "HighRisk", "MediumRisk"))
    # saveRDS(list(data = datatmp, plot =  ggsurv$plot, dataCluster =  dataReact$plotLIST),"~/Desktop/Species05Clustered_3CL.Rds")
    
    ggsurv
  })
  
  output$ChordDiagram <- renderPlot({
    dataClustered <- req(dataReact$dataClustered)
    vardata <- merge(dataClustered, Metadata) %>%
      select(-id)
    Nclust <- as.numeric(max(dataClustered$Cluster))

    # paletteCluster = viridisLite::turbo(Nclust)
    paletteCluster <- RColorBrewer::brewer.pal(Nclust, "Set1")[1:Nclust]
    names(paletteCluster) <- paste0(1:Nclust)
    
    # saveRDS( merge(
    #   Metadata %>% select(id, EDSS_DIAGNOSI, EDSS_PROGRESSIONE),
    #   dataClustered %>% select(id, Cluster)
    # ),"DataCLandEDSS.Rds")
    
    pl <- chordDiagram(Metadata, dataClustered)
    
    pl & scale_fill_manual(values = paletteCluster) & scale_color_manual(values = paletteCluster)
  })
  # saveRDS(dataReact$dataClustered,"~/Desktop/dataClustered_5perc.Rds")
  observe({
    dataClustered <- req(dataReact$dataClustered)
    StatVar <- input$StatVar
    
    vardata <- merge(dataClustered, Metadata) %>% select(-id, -EventTime, -Event)
    vars <- colnames(vardata %>% select(-Cluster))
    
    if (StatVar == "") {
      table <- statistical_tests(vardata)
      table = table[na.omit( match(table$Variable, VarsStat)) ,]
      table$Variable <- names(VarsStat)[match(table$Variable, VarsStat)]
      output$statTable <- renderTable(table)
    } else {
      res <- test_indipendence(
        info_tibble = vardata,
        variable_1 = "Cluster", variable_2 = input$StatVar,
        palette = viridisLite::cividis(length(unique(vardata[[input$StatVar]])))
      )
      
      output$statMosaic <- renderPlot({
        res$plot & theme(
          legend.position = "bottom",
          axis.title.x = element_text(
            face = "bold.italic",
            size = rel(1.2)
          ),
          axis.text = element_text(
            face = "italic",
            size = rel(1)
          )
        )
      })
    }
  })
  
  output$butt_download <- downloadHandler(
    filename = function() {
      paste0("Cluster_", selected_var, ifelse(input$reduce, "_PCAreduced", ""), ".Rds")
    },
    content = function(file) {
      saveRDS(dataReact$dataClustered, file)
    }
  )
}
