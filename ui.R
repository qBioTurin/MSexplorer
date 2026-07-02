library(DT)
library(shinybusy)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(ggplot2)
library(dplyr)
library(ggalluvial)

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



ui <- dashboardPage(
  skin = "blue",
  title = "MSexplorer",
  dashboardHeader(
    title = tags$span(
      icon("dna", style = "margin-right: 8px;"),
      "MSexplorer",
      style = "font-weight: bold; font-size: 20px;"
    ),
    titleWidth = 280
  ),
  dashboardSidebar(
    width = 280,
    tags$style(HTML("
      .sidebar-menu li a {
        font-size: 15px;
        padding: 15px 5px 15px 20px;
        transition: all 0.3s ease;
      }
      .sidebar-menu li a:hover {
        background-color: rgba(255, 255, 255, 0.1) !important;
        border-left: 4px solid #3c8dbc;
        padding-left: 16px;
      }
      .sidebar-menu .active > a {
        border-left: 4px solid #3c8dbc;
      }
    ")),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Explorer", tabName = "dataexplorer", icon = icon("database")),
      menuItem("PCA Analysis", tabName = "pca", icon = icon("chart-line")),
      menuItem("Clustering", tabName = "clustering", icon = icon("sitemap")),
      menuItem("3D PCA", tabName = "pca3d", icon = icon("cube")),
      menuItem("Statistics", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("Survival Analysis", tabName = "survival", icon = icon("heartbeat")),
      menuItem("Chord Diagram", tabName = "chord", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom styling */
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }

        /* Box shadows and transitions */
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          transition: all 0.3s ease;
          margin-bottom: 20px;
        }

        .box:hover {
          box-shadow: 0 4px 16px rgba(0,0,0,0.15);
          transform: translateY(-2px);
        }

        .box-header {
          border-radius: 8px 8px 0 0;
        }

        /* Info boxes styling */
        .info-box-icon {
          border-radius: 8px 0 0 8px;
        }

        /* Value boxes */
        .small-box {
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          transition: all 0.3s ease;
        }

        .small-box:hover {
          box-shadow: 0 4px 16px rgba(0,0,0,0.15);
          transform: translateY(-2px);
        }

        /* Buttons */
        .btn {
          border-radius: 6px;
          font-weight: 600;
          transition: all 0.3s ease;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        .btn:hover {
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }

        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
        }

        /* Feature cards */
        .feature-card {
          background: white;
          border-radius: 12px;
          padding: 25px;
          margin-bottom: 20px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.08);
          transition: all 0.3s ease;
          border-left: 4px solid #3c8dbc;
        }

        .feature-card:hover {
          box-shadow: 0 6px 20px rgba(0,0,0,0.12);
          transform: translateY(-4px);
        }

        .feature-icon {
          font-size: 42px;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          margin-bottom: 15px;
        }

        .feature-title {
          font-size: 20px;
          font-weight: 700;
          color: #2c3e50;
          margin-bottom: 12px;
        }

        .feature-description {
          font-size: 14px;
          color: #5a6c7d;
          line-height: 1.6;
        }

        /* Welcome section */
        .welcome-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 30px;
          border-radius: 12px;
          margin-bottom: 30px;
          box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
        }

        .welcome-title {
          font-size: 32px;
          font-weight: 800;
          margin-bottom: 15px;
        }

        .welcome-subtitle {
          font-size: 16px;
          opacity: 0.95;
          line-height: 1.6;
        }

        /* Data table styling */
        .dataTables_wrapper {
          border-radius: 8px;
          overflow: hidden;
        }

        table.dataTable thead th {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          font-weight: 600;
        }

        table.dataTable tbody tr:hover {
          background-color: rgba(102, 126, 234, 0.05);
        }

        /* Select inputs */
        .selectize-input {
          border-radius: 6px;
          border: 2px solid #e1e8ed;
          transition: all 0.3s ease;
        }

        .selectize-input:focus {
          border-color: #667eea;
          box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
        }

        /* Slider */
        .irs--shiny .irs-bar {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }

        .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }

        /* Fix selectize dropdown z-index to appear above boxes */
        .selectize-dropdown {
          z-index: 10000 !important;
        }

        .selectize-input {
          z-index: 1000 !important;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "home",
        div(
          class = "welcome-header",
          div(
            class = "welcome-title",
            icon("dna", style = "margin-right: 15px;"),
            "Welcome to MSexplorer"
          ),
          div(
            class = "welcome-subtitle",
            "A comprehensive Shiny application for exploring and analyzing Multiple Sclerosis (MS) microbiome data. Navigate through the menu to discover powerful analysis tools and visualizations."
          )
        ),
        fluidRow(
          column(12,
                 align = "center",
                 img(src = "home.png", width = "30%", style = "margin: 20px 0; border-radius: 12px; box-shadow: 0 8px 24px rgba(0,0,0,0.15);")
          )
        ),
        fluidRow(
          column(
            4,
            div(
              class = "feature-card",
              div(class = "feature-icon", icon("database")),
              div(class = "feature-title", "Data Explorer"),
              div(
                class = "feature-description",
                "Explore differential abundance analysis results for bacterial species. View p-values from MaAsLin3 across four clinical discriminants: subtentorial lesions, gadolinium contrast, lesion burden, and spinal cord lesions. Filter and search bacteria with interactive tables."
              )
            )
          ),
          column(
            4,
            div(
              class = "feature-card",
              div(class = "feature-icon", icon("chart-line")),
              div(class = "feature-title", "PCA Analysis"),
              div(
                class = "feature-description",
                "Perform Principal Component Analysis using entropy-based metrics computed from relative abundance of differential species. Visualize patient clusters and feature contributions across different abundance thresholds."
              )
            )
          ),
          column(
            4,
            div(
              class = "feature-card",
              div(class = "feature-icon", icon("sitemap")),
              div(class = "feature-title", "Clustering"),
              div(
                class = "feature-description",
                "Identify patient clusters using k-means clustering. Determine optimal number of clusters and visualize group distributions. Compare cluster characteristics across clinical parameters."
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              class = "feature-card",
              div(class = "feature-icon", icon("cube")),
              div(class = "feature-title", "3D PCA"),
              div(
                class = "feature-description",
                "Interactive 3D visualization of PCA results. Rotate and explore the three-dimensional representation of patient clustering patterns with enhanced spatial perspective."
              )
            )
          ),
          column(
            4,
            div(
              class = "feature-card",
              div(class = "feature-icon", icon("chart-bar")),
              div(class = "feature-title", "Statistics"),
              div(
                class = "feature-description",
                "Explore statistical associations between clusters and clinical variables. View mosaic plots and contingency tables for sex, clinical presentation, treatment, and disease progression markers."
              )
            )
          ),
          column(
            4,
            div(
              class = "feature-card",
              div(class = "feature-icon", icon("heartbeat")),
              div(class = "feature-title", "Survival Analysis"),
              div(
                class = "feature-description",
                "Kaplan-Meier survival curves comparing disease worsening across patient clusters. Assess time-to-event outcomes and cluster-specific prognosis patterns with log-rank tests."
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              class = "feature-card",
              div(class = "feature-icon", icon("project-diagram")),
              div(class = "feature-title", "Chord Diagram"),
              div(
                class = "feature-description",
                "Visualize relationships between clusters and clinical variables using alluvial/chord diagrams. Track how patients flow through different categorical classifications."
              )
            )
          )
        ),
        p(img(src = "loghi.png", height = "40%", width = "99%", style = "margin:20px 0px"), align = "center"),
        column(12,
               align = "center",
               hr(),
               div(
                 style = "font-size:14px; color: #666666;",
                 HTML("For citation details, please refer to the associated publication and the repository metadata.")
               ),
               br(),
               tags$a(
                 href = "https://github.com/qBioTurin/MSexplorer",
                 target = "_blank",
                 icon("github"), " GitHub Repository",
                 style = "color: #1d8fbd; font-size:14px; text-decoration:none;"
               )
        )
      ),
      tabItem(
        tabName = "dataexplorer",
        tags$div(
          style = "padding: 10px;",
            fluidRow(
              box(
                title = tags$span(icon("database", style = "margin-right: 10px;"), "Data Explorer: Bacterial P-values Analysis"),
                status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                tags$div(
                  style = "font-size: 14px; line-height: 1.8;",
                  h4(style = "color: #2c3e50; font-weight: 600; margin-top: 0;", "Workflow Overview"),
                  p(
                    "This workflow begins from a taxonomic abundance report produced by ",
                    tags$strong("Bracken"), ". We perform an automated decontamination step using ",
                    tags$strong("DESeq2"), " and then applied a minimum abundance threshold of  ",
                    tags$strong("0.1% (0.001)"), 
                    " to generate the input dataset for differential abundance analysis."
                  ),
                  p(
                    "We used  ", tags$strong("MaAsLin3 as differential analysis method"),
                    " considering four MS clinical features (discriminants) of interest:"
                  ),
                  tags$ul(
                    style = "margin-left: 20px; margin-bottom: 15px;",
                    tags$li(tags$strong("Subtentorial Lesions")),
                    tags$li(tags$strong("Gadolinium Contrast")),
                    tags$li(tags$strong("Lesion Burden")),
                    tags$li(tags$strong("Spinal Cord Lesions"))
                  ),
                  p(
                    "A species is defined as a ",
                    tags$strong("differential abundant signature (DAS)"), " if it shows a ",
                    tags$em("p-value < 0.05"), " for at least one of the clinical features. 
                    The merged outputs are then used to generate the final DAS lists and downstream visualizations."
                  ),
                  tags$div(
                    style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #667eea; border-radius: 4px;",
                    icon("lightbulb", style = "color: #667eea; margin-right: 8px;"),
                    tags$strong("Tip:"), " Use the filters below to focus on specific analysis tools and clinical discriminants."
                  )
                )
              )
            ),
          fluidRow(
            box(
              width = 12, status = "primary",
              tags$div(
                style = "margin-bottom: 15px; padding: 12px; background-color: #f8f9fa; border-radius: 8px; border-left: 4px solid #667eea;",
                tags$strong(style = "color: #2c3e50; font-size: 14px;", icon("palette"), " P-value Color Legend:"),
                tags$div(
                  style = "margin-top: 8px; display: flex; align-items: center; gap: 20px;",
                  tags$span(
                    style = "display: inline-flex; align-items: center;",
                    tags$span(style = "display: inline-block; width: 30px; height: 20px; background-color: #ffe6e6; border: 1px solid #ddd; border-radius: 4px; margin-right: 8px;"),
                    tags$span(style = "color: #2c3e50; font-size: 13px;", tags$strong("p < 0.01"), " (highly significant)")
                  ),
                  tags$span(
                    style = "display: inline-flex; align-items: center;",
                    tags$span(style = "display: inline-block; width: 30px; height: 20px; background-color: #fff9e6; border: 1px solid #ddd; border-radius: 4px; margin-right: 8px;"),
                    tags$span(style = "color: #2c3e50; font-size: 13px;", tags$strong("0.01 ≤ p < 0.05"), " (significant)")
                  ),
                  tags$span(
                    style = "display: inline-flex; align-items: center;",
                    tags$span(style = "display: inline-block; width: 30px; height: 20px; background-color: white; border: 1px solid #ddd; border-radius: 4px; margin-right: 8px;"),
                    tags$span(style = "color: #2c3e50; font-size: 13px;", tags$strong("p ≥ 0.05"), " (not significant)")
                  )
                )
              ),
              DT::DTOutput("bacteriaTable")
            )
          )
        )
      ),
      tabItem(
        tabName = "pca",
        tags$div(
          style = "padding: 10px;",
          fluidRow(
            box(
              width = 12, status = "primary",
              fluidRow(
                # column(
                #   3,
                #   #tags$label(style = "font-weight: 600; font-size: 14px; color: #2c3e50;", "Select relative abundance threshold:"),
                #   # selectInput("selected_perc",
                #   #             label = NULL,
                #   #             choices = c("0.1%" = "001", "1%" = "01", "0%" = "0", "01WoC" = "01WoC"), selected = "001"
                #   # ),
                #   # selectInput(inputId = "selected_var",
                #   #             label =  "Select Method to consider:",
                #   #             choice = c("Both","Maaslin3"), selected = "Both" ),
                #   fluidRow(
                #     column(
                #       12,
                #       actionButton("button_start",
                #                    label = tags$span(icon("play"), " Start Analysis"),
                #                    class = "btn-primary",
                #                    style = "width: 100%; margin-top: 10px; font-size: 16px; padding: 10px;"
                #       )
                #     )
                #   )
                #   # choices = c("0.1%"="264","0.325%" = "167","1%" = "116","5%" = "39"),selected = "264" )
                # ),
                column(
                  12,
                  withMathJax(
                    HTML("
  <h3><b>PCA Index: Entropy-Based Metric</b></h3>

  <p>
  The index used for the PCA analysis in this application is the <b>entropy</b>
  computed from the relative abundance of differential species for each clinical factor.
  </p>

  <p>
  Let
  \\[
  CF = \\{\\textit{lesionBurden},\\ \\textit{spinalcord},\\ \\textit{subtentorial},\\ \\textit{gadolinium}\\}
  \\]
  be the set of considered <b>clinical factors</b>, and
  </p>

  <p>
  \\[
  DAS_{cf} = \\{ \\text{species} \\mid \\text{differential abundance species given } cf \\}
  \\]
  the set of <b>differentially abundant species</b> associated with each clinical factor
  \\( cf \\in CF \\).
  </p>

  <p>
  We define
  \\[
  MS = \\{ pts_1, \\dots, pts_{48} \\}
  \\]
  as the set of <b>patients</b> included in the study.
  </p>

  <p>
  For each patient \\( pts \\in MS \\) and clinical factor \\( cf \\in CF \\), let
  \\( relAbd_j^{pts,cf} \\)
  be the <b>relative abundance</b> of the \\( j^{th} \\) species (\\( j \\in DAS_{cf} \\)).
  </p>

  <p>
  The <b>entropy</b> for each patient–factor pair is then computed as:
  </p>

  <p>
  \\[
  H_{pts,cf} = - \\sum_{ j \\in DAS_{cf} } relAbd_j^{pts,cf} \\log_2(relAbd_j^{pts,cf})
  \\qquad pts \\in MS,\\ cf \\in CF
  \\]
  </p>

  <p>
  This entropy value \\( H_{pts,cf} \\) serves as the <b>index</b> used in the PCA to summarize
  the variability in microbial profiles across patients and clinical conditions.
  </p>
  ")
                  )
                ),
                # column(4,
                #        selectInput("selected_patient", label = "Patients to consider:",
                #                    choices = c("Healthy+Patients" = "All", "Patients" = "MS"),selected = "All" )#"Healthy"="HD","Patients" = "MS"),selected = "All" )
                # ),
              )
            )
          ),
          fluidRow(
            box(plotOutput("pcaPlotvar"), width = 6),
            box(DT::DTOutput("FeatureComp"), width = 6),
            box(plotOutput("pcaPlot", width = "100%", height = "600px"), width = 12),
            box(title = tags$span(icon("table"), " Input Data Table"),
                status = "primary", solidHeader = TRUE,
                DT::DTOutput("inputData"),
                downloadButton("downloadInputData", "Download Data", style = "margin-top: 10px;"),
                width = 12)
          )
        )
      ),
      tabItem(
        tabName = "clustering",
        tags$div(
          style = "padding: 10px;",
          fluidRow(
            box(
              title = tags$span(icon("chart-line"), " Silhouette Analysis"),
              status = "primary", solidHeader = TRUE,
              plotOutput("clustChoicePlot", height = "300px"), width = 6
            ),
            box(
              title = tags$span(icon("sliders-h"), " Cluster Configuration"),
              status = "info", solidHeader = TRUE,
              fluidRow(
                column(
                  12,
                  tags$label(style = "font-weight: 600; font-size: 14px; color: #2c3e50;", "Number of Clusters:"),
                  sliderInput("clusterSlider", label = NULL, min = 2, max = 6, value = 2, step = 1)
                )
              ),
              fluidRow(
                column(
                  12,
                  tableOutput("ClusterIndexesTable"),
                  actionButton("clusterStart",
                               label = tags$span(icon("sitemap"), " Run Clustering"),
                               class = "btn-primary",
                               style = "width: 100%; margin-top: 15px; font-size: 16px; padding: 12px;"
                  )
                )
              ),
              width = 6
            )
          ),
          fluidRow(
            box(
              conditionalPanel(
                condition = "input.selected_patient == 'All'",
                plotOutput("percPlot")
              ),
              plotOutput("clustPlot", height = "600px"),
              width = 12
            )
          ),
          fluidRow(
            box(
              title = tags$span(icon("table"), " Cluster Mapping"),
              status = "primary", solidHeader = TRUE,
              DT::DTOutput("clusterMappingTable"),
              downloadButton("downloadClusterTable", "Download Mapping", style = "margin-top: 10px;"),
              width = 12
            )
          )
        )
      ),
      tabItem(
        tabName = "pca3d",
        tags$div(
          style = "padding: 10px;",
          fluidRow(
            box(
              title = tags$span(icon("cube"), " Interactive 3D PCA Visualization"),
              status = "primary", solidHeader = TRUE,
              plotlyOutput("PCA3d", height = "600px"), width = 12
            )
          )
        )
      ),
      tabItem(
        tabName = "statistics",
        tags$div(
          style = "padding: 10px;",
          fluidRow(
            box(
              title = tags$span(icon("cog"), " Variable Selection"),
              status = "info", solidHeader = TRUE,
              tags$label(style = "font-weight: 600; font-size: 14px; color: #2c3e50;", "Metadata Variable:"),
              selectInput("StatVar", label = NULL, choices = c("", VarsStat), selected = ""),
              width = 4
            ),
            conditionalPanel(
              condition = "input.StatVar != ''",
              box(
                title = tags$span(icon("chart-bar"), " Statistical Analysis"),
                status = "primary", solidHeader = TRUE,
                plotOutput("statMosaic", height = "600px"), width = 8
              )
            ),
            conditionalPanel(
              condition = "input.StatVar == ''",
              box(
                title = tags$span(icon("table"), " Summary Statistics"),
                status = "primary", solidHeader = TRUE,
                tableOutput("statTable"), width = 8
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "survival",
        tags$div(
          style = "padding: 10px;",
          fluidRow(
            box(
              title = tags$span(icon("heartbeat"), " Kaplan-Meier Survival Curves"),
              status = "primary", solidHeader = TRUE,
              # selectInput("selected_event", label = "Select time:",
              #             choices = c("12 months"="T12","24 months" = "T24"),selected = "T12" ),
              plotOutput("SurvivalPlot", height = "600px"), width = 12
            )
          )
        )
      ),
      tabItem(
        tabName = "chord",
        tags$div(
          style = "padding: 10px;",
          fluidRow(
            box(
              title = tags$span(icon("project-diagram"), " Alluvial/Chord Diagram"),
              status = "primary", solidHeader = TRUE,
              plotOutput("ChordDiagram", height = "600px"), width = 12
            )
          )
        )
      )
    )
  )
)
