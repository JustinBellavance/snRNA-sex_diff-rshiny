# set up -----------------------------
# load libraries

#library(shiny)
library(tidyverse)
library(data.table)
library(readxl)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("volcano.R")

data <- read.csv("data/all_cells_all_genes_nopseudo.csv")

data_neurons <- read.csv("data/Neuron_wald_all_genes_nopseudo.csv")

data_SDH_neurons <- read.csv("data/all_SDH_cells_all_genes_nopseudo.csv")

data_SDH_inhib_neurons <- read.csv("data/all_SDHinhib_cells_all_genes_nopseudo.csv")

data_SDH_excit_neurons <- read.csv("data/all_SDHexcit_cells_all_genes_nopseudo.csv")

#names(data)[names(data) == colnames(data)[1]] <- 'gene'

#replace all rows where log2FoldChange == NA to 0.
data$log2FoldChange[is.na(data$log2FoldChange)] <- 0
data_neurons$log2FoldChange[is.na(data_neurons$log2FoldChange)] <- 
data_SDH_neurons$log2FoldChange[is.na(data_SDH_neurons$log2FoldChange)] <- 0
data_SDH_inhib_neurons$log2FoldChange[is.na(data_SDH_inhib_neurons$log2FoldChange)] <- 0
data_SDH_excit_neurons$log2FoldChange[is.na(data_SDH_excit_neurons$log2FoldChange)] <- 0

#replace all rows where pvalue, padj == NA to 1.
data$pvalue[is.na(data$pvalue)] <- 1
data$padj[is.na(data$padj)] <- 1

data_neurons$pvalue[is.na(data_neurons$pvalue)] <- 1
data_neurons$padj[is.na(data_neurons$padj)] <- 1

data_SDH_neurons$pvalue[is.na(data_SDH_neurons$pvalue)] <- 1
data_SDH_neurons$padj[is.na(data_SDH_neurons$padj)] <- 1

data_SDH_inhib_neurons$pvalue[is.na(data_SDH_inhib_neurons$pvalue)] <- 1
data_SDH_inhib_neurons$padj[is.na(data_SDH_inhib_neurons$padj)] <- 1

data_SDH_excit_neurons$pvalue[is.na(data_SDH_excit_neurons$pvalue)] <- 1
data_SDH_excit_neurons$padj[is.na(data_SDH_excit_neurons$padj)] <- 1

# ui -----------------------------
#have one page for all different data frames means I have to uniquely name all of the same attributes 4 times.
ui <- fluidPage(
  
  tabsetPanel(
      id = "tabsetPanelID",
      type = "tabs",
      tabPanel("All Cells", tabsetPanel(
        tabPanel("Volcano Plot",
                 
                 h2("Interactive Volcano Plot"),
                 sidebarLayout(
                   
                   # VOLCANO PLOT SIDE PANEL ------
                   sidebarPanel(width = 4,
                                
                                # SET PVAL AND LOGFC THRESHOLDS ----- 
                                h4("Set significance and effect size thresholds:"),
                                
                                # set pvalue threshold 
                                numericInput("pvalue_threshold",
                                             "Set significance threshold", 
                                             value = 0.05, 
                                             min = 0, 
                                             step = 0.001),
                                
                                # set logfc threshold
                                uiOutput("logfc_slider"),
                                
                                # CUSTOMIZE PLOT -----
                                h4("Customize plot:"),
                                
                                # show/hide logfc and pval line
                                checkboxInput("show_pvalue_threshold",
                                              "Show significance threshold line",
                                              value = FALSE),
                                
                                # show/hide logfc lines
                                checkboxInput("show_logfc_threshold",
                                              "Show effect size threshold line",
                                              value = FALSE),
                                
                                # color differentially expressed genes
                                checkboxInput("color_by_de",
                                              "Color significantly different features",
                                              TRUE),
                                
                                # HIGHLIGHT GENES -----
                                h4("Label features of interest:"),
                                
                                #gene selector menu
                                selectizeInput("selected_genes",
                                               "Select feature(s) to label",
                                               NULL,
                                               multiple = TRUE),
                                
                                # output ui for axis label inputs
                                uiOutput("y_axis_labeler"),
                                uiOutput("x_axis_labeler"),
                                
                                # label legend
                                textInput("legend_title",
                                          "Specify legend title",
                                          value = "Differentially Expressed")
                   ),
                   # VOLCANO PLOT MAIN PANEL -----
                   mainPanel(
                     # output info from click
                     p(strong("Plot interactivity:")),
                     p("- View a point's feature label, effect size, and significance by hovering over a point."),
                     p("- Add labels to a feature of interest by using the gene selection dropdown in the sidebar or clicking on the point on the plot."),
                     p("- Remove a label by deleting the selection from the gene selection dropdown or clicking the point on plot a second time."),
                     p("- To zoom click and drag over the plot to select the area you wish to zoom in on. Then, double click to zoom into the selected area. Double click again to zoom out."),
                     verbatimTextOutput("click_info",
                                        placeholder = TRUE),
                     
                     # output ggplot volcano
                     plotOutput("volcano_plot",
                                width = "100%",
                                height = "600px",
                                hover = "volcano_hover",
                                click = "volcano_click",
                                dblclick = "volcano_dbl_click",
                                brush = brushOpts(
                                  id = "volcano_brush",
                                  resetOnNew = TRUE)),
                     
                     # Download button for plot
                     downloadButton('download_volcano', 'Download volcano plot as PDF'),
                     
                     br(),
                     br(),
                     
                     # HIGHLIGHTED GENES TABLE -----
                     dataTableOutput("gene_highlight_tbl")
                  )
                   
              ) # end sidebarLayout
        ),# end volcano plot tabPanel

        tabPanel("Dataframe",
                 sidebarLayout(
                   
                   # DATA PANEL SIDEBAR
                   sidebarPanel(width = 3,
                                
                                # some text explanation
                                em("Threshold for what is considered differentially expressed is set in Volcano Plot tab by using 
                                 the significance and effect size sliders"),
                                
                                # Show differentiall expressed genes only
                                checkboxInput("show_de",
                                              "Show only significantly different features",
                                              FALSE)),
                   
                   # DATA PANEL MAIN PANEL
                   mainPanel(dataTableOutput("gene_data")))
        ) #end of tabpanel
      )),
      tabPanel("Neurons", tabsetPanel(
        tabPanel("Volcano Plot",
                 h2("Interactive Volcano Plot"),
                 sidebarLayout(
                   
                   # VOLCANO PLOT SIDE PANEL ------
                   sidebarPanel(width = 4,
                                
                                # SET PVAL AND LOGFC THRESHOLDS ----- 
                                h4("Set significance and effect size thresholds:"),
                                
                                # set pvalue threshold 
                                numericInput("pvalue_threshold_neurons",
                                             "Set significance threshold", 
                                             value = 0.05, 
                                             min = 0, 
                                             step = 0.001),
                                
                                # set logfc threshold
                                uiOutput("logfc_slider_neurons"),
                                
                                # CUSTOMIZE PLOT -----
                                h4("Customize plot:"),
                                
                                # show/hide logfc and pval line
                                checkboxInput("show_pvalue_threshold_neurons",
                                              "Show significance threshold line",
                                              value = FALSE),
                                
                                # show/hide logfc lines
                                checkboxInput("show_logfc_threshold_neurons",
                                              "Show effect size threshold line",
                                              value = FALSE),
                                
                                # color differentially expressed genes
                                checkboxInput("color_by_de_neurons",
                                              "Color significantly different features",
                                              TRUE),
                                
                                # HIGHLIGHT GENES -----
                                h4("Label features of interest:"),
                                
                                #gene selector menu
                                selectizeInput("selected_genes_neurons",
                                               "Select feature(s) to label",
                                               NULL,
                                               multiple = TRUE),
                                
                                # output ui for axis label inputs
                                uiOutput("y_axis_labeler_neurons"),
                                uiOutput("x_axis_labeler_neurons"),
                                
                                # label legend
                                textInput("legend_title_neurons",
                                          "Specify legend title",
                                          value = "Differentially Expressed")
                   ),
                   
                   # VOLCANO PLOT MAIN PANEL -----
                   mainPanel(
                     # output info from click
                     p(strong("Plot interactivity:")),
                     p("- View a point's feature label, effect size, and significance by hovering over a point."),
                     p("- Add labels to a feature of interest by using the gene selection dropdown in the sidebar or clicking on the point on the plot."),
                     p("- Remove a label by deleting the selection from the gene selection dropdown or clicking the point on plot a second time."),
                     p("- To zoom click and drag over the plot to select the area you wish to zoom in on. Then, double click to zoom into the selected area. Double click again to zoom out."),
                     verbatimTextOutput("click_info_neurons",
                                        placeholder = TRUE),
                     
                     # output ggplot volcano
                     plotOutput("volcano_plot_neurons",
                                width = "100%",
                                height = "600px",
                                hover = "volcano_hover_neurons",
                                click = "volcano_click_neurons",
                                dblclick = "volcano_dbl_click_neurons",
                                brush = brushOpts(
                                  id = "volcano_brush_neurons",
                                  resetOnNew = TRUE)),
                     
                     # Download button for plot
                     downloadButton('download_volcano_neurons', 'Download volcano plot as PDF'),
                     
                     br(),
                     br(),
                     
                     # HIGHLIGHTED GENES TABLE -----
                     dataTableOutput("gene_highlight_tbl_neurons")
                   )
                   
                 ) # end sidebarLayout  
        ), 
        tabPanel("Dataframe",
                 
                 # DATA PANEL SIDEBAR
                 tabPanel("Data - neurons",
                          sidebarLayout(
                            
                            # DATA PANEL SIDEBAR
                            sidebarPanel(width = 3,
                                         
                                         # some text explanation
                                         em("Threshold for what is considered differentially expressed is set in Volcano Plot tab by using 
                                 the significance and effect size sliders"),
                                         
                                         # Show differentiall expressed genes only
                                         checkboxInput("show_de_neurons",
                                                       "Show only significantly different features",
                                                       FALSE)
                            ),
                            
                            # DATA PANEL MAIN PANEL
                            mainPanel(dataTableOutput("gene_data_neurons"))
                  )
        )
      ))), # end of neurons tabPanel
      tabPanel("SDH Neurons", tabsetPanel(
        tabPanel("Volcano Plot",
                 h2("Interactive Volcano Plot"),
                 sidebarLayout(
                   
                   # VOLCANO PLOT SIDE PANEL ------
                   sidebarPanel(width = 4,
                                
                                # SET PVAL AND LOGFC THRESHOLDS ----- 
                                h4("Set significance and effect size thresholds:"),
                                
                                # set pvalue threshold 
                                numericInput("pvalue_threshold_SDH_neurons",
                                             "Set significance threshold", 
                                             value = 0.05, 
                                             min = 0, 
                                             step = 0.001),
                                
                                # set logfc threshold
                                uiOutput("logfc_slider_SDH_neurons"),
                                
                                # CUSTOMIZE PLOT -----
                                h4("Customize plot:"),
                                
                                # show/hide logfc and pval line
                                checkboxInput("show_pvalue_threshold_SDH_neurons",
                                              "Show significance threshold line",
                                              value = FALSE),
                                
                                # show/hide logfc lines
                                checkboxInput("show_logfc_threshold_SDH_neurons",
                                              "Show effect size threshold line",
                                              value = FALSE),
                                
                                # color differentially expressed genes
                                checkboxInput("color_by_de_SDH_neurons",
                                              "Color significantly different features",
                                              TRUE),
                                
                                # HIGHLIGHT GENES -----
                                h4("Label features of interest:"),
                                
                                #gene selector menu
                                selectizeInput("selected_genes_SDH_neurons",
                                               "Select feature(s) to label",
                                               NULL,
                                               multiple = TRUE),
                                
                                # output ui for axis label inputs
                                uiOutput("y_axis_labeler_SDH_neurons"),
                                uiOutput("x_axis_labeler_SDH_neurons"),
                                
                                # label legend
                                textInput("legend_title_SDH_neurons",
                                          "Specify legend title",
                                          value = "Differentially Expressed")
                   ),
                   
                   # VOLCANO PLOT MAIN PANEL -----
                   mainPanel(
                     # output info from click
                     p(strong("Plot interactivity:")),
                     p("- View a point's feature label, effect size, and significance by hovering over a point."),
                     p("- Add labels to a feature of interest by using the gene selection dropdown in the sidebar or clicking on the point on the plot."),
                     p("- Remove a label by deleting the selection from the gene selection dropdown or clicking the point on plot a second time."),
                     p("- To zoom click and drag over the plot to select the area you wish to zoom in on. Then, double click to zoom into the selected area. Double click again to zoom out."),
                     verbatimTextOutput("click_info_SDH_neurons",
                                        placeholder = TRUE),
                     
                     # output ggplot volcano
                     plotOutput("volcano_plot_SDH_neurons",
                                width = "100%",
                                height = "600px",
                                hover = "volcano_hover_SDH_neurons",
                                click = "volcano_click_SDH_neurons",
                                dblclick = "volcano_dbl_click_SDH_neurons",
                                brush = brushOpts(
                                  id = "volcano_brush_SDH_neurons",
                                  resetOnNew = TRUE)),
                     
                     # Download button for plot
                     downloadButton('download_volcano_SDH_neurons', 'Download volcano plot as PDF'),
                     
                     br(),
                     br(),
                     
                     # HIGHLIGHTED GENES TABLE -----
                     dataTableOutput("gene_highlight_tbl_SDH_neurons"))
                   
                 ) # end sidebarLayout
        ), 
        tabPanel("Dataframe",
                 sidebarLayout(
                   
                   # DATA PANEL SIDEBAR
                   sidebarPanel(width = 3,
                                
                                # some text explanation
                                em("Threshold for what is considered differentially expressed is set in Volcano Plot tab by using 
                                 the significance and effect size sliders"),
                                
                                # Show differentiall expressed genes only
                                checkboxInput("show_de_SDH_neurons",
                                              "Show only significantly different features",
                                              FALSE)),
                   
                   # DATA PANEL MAIN PANEL
                   mainPanel(dataTableOutput("gene_data_SDH_neurons")))
        )
      )),
      tabPanel("SDH Inhibitory Neurons", tabsetPanel(
        tabPanel("Volcano Plot",
                 h2("Interactive Volcano Plot"),
                 sidebarLayout(
                   
                   # VOLCANO PLOT SIDE PANEL ------
                   sidebarPanel(width = 4,
                                
                                # SET PVAL AND LOGFC THRESHOLDS ----- 
                                h4("Set significance and effect size thresholds:"),
                                
                                # set pvalue threshold 
                                numericInput("pvalue_threshold_SDH_inhib_neurons",
                                             "Set significance threshold", 
                                             value = 0.05, 
                                             min = 0, 
                                             step = 0.001),
                                
                                # set logfc threshold
                                uiOutput("logfc_slider_SDH_inhib_neurons"),
                                
                                # CUSTOMIZE PLOT -----
                                h4("Customize plot:"),
                                
                                # show/hide logfc and pval line
                                checkboxInput("show_pvalue_threshold_SDH_inhib_neurons",
                                              "Show significance threshold line",
                                              value = TRUE),
                                
                                # show/hide logfc lines
                                checkboxInput("show_logfc_threshold_SDH_inhib_neurons",
                                              "Show effect size threshold line",
                                              value = TRUE),
                                
                                # color differentially expressed genes
                                checkboxInput("color_by_de_SDH_inhib_neurons",
                                              "Color significantly different features",
                                              TRUE),
                                # HIGHLIGHT GENES -----
                                h4("Label features of interest:"),
                                
                                #gene selector menu
                                selectizeInput("selected_genes_SDH_inhib_neurons",
                                               "Select feature(s) to label",
                                               NULL,
                                               multiple = TRUE),
                                
                                # output ui for axis label inputs
                                uiOutput("y_axis_labeler_SDH_inhib_neurons"),
                                uiOutput("x_axis_labeler_SDH_inhib_neurons"),
                                
                                # label legend
                                textInput("legend_title_SDH_inhib_neurons",
                                          "Specify legend title",
                                          value = "Differentially Expressed")
                    ),
                   # VOLCANO PLOT MAIN PANEL -----
                   mainPanel(
                     # output info from click
                     p(strong("Plot interactivity:")),
                     p("- View a point's feature label, effect size, and significance by hovering over a point."),
                     p("- Add labels to a feature of interest by using the gene selection dropdown in the sidebar or clicking on the point on the plot."),
                     p("- Remove a label by deleting the selection from the gene selection dropdown or clicking the point on plot a second time."),
                     p("- To zoom click and drag over the plot to select the area you wish to zoom in on. Then, double click to zoom into the selected area. Double click again to zoom out."),
                     verbatimTextOutput("click_info_SDH_inhib_neurons",
                                        placeholder = TRUE),
                     
                     # output ggplot volcano
                     plotOutput("volcano_plot_SDH_inhib_neurons",
                                width = "100%",
                                height = "600px",
                                hover = "volcano_hover_SDH_inhib_neurons",
                                click = "volcano_click_SDH_inhib_neurons",
                                dblclick = "volcano_dbl_click_SDH_inhib_neurons",
                                brush = brushOpts(
                                  id = "volcano_brush_SDH_inhib_neurons",
                                  resetOnNew = TRUE)),
                     
                     # Download button for plot
                     downloadButton('download_volcano_SDH_inhib_neurons', 'Download volcano plot as PDF'),
                     
                     br(),
                     br(),
                     
                     # HIGHLIGHTED GENES TABLE -----
                     dataTableOutput("gene_highlight_tbl_SDH_inhib_neurons"))
                   
                 ) # end sidebarLayout       
        ), tabPanel("Dataframe",
                    sidebarLayout(
                      
                      # DATA PANEL SIDEBAR
                      sidebarPanel(width = 3,
                                   
                      # some text explanation
                      em("Threshold for what is considered differentially expressed is set in Volcano Plot tab by using 
                      the significance and effect size sliders"),
                     
                      # Show differentiall expressed genes only
                      checkboxInput("show_de_SDH_inhib_neurons",
                                   "Show only significantly different features",
                                   FALSE)
                      ),
                      
                      # DATA PANEL MAIN PANEL
                      mainPanel(dataTableOutput("gene_data_SDH_inhib_neurons"))
                    )
        ) # end of dataframe 
      )), # end of inhibitory sdh neurons tabset panel
      tabPanel("SDH Excitatory Neurons", tabsetPanel(
        tabPanel("Volcano Plot",
                 h2("Interactive Volcano Plot"),
                 sidebarLayout(
                   
                   # VOLCANO PLOT SIDE PANEL ------
                   sidebarPanel(width = 4,
                                
                                # SET PVAL AND LOGFC THRESHOLDS ----- 
                                h4("Set significance and effect size thresholds:"),
                                
                                # set pvalue threshold 
                                numericInput("pvalue_threshold_SDH_excit_neurons",
                                             "Set significance threshold", 
                                             value = 0.05, 
                                             min = 0, 
                                             step = 0.001),
                                
                                # set logfc threshold
                                uiOutput("logfc_slider_SDH_excit_neurons"),
                                
                                
                                # CUSTOMIZE PLOT -----
                                h4("Customize plot:"),
                                
                                # show/hide logfc and pval line
                                checkboxInput("show_pvalue_threshold_SDH_excit_neurons",
                                              "Show significance threshold line",
                                              value = FALSE),
                                
                                # show/hide logfc lines
                                checkboxInput("show_logfc_threshold_SDH_excit_neurons",
                                              "Show effect size threshold line",
                                              value = FALSE),
                                
                                # HIGHLIGHT GENES -----
                                h4("Label features of interest:"),
                                
                                #gene selector menu
                                selectizeInput("selected_genes_SDH_excit_neurons",
                                               "Select feature(s) to label",
                                               NULL,
                                               multiple = TRUE),
                                
                                # output ui for axis label inputs
                                uiOutput("y_axis_labeler_SDH_excit_neurons"),
                                uiOutput("x_axis_labeler_SDH_excit_neurons"),
                                
                                # label legend
                                textInput("legend_title_SDH_excit_neurons",
                                          "Specify legend title",
                                          value = "Differentially Expressed")
                 ),
                 
                 # VOLCANO PLOT MAIN PANEL -----
                 mainPanel(
                   # output info from click
                   p(strong("Plot interactivity:")),
                   p("- View a point's feature label, effect size, and significance by hovering over a point."),
                   p("- Add labels to a feature of interest by using the gene selection dropdown in the sidebar or clicking on the point on the plot."),
                   p("- Remove a label by deleting the selection from the gene selection dropdown or clicking the point on plot a second time."),
                   p("- To zoom click and drag over the plot to select the area you wish to zoom in on. Then, double click to zoom into the selected area. Double click again to zoom out."),
                   verbatimTextOutput("click_info_SDH_excit_neurons",
                                      placeholder = TRUE),
                   
                   # output ggplot volcano
                   plotOutput("volcano_plot_SDH_excit_neurons",
                              width = "100%",
                              height = "600px",
                              hover = "volcano_hover_SDH_excit_neurons",
                              click = "volcano_click_SDH_excit_neurons",
                              dblclick = "volcano_dbl_click_SDH_excit_neurons",
                              brush = brushOpts(
                                id = "volcano_brush_SDH_excit_neurons",
                                resetOnNew = TRUE)),
                   
                   # Download button for plot
                   downloadButton('download_volcano_SDH_excit_neurons', 'Download volcano plot as PDF'),
                   
                   br(),
                   br(),
                   
                   # HIGHLIGHTED GENES TABLE -----
                   dataTableOutput("gene_highlight_tbl_SDH_excit_neurons"))
                 
          ) # end sidebarLayout     
        ), tabPanel("Dataframe",
                    sidebarLayout(
                      
                      # DATA PANEL SIDEBAR
                      sidebarPanel(width = 3,
                                   
                                   # some text explanation
                                   em("Threshold for what is considered differentially expressed is set in Volcano Plot tab by using 
                                  the significance and effect size sliders"),
                                   
                                   # Show differentiall expressed genes only
                                   checkboxInput("show_de_SDH_excit_neurons",
                                                 "Show only significantly different features",
                                                 FALSE)
                      ),
                      
                      # DATA PANEL MAIN PANEL
                      mainPanel(dataTableOutput("gene_data_SDH_excit_neurons"))
                    )
        ) # end of dataframe tab panel
     )) # end of tab panel SDH Excitatory Neurons
)) # end fluidPage and full tabset panel

# server -------------------------
server <- function(input, output, session) {
  
  # IDENTIFY DIFFERENTIALLY EXPRESSED GENES -----
  
  # render UI for logfc slider
  # min and max set reactively with logfc based on selected logfc input col
  output$logfc_slider <- renderUI({
    sliderInput("logfc_threshold",
                "Select effect size threshold",
                min = 0,
                max = round(max(data[["log2FoldChange"]])),
                value = 0,
                step = .1)
  })
  
  # use columns and thresholds selected in UI
  is_de <- reactive({
    abs(data[["log2FoldChange"]]) >= input$logfc_threshold & data[["padj"]] <= input$pvalue_threshold
  })
  
  # FILTERABLE DATAFRAME BY DE GENE -----
  
  is_male <- reactive({
    data[["log2FoldChange"]] > 0
  })
  
  # reactively filter data based on checkbox
  de_gene_data <- reactive({
    if (input$show_de) {
      filter(data, is_de())
    } else {
      data
    }
  })
  
  # render data frame of gene data
  output$gene_data <- renderDataTable(
    de_gene_data()
  )
  
  # X AND Y AXES LABELER -----
  
  # capture pvalue column selected and default value with it
  reactive_pvalue_value <- reactive({
    "-log10(padj)"
  })
  
  # enter custom x (logfc) axis label
  output$x_axis_labeler <- renderUI({
    textInput("x_axis_lab",
              "Specify X axis label",
              value = "log2FoldChange",
              placeholder = "ex: Log Fold Change")
  })
  
  # enter custom x (logfc) axis label
  output$y_axis_labeler <- renderUI({
    textInput("y_axis_lab",
              "Specify Y axis label",
              value = reactive_pvalue_value(),
              placeholder = "ex: -log10(FDR)")
  })
  
  # HIGHLIGHTED GENE TABLE -----
  
  # initialize gene_list$clicked_gene_list as NULL
  # This will reactively update
  gene_list <- reactiveValues(clicked_gene_list = NULL)
  
  # store clicked gene info
  clicked_gene <- reactive({
    nearPoints(data_w_log_pval(),
               input$volcano_click,
               xvar = "log2FoldChange",
               yvar = data$log_pval,
               maxpoints = 1) %>%
      select("gene")
  })
  
  # when a point is clicked on the volcano plot
  # add gene to clicked gene list
  # if the point has been clicked twice, remove from list
  observeEvent(input$volcano_click, {
    # create variable of what has been clicked + selected
    if (is.null(input$selected_genes)) {
      gene_list$clicked_gene_list <- NULL
    }
    # if gene_list is empty
    # get point info and save gene
    if (is.null(gene_list$clicked_gene_list)) {
      gene_list$clicked_gene_list <- clicked_gene()
      # if gene_list is not NULL
      # check to see if gene is in gene_list
    } else {
      gene_present <- clicked_gene() %in% input$selected_genes
      # if TRUE (gene is present already)
      # remove gene from gene list
      if (gene_present) {
        present_idx <- !grepl(clicked_gene(), input$selected_genes)
        # remove row
        gene_list$clicked_gene_list <- input$selected_genes[present_idx]
      } else {
        gene_list$clicked_gene_list <- c(clicked_gene(), input$selected_genes)
      }
    }
  })
  
  observe({
    updateSelectizeInput(session, 
                         "selected_genes",
                         label = "Select feature(s) to label",
                         choices = sort(data[["gene"]]),
                         selected = gene_list$clicked_gene_list,
                         server = TRUE
    )
  })
  
  
  # reactive function that subsets data by highlighted_gene vector
  highlight_gene_data <- reactive({
    if (length(input$selected_genes) > 0) {
      highlight_gene_data <- data[data[["gene"]] %in% input$selected_genes, c("gene", "log2FoldChange", "padj")]
    } else {
      highlight_gene_data <- data.frame(NA, NA, NA)
      names(highlight_gene_data) <- c("gene", "log2FoldChange", "padj")
    }
  })
  
  # render a data table of highlighted genes info
  output$gene_highlight_tbl <- renderDataTable({
    highlight_gene_data()
  })
  
  # ZOOM PLOT WITH BRUSH -----
  # initialize reactive value
  # this is the value that will be input into volcanoPlot()
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # when there is a double click on the plot
  # if brush is null, nothing happens,
  # if brush is not null, assign values to ranges
  observeEvent(input$volcano_dbl_click, {
    brush <- input$volcano_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # PLOT AND RENDER VOLCANO -----
  
  # volcano plot in reactive function 
  reactive_volcano <- reactive({
    plotVolcano(data = data, 
                logfc_col = "log2FoldChange", 
                pvalue_col = "padj",
                gene_col = "gene",
                pvalue_thresh = input$pvalue_threshold,
                logfc_thresh = input$logfc_threshold,
                de_vec = is_de(),
                is_male = is_male(),
                show_logfc_thresh = input$show_logfc_threshold,
                show_pvalue_thresh = input$show_pvalue_threshold,
                highlight_genes = input$selected_genes,
                x_label = input$x_axis_lab,
                y_label = input$y_axis_lab,
                legend_title = input$legend_title,
                xlim = ranges$x,
                ylim = ranges$y)
  })
  
  # output volcano plot
  output$volcano_plot <- renderPlot({
    reactive_volcano()
  })
  
  # DISPLAY GENE INFO ON HOVER OVER -----
  
  # Create -log10 pvalue column
  data_w_log_pval <- reactive({
    # make new cols and select
    reduced_data <- data %>%
      mutate(log_pval = -log10(data[["padj"]]))
  })
  
  # Collect nearpoint info and reduce to only gene_col, logfc_col and pvalue_col
  point_info <- reactive({
    nearpoint_out <- nearPoints(data_w_log_pval(), input$volcano_hover, xvar = "log2FoldChange", yvar = data$log_pval, maxpoints = 1)
    nearpoint_out %>%
      select("gene", "log2FoldChange", "padj")
  })
  
  # render printed text
  output$click_info <- renderPrint({
    point_info()
  })
  
  # DOWNLOAD HANDLER -----
  
  output$download_volcano <- downloadHandler(
    filename = function() {
      paste0("volcano-plot-", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      ggsave(file, reactive_volcano(), device = "pdf", width = 10, height = 5, units = "in")
    })
  
  #--------------------------------------------------------------------------------------------------
  #NEURONS ONLY PAGE
  
  # IDENTIFY DIFFERENTIALLY EXPRESSED GENES -----
  
  # render UI for logfc slider
  # min and max set reactively with logfc based on selected logfc input col
  output$logfc_slider_neurons <- renderUI({
    sliderInput("logfc_threshold_neurons",
                "Select effect size threshold",
                min = 0,
                max = round(max(data_neurons[["log2FoldChange"]])),
                value = 0,
                step = .1)
  })
  
  # use columns and thresholds selected in UI
  is_de_neurons <- reactive({
    abs(data_neurons[["log2FoldChange"]]) >= input$logfc_threshold_neurons & data_neurons[["padj"]] <= input$pvalue_threshold_neurons
  })
  
  # FILTERABLE DATAFRAME BY DE GENE -----
  
  is_male_neurons <- reactive({
    data_neurons[["log2FoldChange"]] > 0
  })
  
  # reactively filter data based on checkbox
  de_gene_data_neurons <- reactive({
    if (input$show_de_neurons) {
      filter(data_neurons, is_de_neurons())
    } else {
      data_neurons
    }
  })
  
  # render data frame of gene data
  output$gene_data_neurons <- renderDataTable(
    de_gene_data_neurons()
  )
  
  # X AND Y AXES LABELER -----
  
  # capture pvalue column selected and default value with it
  reactive_pvalue_value_neurons <- reactive({
    "-log10(padj)"
  })
  
  # enter custom x (logfc) axis label
  output$x_axis_labeler_neurons <- renderUI({
    textInput("x_axis_lab_neurons",
              "Specify X axis label",
              value = "log2FoldChange",
              placeholder = "ex: Log Fold Change")
  })
  
  # enter custom x (logfc) axis label
  output$y_axis_labeler_neurons <- renderUI({
    textInput("y_axis_lab_neurons",
              "Specify Y axis label",
              value = reactive_pvalue_value_neurons(),
              placeholder = "ex: -log10(FDR)")
  })
  
  # HIGHLIGHTED GENE TABLE -----
  
  # initialize gene_list$clicked_gene_list as NULL
  # This will reactively update
  gene_list_neurons <- reactiveValues(clicked_gene_list_neurons = NULL)
  
  # store clicked gene info
  clicked_gene_neurons <- reactive({
    nearPoints(data_w_log_pval_neurons(),
               input$volcano_click_neurons,
               xvar = "log2FoldChange",
               yvar = data_neurons$log_pval,
               maxpoints = 1) %>%
      select("gene")
  })
  
  # when a point is clicked on the volcano plot
  # add gene to clicked gene list
  # if the point has been clicked twice, remove from list
  observeEvent(input$volcano_click_neurons, {
    # create variable of what has been clicked + selected
    if (is.null(input$selected_genes_neurons)) {
      gene_list_neurons$clicked_gene_list_neurons <- NULL
    }
    # if gene_list is empty
    # get point info and save gene
    if (is.null(gene_list_neurons$clicked_gene_list_neurons)) {
      gene_list_neurons$clicked_gene_list_neurons <- clicked_gene_neurons()
      # if gene_list is not NULL
      # check to see if gene is in gene_list
    } else {
      gene_present_neurons <- clicked_gene_neurons() %in% input$selected_genes_neurons
      # if TRUE (gene is present already)
      # remove gene from gene list
      if (gene_present_neurons) {
        present_idx_neurons <- !grepl(clicked_gene_neurons(), input$selected_genes_neurons)
        # remove row
        gene_list_neurons$clicked_gene_list_neurons <- input$selected_genes_neurons[present_idx_neurons]
      } else {
        gene_list_neurons$clicked_gene_list_neurons <- c(clicked_gene_neurons(), input$selected_genes_neurons)
      }
    }
  })
  
  observe({
    updateSelectizeInput(session, 
                         "selected_genes_neurons",
                         label = "Select feature(s) to label",
                         choices = sort(data_neurons[["gene"]]),
                         selected = gene_list_neurons$clicked_gene_list_neurons,
                         server = TRUE
    )
  })
  
  
  # reactive function that subsets data by highlighted_gene vector
  highlight_gene_data_neurons <- reactive({
    if (length(input$selected_genes_neurons) > 0) {
      highlight_gene_data_neurons <- data_neurons[data_neurons[["gene"]] %in% input$selected_genes_neurons, c("gene", "log2FoldChange", "padj")]
    } else {
      highlight_gene_data_neurons <- data.frame(NA, NA, NA)
      names(highlight_gene_data_neurons) <- c("gene", "log2FoldChange", "padj")
    }
  })
  
  # render a data table of highlighted genes info
  output$gene_highlight_tbl_neurons <- renderDataTable({
    highlight_gene_data_neurons()
  })
  
  # ZOOM PLOT WITH BRUSH -----
  # initialize reactive value
  # this is the value that will be input into volcanoPlot()
  ranges_neurons <- reactiveValues(x = NULL, y = NULL)
  
  # when there is a double click on the plot
  # if brush is null, nothing happens,
  # if brush is not null, assign values to ranges
  observeEvent(input$volcano_dbl_click_neurons, {
    brush_neurons <- input$volcano_brush_neurons
    if (!is.null(brush_neurons)) {
      ranges_neurons$x <- c(brush_neurons$xmin, brush_neurons$xmax)
      ranges_neurons$y <- c(brush_neurons$ymin, brush_neurons$ymax)
      
    } else {
      ranges_neurons$x <- NULL
      ranges_neurons$y <- NULL
    }
  })
  
  # PLOT AND RENDER VOLCANO -----
  
  # volcano plot in reactive function 
  reactive_volcano_neurons <- reactive({
    plotVolcano(data = data_neurons, 
                logfc_col = "log2FoldChange", 
                pvalue_col = "padj",
                gene_col = "gene",
                pvalue_thresh = input$pvalue_threshold_neurons,
                logfc_thresh = input$logfc_threshold_neurons,
                de_vec = is_de_neurons(),
                is_male = is_male_neurons(),
                color_by_de = input$color_by_de_neurons,
                show_logfc_thresh = input$show_logfc_threshold_neurons,
                show_pvalue_thresh = input$show_pvalue_threshold_neurons,
                highlight_genes = input$selected_genes_neurons,
                x_label = input$x_axis_lab_neurons,
                y_label = input$y_axis_lab_neurons,
                legend_title = input$legend_title_neurons,
                xlim = ranges_neurons$x,
                ylim = ranges_neurons$y)
  })
  
  # output volcano plot
  output$volcano_plot_neurons <- renderPlot({
    reactive_volcano_neurons()
  })
  
  # DISPLAY GENE INFO ON HOVER OVER -----
  
  # Create -log10 pvalue column
  data_w_log_pval_neurons <- reactive({
    # make new cols and select
    reduced_data_neurons <- data_neurons %>%
      mutate(log_pval = -log10(data_neurons[["padj"]]))
  })
  
  # Collect nearpoint info and reduce to only gene_col, logfc_col and pvalue_col
  point_info_neurons <- reactive({
    nearpoint_out_neurons <- nearPoints(data_w_log_pval_neurons(), input$volcano_hover_neurons, xvar = "log2FoldChange", yvar = data$log_pval_neurons, maxpoints = 1)
    nearpoint_out_neurons %>%
      select("gene", "log2FoldChange", "padj")
  })
  
  # render printed text
  output$click_info_neurons <- renderPrint({
    point_info_neurons()
  })
  
  # DOWNLOAD HANDLER -----
  
  output$download_volcano_neurons <- downloadHandler(
    filename = function() {
      paste0("volcano-plot-neurons", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      ggsave(file, reactive_volcano_neurons(), device = "pdf", width = 10, height = 5, units = "in")
    })
  
  
  #--------------------------------------------------------------------------------------------------
  # SDH NEURONS ONLY PAGE
  
  # IDENTIFY DIFFERENTIALLY EXPRESSED GENES -----
  
  # render UI for logfc slider
  # min and max set reactively with logfc based on selected logfc input col
  output$logfc_slider_SDH_neurons <- renderUI({
    sliderInput("logfc_threshold_SDH_neurons",
                "Select effect size threshold",
                min = 0,
                max = round(max(data_SDH_neurons[["log2FoldChange"]])),
                value = 0,
                step = .1)
  })
  
  # use columns and thresholds selected in UI
  is_de_SDH_neurons <- reactive({
    abs(data_SDH_neurons[["log2FoldChange"]]) >= input$logfc_threshold_SDH_neurons & data_SDH_neurons[["padj"]] <= input$pvalue_threshold_SDH_neurons
  })
  
  # FILTERABLE DATAFRAME BY DE GENE -----
  
  is_male_SDH_neurons <- reactive({
    data_SDH_neurons[["log2FoldChange"]] > 0
  })
  
  # reactively filter data based on checkbox
  de_gene_data_SDH_neurons <- reactive({
    if (input$show_de_SDH_neurons) {
      filter(data_SDH_neurons, is_de_SDH_neurons())
    } else {
      data_SDH_neurons
    }
  })
  
  # render data frame of gene data
  output$gene_data_SDH_neurons <- renderDataTable(
    de_gene_data_SDH_neurons()
  )
  
  # X AND Y AXES LABELER -----
  
  # capture pvalue column selected and default value with it
  reactive_pvalue_value_SDH_neurons <- reactive({
    "-log10(padj)"
  })
  
  # enter custom x (logfc) axis label
  output$x_axis_labeler_SDH_neurons <- renderUI({
    textInput("x_axis_lab_SDH_neurons",
              "Specify X axis label",
              value = "log2FoldChange",
              placeholder = "ex: Log Fold Change")
  })
  
  # enter custom x (logfc) axis label
  output$y_axis_labeler_SDH_neurons <- renderUI({
    textInput("y_axis_lab_SDH_neurons",
              "Specify Y axis label",
              value = reactive_pvalue_value_SDH_neurons(),
              placeholder = "ex: -log10(FDR)")
  })
  
  # HIGHLIGHTED GENE TABLE -----
  
  # initialize gene_list$clicked_gene_list as NULL
  # This will reactively update
  gene_list_SDH_neurons <- reactiveValues(clicked_gene_list_SDH_neurons = NULL)
  
  # store clicked gene info
  clicked_gene_SDH_neurons <- reactive({
    nearPoints(data_w_log_pval_SDH_neurons(),
               input$volcano_click_SDH_neurons,
               xvar = "log2FoldChange",
               yvar = data_SDH_neurons$log_pval,
               maxpoints = 1) %>%
      select("gene")
  })
  
  # when a point is clicked on the volcano plot
  # add gene to clicked gene list
  # if the point has been clicked twice, remove from list
  observeEvent(input$volcano_click_SDH_neurons, {
    # create variable of what has been clicked + selected
    if (is.null(input$selected_genes_SDH_neurons)) {
      gene_list_SDH_neurons$clicked_gene_list_SDH_neurons <- NULL
    }
    # if gene_list is empty
    # get point info and save gene
    if (is.null(gene_list_SDH_neurons$clicked_gene_list_SDH_neurons)) {
      gene_list_SDH_neurons$clicked_gene_list_SDH_neurons <- clicked_gene_SDH_neurons()
      # if gene_list is not NULL
      # check to see if gene is in gene_list
    } else {
      gene_present_SDH_neurons <- clicked_gene_SDH_neurons() %in% input$selected_genes_SDH_neurons
      # if TRUE (gene is present already)
      # remove gene from gene list
      if (gene_present_SDH_neurons) {
        present_idx_SDH_neurons <- !grepl(clicked_gene_SDH_neurons(), input$selected_genes_SDH_neurons)
        # remove row
        gene_list_SDH_neurons$clicked_gene_list_SDH_neurons <- input$selected_genes_SDH_neurons[present_idx_SDH_neurons]
      } else {
        gene_list_SDH_neurons$clicked_gene_list_SDH_neurons <- c(clicked_gene_SDH_neurons(), input$selected_genes_SDH_neurons)
      }
    }
  })
  
  observe({
    updateSelectizeInput(session, 
                         "selected_genes_SDH_neurons",
                         label = "Select feature(s) to label",
                         choices = sort(data_SDH_neurons[["gene"]]),
                         selected = gene_list_SDH_neurons$clicked_gene_list_SDH_neurons,
                         server = TRUE
    )
  })
  
  
  # reactive function that subsets data by highlighted_gene vector
  highlight_gene_data_SDH_neurons <- reactive({
    if (length(input$selected_genes_SDH_neurons) > 0) {
      highlight_gene_data_SDH_neurons <- data_SDH_neurons[data_SDH_neurons[["gene"]] %in% input$selected_genes_SDH_neurons, c("gene", "log2FoldChange", "padj")]
    } else {
      highlight_gene_data_SDH_neurons <- data.frame(NA, NA, NA)
      names(highlight_gene_data_SDH_neurons) <- c("gene", "log2FoldChange", "padj")
    }
  })
  
  # render a data table of highlighted genes info
  output$gene_highlight_tbl_SDH_neurons <- renderDataTable({
    highlight_gene_data_SDH_neurons()
  })
  
  # ZOOM PLOT WITH BRUSH -----
  # initialize reactive value
  # this is the value that will be input into volcanoPlot()
  ranges_SDH_neurons <- reactiveValues(x = NULL, y = NULL)
  
  # when there is a double click on the plot
  # if brush is null, nothing happens,
  # if brush is not null, assign values to ranges
  observeEvent(input$volcano_dbl_click_SDH_neurons, {
    brush_SDH_neurons <- input$volcano_brush_SDH_neurons
    if (!is.null(brush_SDH_neurons)) {
      ranges_SDH_neurons$x <- c(brush_SDH_neurons$xmin, brush_SDH_neurons$xmax)
      ranges_SDH_neurons$y <- c(brush_SDH_neurons$ymin, brush_SDH_neurons$ymax)
      
    } else {
      ranges_SDH_neurons$x <- NULL
      ranges_SDH_neurons$y <- NULL
    }
  })
  
  # PLOT AND RENDER VOLCANO -----
  
  # volcano plot in reactive function 
  reactive_volcano_SDH_neurons <- reactive({
    plotVolcano(data = data_SDH_neurons, 
                logfc_col = "log2FoldChange", 
                pvalue_col = "padj",
                gene_col = "gene",
                pvalue_thresh = input$pvalue_threshold_SDH_neurons,
                logfc_thresh = input$logfc_threshold_SDH_neurons,
                de_vec = is_de_SDH_neurons(),
                is_male = is_male_SDH_neurons(),
                color_by_de = input$color_by_de_SDH_neurons,
                show_logfc_thresh = input$show_logfc_threshold_SDH_neurons,
                show_pvalue_thresh = input$show_pvalue_threshold_SDH_neurons,
                highlight_genes = input$selected_genes_SDH_neurons,
                x_label = input$x_axis_lab_SDH_neurons,
                y_label = input$y_axis_lab_SDH_neurons,
                legend_title = input$legend_title_SDH_neurons,
                xlim = ranges_SDH_neurons$x,
                ylim = ranges_SDH_neurons$y)
  })
  
  # output volcano plot
  output$volcano_plot_SDH_neurons <- renderPlot({
    reactive_volcano_SDH_neurons()
  })
  
  # DISPLAY GENE INFO ON HOVER OVER -----
  
  # Create -log10 pvalue column
  data_w_log_pval_SDH_neurons <- reactive({
    # make new cols and select
    reduced_data_SDH_neurons <- data_SDH_neurons %>%
      mutate(log_pval = -log10(data_SDH_neurons[["padj"]]))
  })
  
  # Collect nearpoint info and reduce to only gene_col, logfc_col and pvalue_col
  point_info_SDH_neurons <- reactive({
    nearpoint_out_SDH_neurons <- nearPoints(data_w_log_pval_SDH_neurons(), input$volcano_hover_SDH_neurons, xvar = "log2FoldChange", yvar = data$log_pval_SDH_neurons, maxpoints = 1)
    nearpoint_out_SDH_neurons %>%
      select("gene", "log2FoldChange", "padj")
  })
  
  # render printed text
  output$click_info_SDH_neurons <- renderPrint({
    point_info_SDH_neurons()
  })
  
  # DOWNLOAD HANDLER -----
  
  output$download_volcano_SDH_neurons <- downloadHandler(
    filename = function() {
      paste0("volcano-plot-neurons", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      ggsave(file, reactive_volcano_SDH_neurons(), device = "pdf", width = 10, height = 5, units = "in")
    })
  
  #--------------------------------------------------------------------------------------------------
  # SDH INHIB NEURONS ONLY PAGE
  
  # IDENTIFY DIFFERENTIALLY EXPRESSED GENES -----
  
  # render UI for logfc slider
  # min and max set reactively with logfc based on selected logfc input col
  output$logfc_slider_SDH_inhib_neurons <- renderUI({
    sliderInput("logfc_threshold_SDH_inhib_neurons",
                "Select effect size threshold",
                min = 0,
                max = round(max(data_SDH_inhib_neurons[["log2FoldChange"]])),
                value = 0,
                step = .1)
  })
  
  # use columns and thresholds selected in UI
  is_de_SDH_inhib_neurons <- reactive({
    abs(data_SDH_inhib_neurons[["log2FoldChange"]]) >= input$logfc_threshold_SDH_inhib_neurons & data_SDH_inhib_neurons[["padj"]] <= input$pvalue_threshold_SDH_inhib_neurons
  })
  
  # FILTERABLE DATAFRAME BY DE GENE -----
  
  is_male_SDH_inhib_neurons <- reactive({
    data_SDH_inhib_neurons[["log2FoldChange"]] > 0
  })
  
  # reactively filter data based on checkbox
  de_gene_data_SDH_inhib_neurons <- reactive({
    if (input$show_de_SDH_inhib_neurons) {
      filter(data_SDH_inhib_neurons, is_de_SDH_inhib_neurons())
    } else {
      data_SDH_inhib_neurons
    }
  })
  
  # render data frame of gene data
  output$gene_data_SDH_inhib_neurons <- renderDataTable(
    de_gene_data_SDH_inhib_neurons()
  )
  
  # X AND Y AXES LABELER -----
  
  # capture pvalue column selected and default value with it
  reactive_pvalue_value_SDH_inhib_neurons <- reactive({
    paste0("-log10(", "padj", ")")
  })
  
  # enter custom x (logfc) axis label
  output$x_axis_labeler_SDH_inhib_neurons <- renderUI({
    textInput("x_axis_lab_SDH_inhib_neurons",
              "Specify X axis label",
              value = "log2FoldChange",
              placeholder = "ex: Log Fold Change")
  })
  
  # enter custom x (logfc) axis label
  output$y_axis_labeler_SDH_inhib_neurons <- renderUI({
    textInput("y_axis_lab_SDH_inhib_neurons",
              "Specify Y axis label",
              value = reactive_pvalue_value_SDH_inhib_neurons(),
              placeholder = "ex: -log10(FDR)")
  })
  
  # HIGHLIGHTED GENE TABLE -----
  
  # initialize gene_list$clicked_gene_list as NULL
  # This will reactively update
  gene_list_SDH_inhib_neurons <- reactiveValues(clicked_gene_list_SDH_inhib_neurons = NULL)
  
  # store clicked gene info
  clicked_gene_SDH_inhib_neurons <- reactive({
    nearPoints(data_w_log_pval_SDH_inhib_neurons(),
               input$volcano_click_SDH_inhib_neurons,
               xvar = "log2FoldChange",
               yvar = data_SDH_inhib_neurons$log_pval,
               maxpoints = 1) %>%
      select("gene")
  })
  
  # when a point is clicked on the volcano plot
  # add gene to clicked gene list
  # if the point has been clicked twice, remove from list
  observeEvent(input$volcano_click_SDH_inhib_neurons, {
    # create variable of what has been clicked + selected
    if (is.null(input$selected_genes_SDH_inhib_neurons)) {
      gene_list_SDH_inhib_neurons$clicked_gene_list_SDH_inhib_neurons <- NULL
    }
    # if gene_list is empty
    # get point info and save gene
    if (is.null(gene_list_SDH_inhib_neurons$clicked_gene_list_SDH_inhib_neurons)) {
      gene_list_SDH_inhib_neurons$clicked_gene_list_SDH_inhib_neurons <- clicked_gene_SDH_inhib_neurons()
      # if gene_list is not NULL
      # check to see if gene is in gene_list
    } else {
      gene_present_SDH_inhib_neurons <- clicked_gene_SDH_inhib_neurons() %in% input$selected_genes_SDH_inhib_neurons
      # if TRUE (gene is present already)
      # remove gene from gene list
      if (gene_present_SDH_inhib_neurons) {
        present_idx_SDH_inhib_neurons <- !grepl(clicked_gene_SDH_inhib_neurons(), input$selected_genes_SDH_inhib_neurons)
        # remove row
        gene_list_SDH_inhib_neurons$clicked_gene_list_SDH_inhib_neurons <- input$selected_genes_SDH_inhib_neurons[present_idx_SDH_inhib_neurons]
      } else {
        gene_list_SDH_inhib_neurons$clicked_gene_list_SDH_inhib_neurons <- c(clicked_gene_SDH_inhib_neurons(), input$selected_genes_SDH_inhib_neurons)
      }
    }
  })
  
  observe({
    updateSelectizeInput(session, 
                         "selected_genes_SDH_inhib_neurons",
                         label = "Select feature(s) to label",
                         choices = sort(data_SDH_inhib_neurons[["gene"]]),
                         selected = gene_list_SDH_inhib_neurons$clicked_gene_list_SDH_inhib_neurons,
                         server = TRUE
    )
  })
  
  
  # reactive function that subsets data by highlighted_gene vector
  highlight_gene_data_SDH_inhib_neurons <- reactive({
    if (length(input$selected_genes_SDH_inhib_neurons) > 0) {
      highlight_gene_data_SDH_inhib_neurons <- data_SDH_inhib_neurons[data_SDH_inhib_neurons[["gene"]] %in% input$selected_genes_SDH_inhib_neurons, c("gene", "log2FoldChange", "padj")]
    } else {
      highlight_gene_data_SDH_inhib_neurons <- data.frame(NA, NA, NA)
      names(highlight_gene_data_SDH_inhib_neurons) <- c("gene", "log2FoldChange", "padj")
    }
  })
  
  # render a data table of highlighted genes info
  output$gene_highlight_tbl_SDH_inhib_neurons <- renderDataTable({
    highlight_gene_data_SDH_inhib_neurons()
  })
  
  # ZOOM PLOT WITH BRUSH -----
  # initialize reactive value
  # this is the value that will be input into volcanoPlot()
  ranges_SDH_inhib_neurons <- reactiveValues(x = NULL, y = NULL)
  
  # when there is a double click on the plot
  # if brush is null, nothing happens,
  # if brush is not null, assign values to ranges
  observeEvent(input$volcano_dbl_click_SDH_inhib_neurons, {
    brush_SDH_inhib_neurons <- input$volcano_brush_SDH_inhib_neurons
    if (!is.null(brush_SDH_inhib_neurons)) {
      ranges_SDH_inhib_neurons$x <- c(brush_SDH_inhib_neurons$xmin, brush_SDH_inhib_neurons$xmax)
      ranges_SDH_inhib_neurons$y <- c(brush_SDH_inhib_neurons$ymin, brush_SDH_inhib_neurons$ymax)
      
    } else {
      ranges_SDH_inhib_neurons$x <- NULL
      ranges_SDH_inhib_neurons$y <- NULL
    }
  })
  
  # PLOT AND RENDER VOLCANO -----
  
  # volcano plot in reactive function 
  reactive_volcano_SDH_inhib_neurons <- reactive({
    plotVolcano(data = data_SDH_inhib_neurons, 
                logfc_col = "log2FoldChange", 
                pvalue_col = "padj",
                gene_col = "gene",
                pvalue_thresh = input$pvalue_threshold_SDH_inhib_neurons,
                logfc_thresh = input$logfc_threshold_SDH_inhib_neurons,
                de_vec = is_de_SDH_inhib_neurons(),
                is_male = is_male_SDH_inhib_neurons(),
                color_by_de = input$color_by_de_SDH_inhib_neurons,
                show_logfc_thresh = input$show_logfc_threshold_SDH_inhib_neurons,
                show_pvalue_thresh = input$show_pvalue_threshold_SDH_inhib_neurons,
                highlight_genes = input$selected_genes_SDH_inhib_neurons,
                x_label = input$x_axis_lab_SDH_inhib_neurons,
                y_label = input$y_axis_lab_SDH_inhib_neurons,
                legend_title = input$legend_title_SDH_inhib_neurons,
                xlim = ranges_SDH_inhib_neurons$x,
                ylim = ranges_SDH_inhib_neurons$y)
  })
  
  # output volcano plot
  output$volcano_plot_SDH_inhib_neurons <- renderPlot({
    reactive_volcano_SDH_inhib_neurons()
  })
  
  # DISPLAY GENE INFO ON HOVER OVER -----
  
  # Create -log10 pvalue column
  data_w_log_pval_SDH_inhib_neurons <- reactive({
    # make new cols and select
    reduced_data_SDH_inhib_neurons <- data_SDH_inhib_neurons %>%
      mutate(log_pval = -log10(data_SDH_inhib_neurons[["padj"]]))
  })
  
  # Collect nearpoint info and reduce to only gene_col, logfc_col and pvalue_col
  point_info_SDH_inhib_neurons <- reactive({
    nearpoint_out_SDH_inhib_neurons <- nearPoints(data_w_log_pval_SDH_inhib_neurons(), input$volcano_hover_SDH_inhib_neurons, xvar = "log2FoldChange", yvar = data$log_pval_SDH_inhib_neurons, maxpoints = 1)
    nearpoint_out_SDH_inhib_neurons %>%
      select("gene", "log2FoldChange", "padj")
  })
  
  # render printed text
  output$click_info_SDH_inhib_neurons <- renderPrint({
    point_info_SDH_inhib_neurons()
  })
  
  # DOWNLOAD HANDLER -----
  
  output$download_volcano_SDH_inhib_neurons <- downloadHandler(
    filename = function() {
      paste0("volcano-plot-neurons", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      ggsave(file, reactive_volcano_SDH_inhib_neurons(), device = "pdf", width = 10, height = 5, units = "in")
    })
  
  #--------------------------------------------------------------------------------------------------
  # SDH EXCIT NEURONS ONLY PAGE
  
  # IDENTIFY DIFFERENTIALLY EXPRESSED GENES -----
  
  # render UI for logfc slider
  # min and max set reactively with logfc based on selected logfc input col
  output$logfc_slider_SDH_excit_neurons <- renderUI({
    sliderInput("logfc_threshold_SDH_excit_neurons",
                "Select effect size threshold",
                min = 0,
                max = round(max(data_SDH_excit_neurons[["log2FoldChange"]])),
                value = 0,
                step = .1)
  })
  
  # use columns and thresholds selected in UI
  is_de_SDH_excit_neurons <- reactive({
    abs(data_SDH_excit_neurons[["log2FoldChange"]]) >= input$logfc_threshold_SDH_excit_neurons & data_SDH_excit_neurons[["padj"]] <= input$pvalue_threshold_SDH_excit_neurons
  })
  
  # FILTERABLE DATAFRAME BY DE GENE -----
  
  is_male_SDH_excit_neurons <- reactive({
    data_SDH_excit_neurons[["log2FoldChange"]] > 0
  })
  
  # reactively filter data based on checkbox
  de_gene_data_SDH_excit_neurons <- reactive({
    if (input$show_de_SDH_excit_neurons) {
      filter(data_SDH_excit_neurons, is_de_SDH_excit_neurons())
    } else {
      data_SDH_excit_neurons
    }
  })
  
  # render data frame of gene data
  output$gene_data_SDH_excit_neurons <- renderDataTable(
    de_gene_data_SDH_excit_neurons()
  )
  
  # X AND Y AXES LABELER -----
  
  # capture pvalue column selected and default value with it
  reactive_pvalue_value_SDH_excit_neurons <- reactive({
    paste0("-log10(", "padj", ")")
  })
  
  # enter custom x (logfc) axis label
  output$x_axis_labeler_SDH_excit_neurons <- renderUI({
    textInput("x_axis_lab_SDH_excit_neurons",
              "Specify X axis label",
              value = "log2FoldChange",
              placeholder = "ex: Log Fold Change")
  })
  
  # enter custom x (logfc) axis label
  output$y_axis_labeler_SDH_excit_neurons <- renderUI({
    textInput("y_axis_lab_SDH_excit_neurons",
              "Specify Y axis label",
              value = reactive_pvalue_value_SDH_excit_neurons(),
              placeholder = "ex: -log10(FDR)")
  })
  
  # HIGHLIGHTED GENE TABLE -----
  
  # initialize gene_list$clicked_gene_list as NULL
  # This will reactively update
  gene_list_SDH_excit_neurons <- reactiveValues(clicked_gene_list_SDH_excit_neurons = NULL)
  
  # store clicked gene info
  clicked_gene_SDH_excit_neurons <- reactive({
    nearPoints(data_w_log_pval_SDH_excit_neurons(),
               input$volcano_click_SDH_excit_neurons,
               xvar = "log2FoldChange",
               yvar = data_SDH_excit_neurons$log_pval,
               maxpoints = 1) %>%
      select("gene")
  })
  
  # when a point is clicked on the volcano plot
  # add gene to clicked gene list
  # if the point has been clicked twice, remove from list
  observeEvent(input$volcano_click_SDH_excit_neurons, {
    # create variable of what has been clicked + selected
    if (is.null(input$selected_genes_SDH_excit_neurons)) {
      gene_list_SDH_excit_neurons$clicked_gene_list_SDH_excit_neurons <- NULL
    }
    # if gene_list is empty
    # get point info and save gene
    if (is.null(gene_list_SDH_excit_neurons$clicked_gene_list_SDH_excit_neurons)) {
      gene_list_SDH_excit_neurons$clicked_gene_list_SDH_excit_neurons <- clicked_gene_SDH_excit_neurons()
      # if gene_list is not NULL
      # check to see if gene is in gene_list
    } else {
      gene_present_SDH_excit_neurons <- clicked_gene_SDH_excit_neurons() %in% input$selected_genes_SDH_excit_neurons
      # if TRUE (gene is present already)
      # remove gene from gene list
      if (gene_present_SDH_excit_neurons) {
        present_idx_SDH_excit_neurons <- !grepl(clicked_gene_SDH_excit_neurons(), input$selected_genes_SDH_excit_neurons)
        # remove row
        gene_list_SDH_excit_neurons$clicked_gene_list_SDH_excit_neurons <- input$selected_genes_SDH_excit_neurons[present_idx_SDH_excit_neurons]
      } else {
        gene_list_SDH_excit_neurons$clicked_gene_list_SDH_excit_neurons <- c(clicked_gene_SDH_excit_neurons(), input$selected_genes_SDH_excit_neurons)
      }
    }
  })
  
  observe({
    updateSelectizeInput(session, 
                         "selected_genes_SDH_excit_neurons",
                         label = "Select feature(s) to label",
                         choices = sort(data_SDH_excit_neurons[["gene"]]),
                         selected = gene_list_SDH_excit_neurons$clicked_gene_list_SDH_excit_neurons,
                         server = TRUE
    )
  })
  
  
  # reactive function that subsets data by highlighted_gene vector
  highlight_gene_data_SDH_excit_neurons <- reactive({
    if (length(input$selected_genes_SDH_excit_neurons) > 0) {
      highlight_gene_data_SDH_excit_neurons <- data_SDH_excit_neurons[data_SDH_excit_neurons[["gene"]] %in% input$selected_genes_SDH_excit_neurons, c("gene", "log2FoldChange", "padj")]
    } else {
      highlight_gene_data_SDH_excit_neurons <- data.frame(NA, NA, NA)
      names(highlight_gene_data_SDH_excit_neurons) <- c("gene", "log2FoldChange", "padj")
    }
  })
  
  # render a data table of highlighted genes info
  output$gene_highlight_tbl_SDH_excit_neurons <- renderDataTable({
    highlight_gene_data_SDH_excit_neurons()
  })
  
  # ZOOM PLOT WITH BRUSH -----
  # initialize reactive value
  # this is the value that will be input into volcanoPlot()
  ranges_SDH_excit_neurons <- reactiveValues(x = NULL, y = NULL)
  
  # when there is a double click on the plot
  # if brush is null, nothing happens,
  # if brush is not null, assign values to ranges
  observeEvent(input$volcano_dbl_click_SDH_excit_neurons, {
    brush_SDH_excit_neurons <- input$volcano_brush_SDH_excit_neurons
    if (!is.null(brush_SDH_excit_neurons)) {
      ranges_SDH_excit_neurons$x <- c(brush_SDH_excit_neurons$xmin, brush_SDH_excit_neurons$xmax)
      ranges_SDH_excit_neurons$y <- c(brush_SDH_excit_neurons$ymin, brush_SDH_excit_neurons$ymax)
      
    } else {
      ranges_SDH_excit_neurons$x <- NULL
      ranges_SDH_excit_neurons$y <- NULL
    }
  })
  
  # PLOT AND RENDER VOLCANO -----
  
  # volcano plot in reactive function 
  reactive_volcano_SDH_excit_neurons <- reactive({
    plotVolcano(data = data_SDH_excit_neurons, 
                logfc_col = "log2FoldChange", 
                pvalue_col = "padj",
                gene_col = "gene",
                pvalue_thresh = input$pvalue_threshold_SDH_excit_neurons,
                logfc_thresh = input$logfc_threshold_SDH_excit_neurons,
                de_vec = is_de_SDH_excit_neurons(),
                is_male = is_male_SDH_excit_neurons(),
                color_by_de = input$color_by_de_SDH_excit_neurons,
                show_logfc_thresh = input$show_logfc_threshold_SDH_excit_neurons,
                show_pvalue_thresh = input$show_pvalue_threshold_SDH_excit_neurons,
                highlight_genes = input$selected_genes_SDH_excit_neurons,
                x_label = input$x_axis_lab_SDH_excit_neurons,
                y_label = input$y_axis_lab_SDH_excit_neurons,
                legend_title = input$legend_title_SDH_excit_neurons,
                xlim = ranges_SDH_excit_neurons$x,
                ylim = ranges_SDH_excit_neurons$y)
  })
  
  # output volcano plot
  output$volcano_plot_SDH_excit_neurons <- renderPlot({
    reactive_volcano_SDH_excit_neurons()
  })
  
  # DISPLAY GENE INFO ON HOVER OVER -----
  
  # Create -log10 pvalue column
  data_w_log_pval_SDH_excit_neurons <- reactive({
    # make new cols and select
    reduced_data_SDH_excit_neurons <- data_SDH_excit_neurons %>%
      mutate(log_pval = -log10(data_SDH_excit_neurons[["padj"]]))
  })
  
  # Collect nearpoint info and reduce to only gene_col, logfc_col and pvalue_col
  point_info_SDH_excit_neurons <- reactive({
    nearpoint_out_SDH_excit_neurons <- nearPoints(data_w_log_pval_SDH_excit_neurons(), input$volcano_hover_SDH_excit_neurons, xvar = "log2FoldChange", yvar = data$log_pval_SDH_excit_neurons, maxpoints = 1)
    nearpoint_out_SDH_excit_neurons %>%
      select("gene", "log2FoldChange", "padj")
  })
  
  # render printed text
  output$click_info_SDH_excit_neurons <- renderPrint({
    point_info_SDH_excit_neurons()
  })
  
  # DOWNLOAD HANDLER -----
  
  output$download_volcano_SDH_excit_neurons <- downloadHandler(
    filename = function() {
      paste0("volcano-plot-neurons", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      ggsave(file, reactive_volcano_SDH_excit_neurons(), device = "pdf", width = 10, height = 5, units = "in")
    })  
}

# build app ----------------------
shinyApp(ui, server)

#------------------------------------
#rsconnect::deployApp('.', 
# appName = "snRNA_Visualization", 
# account = "justinfuzz")
