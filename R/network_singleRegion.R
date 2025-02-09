# module UI function
networkSingleUI <- function(id, label = "Network Single Region") {
  ns <- NS(id)
  
  tagList(sidebarLayout(
    sidebarPanel(
      selectInput(
        ns("network_region"),
        "Choose a brain region:",
        choices = c(
          "Amygdala" = "AMY",
          "Cerebellum" = "CER",
          "Dorsal DG of Hippocampus" = "dDG",
          "Dorsal CA1 of Hippocampus" = "dCA1",
          "Prefrontal Cortex" = "PFC",
          "PVN of Hypothalamus" = "PVN",
          "Ventral DG of Hippocampus" = "vDG",
          "Ventral CA1 of Hippocampus" = "vCA1"
        )
      ),
      searchInput(
        inputId = ns("network_gene"),
        label = "Enter comma-separated list of genes: ",
        placeholder = "e.g. Nr3c1,Fkbp5",
        btnSearch = icon("search", class = "fas fa-search", lib = "font-awesome"),
        btnReset = icon("remove", class = "fas fa-remove", lib = "font-awesome"),
        width = "100%"
      ),
      fileInput(
        inputId = ns("file1"),
        label = "Upload file with list of genes",
        accept = "text/plain",
        buttonLabel = "Upload",
        placeholder = "Upload file with list of genes"
      ),
      radioButtons(
        ns("network_type"),
        label = "Select type of network to display:",
        choiceNames = list("Vehicle", "Differential", "Treatment"),
        choiceValues = list("baseline", "diff", "treatment"),
        selected = "diff"
      ),
      radioButtons(
        ns("vis_neighbours"),
        label = "Include gene neighbourhood",
        choiceNames = c("Yes", "No"),
        choiceValues = c(TRUE, FALSE),
        selected = FALSE
      ),
      radioButtons(
        ns("id_type"),
        label = "Select type of gene ID for plot and table:",
        choices = list("Ensembl", "Gene Symbol"),
        selected = "Gene Symbol"
      ),
      downloadButton(ns("download2"), "Download (filtered) table as csv"),
      width = 3
    ),
    mainPanel(
      # visNetwork
      fluidPage(
        br(),
        tags$style(".fa-project-diagram {color:#2980B9}"),
        h3(p(
          em("Network analysis of gene expression "),
          icon("project-diagram", lib = "font-awesome"),
          style = "color:black;text-align:center"
        )),
        hr(),
        h4(span(textOutput(
          ns("network_type")
        ), style = "color:black;text-align:center")),
        strong("Visualize"),
        span(" a network in a "),
        strong("single brain region"),
        span(
          " after vehicle treatment, Dexamethasone treatment or on a differential level.
             Please choose the brain region you are interested in on the left panel.
             You can enter your "
        ),
        strong("genes of interest"),
        span(
          " either as a comma-separated list or upload a file
             with the respective gene IDs in gene symbol or ENSEMBL format. Furthermore, you have the
             option to "
        ),
        strong("display neighbouring genes"),
        span(
          " and the respective connections in addition to
             your genes of interest. Gene IDs can be displayed in the network as gene symbol or
             ENSEMBL id. You have the option to "
        ),
        strong("filter and download"),
        span(" the
             network data table."),
        br(),br(),
        span(em("Hint: If no network shows up without including the neighbourhood, there are 
                probably no direct connections between the genes you entered. Please try to 
                include the gene neighbourhood."), br(), 
             em("However, if you include the gene neighbourhood, networks become quickly very large
                and computationally expensive . We recommend to include the gene neighbourhood
                only for up to 4 input genes.")),
        br(),
        br(),
        visNetworkOutput(ns("network"), height = "600px") %>%
          withSpinner(color = "#0dc5c1"),
        br(),br(),
        span(em("Hint: By hovering over the column names you get a more detailed description of the columns.")),
        br(),br(),
        DT::dataTableOutput(ns("network_table"))
      )
    )
  ))
  
}



# module server function
networkSingleServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      ### SINGLE REGION NETWORK -------------------------------
      
      # Display correct type of network in title
      output$network_type <- renderText({ 
        v <- c("Vehicle Network", "Differential Network", "Treatment Network")
        k <- c("baseline", "diff", "treatment")
        l <- setNames(as.list(v), k)
        
        n <- l[[input$network_type]]
        })
      
      # Load data from one brain region
      network_df <- reactive({
        file_path <- paste0(
          "tables/network/",
          "04_singleRegion_",
          input$network_region,
          "_filtered_",
          input$network_type,
          "Network.csv"
        )
        # Read data
        if (input$network_type == "diff") {
          data <-
            fread(file = file_path) %>%
            dplyr::select(target, predictor, beta_mean.base, beta_mean.dex, z, p_adj)
        } else {
          data <-
            fread(file = file_path) %>%
            dplyr::select(target, predictor, beta_mean)
        }
        
      })
      
      # DE genes of region
      de_genes <- reactive({
        # Read and subset DE genes for coloring
        df_de <- fread(paste0( "tables/de/02_",
                              input$network_region,"_deseq2_Dex_1_vs_0_lfcShrink.txt"))
        na_indices <- which(is.na(df_de$padj))
        df_de$padj[na_indices] <- 1
        df_de <- df_de[df_de$padj <= 0.1,]
        df_de$Ensembl_ID
      })
      
      # hub genes of region
      hub_genes <- reactive({
        # Read and subset hub genes for coloring
        df_hub <- fread(paste0("tables/network/04_",
                               input$network_region,"_funcoup_differential",
                               "_nodebetweennessNorm_betacutoff0.01.csv")) %>%
          filter(nodebetweenness_norm >= 1)
        df_hub$ensembl_id
      })
      
      
      # input genes
      input_genes <- reactive({
        
        if (isTruthy(input$network_gene)){
          genes <- input$network_gene
          genes <- stringr::str_split(genes, ",\\s?")[[1]]
          genes <- reformat_genes(genes)
        } else if (isTruthy(input$file1)){
          genes <- read.csv(input$file1$datapath, header = FALSE)$V1
          genes <- reformat_genes(genes)
        }
        #genes
      })
      
      
      # bring input genes to correct format
      reformat_genes <- function(list_genes){
        if (!startsWith(list_genes[1], "ENSMUSG")){
          format_gene <- sapply(list_genes, stringr::str_to_title)
          format_gene <- mapIds(org.Mm.eg.db, keys = format_gene, column = "ENSEMBL", keytype = "SYMBOL")
        } else {
          format_gene <- list_genes
        }
        return(format_gene)
      }
      
      
      # Network visualization
      output$network <- renderVisNetwork({
        
        req(input_genes())
        req(network_df())
        req(de_genes())
        req(hub_genes())
        
        # Get Nodes and Edges
        network <- read_network(network_df(),
                                de_genes(),
                                hub_genes(),
                                input_genes(),
                                input$network_type,
                                input$id_type,
                                input$vis_neighbours)
        
        visNetwork(network$nodes, network$edges) %>%
          visEdges(arrows = "to") %>%
          visOptions(highlightNearest = TRUE) %>%
          visLegend(addEdges = network$ledges, addNodes = network$lnodes, 
                    useGroups = FALSE)
        #visIgraphLayout()
      })
      
      # network table
      network_data <- reactive({
        
        req(input_genes())
        req(network_df())
        
        # input genes
        gene <- input_genes()
        
        # Get data and subset to neighbours of gene of interest
        data <- network_df() %>%
          dplyr::rename("from" = "target", "to" = "predictor")
        data <- simplify_network_df(data)
        data <- subset_network(data, gene, input$vis_neighbours)
        
        # ID type to gene symbol
        if(input$id_type == "Gene Symbol"){
          data$from <- mapIds(org.Mm.eg.db, keys = data$from, 
                              column = "SYMBOL", keytype = "ENSEMBL")
          data$to <- mapIds(org.Mm.eg.db, keys = data$to, 
                            column = "SYMBOL", keytype = "ENSEMBL")
        }
        
        data %>% 
          dplyr::rename("target" = "from", "predictor" = "to")
      })
      
      # Data table with network results
      output$network_table <- DT::renderDataTable({
        
        if(input$network_type == "diff"){
          hover_info <- c("Gene ID of the dependent variable",
                          "Gene ID of the independent variable",
                          "Mean beta weight for the predictor across multiple refits of the model after vehicle treatment",
                          "Mean beta weight for the predictor across multiple refits of the model after Dexamethasone treatment",
                          "Z-value calculated from mean beta values and their standard errors in vehicle and treatment condition",
                          "FDR corrected p-value for the z-value")
          
        }else{
          hover_info <- c("Gene ID of the dependent variable",
                          "Gene ID of the independent variable",
                          "Mean beta weight for the predictor across multiple refits of the model")
        }
        
        network_data() %>%
          datatable(filter = list(position = 'top'),
                    options = list(
                      headerCallback= JS(callHeaderCallback(hover_info))
                    ))
      }, server = TRUE) # FALSE to enable download of all pages with button
      
      
      # Download of (filtered) network table
      output$download2 <- downloadHandler(
        filename = function() {
          paste0("network_dexStimMouse_", input$region, ".csv")
        },
        content = function(file) {
          indices <- input$network_table_rows_all
          write.csv(network_data()[indices, ], file)
        }
      )
      
    }
    
  )
  
}