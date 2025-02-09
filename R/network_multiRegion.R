# module UI function
networkMultiUI <- function(id, label = "Network Multiple Regions"){
  
  ns <- NS(id)
  
  tagList(sidebarLayout(
    
    sidebarPanel(
      pickerInput(
        inputId = ns("network_multireg"),
        label = "Select brain regions",
        choices = c("Amygdala" = "AMY", 
                    "Cerebellum" = "CER", 
                    "Dorsal DG of Hippocampus" = "dDG", 
                    "Dorsal CA1 of Hippocampus" = "dCA1", 
                    "Prefrontal Cortex" = "PFC", 
                    "PVN of Hypothalamus" = "PVN", 
                    "Ventral DG of Hippocampus" = "vDG", 
                    "Ventral CA1 of Hippocampus" = "vCA1"),
        options = list(
          `actions-box` = TRUE,
          size = 8,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
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
        placeholder = "Upload file with list of genes"),
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
      checkboxGroupInput(
        ns("tableContent"),
        label = "Table content (diff. network):",
        choices = list(
          "z-scores" = "z",
          "p-values" = "p",
          "beta values" = "beta"
        ),
        selected = "z"
      ), 
      downloadButton(ns("download2"),"Download (filtered) table as csv"),
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
        h4(span(textOutput(ns("network_type")), style = "color:black;text-align:center")),
        strong("Visualize"),span(" a network in "), strong("multiple brain regions"),
        span(" after vehicle treatment, Dexamethasone treatment or on a differential level. 
             Please select the brain regions you are interested in on the left panel. 
             You can enter your "), strong("genes of interest"), span(" either as a comma-separated list or upload a file
             with the respective gene IDs in gene symbol or ENSEMBL format. Furthermore, you have the 
             option to "), strong("display neighbouring genes"), span(" and the respective connections in addition to 
             your genes of interest. Gene IDs can be displayed in the network as gene symbol or 
             ENSEMBL id. You have the option to "), strong("filter and download"), span(" the
             network data table."),
        br(),br(),
        span(em("Hint: If no network shows up without including the neighbourhood, there are
                probably no direct connections between the genes you entered. Please try to 
                include the gene neighbourhood."),br(), 
             em("However, if you include the gene neighbourhood, networks become quickly very large
                and computationally expensive . We recommend to include the gene neighbourhood
                only for up to 4 input genes.")),
        br(),br(),
        visNetworkOutput(ns("network_diff_multi"), height = "600px")
          %>% withSpinner(color="#0dc5c1"),
        br(),br(),
        span(em("Hint: By hovering over the column names you get a more detailed description of the columns.")),
        br(),br(),
        DT::dataTableOutput(ns("network_table"))
      )
    )
    
    
  ))
}



# module server function
networkMultiServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      ### MULTI REGION NETWORK -------------------------------
      
      # Display correct type of network in title
      output$network_type <- renderText({ 
        v <- c("Vehicle Network", "Differential Network", "Treatment Network")
        k <- c("baseline", "diff", "treatment")
        l <- setNames(as.list(v), k)
        
        n <- l[[input$network_type]]
      })
      
      # Load data from multiple brain region
      network_data <- reactive({
        
        req(input$network_multireg)
        
        # Read data tables from all required regions
        list_network <- list()
        
        for (reg in input$network_multireg){
          file_path <- paste0(
            "tables/network/",
            "04_singleRegion_",
            reg,
            "_filtered_",
            input$network_type,
            "Network.csv"
          )
          # Read data
          if (input$network_type == "diff") {
            data <-
              fread(file = file_path) %>%
              dplyr::select(target, predictor, beta_mean.base, beta_mean.dex, z, p_adj)
              #dplyr::select(target, predictor, z) %>%
              #dplyr::rename_with(.fn = ~ reg, .cols = z)
            
            cols <- colnames(data)[3:6]
            data <- data %>%
               dplyr::rename_with(.fn = ~paste0(., ".", reg), .cols = all_of(cols) )
          } else {
            data <-
              fread(file = file_path) %>%
              dplyr::select(target, predictor, beta_mean)
            # cols <- colnames(data)[3]
            data <- data %>%
              dplyr::rename_with(.fn = ~ reg, .cols = beta_mean)
              # dplyr::rename_with(.fn = ~paste0(., ".", reg), .cols = all_of(cols) )
          }
          list_network[[reg]] <- data
        }
        
        data_joined <- list_network %>% 
          purrr::reduce(full_join, by = c("target","predictor"))
        
        return(data_joined)
      })
      
      
      
      # DE genes of required brain regions
      de_genes <- reactive({
        
        list_de <- list()
        # Read and subset DE genes for coloring
        for (reg in input$network_multireg){
          df_de <- fread(paste0("tables/de/02_",
                              reg,"_deseq2_Dex_1_vs_0_lfcShrink.txt"))
          na_indices <- which(is.na(df_de$padj))
          df_de$padj[na_indices] <- 1
          df_de <- df_de[df_de$padj <= 0.1,]
          
          df_de <- df_de %>%
            dplyr::select(Ensembl_ID, padj) %>%
            dplyr::rename_with(.fn = ~ reg, .cols = padj)
          
          list_de[[reg]] <- df_de
        }
        
        de_joined <- list_de %>% 
          purrr::reduce(full_join, by = c("Ensembl_ID"))
        
        return(de_joined)
      })
      
      
      # hub genes of required brain regions
      hub_genes <- reactive({
        
        list_hub <- list()
        # Read and subset hub genes for coloring
        for (reg in input$network_multireg) {
          df_hub <-
            fread(
              paste0(
                "tables/network/04_",
                reg,
                "_funcoup_differential",
                "_nodebetweennessNorm_betacutoff0.01.csv"
              )
            ) %>%
            filter(nodebetweenness_norm >= 1) %>%
            dplyr::select(ensembl_id, nodebetweenness_norm) %>%
            dplyr::rename_with(.fn = ~ reg, .cols = nodebetweenness_norm)
          
          list_hub[[reg]] <- df_hub
        }
        
        hub_joined <- list_hub %>% 
          purrr::reduce(full_join, by = c("ensembl_id"))
        
        return(hub_joined)
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
      })
      
      
      # bring input genes to correct format
      reformat_genes <- function(list_genes){
        if (!startsWith(list_genes[1], "ENSMUSG")){
          format_gene <- sapply(list_genes, stringr::str_to_title)
          format_gene <- mapIds(org.Mm.eg.db, keys = format_gene, column = "ENSEMBL", keytype = "SYMBOL")
          ids_na <- names(format_gene)[which(is.na(format_gene))]
          #print(ids_na)
          if (length(ids_na) > 0) {
            showNotification(paste("No Ensembl ID found for following genes: ",
                                   ids_na), type = "message", duration = 5)
          }
          format_gene <- format_gene[which(!is.na(format_gene))]
        } else {
          format_gene <- list_genes
        }
        return(format_gene)
      }
      
      
      # Network visualization
      output$network_diff_multi <- renderVisNetwork({
        
        req(input_genes())
        req(network_data())
        req(de_genes())
        req(hub_genes())
        
        # Get Nodes and Edges
        network <- network(network_data(),
                                de_genes(),
                                hub_genes(),
                                input_genes(),
                                input$network_type,
                                input$id_type,
                                input$vis_neighbours)
        
        visNetwork(network$nodes, network$edges) %>%
          visEdges(arrows = "to") %>%
          visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
          visLegend(addEdges = network$ledges, addNodes = network$lnodes, 
                    useGroups = FALSE) %>%
          visIgraphLayout()
      })
      
      
      # Data table with network results
      output$network_table <- DT::renderDataTable({
        
        n_reg <- length(input$network_multireg)
        
        hover_info <- c("Gene ID of the dependent variable",
                        "Gene ID of the independent variable")
        
        if(input$network_type == "diff"){
          add_hover <- c()
          
          if ("beta" %in% input$tableContent){
            add_hover <- c(add_hover, "Mean beta weight for the predictor across multiple refits of the model in the specified brain region after vehicle treatment",
                           "Mean beta weight for the predictor across multiple refits of the model in the specified brain region after Dexamethasone treatment")
          }
          if ("z" %in% input$tableContent){
            add_hover <- c(add_hover, "Z-value calculated from mean beta values and their standard errors in vehicle and treatment condition in the specified brain region")
          }
          if("p" %in% input$tableContent){
            add_hover <- c(add_hover, "FDR corrected p-value for the z-value in the specified region")
          }
          
          hover_info <- c(hover_info, 
                          rep(add_hover, n_reg))
          
        } else {
          hover_info <- c(hover_info,
                          rep("Mean beta weight for the predictor across multiple refits of the model
                              in the specified brain region", n_reg))
        }

        network_table() %>%
          datatable(filter = list(position = 'top'),
                    options = list(
                      headerCallback= JS(callHeaderCallback(hover_info))
                    ))
      }, server = TRUE) # FALSE to enable download of all pages with button

      
      # Download of (filtered) network table
      output$download2 <- downloadHandler(
        filename = function() {
          paste0("network_dexStimMouse_multiRegion.csv")
        },
        content = function(file) {
          indices <- input$network_table_rows_all
          write.csv(network_table()[indices, ], file)
        }
      )
      
      
      # prepare network for visualization
      network <- function(data, de_genes, hub_genes, input_genes, mode, id_type, neighbours){
        
        # input genes
        print(input_genes)
        
        # filter data
        data <- data %>%
          dplyr::rename(from = predictor, to = target) %>%
          dplyr::filter(from != to) 
        data <- subset_network(data, input_genes, neighbours)
        
        # color palettes for edges and nodes
        # edge palette --> assumes that data stores only target, predictor and z scores
        # edge_colors <- brewer.pal(ncol(data)-1, "Blues")[2:(ncol(data)-1)]
        
        # Edges
        if(mode == "baseline" | mode == "treatment"){
          # color palettes for edges and nodes
          edge_colors <- brewer.pal(ncol(data)-1, "Blues")[2:(ncol(data)-1)]
          
          # count regions per diff edge that are not na
          data$edge_reg <- apply(data[,3:ncol(data)], 1, function(x) names(which(!is.na(x))) )
          data$count_reg <- sapply(data$edge_reg, length)
          data$title <- paste0("<p><b>Connection in: </b>", sapply(data$edge_reg, paste, collapse = ", "),
                               "</p>")
          # assign color to edges according to number of regions
          data$c <- sapply(data$count_reg, function(x) edge_colors[x])
          
          #print(head(data))
          # df for edges
          relations <- data.table(from=data$from,
                                  to=data$to,
                                  #beta = data$beta_mean,
                                  color = data$c,
                                  title = data$title)
          # df for edge legend
          ledges <- data.frame(color = edge_colors,
                               label = as.character(1:(ncol(data)-6)), 
                               font.align = "top")
          #print(ledges)
        } else {
          # remove columns
          data <- data %>% dplyr::select(-contains("beta"), -contains("p_adj"))
          
          # color palettes for edges and nodes
          # edge palette --> assumes that data stores only target, predictor and z scores
          edge_colors <- brewer.pal(ncol(data)-1, "Blues")[2:(ncol(data)-1)]
          
          # count regions per diff edge that are not na
          data$edge_reg <- apply(data[,3:ncol(data)], 1, function(x) names(which(!is.na(x))) )
          data$count_reg <- sapply(data$edge_reg, length)
          data$title <- paste0("<p><b>Diff. co-expressed in: </b>", sapply(data$edge_reg, paste, collapse = ", "),
                               "</p>")
          # assign color to edges according to number of regions
          data$c <- sapply(data$count_reg, function(x) edge_colors[x])
          
          #print(head(data))
          # df for edges
          relations <- data.table(from=data$from,
                                  to=data$to,
                                  #z = data$z,
                                  #p_adj = data$p_adj,
                                  color = data$c,
                                  title = data$title)
          # df for edge legend
          ledges <- data.frame(color = edge_colors,
                               label = as.character(1:(ncol(data)-6)), 
                               font.align = "top")
          #print(ledges)
        }
        #print(nrow(relations))
        
        
        # Nodes
        # get unique nodes with correct id
        nodes <- data.frame("id" = unique(union(
          c(relations$from, relations$to),
          input_genes
        )), stringsAsFactors = FALSE)
        if (id_type == "Gene Symbol"){
          #print(nodes$id)
          nodes$label <- mapIds(org.Mm.eg.db, keys = nodes$id,
                                column = "SYMBOL", keytype = "ENSEMBL")
        } else {
          nodes$label <- nodes$id
        }
        
        # count regions where gene is DE or/and hub
        nodes_de <- left_join(x = nodes, y = de_genes,
                              by = c("id" = "Ensembl_ID"))
        nodes_hub <- left_join(x = nodes, y = hub_genes,
                               by = c("id" = "ensembl_id"))
        
        nodes$de_reg <- apply(nodes_de[,3:ncol(nodes_de)], 1, 
                              function(x) names(which(!is.na(x))) )
        nodes$de_count <- sapply(nodes$de_reg, length)
        
        nodes$hub_reg <- apply(nodes_hub[,3:ncol(nodes_hub)], 1, 
                               function(x) names(which(!is.na(x))) )
        nodes$hub_count <- sapply(nodes$hub_reg, length)
        
        # set color according to DE/hub status
        nodes$color <- rep("darkblue", nrow(nodes_de))
        nodes$color[nodes$de_count > 0] <- "orange"
        nodes$color[nodes$hub_count > 0] <- "darkred"
        nodes$color[nodes$de_count > 0 & nodes$hub_count > 0] <- "purple"
        
        #nodes$opacity <- nodes$de_count + nodes$hub_count
        #nodes$opacity <- nodes$opacity/max(nodes$opacity)
        
        nodes$title <- paste0("<p><i>",nodes$label,"</i>",
                              "<br><b>DE in regions: </b>", sapply(nodes$de_reg, paste, collapse = ", "),
                              "<br><b>Hub in regions: </b>", sapply(nodes$hub_reg, paste, collapse = ", "),"</p>")
        
        #print(head(nodes))
        
        # df for node data frame
        lnodes <- data.frame(label = c("DE", "hub", "DE & hub", "no DE & no hub"),
                             font.color = c("black", "white", "white", "white"),
                             shape = c( "ellipse"), 
                             color = c("orange", "darkred", "purple", "darkblue"),
                             title = "Informations", id = 1:4)
        
        
        return(list("edges" = relations, "nodes" = nodes, "ledges" = ledges, "lnodes" = lnodes))
        
      }
      
      
      
      # prepare network table for visualization
      network_table <- reactive({
        
        req(network_data())
        req(input_genes())
        
        # filter data
        # remove self connections
        data <- network_data() %>%
          dplyr::rename(from = predictor, to = target) %>%
          dplyr::filter(from != to) 
        data <- subset_network(data, input_genes(), input$vis_neighbours) %>%
          dplyr::rename(predictor = from, target = to)
        
        # Network table
        if(input$network_type == "baseline" | input$network_type == "treatment"){

          # add beta to column name
          cols <- colnames(data)[3:ncol(data)]
          data <- data %>%
            # dplyr::rename_with(.fn = ~ reg, .cols = beta_mean)
            dplyr::rename_with(.fn = ~paste0("beta.", .), .cols = all_of(cols) )
          
        } else {
          
          if (!("beta" %in% input$tableContent)){
            data <- data %>% dplyr::select(-contains("beta"))
          }
          if (!("z" %in% input$tableContent)){
            data <- data %>% dplyr::select(-contains("z"))
          }
          if(!("p" %in% input$tableContent)){
            data <- data %>% dplyr::select(-contains("p_adj"))
          }
          
        }
        
        if (input$id_type == "Gene Symbol"){
          data$predictor <- mapIds(org.Mm.eg.db, keys = data$predictor,
                              column = "SYMBOL", keytype = "ENSEMBL")
          data$target <- mapIds(org.Mm.eg.db, keys = data$target,
                            column = "SYMBOL", keytype = "ENSEMBL")
        }
        
        data
        
      })
      
      
    }
    
  )
}