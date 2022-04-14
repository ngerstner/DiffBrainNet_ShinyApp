# Define server logic required to generate and plot 
server <- shinyServer(function(input, output) {
  
  ### TERMS OF USE & IMPRINT ----------------------------------
  
  observeEvent(input$termsOfUse, {
    showModal(modalDialog(
      h4("Terms of Use, v1.0 (03/2022)"),
      tags$ol(
        tags$p("By using the software application DiffBrainNet (hereinafter “DiffBrainNet”), 
               you acknowledge that you have read these terms and conditions, understand 
               them, and agree to be bound by them. If you do not agree with these 
               terms and conditions, you must not use DiffBrainNet. Please read the 
               following Terms of Use and any accompanying documentation carefully 
               before you use the application DiffBrainNet."),
        tags$li(
          tags$h5("Contents and availability"),
          tags$p(
            "DiffBrainNet consists of the entire web application, i.e., the
                 texts, figures, and data shown on this page, as well as the programming
                 source code with which the visualizations and results are generated. "
          ),
          tags$p(
            "The use of DiffBrainNet is licensed to You by the Max-Planck-Gesellschaft 
            zur Förderung der Wissenschaften e.V., here represented by the managing 
            director Elisabeth Binder, acting for the Max Planck Institute of Psychiatry, 
            with offices at Kraepelinstr. 2-10, 80804 Munich, Germany (hereinafter: 
            Licensor) subject to these Terms of Use."
          ),
          tags$p(
            "All figures, texts, and data shown in DiffBrainNet are licensed under 
            the Creative Commons Attribution license (",
            a(href="https://creativecommons.org/licenses/by/4.0/", 
              "https://creativecommons.org/licenses/by/4.0/", target="_blank"),"). 
            The use of the DiffBrainNet software is licensed under the GNU Affero 
            General Public License v3.0 (",
            a(href="http://www.gnu.org./licenses/", "http://www.gnu.org./licenses/", target="_blank"),
            "). "
          ),
          tags$p(
            "The Licensor does not bear any responsibility for i) third-party contents, 
            ii) for the use of DiffBrainNet different from the Purpose set out below, 
            iii) for the accessibility via external links, iv) nor for its availability. 
            Furthermore, the Licensor waives any responsibility for contents that 
            may be illegal or violate common decency."
          )
        ), 
        tags$li(
          tags$h5("Purpose"),
          tags$p(
            "The Licensor has built DiffBrainNet in order to allow the scientific 
            community to explore the transcriptional landscape of 8 mouse brain 
            regions before and after treatment with dexamethasone. "
          ),
          tags$p(
            "The use and output of DiffBrainNet can only be complementary to and 
            neither replace informed judgement, nor can it be used as a stand-alone 
            recommendation without further validation. "
          )
        ),
        tags$li(
          tags$h5("Rules of Use"),
          tags$p(
            "You accept to use DiffBrainNet non-commercially and for the aforementioned 
            Purpose. You are not authorized to make the contents of the use of DiffBrainNet 
            available to third parties. In duly substantiated exceptional cases, 
            the Licensor may restrict Your right to use DiffBrainNet."
          )
        ),
        tags$li(
          tags$h5("Proprietary rights to the contents of DiffBrainNet"),
          tags$p(
            "You agree to respect copyrights, rights to names, trademarks and other 
            intellectual property rights, if any, of the Licensor and of third parties 
            when making use of DiffBrainNet. By enabling access to the use of DiffBrainNet, 
            the Licensor does not grant any licence nor any other right of use."
          )
        ),
        tags$li(
          tags$h5("Improper use of DiffBrainNet"),
          tags$p(
            "You agree to refrain from any improper use of the use of DiffBrainNet; 
            in particular, no security precautions must be circumvented. DiffBrainNet 
            may not be used to create fake, libellous, misleading, or defamatory 
            content of any kind."
          ),
          tags$p(
            "Furthermore, no facilities may be used nor may any applications be 
            run that could lead to a damage or a performance failure of any of the 
            facilities from which the use of DiffBrainNet is provided, in particular 
            through changes in the physical or logical structure of the servers 
            or its network or of any other network. No commercial, systematic use 
            of DiffBrainNet is permitted without the Licensor's consent."
          )
        ),
        tags$li(
          tags$h5("Limitation of Liability (for Civil Law Countries)"),
          tags$p(
            "Without prejudice to Licensor´s responsibility for tort or for violation 
            of mandatory laws of the Federal Republic of Germany, the Licensor shall 
            not be liable for any damages You incur when using DiffBrainNet."
          )
        ),
        tags$li(
          tags$h5(
            "Representation, Warranty, Limitation of Liability, Indemnification 
            (for Common Law Countries)"
          ),
          tags$ol(
            tags$li(
              tags$h5("Representation and Warranty"),
              tags$p(
                "LICENSOR REPRESENTS THAT LICENSOR HAS ALL RIGHTS REQUIRED TO MAKE 
                AVAILABLE AND DISTRIBUTE THE MATERIALS. EXCEPT FOR SUCH REPRESENTATION, 
                THE MATERIALS ARE PROVIDED “AS IS” AND “AS AVAILABLE” AND WITHOUT 
                WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING, BUT NOT LIMITED 
                TO, NON-INFRINGEMENT, MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
                PURPOSE, AND ANY WARRANTIES IMPLIED BY ANY COURSE OF PERFORMANCE OR 
                USAGE OF TRADE, ALL OF WHICH ARE EXPRESSLY DISCLAIMED."
              ),
              tags$p(
                "WITHOUT LIMITING THE FOREGOING, LICENSOR DOES NOT WARRANT THAT: (A) 
                THE MATERIALS ARE ACCURATE, COMPLETE, RELIABLE OR CORRECT; (B) THE 
                MATERIALS FILES WILL BE SECURE; (C) THE MATERIALS WILL BE AVAILABLE 
                AT ANY PARTICULAR TIME OR LOCATION; (D) ANY DEFECTS OR ERRORS WILL 
                BE CORRECTED; (E) THE MATERIALS AND ACCOMPANYING FILES ARE FREE OF 
                VIRUSES OR OTHER HARMFUL COMPONENTS; OR (F) THE RESULTS OF USING THE 
                MATERIALS WILL MEET DOWNLOADER’S REQUIREMENTS. DOWNLOADER’S USE OF 
                THE MATERIALS IS SOLELY AT DOWNLOADER’S OWN RISK."
              )
            ),
            tags$li(
              tags$h5("Limitation of Liability"),
              tags$p(
                "IN NO EVENT SHALL LICENSOR BE LIABLE UNDER CONTRACT, TORT, STRICT 
                LIABILITY, NEGLIGENCE OR ANY OTHER LEGAL THEORY WITH RESPECT TO THE 
                MATERIALS (I) FOR ANY DIRECT DAMAGES, OR (II) FOR ANY LOST PROFITS 
                OR SPECIAL, INDIRECT, INCIDENTAL, PUNITIVE, OR CONSEQUENTIAL DAMAGES 
                OF ANY KIND WHATSOEVER."
              )
            ),
            tags$li(
              tags$h5("Indemnification"),
              tags$p(
                "You will indemnify and hold Licensor harmless from and against any 
                and all loss, cost, expense, liability, or damage, including, without 
                limitation, all reasonable attorneys’ fees and court costs, arising 
                from i) Your misuse of the use of the Application DiffBrainNet; (ii) 
                Your violation of these Terms of Use; or (iii) infringement by You 
                or any third party of any intellectual property or other right of 
                any person or entity contained in the use of the Application DiffBrainNet. 
                Such losses, costs, expenses, damages, or liabilities shall include, 
                without limitation, all actual, general, special, and consequential 
                damages."
              )
            )
          )
        )
      ), easyClose = TRUE, size = "l", footer = NULL)
    )
  })
  
  observeEvent(input$show, {
    showModal(modalDialog(
      #HTML('<img src="graphical_abstract.png" alt="Graphical Abstract">'),
      # tags$iframe(style="height:600px; width:100%", src="graphicalabstract_new.pdf"),
      tags$img(src="graphicalabstract_new.svg",
               width="100%", height= "100%"),
      footer=tags$div(
              tags$b("Graphical Abstract:"),
              tags$p("Schematic representation of experimental and analytical steps. 
              DiffBrainNet is a resource of differential expression and differential 
              networks in 8 mouse brain regions. (Experiment) C57Bl/6 mice were treated 
              intraperitoneally with 10mg/kg Dexamethasone or 0.9% saline as vehicle 
              for 4hours. Eight different brain regions were isolated: amygdala – AMY, 
              cerebellar cortex – CER, prefrontal cortex – PFC, paraventricular nucleus 
              of the hypothalamus – PVN, dorsal Cornu Ammonis 1 – dCA1, ventral Cornu 
              Ammonis 1 – vCA1, dorsal dentate gyrus – dDG, ventral dentate gyrus – vDG. 
              (Analysis) We performed RNA sequencing in the 8 brain regions, followed 
              by differential expression analysis (DE) and differential prior-knowledge-based 
              genome-wide network analysis (DN). (Results) DiffBrainNet includes 
              differential expression results and network results for all brain regions.")),
      size="l",
      easyClose = TRUE
    ))
  })
  
  
  
  ### DIFFERENTIAL EXPRESSION ----------------------------------
  
  # DE data set
  de_data <- reactive({
    # read DE table to create volcano plot
    table <- fread(paste0("tables/de/02_",
                          input$region,"_deseq2_Dex_1_vs_0_lfcShrink.txt"))
    # table$Gene_Symbol <- mapIds(org.Mm.eg.db, keys = table$V1, 
    #                             keytype = "ENSEMBL", column="SYMBOL")
    table <- table %>%
      dplyr::select(Gene_Symbol, Ensembl_ID:padj) %>%
      dplyr::mutate_at(vars(baseMean, log2FoldChange, lfcSE), ~round(.,4)) %>%
      dplyr::mutate_at(vars(pvalue, padj), ~signif(.,6))
  })

  
  # Volcano Plot displaying DE results
  output$volcano_plot <- renderPlotly({
    
    # req(de_data())
    
    # add column that indicates if gene is significant
    indices <- input$de_table_rows_all
    table <- de_data()[indices,] %>%
      dplyr::mutate(sig = as.factor(ifelse(padj <= 0.1, "FDR <= 0.1", "FDR > 0.1")))
    
    # volcano plot
    # volcano_plot <- plot_ly(data = table, x = ~log2FoldChange, y = ~-log10(padj),
    #                         color = ~sig, colors = "Set1",
    #                         text = ~paste0("Gene: ", V1),
    #                         source = "volcano_plot") %>%
    #   layout(title = paste0("Volcano Plot ", input$region))
    volcano_plot <-
      ggplot(data = table, aes(
        x = log2FoldChange,
        y = -log10(padj),
        col = sig,
        text = paste0("Ensembl_ID: ", Ensembl_ID, "\nGene_Symbol: ", Gene_Symbol)
      )) +
      geom_point() +
      scale_colour_manual(breaks = c("FDR <= 0.1", "FDR > 0.1"),
                          values = c("orange", "darkgrey")) +
      ylab("-log10(FDR)")+
      theme_light() +
      theme(plot.title = element_text(size = 11), 
            legend.title = element_blank()) +
      ggtitle(label = paste0("Volcano Plot ", input$region))
    ggplotly(volcano_plot,
             source = "volcano_plot")
  })
  
  # Boxplot displaying norm expression values
  output$exp_plot <- renderPlotly({
    
    # req(de_data())
    
    # access data from click event
    d <- event_data("plotly_click", source = "volcano_plot",
                    priority = "event")
    # print(d)
    # don't show anythiny if no point was clicked
    if (is.null(d)) return(NULL) 
    
    # identify ensembl ID using the DE data
    ensembl_id <- de_data()[input$de_table_rows_all,]$Ensembl_ID[d$pointNumber + 1] # use pointNumber/rowNumber of clicked point
    gene_symbol <- de_data()[input$de_table_rows_all,]$Gene_Symbol[d$pointNumber + 1]
    
    # read table with normalized expression values
    exp_table <- fread(paste0("tables/de/02_",
                              input$region,"_deseq2_expression_vsd.txt"))
    
    # subset to row of clicked gene and reformat 
    exp_gene <- data.table::transpose(exp_table[V1 == ensembl_id,2:ncol(exp_table)], keep.names = "condition") %>%
      dplyr::rename("expression" = "V1")
    exp_gene$condition <- str_replace(exp_gene$condition, ".*\\_","")
    exp_gene$mouse_id <- str_extract(exp_gene$condition, pattern = "[0-9]+")
    exp_gene$condition <- as.factor(str_replace(exp_gene$condition, "[0-9]+",""))
    
    # boxplot with jitter points of expression values in CNTRL and DEX
    exp_plot <-
      ggplot(exp_gene, aes(x = condition, y = expression, fill = condition)) +
      geom_boxplot() +
      geom_jitter(color = "black",
                  size = 0.4,
                  alpha = 0.9,
                  aes(text = sprintf('mouse_ID: %s', mouse_id))) +
      scale_fill_manual("",
                        breaks = c("CNTRL", "DEX"),
                        labels = c("Vehicle", "Treatment"),
                        values = c("#B0BFBB", "#46866E")) +
      scale_x_discrete(breaks = c("CNTRL", "DEX"),
                       labels = c("Vehicle", "Treatment")) +
      theme_light() +
      theme(legend.position = "none",
            plot.title = element_text(size = 11)) +
      ggtitle(paste0("Gene expression of ", gene_symbol, " in ", input$region)) +
      xlab("") +
      ylab("Normalized expression")
    # ggplot to plotly
    ggplotly(exp_plot)
    
  })
  
  
  # Data table with DE results
  output$de_table <- DT::renderDataTable({
    
    # display information when hovering column names
    hover_info <- c("Gene ID in gene symbol format",
                    "Gene ID in Ensembl format",
                    "Mean expression in vehicle condition",
                    "Logarithmized fold change of gene expression between control and treatment condition",
                    "Standard Error of the fold change",
                    "Respective p-value of the fold change",
                    "FDR corrected p-value")
    
    de_data() %>%
      # dplyr::rename("Ensembl_ID" = "V1") %>%
      datatable(filter = list(position = 'top'),
                options = list(
                  headerCallback= JS(callHeaderCallback(hover_info))
                ))
  }, server = TRUE) # FALSE to enable download of all pages with button
  
  # Download of (filtered) DE results
  output$download1 <- downloadHandler(
    filename = function() {
      paste0("DE_dexStimMouse_", input$region, ".csv")
    },
    content = function(file) {
      indices <- input$de_table_rows_all
      write.csv(de_data()[indices, ], file)
    }
  )
  
  # Upset plot and table from upsetPlot module
  upset_de <- upsetPlotServer("upsetDE")
  
  
  ############# NETWORK ANALYSIS ##############################
  
  
  # Network data (nodedegree/nodebetweenness)
  overview_data <- reactive({
    
    # Read nodedegrees/nodebetweenness
    filename <- list.files(path = paste0("tables/network"),
                           pattern = paste0("*_", input$overview_region,
                                            "_funcoup_", input$overview_network, "_",
                                            input$overview_metric,"Norm_betacutoff0.01.csv"),
                           full.names = TRUE)
    data <- fread(file = filename)
    
  })
  
  # Upset plot and table from upsetPlot module
  upset_hub <- comparisonHubGenesServer("compHubGenes")
  
  
  # Histogram network overview
  output$histogram_network <- renderPlotly({
    
    req(overview_data())
    
    # Histogram of nodedegree/nodebetweenness
    if (input$overview_metric == "nodedegrees"){
      hist_network <- ggplot(overview_data(), aes(x=nodedegree)) +
        geom_histogram(bins=80)
    } else {
      hist_network <- ggplot(overview_data(), aes(x=nodebetweenness)) +
        geom_histogram(bins=80)
    }
    
    ggplotly(hist_network)
  })
  
  ### SINGLE REGION NETWORK -------------------------------
  
  # Single region network and table from networkSingle module
  net_single <- networkSingleServer("singleVisualization")
  
  
  ### MULTI REGION NETWORK ---------------------------------------
  
  # Multi region network and table from networkMulti module
  net_multi <- networkMultiServer("multiVisualization")
  
  
})

