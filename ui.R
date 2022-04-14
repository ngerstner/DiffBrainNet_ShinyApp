
ui <- fluidPage(theme = shinytheme("flatly"),
titlePanel(title=div(img(src="DiffBrainNet_logo.png", height = 50, width = 200), "Glucocorticoid receptor regulated gene expression in the mouse brain")),
navbarPage("Explore!",
           
           tabPanel(icon("home"),
                    tags$head(tags$style(".modal-footer {text-align: left;}")),
                    fluidRow(column(tags$img(src="mouse_javi_mpi.png",
                                             width="100%", height= "100%"),
                                    width=floor(0.4*12)),
                             column(
                               p(strong("Abstract."), "Network analysis can identify the molecular connectivity 
                               that is underpinning function at the control level, after a stimulus or a disease state. We inferred 
                               regression-based prior-knowledge-guided gene networks in 8 brain regions of the mouse brain: 
                               the prefrontal cortex, the amygdala, the paraventricular nucleus of the hypothalamus, the dorsal 
                               and ventral Cornu ammonis 1, the dorsal and ventral dentate gyrus and the cerebellar cortex. 
                               We constructed networks at the control and treatment level using KiMONo (Ogris et al.) and at 
                               the differential level using DiffGRN (Kim et al.).",br(),
                               "As a stimulus we used dexamethasone, a synthetic glucocorticoid that is used 
                               to activate the glucocorticoid receptors. Glucocorticoid receptors, when coupled with glucocorticoids 
                               like dexamethasone, act as transcription factors modulating the transcriptional landscape. 
                               We provide differential networks and differential 
                               expression analysis (DESeq2, Love et al.) that can be used to analyse the effects of dexamethasone 
                               both at the molecular connectivity and at the gene level in each brain region.", br(),
                               "DiffBrainNet is an analysis framework and a resource for studying the transcriptional landscape 
                               of 8 mouse brain regions at the control, dexamethasone-treatment and differential level. It can be 
                               used to pinpoint molecular pathways important for the basic function and response to glucocorticoids 
                               in a brain-region specific manner. DiffBrainNet can also support the identification and analysis 
                               of biological processes regulated by brain and psychiatric diseases risk genes at the control and 
                               differential levels.", br(), 
                                 actionButton("show", "Show graphical abstract",
                                              style="color: #fff; background-color: #2980B9; border-color: #2e6da4"),
                                 style="text-align:justify;color:#fff;background-color:#2980B9;padding:15px;border-radius:10px"),
                               br(),
                               p(strong("Data and code availability."), "Raw and normalized gene expression data generated in this
                                 study are provided at GEO under", a(href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE190712", "GSE190712", target="_blank"),
                                 ". Differential expression and differential network data can be downloaded from this resource.",
                                 "Data analysis scripts are available on our", a(href="https://github.molgen.mpg.de/mpip/DiffBrainNet", "github page", target="_blank"),
                                 ". Source code of this shiny app is also available via ",
                                 a(href="https://github.molgen.mpg.de/mpip/DiffBrainNet_ShinyApp", "github", target="_blank"), ". ",
                                 style="text-align:justify;color:black;background-color:#AED6F1;padding:15px;border-radius:10px"),
                               width = floor(0.45*12)
                             ),
                             column(
                               br(),
                               tags$img(src="mpilogo.jpeg", width="80%", height= "80%"),
                               br(),
                               br(),
                               p("Department of Translational Research in Psychiatry"),
                               p("Medical Genomics Group"),
                               p("For more information please visit the website of the", #em("the MPI of Psychiatry"),
                                 a(href="https://www.psych.mpg.de/", em("MPI of Psychiatry"), target="_blank")),
                               width=ceiling(0.15*12)+1
                             ))),
           
           navbarMenu("Diff. Expression Analysis",
           tabPanel("Single Brain Region",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("region", "Choose a brain region:", 
                                    choices = c("Amygdala" = "AMY", 
                                                "Cerebellum" = "CER", 
                                                "Dorsal DG of Hippocampus" = "dDG", 
                                                "Dorsal CA1 of Hippocampus" = "dCA1", 
                                                "Prefrontal Cortex" = "PFC", 
                                                "PVN of Hypothalamus" = "PVN", 
                                                "Ventral DG of Hippocampus" = "vDG", 
                                                "Ventral CA1 of Hippocampus" = "vCA1")),
                        downloadButton("download1","Download (filtered) table as csv"),
                        width = 3
                      ),
                      mainPanel(
                        fluidPage(
                          br(),
                          tags$style(".fa-chart-bar {color:#2980B9}"),
                          h3(p(em("Differential Gene Expression "),icon("chart-bar",lib = "font-awesome"),style="color:black;text-align:center")),
                          hr(),
                          span("Explore the transcriptomic response to Dexamethasone treatment of a brain region of your 
                          interest on a "), strong("differential expression level"), span(". You can select one of 8
                          different brain regions on the left panel. The volcano plot shows the log2-transformed fold change 
                          and the -log10-transformed FDR corrected p-value of the genes detected in our dataset. Please "), 
                          strong("click on a point/gene"), span(" in the volcano plot to see its 
                          normalized expression levels at the vehicle and treatment condition. You can filter and
                          download the table of the differentially expressed genes."),
                          br(),br(),
                          #  style="text-align:left;color:black"),
                          splitLayout(cellWidths = c("55%", "45%"),
                          plotlyOutput("volcano_plot"),
                          plotlyOutput("exp_plot")),
                          br(),br(),
                          span(em("Hint: By hovering over the column names you get a more detailed description of the columns.")),
                          br(),br(),
                          DT::dataTableOutput("de_table")
                        )
                      )
                    )
           ),
           tabPanel("Comparison Brain Regions",
                    # UI defined in upsetPlot module
                    upsetPlotUI("upsetDE")
           )
           ),
           
           navbarMenu(
             "Diff. Network Analysis",
             tabPanel(
               "Introduction diff. networks",
               fluidPage(
                 br(),
                 tags$style(".fa-project-diagram {color:#2980B9}"),
                 h3(p(
                   em("Introduction: Differential network analysis"),
                   icon("project-diagram", lib = "font-awesome"),
                   style = "color:black;text-align:center"
                 )),
                 hr(),
                 
                 tags$img(src="DiffNetworks.png",
                          width="60%", height= "60%", 
                          style="display: block; margin-left: auto; margin-right: auto;")
               )
                 
                 
               
             ),
             
             tabPanel(
               "Network overview",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     "overview_region",
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
                   radioButtons(
                     "overview_metric",
                     label = "Select network metric:",
                     choices = list("Nodedegree" = "nodedegrees",
                                    "Nodebetweenness" = "nodebetweenness"),
                     selected = "nodebetweenness"
                   ),
                   radioButtons(
                     "overview_network",
                     label = "Select network:",
                     choices = list("Vehicle" = "baseline",
                                    "Differential" = "differential",
                                    "Treatment" = "treatment"),
                     selected = "differential"
                   ),
                   width = 3
                 ),
                 mainPanel(
                   fluidPage(
                     br(),
                     tags$style(".fa-project-diagram {color:#2980B9}"),
                     h3(p(
                       em("Network analysis of gene expression: Overview "),
                       icon("project-diagram", lib = "font-awesome"),
                       style = "color:black;text-align:center"
                     )),
                     hr(),
                     h4(
                       p("Distribution nodedegrees/nodebetweenness", 
                         style = "color:black;text-align:center")
                     ),
                     plotlyOutput("histogram_network")
                   )
                 )
               )),
             
             tabPanel(
               "Comparison hub genes",
               comparisonHubGenesUI("compHubGenes")
             ),
             
             tabPanel(
               "Network visualization",
               # UI from networkSingle module
               networkSingleUI("singleVisualization")
             ),
             
             tabPanel(
               "Multi-region network visualization",
               # UI from networkMulti module
               networkMultiUI("multiVisualization")
             )
           ), 
           
           
           # navbarMenu("More",
           #            tabPanel("Data download",
           #                     # DT::dataTableOutput("table")
           #            ),
           #            tabPanel("Terms of Use",
           #                     
           #                     # fluidRow(
           #                     #   column(6,
           #                     #          includeMarkdown("about.md")
           #                     #   ),
           #                     #   column(3,
           #                     #          img(class="img-polaroid",
           #                     #              src=paste0("http://upload.wikimedia.org/",
           #                     #                         "wikipedia/commons/9/92/",
           #                     #                         "1919_Ford_Model_T_Highboy_Coupe.jpg")),
           #                     #          tags$small(
           #                     #            "Source: Photographed at the Bay State Antique ",
           #                     #            "Automobile Club's July 10, 2005 show at the ",
           #                     #            "Endicott Estate in Dedham, MA by ",
           #                     #            a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
           #                     #              "User:Sfoskett")
           #                     #          )
           #                     #   )
           #                     # )
           #            )
           # ),
           footer = tags$footer(
             p(tags$span(actionLink(inputId = "termsOfUse", label = "Terms of Use")),
             "   ",
             a(href="https://www.psych.mpg.de/2354/impress", "Imprint", target="_blank"), 
             "   ",
             "   © 2022 MPI of Psychiatry"),
             align="center")
)
)
