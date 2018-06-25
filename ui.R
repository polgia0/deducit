ui <- dashboardPage(
  dashboardHeader(title="Deducit",
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Author",
                   message =HTML(paste("This is a project made under:","GPL2","by: Gianmarco Polotti", sep="<br/>")),
                   time = "21/06/2018"
                 ),
                 messageItem(
                   from = "Questions",
                   message = HTML(paste("You find several help","buttons on the pages.","Click on them to get quick suggestions", sep="<br/>")),
                   icon = icon("question"),
                   time = "21/06/2018"
                 ),
                 messageItem(
                   from = "Disclaimer",
                   message =HTML(paste( "The Author has made every",
                                        "attempt to ensure the accuracy",
                                        "and reliability of the calculation.",
                                        "However, the results are provided",
                                        "'as is' withoutwarranty of any kind.",
                                        "The AUthor does not accept",
                                        "any reponsibility or liability",
                                        "for the accuracy,content,",
                                        "completeness, legality, or",
                                        "reliability ofthe calculation",
                                        "contained in this software."
                   ,sep="<br/>")),
                   icon = icon("life-ring"),
                   time = "21/06/2018"
                 )
    )
  ),
  dashboardSidebar(
    sidebarMenu(id="sidemenu",
      menuItem("Data Set", tabName = "dataset",icon = icon("th"),
            menuSubItem("Load", tabName = "loadf",icon = shiny::icon("file-text")),
            menuSubItem("Validate Variables", tabName = "valcol"),
            menuSubItem("Validate Objects", tabName = "valrow"),
            menuSubItem("Set Group", tabName = "grpcol"),
            menuSubItem("Training/Test", tabName = "valtest"),
            menuSubItem("View", tabName = "view"),
            menuSubItem("Summary", tabName = "summary")
      ),
      menuItem("EDA",tabName = "charts",icon = shiny::icon("line-chart"),
            menuSubItem("Trend", tabName = "trend"),
            menuSubItem("Box", tabName = "box"),
            menuSubItem("2D Plot", tabName = "2dplot"),
            menuSubItem("3D Plot", tabName = "3dplot"),
            menuSubItem("Contour", tabName = "3dcont")
      ),
      menuItem("PCA", tabName = 'pca', icon = shiny::icon("braille"),
            menuSubItem("Settings", tabName ="pca_comp" ),
            menuSubItem("Variance", tabName ="pca_var" ),
            menuSubItem("Scores", tabName ="pca_scores" ),
            menuSubItem("Loadings", tabName = "pca_load"),
            menuSubItem("Biplot", tabName = "pca_biplt"),
            menuSubItem("T^2 Hotelling", tabName = "pca_ht2"),
            menuSubItem("Q-SPE", tabName ="pca_Q" ),
            menuSubItem("Column Residuals", tabName = "pca_cres"),
            menuSubItem("SPE Contribution", tabName = "pca_spec"),
            menuSubItem("Score Contrib. Obj to Avg", tabName ="pca_scoavg" ),
            menuSubItem("Score Contrib. Obj to Obj", tabName ="pca_scoo" ),
            menuSubItem("Additional Data", tabName = "pca_add")
      ),
      menuItem("PLS", tabName = "pls", icon = shiny::icon("braille"),
            menuSubItem("Settings", tabName ="pls_comp" ),
            menuSubItem("Fitted Values", tabName ="pls_fit" ),
            menuSubItem("Coefficients", tabName ="pls_coeff" ),
            menuSubItem("W*i Space", tabName = "pls_wi"),
            menuSubItem("Qi Space", tabName = "pls_qi"),
            menuSubItem("W*Qi Space", tabName ="pls_wq" ),
            menuSubItem("T^2 Hotelling", tabName = "pls_ht2"),
            menuSubItem("Scores", tabName = "pls_sco"),
            menuSubItem("SPE X", tabName ="pls_spex" ),
            menuSubItem("SPE Y", tabName ="pls_spey" ),
            menuSubItem("T U Space", tabName ="pls_TU" ),
            menuSubItem("VIP", tabName ="pls_VIP" ),
            menuSubItem("Additional Data", tabName = "pls_add")
      ),
      menuItem("Download",tabName = "DownloadData",icon = shiny::icon("download")),
      menuItem("Help", tabName = "Help", icon = shiny::icon("info")),
      hr(),
      menuItem("Manual",icon = shiny::icon("file-pdf-o"),href = "http://openresearchsoftware.metajnl.com"),
      menuItem("Source code",icon = icon("file-code-o"),href = "https://github.com/foocheung/adat"),
      menuItem("Bug Reports", icon = icon("bug"),href = "https://github.com/foocheung/adat/issues"),
      menuItem("How To Guide", icon = icon("file-pdf-o"),href = "howto.pdf")
    ),
    HTML('<br><br><br><br>'),
    HTML('<p><center>Further Help ? <br>Contact the developer at <font color="cyan"><br> gianmarco.polotti @ gmail . com </font></center>')
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),  
    fluidRow(
      tabItems(
        tabItem(
          tabName = "loadf",
          # column(
          #   8,
          #   offset = 1,
          #   img(src = 'https://chi.nhlbi.nih.gov/web/sites/all/themes/irptheme/logo.png', align = "center")
          # ),
          column(10,offset = 1,h1("Web Tool For Data Processing", style = "font-family: 'Source Sans Pro';")),
          br(),
          br(),
          br(),
          column(12,offset = 1,
                 box(
                   fileInput("file1", "Choose CSV File",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                   actionButton("show", "Help", icon("question-circle"),style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   #tags$a(href = 'PLASMA.1.3k.HybNorm.MedNorm.Cal.20151030.adat',target = "_blank",class = "btn",icon("download"),'Download Test CSV file'),
                   tags$hr(),
                   checkboxInput("header", "Header", TRUE),
                   checkboxInput("rowindex", "Row Index", TRUE),
                   radioButtons("sep","Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ";"),
                   radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"'),
                   radioButtons("dec", "Decimal",choices = c(Comma = ",",Point = "."),selected = ",")
                   , background ="light-blue"),
                 br(),
                 column(12,offset = 0,fluidRow(column(6, verbatimTextOutput("value"))))
          )
        ),
        tabItem(tabName = "valcol",
                fluidPage(
                  titlePanel("Remove Variables(i.e. Columns)"),
                  br(),
                  br(),
                  fluidRow(column(12,offset = 1,uiOutput("subcol"))),                    
                  br(),
                  br(),
                  br(),
                  br(),
                  fluidRow(column(12,offset = 0,verbatimTextOutput("mysubcol")))
                )
        ),
        tabItem(tabName = "valrow",
                fluidPage(
                  titlePanel("Remove Objects (i.e. Rows)"),
                  br(),
                  br(),
                  fluidRow(column(12,offset = 1,uiOutput("subrow"))),                    
                  br(),
                  br(),
                  br(),
                  br(),
                  fluidRow(column(12,offset = 0,verbatimTextOutput("mysubrow")))
                )
        ),
        tabItem(tabName = "valtest",
                fluidPage(
                  titlePanel("Select Test Objects"),
                  br(),
                  br(),
                  fluidRow(column(12,offset = 1,uiOutput("subtest"))),                    
                  br(),
                  br(),
                  br(),
                  br(),
                  fluidRow(column(12,offset = 0,verbatimTextOutput("mysubtest")))
                )
        ),
        tabItem(tabName = "grpcol",
                fluidPage(
                  titlePanel("Select Column as Group"),
                  br(),
                  br(),
                  fluidRow(column(12,offset = 1,uiOutput("subgrp"))),                    
                  br(),
                  br(),
                  br(),
                  br(),
                  fluidRow(column(12,offset = 0,verbatimTextOutput("mysubgrp")))
                )
        ),
        tabItem(tabName = "view",
                fluidPage(
                  titlePanel("Viewing Data Set"),
                  br(),
                  fluidRow(column(12,offset = 1,uiOutput("viewvar"))),                    
                  hr(),
                  fluidRow(mainPanel(DT::dataTableOutput("viewDS"),width=12)),
                  hr(),
                  fluidRow(mainPanel(DT::dataTableOutput("viewDStest"),width=12))
                )
        ),
        tabItem(tabName = "summary",
                fluidPage(
                  fluidRow(mainPanel(DT::dataTableOutput("viewsum"),width=12))
                )
        ),
        tabItem("trend",
                fluidPage(
                  titlePanel("Trends"),
                  sidebarPanel(tags$style(".well {background-color:#368BC1;}"),uiOutput("trendvar"),uiOutput("trendgroup"),uiOutput("trendcolor")),
                  sidebarPanel(uiOutput("trendfilter1"),uiOutput("trend_ui1")),
                  sidebarPanel(uiOutput("trendfilter2"),uiOutput("trend_ui2")),
                  fluidRow(mainPanel(plotOutput("trends",height=500,click=clickOpts(id="trend_click"),brush=brushOpts(id="trend_brush",fill="#ccc",direction = "x")),width=12)),
                  column(6,offset = 1,fluidRow(actionButton("trenddelb", "Delete Label",width ='100px'),downloadButton("trendexct", "Extract",width ='100px')))
                )
        ),
        tabItem("2dplot",
                fluidPage(
                  titlePanel("2D Plot"),
                  sidebarPanel(uiOutput("d2varx"),
                               uiOutput("d2vary"),
                               checkboxInput("d2values",label=tags$b("Value Labels"), value = FALSE, width = NULL)
                  ),
                  fluidRow(mainPanel(plotOutput("d2plot", width ="1200px", height = "800px"),width=12))
                )
        ),
        tabItem("3dplot",
                fluidPage(
                  titlePanel("3D Plot"),
                  sidebarPanel(tags$style(".well {background-color:#368BC1;}"),uiOutput("d3varx"),uiOutput("d3vary"),uiOutput("d3varz")),
                  sidebarPanel(tags$style(".well {background-color:#368BC1;}"),
                               checkboxInput("d3surf",label=tags$b("Surface"), value = FALSE, width = NULL),
                               checkboxInput("d3values",label=tags$b("Thick Values"), value = FALSE, width = NULL)
                  ),
                  fluidRow(mainPanel(mainPanel(rglwidgetOutput("d3plt",width="1200px",height="800px"),width=12)))
                )
        ),
        tabItem("3dcont",
                fluidPage(
                  titlePanel("3D Contour Plot"),
                  sidebarPanel(tags$style(".well {background-color:#368BC1;}"),uiOutput("d3contx"),uiOutput("d3conty"),uiOutput("d3contz")),
                  sidebarPanel(tags$style(".well {background-color:#368BC1;}"),
                               sliderInput("nlev",label="# levels",min=10,max=300,value=10)
                  ),
                  fluidRow(mainPanel(plotOutput("d3contour", width ="1200px", height = "800px"),width=12))
                )
        ),
        tabItem("box",
                fluidPage(
                  titlePanel("Box Plot"),
                  br(),
                  fluidRow(column(12,offset = 1,uiOutput("selvar"))),                    
                  hr(),
                  fluidRow(mainPanel(plotOutput("boxplot", width ="600px", height = "500px"),width=12))
                )
        ),
        tabItem(tabName ="pca_comp",
                fluidPage(
                        fluidRow(
                          box(column(10,offset = 1,uiOutput("pcasliderncp")),width=6),
                          box(
                              column(10,offset = 1,checkboxGroupInput("ncpcheckGroup", label =tags$b("Preprocessing"), choices = list("Center" = 1, "Scale" = 2),selected = c(1,2))),
                          background ="light-blue",width=6)
                          )
                )
        ),
        tabItem(tabName ="pca_var",
                fluidPage(
                        box(fluidRow(column(6,offset = 1,checkboxInput("labvar",label=tags$b("Value Labels"), value = FALSE, width = NULL))),background ="light-blue"),
                        fluidRow(mainPanel(plotOutput("pcavarsing"),width=12)),
                        br(),
                        fluidRow(mainPanel(plotOutput("pcavarcum"),width=12)),
                        br(),
                        downloadButton("pcavardwnl", "Download")
                )
        ),
        tabItem(tabName ="pca_scores",
                fluidPage(
                       fluidRow(
                            column(6,offset = 1,
                                   box(
                                      checkboxInput("labsco",label=tags$b("Value Labels"),value=TRUE,width=NULL),
                                      checkboxInput("isosco",label=tags$b("Isometric(x,y same scale)"), value = TRUE, width = NULL),
                                      checkboxInput("ellsco",label=tags$b("Ellipse"), value =TRUE, width = NULL),
                                   background ="light-blue",width=10),
                                   sliderInput("alpsco",label="% Alpha",min=1,max=99,value=5)
                            ),
                            box(
                                column(10,offset = 1,
                                      uiOutput("compxsco"),
                                      uiOutput("compysco"),
                                      verbatimTextOutput("pcascores_clickinfo"),
                                      actionButton("pcascoregrp", "New Group",width ='100px'),actionButton("pcascorerem", "Remove",width ='100px')
                                ),
                            background ="light-blue",width=5)
                       ),
                       fluidRow(plotOutput("pcascores",height=600,click=clickOpts(id="pcascores_click"),brush=brushOpts(id="pcascores_brush"))),
                       br(),
                       downloadButton("pcascoredwnl", "Download",width ='100px')
                )
        ),
        tabItem(tabName ="pca_load",
                fluidPage(
                       fluidRow(
                            box(
                                column(10,offset = 1,
                                                checkboxInput("lablod",label=tags$b("Value Labels"), value = TRUE, width = NULL),
                                                checkboxInput("arrlod",label=tags$b("Arrows"), value = TRUE, width = NULL),
                                                checkboxInput("isolod",label=tags$b("Isometric(x,y same scale)"), value = TRUE, width = NULL)
                                ),
                            background ="light-blue",width=6),
                            box(
                                column(10,offset = 1,uiOutput("compxlo"),uiOutput("compylo")),
                            background ="light-blue",width=6)
                       ),
                       fluidRow(plotOutput("pcaloadings",height=600)),
                       br(),
                       downloadButton("pcaloddwnl", "Download")
              )
        ),
        tabItem(tabName ="pca_biplt",
                fluidPage(
                    fluidRow(
                          column(6,offset = 1,
                              box(
                                  checkboxInput("labbi",label=tags$b("Value Labels"), value = TRUE, width = NULL),
                                  checkboxInput("ellbi",label=tags$b("Ellipse"),value=TRUE,width=NULL),
                                  checkboxInput("isobi",label=tags$b("Isometric(x,y same scale)"),value=TRUE,width=NULL),
                                  background ="light-blue",width=10),
                              sliderInput("alpbi",label=tags$b("% Alpha"),min=1,max =99,value=5)
                          ),
                          box(
                              column(10,offset = 1,uiOutput("compxbi"),uiOutput("compybi")),
                          background ="light-blue",width=5)
                    ),
                    fluidRow(plotOutput("pcabiplt",height=600)),
                    br(),
                    downloadButton("pcabidwnl", "Download")
                )
        ),
        tabItem(tabName ="pca_ht2",
                fluidPage(
                      fluidRow(
                          box(
                              column(10,offset = 1,
                                            checkboxInput("labht",label=tags$b("Value Labels"), value = FALSE, width = NULL),
                                            checkboxInput("linht",label=tags$b("Limit"), value = TRUE, width = NULL)
                              ),
                          background ="light-blue",width=6),
                          box(sliderInput("alpht", label =tags$b("% Alpha"), min = 1, max =99, value = 5),width=6)
                      ),
                      fluidRow(plotOutput("pcaht2",height=600)),
                      br(),
                      downloadButton("pcaht2dwnl", "Download")
                )
        ),
        tabItem(tabName ="pca_Q",
                fluidPage(
                      fluidRow(
                          box(
                              column(10,offset = 1,
                                          checkboxInput("labQ",label=tags$b("Value Labels"), value = FALSE, width = NULL),
                                          checkboxInput("linQ",label=tags$b("Limit"), value = TRUE, width = NULL)
                              ),
                          background ="light-blue",width=6),
                          box(sliderInput("alpQ", label =tags$b("% Alpha"),min=1,max=99,value=5),width=6)
                      ),
                      fluidRow(plotOutput("pcaQ",height=600)),
                      br(),
                      downloadButton("pcaQdwnl", "Download")
                )
        ),
        tabItem(tabName ="pca_cres",
                fluidPage(
                      fluidRow(
                          box(
                              column(10,offset=1,
                                         checkboxInput("labres",label=tags$b("Value Labels"), value = FALSE, width = NULL),
                                         checkboxInput("linres",label=tags$b("Limit"), value = TRUE, width = NULL)
                              ),
                          background ="light-blue",width=6),
                          box(sliderInput("alpres", label =tags$b("% Alpha"),min=1,max=99,value=5),width=6)
                      ),
                      fluidRow(plotOutput("pcacres",height=600)),
                      br(),
                      downloadButton("pcaresdwnl", "Download")
                )
        ),
        tabItem(tabName ="pca_spec",
                fluidPage(
                      fluidRow(
                          box(
                              column(10,offset=1,
                                        checkboxInput("labspec",label=tags$b("Value Labels"), value = FALSE, width = NULL),
                                        checkboxInput("limspec",label=tags$b("Fix Scale"), value = TRUE, width = NULL)
                              ),
                          background ="light-blue",width=6),
                          box(uiOutput("pcaspe"),width=6)
                      ),
                      fluidRow(plotOutput("pcaspec",height=600)),
                      br(),
                      downloadButton("pcaspecdwnl", "Download")
                )
        ),
        tabItem(tabName ="pca_scoo",
                fluidPage(
                      fluidRow(
                          box(
                              column(10,offset = 1,
                                      checkboxInput("labscoo",label=tags$b("Value Labels"), value = FALSE, width = NULL),
                                      uiOutput("compxscoo")
                              ),
                          background ="light-blue",width=6),
                          box(uiOutput("pcascoosl"),width=6)
                      ),
                      fluidRow(plotOutput("pcascoo",height=600))
                )
        ),
        tabItem(tabName ="pca_scoavg",
                fluidPage(
                      fluidRow(
                          box(
                              column(10,offset = 1,
                                    checkboxInput("labscoavg",label=tags$b("Value Labels"), value = FALSE, width = NULL),
                                    uiOutput("compxscoavg")
                              ),
                          background ="light-blue",width=6),
                          box(uiOutput("pcascoasl"),width=6)
                      ),
                      fluidRow(plotOutput("pcascoavg",height=600))
                )
        ),
        tabItem(tabName ="pca_add",
                fluidPage(
                      fluidRow(
                          column(6,offset = 1,
                              box(
                                  checkboxInput("labpcaadd",label=tags$b("Value Labels"), value = TRUE, width = NULL),
                                  checkboxInput("ellpcaadd",label=tags$b("Ellipse"), value = TRUE, width = NULL),
                              background ="light-blue",width=10),
                              sliderInput("alppcaadd",label=tags$b("% Alpha"),min=1,max=99,value=5)
                          ),
                          box(
                              column(10,offset = 1,uiOutput("compxpcaadd"),uiOutput("compypcaadd")),
                          background ="light-blue",width=5)
                      ),
                      fluidRow(plotOutput("pcaadd",height=600)),
                      br(),
                      downloadButton("pcaadddwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_comp",
                fluidPage(
                      titlePanel("PLS settings"),
                      fluidRow(
                        box(
                            column(10,offset = 1,uiOutput("plssliderncp")),
                        width=6),
                        box(
                            column(4,offset = 1,checkboxGroupInput("plscheckGroup", label = tags$b("Preprocessing"), choices = list("Center" = 1, "Scale" = 2),selected = c(1,2))),
                        background ="light-blue",width=6)
                      ),
                      br(),
                      box(
                          fluidRow(column(12,offset = 1,uiOutput("plssubcol"))), 
                      width=12),
                      br(),
                      br(),
                      fluidRow(column(12,offset = 0,verbatimTextOutput("plsmysubcol")))
                )
        ),
        tabItem(tabName ="pls_fit",
                fluidPage(
                      fluidRow(
                        box(
                            column(10,offset = 1,uiOutput("yplsfit")),
                        background ="light-blue",width=5)
                      ),
                      br(),
                      br(),
                      fluidRow(mainPanel(plotOutput("plsfit",height=600),width=12)),
                      br(),
                      downloadButton("plsfittdwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_coeff",
                fluidPage(
                  fluidRow(
                    box(
                        column(4,offset = 1,uiOutput("yplscoeff")),
                        column(4,offset = 1,checkboxInput("vplscoeff",label=tags$b("Value Labels"), value = TRUE, width = NULL)),
                    background ="light-blue",width=10)
                  ),
                  fluidRow(mainPanel(plotOutput("plscoeff",height=600),width=12)),
                  br(),
                  downloadButton("plscoeffdwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_wi",
                fluidPage(
                  fluidRow(
                    box(
                        column(4,offset = 1,uiOutput("xplswi")),
                        column(4,offset = 1,uiOutput("yplswi")),
                        column(4,offset = 1,checkboxInput("vplswi",label=tags$b("Value Labels"), value = TRUE, width = NULL)),
                    background ="light-blue",width=10)
                  ),
                  fluidRow(mainPanel(plotOutput("plswi",height=600),width=12)),
                  br(),
                  downloadButton("plswidwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_qi",
                fluidPage(
                  fluidRow(
                    box(
                        column(4,offset = 1,uiOutput("xplsqi")),
                        column(4,offset = 1,uiOutput("yplsqi")),
                        column(4,offset = 1,checkboxInput("vplsqi",label=tags$b("Value Labels"), value = TRUE, width = NULL)),
                    background ="light-blue",width=10)
                  ),
                  fluidRow(mainPanel(plotOutput("plsqi",height=600),width=12)),
                  br(),
                  downloadButton("plsqidwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_wq",
                fluidPage(
                  fluidRow(
                    box(
                        column(4,offset = 1,uiOutput("xplswq")),
                        column(4,offset = 1,uiOutput("yplswq")),
                        column(4,offset = 1,checkboxInput("vplswq",label=tags$b("Value Labels"), value = TRUE, width = NULL)),
                    background ="light-blue",width=10)    
                  ),
                  fluidRow(mainPanel(plotOutput("plswq",height=600),width=12)),
                  br(),
                  downloadButton("plswqdwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_ht2",
                fluidPage(
                  fluidRow(
                    box(
                        column(10,offset = 1,checkboxInput("vplsht2",label=tags$b("Value Labels"), value = FALSE, width = NULL)),
                    background ="light-blue",width=5)
                  ),
                  fluidRow(mainPanel(plotOutput("plsht2",height=600),width=12)),
                  br(),
                  downloadButton("plsht2dwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_sco",
                fluidPage(
                  fluidRow(
                    column(5,offset = 1,
                           box(
                                   column(10,offset = 1,checkboxInput("vplssco",label=tags$b("Value Labels"), value = TRUE, width = NULL)),
                                   column(10,offset = 1,checkboxInput("eplssco",label=tags$b("Ellipse"), value = TRUE, width = NULL)),
                           background ="light-blue",width=12),
                           sliderInput("aplssco", label =tags$b("% Alpha"), min = 1, max =99, value = 5)
                    ),
                    box(
                        column(10,offset = 1,uiOutput("xplssco")),
                        column(10,offset = 1,uiOutput("yplssco")), 
                    background ="light-blue",width=5)
                  ),
                  fluidRow(mainPanel(plotOutput("plssco",height=600),width=12)),
                  br(),
                  downloadButton("plsscodwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_spex",
                fluidPage(
                  fluidRow(
                    box(
                        column(4,offset = 1,checkboxInput("vplsspex",label=tags$b("Value Labels"), value = FALSE, width = NULL)),
                    background ="light-blue",width=5)
                  ),
                  fluidRow(mainPanel(plotOutput("plsspex",height=600),width=12)),
                  br(),
                  downloadButton("plsspexdwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_spey",
                fluidPage(
                  fluidRow(
                    box(
                        column(4,offset = 1,checkboxInput("vplsspey",label=tags$b("Value Labels"), value = FALSE, width = NULL)),
                    background ="light-blue",width=5)
                  ),
                  fluidRow(mainPanel(plotOutput("plsspey",height=600),width=12)),
                  br(),
                  downloadButton("plsspeydwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_TU",
                fluidPage(
                  fluidRow(
                    box(
                        column(4,offset = 1,uiOutput("xplstu")),
                        column(4,offset = 1,uiOutput("yplstu")),
                        column(4,offset = 1,checkboxInput("vplstu",label=tags$b("Value Labels"), value = TRUE, width = NULL)),
                    background ="light-blue",width=12)  
                  ),
                  fluidRow(mainPanel(plotOutput("plstu",height=600),width=12)),
                  br(),
                  downloadButton("plstudwnl", "Download")
                )
        ),
        tabItem(tabName ="pls_VIP",
                fluidPage(
                  fluidRow(
                    box(
                        column(4,offset = 1,checkboxInput("vplssvip",label=tags$b("Value Labels"), value = FALSE, width = NULL)),
                    background ="light-blue",width=5)     
                  ),
                  fluidRow(mainPanel(plotOutput("plsvip",height=600),width=12)),
                  br(),
                  downloadButton("plsvipdwnl", "Download")
                )
        ),
        tabItem(
                  tabName = "Help",
                  column(10,offset = 1,h1("Help")),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
br(),
                  column(8, offset = 1, htmlOutput("frame"))
        ),
        tabItem(tabName = "DownloadData",uiOutput('moreControls4'))
      )
    )
    
  )
)




