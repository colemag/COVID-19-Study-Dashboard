library(shiny)
library(writexl)
library(readxl)
library(shinydashboard)
library(timevis)
library(janitor)
library(tidyr)
library(dplyr)
library(plotly)
library(vistime)
library(DT)
library(ggpubr)
library(ggplot2)
library(pheatmap)


ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "UT Austin COVID-19", dropdownMenuOutput("messageMenu")),

  ## Burnt Orange Header
  dashboardSidebar(tags$head(tags$style(HTML('.logo {background-color: #BF5700 !important;}
                              .navbar {background-color: #BF5700 !important;}'))),
    sidebarMenu(
      menuItem("Main Dash and Upload", tabName = "maindash", icon = icon("dashboard")),
      menuItem("Patient Timeline", tabName = "timeline", icon = icon("calendar")),
      menuItem("Study Overviews", tabName = "studyoverview", icon = icon("chart-bar")),
      menuItem("Participant Heatmap", tabName = "participantheatmap"),
      menuItem("REDBar Barcoding App", tabName = "barcodes", icon = icon("barcode")),
      menuItem("REDBar Tube Report", tabName = "tubereport", icon = icon("vial")),
      menuItem("Important Documents", tabName = "importantdocs", icon = icon("file-alt"))
    )
  ),
  dashboardBody(tags$style(HTML("
.box.box-solid.box-secondary>.box-header {
  color:#fff;
  background:#666666
                    }

.box.box-solid.box-primary{
border-bottom-color:#666666;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#666666;
}                          ")),
    tabItems(
    tabItem(tabName = "maindash",
            box(
              title = "Welcome to the COVID-19 Dashboard!", status = "primary",  solidHeader = TRUE,
               "Please upload Janelle's patient file to the right (a report upload stright from REDCap is coming soon! But only if
            someone tells me they would use it.)", br(), br(), "This is still in early development and we are constantly
              looking for new features to add! Please email Cole Maguire at colemag@utexas.edu with any suggestions, comments, or concerns!"
            , br()),
            box(
              title = "File Upload Portal", status = "warning", solidHeader = TRUE,
              collapsible = TRUE,
              selectInput("filetypeinput", "What file type are you uploading?", choices = c("Excel", "csv")),
              checkboxInput(inputId = "ltf", label = "Remove Loss to Follow Up Patients?", value = T),
              fileInput("datafile", "Upload Janelle's file",
                        accept = c("text/csv","text/comma-separated-values,text/plain", ".csv", ".xlsx", ".xlx"))
            ), tableOutput("BabsonTable")

            ),
    tabItem(tabName = "timeline",
            fluidRow(
            h2("Interactive Timeline of Enrolled Participants"),
            box(width = 4, selectInput("cohorts_shown", "What cohorts do you want to look at?", choices = c("Longitudinal", "Convalescent", "Both"), selected = "Both")),
            box(width = 4, checkboxInput("removeFinished", "Remove participants who have completed week 4?", value = F)),
            box(width = 12, plotlyOutput("myVistime")),
            br(), br(),
            # actionButton("btn", "Center to today"),
            # timevisOutput("mytime"),
            box(title = "Janelle's Study Progress File", status = "primary", collapsible = TRUE, width = 12, div(style = 'overflow-x: scroll', tableOutput('timelinedata')))
            )),
    tabItem(tabName="studyoverview", h2("Study Overview and Enrollment"),
            valueBoxOutput("totalDrawsBox"),
            valueBoxOutput("totalNumParticipantsBox"),
            valueBoxOutput("ltfNumBox"),
            valueBoxOutput("pediatricBox") , valueBoxOutput("adultBox"),
            valueBoxOutput("geriatricBox"),
            box(title = "Pediatric by Severity", solidHeader = TRUE, width =4, collapsible = TRUE, status = "success", plotOutput("graph1")),
            box(title = "Adult by Severity", solidHeader = TRUE, width =4, collapsible = TRUE, status = "primary", plotOutput("graph2")),
            box(title = "Geriatric by Severity", solidHeader = TRUE, width =4, collapsible = TRUE, status = "warning", plotOutput("graph3")),
            box(title = "Pediatric by Race/Ethnicity", solidHeader = TRUE, width =4, collapsible = TRUE, status = "success", plotOutput("graph5")),
            box(title = "Adult by Race/Ethnicity", solidHeader = TRUE, width =4, collapsible = TRUE, status = "primary", plotOutput("graph6")),
            box( title = "Geriatric by Race/Ethnicity", solidHeader = TRUE,width =4, collapsible = TRUE, status = "warning", plotOutput("graph7")),
            box(title = "Pediatric by Sex", solidHeader = TRUE, width =4, collapsible = TRUE, status = "success", plotOutput("graph8")),
            box(title = "Adult by Sex", solidHeader = TRUE,width =4, collapsible = TRUE, status = "primary", plotOutput("graph9")),
            box(title = "Geriatric by Sex", solidHeader = TRUE,width =4, collapsible = TRUE, status = "warning", plotOutput("graph10")),
            valueBoxOutput("progressBox"),
            fluidRow(column(12, ),box(title = "Convalescent", solidHeader = TRUE, width =4, collapsible = TRUE, status = "info", plotOutput("graph4")), tableOutput("testing2"))
            # box(width =8, status = "warning"),
            ),
    tabItem(tabName="barcodes", h2("Barcode Generating App"),
            fluidRow(box(title = "Barcode Report Selection", status = "success", width = 3,
                         dateInput("dateBarCode", "What collection date would you like to export for?"),
                         numericInput("PBMCnum", "How many tubes of PBMCS?", value = 6),
                         numericInput("WholeBloodnum", "How many tubes of Whole Blood?", value = 1),
                         numericInput("Serumnum", "How many tubes of Serum for Us?", value = 6),
                         numericInput("SerumnumB", "How many tubes of Serum for Babson?", value = 1),
                         numericInput("Plasmanum", "How many tubes of Plasma?", value = 6),
                         selectInput("filetype", "What file type do you want to download the output as?", choices = c("Excel", "csv")),
                         downloadButton("downloadData", "Download")),
       box(title = "Barcode Report", status = "primary", width = 9, div(style = 'overflow-x: scroll', tableOutput('tableout')))
            )),
    tabItem(tabName = "tubereport", h2("Generate Tube Reports for REDCap"),
            fluidRow(
               box(title = "Tube Report Selection", status = "success", width = 3,
                 dateInput("dateTube", "What collection date would you like to export for?"),
                 uiOutput("TubeParticipantCheckbox"),
                 verbatimTextOutput("testing"),
                 checkboxInput("PBMCnumtube", "PBMCs for these patients?", value = T),
                 uiOutput("PBMCsSubject"),
                     numericInput("WholeBloodnumtube", "How many tubes of Whole Blood?", value = 1),
                     numericInput("Serumnumtube", "How many tubes of Serum for Us?", value = 6),
                     numericInput("SerumnumBtube", "How many tubes of Serum for Babson?", value = 1),
                     numericInput("Plasmanumtube", "How many tubes of Plasma?", value = 6),
                     # numericInput("pbmc_num", "How many total PBMCs were recovered?", value = 4000000),
                    # numericInput("total_serum_tubes", "How many tiger top blood tubes were drawn", value = 1),
                    # numericInput("total_pbmc_tubes", "How many green (heparin) blood tubes were drawn?", value = 4),
                    # numericInput("total_plasma_tubes", "How many lavender (EDTA) blood tubes were drawn?", value = 1),
                    # textInput("processername", "Who processed the tubes?"),
                    textInput("process_time", "When were the tubes processed? (please use miltary 24:00 time in CST)"),
                     selectInput("filetypetube", "What file type do you want to download the output as?", choices = c("Excel", "csv"), selected = "csv"),
                     downloadButton("downloadDatatube", "Download")
              ),
              box(title = "Tube Report", status = "primary", width = 9, div(style = 'overflow-x: scroll', tableOutput('tableoutredcap')))
            )
            ),
    tabItem(tabName = "importantdocs", h2("Links to Important Study Documents"),
            tags$a(href="https://austin.maps.arcgis.com/apps/opsdashboard/index.html#/39e4f8d4acb0433baae6d15a931fa984",
                   "Travis County COVID-19 Dashboard"), br(),
            tags$a(href="https://www.ncbi.nlm.nih.gov/research/coronavirus/",
                   "NIH COVID-19 Literature Database")),
    tabItem(tabName = "participantheatmap", h2("Participant Heatmap"),
            checkboxInput("heatmapconval", "Remove convalescent participants?", value = T),
            plotOutput("patientheatmap"))
    )
))



##------------------------------------------------------------ Server


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  library(shiny)
  require(DT)
  output$filedata <- DT::renderDataTable({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(input$filetypeinput == "Excel"){
      middle <- read_excel(infile$datapath)
    } else if (input$filetypeinput == "csv"){
      middle <- read.csv(infile$datapath)
    }

    datatable(middle, options = list(paging=FALSE))
  })

##---------------------------- Timeline data prep

  datasetOuttimeline <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(input$filetypeinput == "Excel"){
      middle <- read_excel(infile$datapath)
    } else if (input$filetypeinput == "csv"){
      middle <- read.csv(infile$datapath)
    }
    middle <- middle[,sapply(middle, function(x) { sum(!is.na(x)) > 0 })]
    if(input$ltf == T){
      ltf <- middle[middle$LTF == "Y", colnames(middle) == "Subject ID"]
      ltf<-ltf[!is.na(ltf)]
      middle <- middle[ !(middle$`Subject ID` %in% ltf ),]
    }

   as.data.frame(middle)
  })

  output$timelinedata <- renderTable({
   inputdata <- datasetOuttimeline()
   inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
   inputdata
 })

  timelinedata <- reactive({
   inputdata <- datasetOuttimeline()
   inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
    if(input$removeFinished == T){
      inputdata <- inputdata[inputdata$`Subject ID` %in% names(which(table(inputdata$`Subject ID`) < 3)), ]
    }
   inputdata
 })

  futuretimedata <- reactive({
   timelinedata <- timelinedata()
   subjectidtimeline <- timelinedata[!duplicated(timelinedata$`Subject ID`), ]
   subjectidtimeline <- subjectidtimeline[subjectidtimeline$Cohort == "Longitudinal", ]

   subjectidtimeline$week1start <- as.character(as.Date(subjectidtimeline$`Date Schedule`) - 6)
   subjectidtimeline$week1end <- as.character(as.Date(subjectidtimeline$`Date Schedule`))
   subjectidtimeline$week2start <- as.character(as.Date(subjectidtimeline$`Date Schedule`) + 1)
   subjectidtimeline$week2end <- as.character(as.Date(subjectidtimeline$`Date Schedule`) + 7)
   subjectidtimeline$week4start <- as.character(as.Date(subjectidtimeline$`Date Schedule`) + 14)
   subjectidtimeline$week4end <- as.character(as.Date(subjectidtimeline$`Date Schedule`) + 21)
   store <- subjectidtimeline[, colnames(subjectidtimeline) == "Subject ID" | colnames(subjectidtimeline) == "week1start" | colnames(subjectidtimeline) == "week1end"|colnames(subjectidtimeline) == "week2start" | colnames(subjectidtimeline) == "week2end" | colnames(subjectidtimeline) == "week4start"| colnames(subjectidtimeline) == "week4end" | colnames(subjectidtimeline) == "Site"]
   store0 <- store[, c(1,2,3,4)]
   colnames(store0) <- c("Subject ID", "Site", "start", "end")
   #store1$`Subject ID` <- paste0(store1$`Subject ID`, ", week 2")
   store0$event <-"week 1"
   store0$color <-"#6fe4e8"
   store1 <- store[, c(1,2,5,6)]
   colnames(store1) <- c("Subject ID", "Site", "start", "end")
   #store1$`Subject ID` <- paste0(store1$`Subject ID`, ", week 2")
   store1$event <-"week 2"
   store1$color <-"blue"
   store2 <- store[, c(1,2,7,8)]
   colnames(store2) <- c("Subject ID", "Site", "start", "end")
   #store2$`Subject ID` <- paste0(store2$`Subject ID`, ", week 4")
   store2$event <-"week 4"
   store2$color <-"orange"
   as.data.frame(rbind(store0, store1, store2))
 })


##------------------- timevis dashboard graphic ------------------------


  output$mytime <- renderTimevis({
    timelinedata <- timelinedata()
    futuretimedata <- futuretimedata()
    timevis(data.frame(
                       content = c(timelinedata$`Subject ID`, futuretimedata$`Subject ID`),
                       start = c(timelinedata$`Date Scheduled`, futuretimedata$start),
                       end = c(rep_len(NA, length(timelinedata$`Subject ID`)), futuretimedata$end))
                      # group = timelinedata$`Subject ID`,
                       #groups = data.frame(id = unique(timelinedata$`Subject ID`), content = unique(timelinedata$`Subject ID`))
    )
    })

  observeEvent(input$btn, {
    centerTime(id = "mytime", time= Sys.Date())
  })

  output$myVistime <- renderPlotly({
    if (input$cohorts_shown == "Both"){
      timelinedata <- timelinedata()
      futuretimedata <- futuretimedata()
      data <- data.frame(
        event = c(timelinedata$`Subject ID`, futuretimedata$`Subject ID`),
        group = c(timelinedata$`Subject ID`, futuretimedata$`Subject ID`),
        start = c(timelinedata$`Date Scheduled`, futuretimedata$start),
        end = c(rep(timelinedata$`Date Scheduled`, 1), futuretimedata$end),
        color = c(rep_len("green", length(timelinedata$`Subject ID`)), futuretimedata$color))

      vistime(data, show_labels = F, optimize_y = T)
    } else if (input$cohorts_shown == "Longitudinal"){
      timelinedata <- timelinedata()
      timelinedata <- timelinedata[timelinedata$Cohort == "Longitudinal", ]
      futuretimedata <- futuretimedata()
      data <- data.frame(
        event = c(timelinedata$`Subject ID`, futuretimedata$`Subject ID`),
        group = c(paste0(timelinedata$`Subject ID`, ", ", timelinedata$Site), paste0(futuretimedata$`Subject ID`, ", ",futuretimedata$Site)),
        start = c(timelinedata$`Date Scheduled`, futuretimedata$start),
        end = c(rep(timelinedata$`Date Scheduled`, 1), futuretimedata$end),
        color = c(rep_len("green", length(timelinedata$`Subject ID`)), futuretimedata$color))
      vistime(data, show_labels = F, optimize_y = T)
    } else if (input$cohorts_shown == "Convalescent") {
      timelinedata <- timelinedata()
      timelinedata <- timelinedata[timelinedata$Cohort == "Convalescent", ]
      data <- data.frame(
        event = c(timelinedata$`Subject ID`),
        group = c(timelinedata$`Subject ID`),
        start = c(timelinedata$`Date Scheduled`),
        end = c(rep(timelinedata$`Date Scheduled`, 1)),
        color = c(rep_len("green", length(timelinedata$`Subject ID`))))
      vistime(data, show_labels = F, optimize_y = T)
    }

  })
##----------------------------- Heatmaps
  output$patientheatmap <- renderPlot({
    timelinedata <- timelinedata()
    if(input$heatmapconval == T){
      timelinedata <- timelinedata[timelinedata$Cohort != "Convalescent", ]
    }
    timelinedata <- timelinedata[, colnames(timelinedata) == "Subject ID" | colnames(timelinedata) == "Visit #" | colnames(timelinedata) == "Subject Sex" | colnames(timelinedata) == "Subject Age" | colnames(timelinedata) == "Severity"]
    colnames(timelinedata) <- gsub("Subject ID", "Subject_ID", colnames(timelinedata))
    colnames(timelinedata) <- gsub("Visit #", "Visit", colnames(timelinedata))
    timelinedata$Visit <- as.character(timelinedata$Visit)
    timelinedata$occured <- 1
    datawide <- timelinedata %>%
      pivot_wider(names_from = Visit, values_from = occured)
    datawide <- as.data.frame(datawide)
    datawide[is.na(datawide)] <- 0
    rownames(datawide) <- datawide$Subject_ID
    datawide <- datawide[,c(-1)]
    datameta <- datawide[,1:3]
    datawide <- datawide[,c(-1,-2,-3)]
    plot1 <- pheatmap(datawide, cluster_rows = F, cluster_cols = F, show_rownames = T, annotation_row = datameta)
    plot1
  })


##---------------------------------- Barcodes
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$filetype == "Excel"){
        paste("BarcodeFile.xlsx", sep = "")
      } else if (input$filetype == "csv"){
        paste("BarcodeFile.csv", sep = "")
      }

    },
    content = function(file) {
      if(input$filetype == "Excel"){
        write_xlsx(datasetOut(), path = file)
      } else if (input$filetype == "csv"){
        write.csv(datasetOut(), file, row.names = FALSE)
      }
    })

  datasetOut <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(input$filetypeinput == "Excel"){
      middle <- read_excel(infile$datapath)
    } else if (input$filetypeinput == "csv"){
      middle <- read.csv(infile$datapath)
    }
    middle <- middle[,sapply(middle, function(x) { sum(!is.na(x)) > 0 })]
    middle$`Date Scheduled` <- as.character(middle$`Date Scheduled`)
    colnames(middle) <- gsub("Subject ID", "Subject.ID", colnames(middle))
    colnames(middle) <- gsub("Visit #", "Visit..", colnames(middle))
    colnames(middle) <- gsub("Subject Sex", "Subject.Sex", colnames(middle))
    colnames(middle) <- gsub("Subject Age", "Subject.Age", colnames(middle))
    middle <- middle[ middle$`Date Scheduled` == input$dateBarCode, ]
    if(input$PBMCnum > 0){
      PBMCS <- do.call("rbind", replicate(input$PBMCnum , middle, simplify = FALSE))
      PBMCS$sampletype <- rep.int("PBMCs", times = nrow(PBMCS))
      PBMCS <- PBMCS[order(PBMCS$Subject.ID), ]
      PBMCS$TubeNum <- rep.int(1:input$PBMCnum, times = nrow(middle))
      active <- PBMCS
    }

    if(input$WholeBloodnum > 0){
      WB <- do.call("rbind", replicate(input$WholeBloodnum , middle, simplify = FALSE))
      WB$sampletype <- rep.int("Whole Blood", times = nrow(WB))
      WB <- WB[order(WB$Subject.ID), ]
      WB$TubeNum <- rep.int(1:input$WholeBloodnum, times = nrow(middle))
      if(input$PBMCnum < 1 ){
        active <- WB
      } else{
        active <- rbind(active, WB)
      }
    }

    if(input$Plasmanum > 0){
      Plasma <- do.call("rbind", replicate(input$Plasmanum , middle, simplify = FALSE))
      Plasma$sampletype <- rep.int("Plasma", times = nrow(Plasma))
      Plasma <- Plasma[order(Plasma$Subject.ID), ]
      Plasma$TubeNum <- rep.int(1:input$Plasmanum, times = nrow(middle))
      if(input$PBMCnum < 1 & input$WholeBloodnum < 1){
        active <- Plasma
      } else {
        active <- rbind(active, Plasma)
      }
    }

    if(input$Serumnum > 0){
      Serum <- do.call("rbind", replicate(input$Serumnum , middle, simplify = FALSE))
      Serum$sampletype <- rep.int("Serum", times = nrow(Serum))
      Serum <- Serum[order(Serum$Subject.ID), ]
      Serum$TubeNum <- rep.int(1:input$Serumnum, times = nrow(middle))
      if(input$PBMCnum < 1 & input$WholeBloodnum < 1 & input$Plasmanum < 1){
        active <- Serum
      } else {
        active <- rbind(active, Serum)
      }

    }

    if(input$SerumnumB > 0){
      SerumB <- do.call("rbind", replicate(input$SerumnumB , middle, simplify = FALSE))
      SerumB$sampletype <- rep.int("Serum", times = nrow(SerumB))
      SerumB <- SerumB[order(SerumB$Subject.ID), ]
      SerumB$TubeNum <- rep.int("B", times = nrow(middle))
      if(input$PBMCnum < 1 & input$WholeBloodnum < 1 & input$Plasmanum < 1 & input$Serumnum < 1){
        active <- SerumB
      } else {
        active <- rbind(active, SerumB)
      }
    }

    # if(input$WholeBloodnum > 0){
    #   out <- as.data.frame(rbind(WB, SerumB, Serum, Plasma, PBMCS))
    # } else {
    #   out <- as.data.frame(rbind(SerumB, Serum, Plasma, PBMCS))
    # }
    out <- as.data.frame(active)

    out$sampletypenum <- out$sampletype
    out$sampletypenum <- gsub("PBMCs", 3, out$sampletypenum)
    out$sampletypenum <- gsub("Plasma", 2, out$sampletypenum)
    out$sampletypenum <- gsub("Serum-Babson", "B", out$sampletypenum)
    out$sampletypenum <- gsub("Serum", 1, out$sampletypenum)
    out$sampletypenum <- gsub("Whole Blood", 0, out$sampletypenum)

    out$sitenum <- out$Site
    out$sitenum <- gsub("DSMC", 1, out$sitenum)
    out$sitenum <- gsub("DCMC", 2, out$sitenum)
    out$sitenum <- gsub("UTHA", 3, out$sitenum)
    out$sitenum <- gsub("[^123]+", 4, out$sitenum)
    #out$sitenum <- gsub("[4]+", 4, out$sitenum)

    Subject.Sex.shorthand <- out$Subject.Sex
    Subject.Sex.shorthand <-gsub("emale.*","",Subject.Sex.shorthand)
    Subject.Sex.shorthand <-gsub("ale.*","",Subject.Sex.shorthand)


    out$SexLabel <- paste0("Sex: ", Subject.Sex.shorthand)
    out$TubeLabel <- paste0("Tube #: ", out$TubeNum)
    out$AgeLabel <- paste0("Age: ", out$Subject.Age)
    out$VisitLabel <- paste0("Visit #: ", out$Visit..)
    out$ID <- paste(out$Subject.ID, out$Visit.., out$sitenum, out$sampletypenum, out$TubeNum, sep = "-")

    out[order(out$Subject.ID, out$Visit.., out$sitenum, out$sampletypenum, as.numeric(out$TubeNum)),]

  })

  output$tableout <- renderTable({
    datasetOut()
  })

  ##-------------------------------- Tube
  datasetOuttube <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(input$filetypeinput == "Excel"){
      middle <- read_excel(infile$datapath)
    } else if (input$filetypeinput == "csv"){
      middle <- read.csv(infile$datapath)
    }
    middle <- middle[,sapply(middle, function(x) { sum(!is.na(x)) > 0 })]
    middle$`Date Scheduled` <- as.character(middle$`Date Scheduled`)
    colnames(middle) <- gsub("Subject ID", "Subject.ID", colnames(middle))
    colnames(middle) <- gsub("Visit #", "Visit..", colnames(middle))
    colnames(middle) <- gsub("Subject Sex", "Subject.Sex", colnames(middle))
    colnames(middle) <- gsub("Subject Age", "Subject.Age", colnames(middle))
    middle <- middle[ middle$`Date Scheduled` == input$dateTube, ]
    middle <- middle[middle$Subject.ID %in% input$TubeParticipantCheckbox,]

    if(input$PBMCnumtube == T){
      numberoftubes <- rep(NA, 1)
      participantnum <- unique(middle$Subject.ID)
    for(i in 1:length(unique(middle$Subject.ID))){
      numberoftubes[i] <- input[[(paste0('PBMCsSubject', i))]]
      middleactive <- middle[as.character(middle$Subject.ID) == as.character(participantnum[i]), ]
      PBMCStemp <- do.call("rbind", replicate(numberoftubes[i], middleactive, simplify = FALSE))
      PBMCStemp$sampletype <- rep.int("PBMCs", times = nrow(PBMCStemp))
      PBMCStemp$TubeNum <- rep.int(1:nrow(PBMCStemp), times = 1)
      if (i ==1){
        PBMCS <- PBMCStemp
      } else if (i > 1){
        PBMCS <- rbind(PBMCS, PBMCStemp)
      }
    }
    PBMCS <- PBMCS[order(PBMCS$Subject.ID), ]

    active <- PBMCS
    }

    # if(input$PBMCnum > 0){
    #   PBMCS <- do.call("rbind", replicate(input$PBMCnumtube , middle, simplify = FALSE))
    #   PBMCS$sampletype <- rep.int("PBMCs", times = nrow(PBMCS))
    #   PBMCS <- PBMCS[order(PBMCS$Subject.ID), ]
    #   PBMCS$TubeNum <- rep.int(1:input$PBMCnumtube, times = nrow(middle))
    #   active <- PBMCS
    # }

    if(input$WholeBloodnumtube > 0){
      WB <- do.call("rbind", replicate(input$WholeBloodnumtube , middle, simplify = FALSE))
      WB$sampletype <- rep.int("Whole Blood", times = nrow(WB))
      WB <- WB[order(WB$Subject.ID), ]
      WB$TubeNum <- rep.int(1:input$WholeBloodnumtube, times = nrow(middle))
      if(input$PBMCnumtube == F ){
        active <- WB
      } else{
        active <- rbind(active, WB)
      }
    }

    if(input$Plasmanumtube > 0){
      Plasma <- do.call("rbind", replicate(input$Plasmanumtube , middle, simplify = FALSE))
      Plasma$sampletype <- rep.int("Plasma", times = nrow(Plasma))
      Plasma <- Plasma[order(Plasma$Subject.ID), ]
      Plasma$TubeNum <- rep.int(1:input$Plasmanumtube, times = nrow(middle))
      if(input$PBMCnumtube == F & input$WholeBloodnumtube < 1){
        active <- Plasma
      } else {
        active <- rbind(active, Plasma)
      }
    }

    if(input$Serumnumtube > 0){
      Serum <- do.call("rbind", replicate(input$Serumnumtube , middle, simplify = FALSE))
      Serum$sampletype <- rep.int("Serum", times = nrow(Serum))
      Serum <- Serum[order(Serum$Subject.ID), ]
      Serum$TubeNum <- rep.int(1:input$Serumnumtube, times = nrow(middle))
      if(input$PBMCnumtube ==F & input$WholeBloodnumtube < 1 & input$Plasmanumtube < 1){
        active <- Serum
      } else {
        active <- rbind(active, Serum)
      }

    }

    if(input$SerumnumBtube > 0){
      SerumB <- do.call("rbind", replicate(input$SerumnumBtube , middle, simplify = FALSE))
      SerumB$sampletype <- rep.int("Serum", times = nrow(SerumB))
      SerumB <- SerumB[order(SerumB$Subject.ID), ]
      SerumB$TubeNum <- rep.int("B", times = nrow(middle))
      if(input$PBMCnumtube == F & input$WholeBloodnumtube < 1 & input$Plasmanumtube < 1 & input$Serumnumtube < 1){
        active <- SerumB
      } else {
        active <- rbind(active, SerumB)
      }
    }

    # if(input$WholeBloodnumtube > 0){
    #   out <- as.data.frame(rbind(WB, SerumB, Serum, Plasma, PBMCS))
    # } else {
    #   out <- as.data.frame(rbind(SerumB, Serum, Plasma, PBMCS))
    # }
    out <- as.data.frame(active)

    out$sampletypenum <- out$sampletype
    out$sampletypenum <- gsub("PBMCs", 3, out$sampletypenum)
    out$sampletypenum <- gsub("Plasma", 2, out$sampletypenum)
    out$sampletypenum <- gsub("Serum-Babson", "B", out$sampletypenum)
    out$sampletypenum <- gsub("Serum", 1, out$sampletypenum)
    out$sampletypenum <- gsub("Whole Blood", 0, out$sampletypenum)

    out$sitenum <- out$Site
    out$sitenum <- gsub("DSMC", 1, out$sitenum)
    out$sitenum <- gsub("DCMC", 2, out$sitenum)
    out$sitenum <- gsub("UTHA", 3, out$sitenum)
    out$sitenum <- gsub("[^123]+", 4, out$sitenum)
    #out$sitenum <- gsub("[4]+", 4, out$sitenum)

    Subject.Sex.shorthand <- out$Subject.Sex
    Subject.Sex.shorthand <-gsub("emale.*","",Subject.Sex.shorthand)
    Subject.Sex.shorthand <-gsub("ale.*","",Subject.Sex.shorthand)


    out$SexLabel <- paste0("Sex: ", Subject.Sex.shorthand)
    out$TubeLabel <- paste0("Tube #: ", out$TubeNum)
    out$AgeLabel <- paste0("Age: ", out$Subject.Age)
    out$VisitLabel <- paste0("Visit #: ", out$Visit..)
    out$ID <- paste(out$Subject.ID, out$Visit.., out$sitenum, out$sampletypenum, out$TubeNum, sep = "-")

    out[order(out$Subject.ID, out$Visit.., out$sitenum, out$sampletypenum, as.numeric(out$TubeNum)),]

  })

  output$PBMCsSubject <- renderUI({
    out <- datasetOuttimeline()
    out <- out[ out$`Date Scheduled` == input$dateTube, ]
    if(input$PBMCnumtube == T){
      groups <- unique(out$`Subject ID`)
      groups1 <- groups[groups %in% input$TubeParticipantCheckbox]
      lapply(1:(length(unique(groups1))), function(i) {
        numericInput((paste0('PBMCsSubject', i)), label = (paste0('How many tubes of PBMCs for participant ',  groups1[[i]], "?")), value = 10)
      })
    }
  })

  output$testing <- renderText({
    out <- datasetOuttimeline()
    out <- out[ out$`Date Scheduled` == input$dateTube, ]
    groups <- unique(out$`Subject ID`)
    groups1 <- groups[groups %in% input$TubeParticipantCheckbox]
  })

  output$TubeParticipantCheckbox <- renderUI({
    out <- datasetOuttimeline()
    out <- out[ out$`Date Scheduled` == input$dateTube, ]

      groups <- unique(out$`Subject ID`)
      checkboxGroupInput(inputId = "TubeParticipantCheckbox", label = "Which participants should be printed", choices = groups, selected = groups)

  })

  redcap <- reactive({
    barcode <- datasetOuttube()
    redcapout <- as.data.frame(cbind(barcode$Subject.ID, barcode$Visit.., barcode$sampletypenum, barcode$TubeNum,
                                     barcode$ID))
    colnames(redcapout) <- c("subject_id", "redcap_event_name", "sample_type", "tube_number", "tube_id_2")
    redcapout$redcap_event_name <- gsub(1, "1_arm_1", redcapout$redcap_event_name)
    redcapout$redcap_event_name <- gsub(2, "2_arm_1", redcapout$redcap_event_name)
    redcapout$redcap_event_name <- gsub(3, "3_arm_1", redcapout$redcap_event_name)
    redcapout$redcap_event_name <- gsub(4, "4_arm_1", redcapout$redcap_event_name)
    redcapout$redcap_event_name <- gsub(5, "5_arm_1", redcapout$redcap_event_name)
    idtable <- as.data.frame(table(redcapout$subject_id))
    for(i in 1:length(unique(redcapout$subject_id))){
      if(i==1){
        instantout <- c(1:(idtable[i, 2]))
      } else {
        tempout <- c(1:(idtable[i, 2]))
        instantout <- c(instantout, tempout)
      }
    }
    redcapout$redcap_repeat_instance <- instantout
    redcapout$redcap_repeat_instrument <- "sample_info"
    redcapout$sample_box <- ""
    redcapout$sample_rack <- ""
    redcapout$sample_box_position <- ""
    redcapout$process_time_v2 <- input$process_time
    redcapout$cryo_vial_pbmc_num_v2 <- ""
    redcapout$pbmc_viability_v2 <- ""
    redcapout$coagulation_v2 <- ""
    redcapout$sample_vol <- ""
    redcapout$sample_notes_v2 <- ""
    var <- ncol(redcapout)
    # redcapout$pbmc_num <- NA
    # redcapout$total_plasma_tubes <- NA
    # redcapout$total_serum_tubes <- NA
    # redcapout$total_pbmc_tubes <- NA
    # redcapout$pbmc_process_name <- NA #c(rep(NA, tubes), input$processername)
    # redcapout$process_time <- NA
    # for(i in 1:unique(redcapout$subject_id)+1){
    #   redcapout <- rbind(redcapout, c((unique(redcapout$subject_id)[i]), rep(NA, var-1), input$pbmc_num, input$total_plasma_tubes, input$total_serum_tubes, input$total_pbmc_tubes,
    #                                   input$processername, input$process_time))
    # }


    redcapout

  })

  output$tableoutredcap <- renderTable({
    redcap()
  })

  output$downloadDatatube <- downloadHandler(
    filename = function() {
      if(input$filetypetube == "Excel"){
        paste("RedCapFile.xlsx", sep = "")
      } else if (input$filetypetube == "csv"){
        paste("RedCapFile.csv", sep = "")
      }

    },
    content = function(file) {
      if(input$filetypetube == "Excel"){
        write_xlsx(redcap(), path = file)
      } else if (input$filetypetube == "csv"){
        write.csv(redcap(), file, row.names = FALSE)
      }
    })

  ##---------------------------------- Study Completion
  convalescent <- reactive({
    timelinedata <- timelinedata()
    subjectidtimeline <- timelinedata[!duplicated(timelinedata$`Subject ID`), ]
    subjectidtimeline <- subjectidtimeline[subjectidtimeline$Cohort == "Convalescent", ]
  })

  output$graph1 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    #inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
    pediatric <- inputdata[inputdata$`Subject Age` < 18, ]
    tableped <- as.data.frame(table(pediatric$Severity))
    colnames(tableped) <- c("Severity", "Participants")
    tableped$Severity <- factor(tableped$Severity, levels = c("Asymptomatic", "Mild", "Moderate", "Severe", "Critical"))
    tableped$Severitygrouping <- tableped$Severity
    tableped$Severitygrouping <- gsub("Severe", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Critical", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("store", "Severe/Critical", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Mild", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Moderate", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("store", "Mild/Moderate", tableped$Severitygrouping)
    tableped$Severitygrouping <- factor(tableped$Severitygrouping, levels = c("Asymptomatic", "Mild/Moderate", "Severe/Critical"))
    ggo <- ggbarplot(tableped, x = "Severitygrouping", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      ylim(0, 3) + geom_hline(yintercept = 3, linetype="dashed", color = "red") +  xlab("Study Grouping") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) + theme(axis.text.x=element_blank())
    ggo
  })

  output$graph2 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    #inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
    pediatric <- inputdata[inputdata$`Subject Age` > 18 & inputdata$`Subject Age` < 65, ]
    tableped <- as.data.frame(table(pediatric$Severity))
    colnames(tableped) <- c("Severity", "Participants")
    tableped$Severity <- factor(tableped$Severity, levels = c("Asymptomatic", "Mild", "Moderate", "Severe", "Critical"))
    tableped$Severitygrouping <- tableped$Severity
    tableped$Severitygrouping <- gsub("Severe", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Critical", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("store", "Severe/Critical", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Mild", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Moderate", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("store", "Mild/Moderate", tableped$Severitygrouping)
    tableped$Severitygrouping <- factor(tableped$Severitygrouping, levels = c("Asymptomatic", "Mild/Moderate", "Severe/Critical"))

    ggo <- ggbarplot(tableped, x = "Severitygrouping", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      ylim(0, 10) + geom_hline(yintercept = 10, linetype="dashed", color = "red") + xlab("Study Grouping") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) + theme(axis.text.x=element_blank())
    ggo
  })

  # output$testing <- renderTable({
  #   inputdata <- datasetOuttimeline()
  #   inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
  #   inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
  #   pediatric <- inputdata[inputdata$`Subject Age` >= 65, ]
  #   pediatric
  # })


  output$testing2 <- renderTable({
    inputdata1 <- datasetOuttimeline()
    inputdata1 <- inputdata1[inputdata1$Cohort == "Longitudinal",]
    inputdata1 <- inputdata1[!duplicated(inputdata1$`Subject ID`), ]
     pediatric <- inputdata1[as.numeric(inputdata1$`Subject Age`) >= 65, ]
     pediatric
    # tableped <- as.data.frame(table(pediatric$Severity))
    # colnames(tableped) <- c("Severity", "Participants")
    # tableped$Severity <- factor(tableped$Severity, levels = c("Asymptomatic", "Mild", "Moderate", "Severe", "Critical"))
    # tableped$Severitygrouping <- tableped$Severity
    # tableped$Severitygrouping <- gsub("Severe", "store", tableped$Severitygrouping)
    # tableped$Severitygrouping <- gsub("Critical", "store", tableped$Severitygrouping)
    # tableped$Severitygrouping <- gsub("store", "Severe/Critical", tableped$Severitygrouping)
    # tableped$Severitygrouping <- gsub("Mild", "store", tableped$Severitygrouping)
    # tableped$Severitygrouping <- gsub("Moderate", "store", tableped$Severitygrouping)
    # tableped$Severitygrouping <- gsub("store", "Mild/Moderate", tableped$Severitygrouping)
    # tableped$Severitygrouping <- factor(tableped$Severitygrouping, levels = c("Asymptomatic", "Mild/Moderate", "Severe/Critical"))
    # tableped
  })
  output$graph3 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    #inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
    pediatric <- inputdata[inputdata$`Subject Age` >= 65, ]
    tableped <- as.data.frame(table(pediatric$Severity))
    colnames(tableped) <- c("Severity", "Participants")
    tableped$Severity <- factor(tableped$Severity, levels = c("Asymptomatic", "Mild", "Moderate", "Severe", "Critical"))
    tableped$Severitygrouping <- tableped$Severity
    tableped$Severitygrouping <- gsub("Severe", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Critical", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("store", "Severe/Critical", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Mild", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("Moderate", "store", tableped$Severitygrouping)
    tableped$Severitygrouping <- gsub("store", "Mild/Moderate", tableped$Severitygrouping)
    tableped$Severitygrouping <- factor(tableped$Severitygrouping, levels = c("Asymptomatic", "Mild/Moderate", "Severe/Critical"))

    ggo <- ggbarplot(tableped, x = "Severitygrouping", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      ylim(0, 5) + geom_hline(yintercept = 5, linetype="dashed", color = "red") + xlab("Study Grouping") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) + theme(axis.text.x=element_blank())
    ggo
  })

  output$graph4 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Convalescent",]
    tableped <- as.data.frame(table(inputdata$Severity))
    colnames(tableped) <- c("Severity", "Participants")
    tableped$Severity <- factor(tableped$Severity, levels = c("Asymptomatic", "Mild", "Moderate", "Severe", "Critical", "Checking"))
    tableped$Severitygrouping <- "one"

    ggo <- ggbarplot(tableped, x = "Severitygrouping", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924", "Checking" = "#A7A7A7")) +
      ylim(0, (40)) + geom_hline(yintercept = 38, linetype="dashed", color = "red") + xlab("Study Grouping") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924", "Checking" = "#A7A7A7")) + theme(axis.text.x=element_blank())
    ggo
  })

  output$graph5 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    #inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
    pediatric <- inputdata[inputdata$`Subject Age` < 18, ]
    colnames(pediatric) <- gsub("Race/Ethnicity", "RaceEthnicity",  colnames(pediatric) )
    tableped <- as.data.frame(table(pediatric$`RaceEthnicity`, pediatric$`Severity`))
    colnames(tableped) <- c("RaceEthnicity","Severity", "Participants")
    tableped$`RaceEthnicity` <- factor(tableped$`RaceEthnicity`, levels = c("Hispanic", "White", "Black"))
    tableped <- tableped[tableped$Participants != 0,]
    ggo <- ggbarplot(tableped, x = "RaceEthnicity", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      xlab("Race/Ethnicity") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924"))
    ggo
  })

  output$graph6 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    #inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
    pediatric <- inputdata[inputdata$`Subject Age` >= 18 & inputdata$`Subject Age` < 65, ]
    colnames(pediatric) <- gsub("Race/Ethnicity", "RaceEthnicity",  colnames(pediatric) )
    tableped <- as.data.frame(table(pediatric$`RaceEthnicity`, pediatric$`Severity`))
    colnames(tableped) <- c("RaceEthnicity","Severity", "Participants")
    tableped$`RaceEthnicity` <- factor(tableped$`RaceEthnicity`, levels = c("Hispanic", "White", "Black"))
    tableped <- tableped[tableped$Participants != 0,]
    ggo <- ggbarplot(tableped, x = "RaceEthnicity", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      xlab("Race/Ethnicity") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924"))
    ggo
  })

  output$graph7 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    #inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
    pediatric <- inputdata[inputdata$`Subject Age` >= 65, ]
    colnames(pediatric) <- gsub("Race/Ethnicity", "RaceEthnicity",  colnames(pediatric) )
    tableped <- as.data.frame(table(pediatric$`RaceEthnicity`, pediatric$`Severity`))
    colnames(tableped) <- c("RaceEthnicity","Severity", "Participants")
    tableped$`RaceEthnicity` <- factor(tableped$`RaceEthnicity`, levels = c("Hispanic", "White", "Black"))
    tableped <- tableped[tableped$Participants != 0,]
    ggo <- ggbarplot(tableped, x = "RaceEthnicity", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      xlab("Race/Ethnicity") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924"))
    ggo
  })

  output$graph8 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    #inputdata$`Date Scheduled` <- as.character((inputdata$`Date Scheduled`))
    pediatric <- inputdata[inputdata$`Subject Age` < 18, ]
    tableped <- as.data.frame(table(pediatric$`Subject Sex`, pediatric$`Severity`))
    colnames(tableped) <- c("Sex","Severity", "Participants")
    tableped$`Sex` <- factor(tableped$`Sex`, levels = c("Male", "Female"))
    tableped <- tableped[tableped$Participants != 0,]
    ggo <- ggbarplot(tableped, x = "Sex", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      xlab("Sex") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924"))
    ggo
  })

  output$graph9 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    pediatric <- inputdata[inputdata$`Subject Age` < 65 & inputdata$`Subject Age` > 18, ]
    tableped <- as.data.frame(table(pediatric$`Subject Sex`, pediatric$`Severity`))
    colnames(tableped) <- c("Sex","Severity", "Participants")
    tableped$`Sex` <- factor(tableped$`Sex`, levels = c("Male", "Female"))
    tableped <- tableped[tableped$Participants != 0,]
    ggo <- ggbarplot(tableped, x = "Sex", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      xlab("Sex") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924"))
    ggo
  })

  output$graph10 <- renderPlot({
    inputdata <- datasetOuttimeline()
    inputdata <- inputdata[inputdata$Cohort == "Longitudinal",]
    inputdata <- inputdata[!duplicated(inputdata$`Subject ID`), ]
    pediatric <- inputdata[inputdata$`Subject Age` >= 65, ]
    tableped <- as.data.frame(table(pediatric$`Subject Sex`, pediatric$`Severity`))
    colnames(tableped) <- c("Sex","Severity", "Participants")
    tableped$`Sex` <- factor(tableped$`Sex`, levels = c("Male", "Female"))
    tableped <- tableped[tableped$Participants != 0,]
    ggo <- ggbarplot(tableped, x = "Sex", y = "Participants",
                     label = TRUE, lab.col = "white", lab.pos = "in", fill = "Severity", color = "Severity") +
      scale_x_discrete(drop=FALSE) + scale_fill_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924")) +
      xlab("Sex") +
      scale_color_manual(values=c("Asymptomatic" = "#24E794", "Mild" = "#41CCE5","Moderate" = "#E7D224",'Severe' = "#E78B24", "Critical" = "#E73924"))
    ggo
  })

  ##---------------------------------- Boxes
  output$progressBox <- renderValueBox({
    convalescent <- convalescent()
    valueBox(
      paste0(nrow(convalescent), "/38" ), "Convalescent Progress for Babson", icon = icon("chart-bar"),
      color = "light-blue"
    )
  })

  output$totalNumParticipantsBox <- renderValueBox({
    datasetOuttimeline <- datasetOuttimeline()
    valueBox(
      paste0(length(unique(datasetOuttimeline$`Subject ID`))), "Total Number of Participants", icon = icon("chart-bar"),
      color = "light-blue"
    )
  })

  output$totalDrawsBox <- renderValueBox({
    datasetOuttimeline <- datasetOuttimeline()
    valueBox(
      paste0(nrow(datasetOuttimeline)), "Total Number of Blood Draws", icon = icon("chart-bar"),
      color = "light-blue"
    )
  })

  output$ltfNumBox <- renderValueBox({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(input$filetypeinput == "Excel"){
      middle <- read_excel(infile$datapath)
    } else if (input$filetypeinput == "csv"){
      middle <- read.csv(infile$datapath)
    }
    middle <- middle[,sapply(middle, function(x) { sum(!is.na(x)) > 0 })]
    ltf <- middle[middle$LTF == "Y", colnames(middle) == "Subject ID"]
    ltf<-ltf[!is.na(ltf)]
    middle <- middle[ (middle$`Subject ID` %in% ltf ),]

    valueBox(
      paste0(length(unique(middle$`Subject ID`))), "Participants Loss to Follow Up", icon = icon("chart-bar"),
      color = "light-blue"
    )
  })

  output$pediatricBox <- renderValueBox({
    timelinedata <- timelinedata()
    subjectidtimeline <- timelinedata[!duplicated(timelinedata$`Subject ID`), ]
    subjectidtimeline <- subjectidtimeline[subjectidtimeline$Cohort == "Longitudinal", ]
    subjectidtimeline <- subjectidtimeline[subjectidtimeline$`Subject Age` < 18, ]
    valueBox(
      nrow(subjectidtimeline), "Longitudinal Pediatric Enrolled", icon = icon("chart-bar"),
      color = "green"
    )
  })

  output$adultBox <- renderValueBox({
    timelinedata <- timelinedata()
    subjectidtimeline <- timelinedata[!duplicated(timelinedata$`Subject ID`), ]
    subjectidtimeline <- subjectidtimeline[subjectidtimeline$Cohort == "Longitudinal", ]
    subjectidtimeline <- subjectidtimeline[subjectidtimeline$`Subject Age` > 18 & subjectidtimeline$`Subject Age` < 65, ]
    valueBox(
      nrow(subjectidtimeline), "Longitudinal Adults Enrolled", icon = icon("chart-bar"),
      color = "blue"
    )
  })

  output$geriatricBox <- renderValueBox({
    timelinedata <- timelinedata()
    subjectidtimeline <- timelinedata[!duplicated(timelinedata$`Subject ID`), ]
    subjectidtimeline <- subjectidtimeline[subjectidtimeline$Cohort == "Longitudinal", ]
    subjectidtimeline <- subjectidtimeline[subjectidtimeline$`Subject Age` >= 65, ]
    valueBox(
      nrow(subjectidtimeline), "Longitudinal Elderly Enrolled", icon = icon("chart-bar"),
      color = "orange"
    )
  })

  # output$BabsonTable <- renderTable({
  #   timelinedata <- timelinedata()
  #   ped <- timelinedata[!duplicated(timelinedata$`Subject ID`), ]
  #   ped <- ped[ped$Cohort == "Longitudinal", ]
  #   ped <- ped[ped$`Subject Age` < 18, ]
  #
  #   adu <- timelinedata[!duplicated(timelinedata$`Subject ID`), ]
  #   adu <- adu[adu$Cohort == "Longitudinal", ]
  #   adu <- adu[adu$`Subject Age` > 18 & adu$`Subject Age` < 65, ]
  #
  #   ger <- timelinedata[!duplicated(timelinedata$`Subject ID`), ]
  #   ger <- ger[ger$Cohort == "Longitudinal", ]
  #   ger <- ger[ger$`Subject Age` >= 65, ]
  #
  #   adu
  # })



})

# Run the application
shinyApp(ui = ui, server = server)
