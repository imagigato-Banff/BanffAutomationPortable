# load packages

library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(collapsibleTree)
library(shinycssloaders)
library(shinyWidgets)
library(rmarkdown)
library(knitr)
library(readxl)
# library(webshot)
library(kableExtra)
library(stringr)
library(shinyjs)
library(writexl)


# if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs(force = T) }

# set Global variables

other_diagnoses_ls <- list(
    "Suspicious for CNI toxicity",
    "Suspicious for pyelonephritis",
    "Immune complex nephropathy, IgA",
    "Immune complex nephropathy, C3",
    "Immune complex nephropathy, membranous",
    "Immune complex nephropathy, lupus",
    "Focal Segmental Glomerulosclerosis (FSGS)",
    "Diabetic nephropathy",
    "Post-transplant lymphoproliferative disease (PTLD)",
    "Drug-induced interstitial nephritis",
    "Neoplasia",
    "Others"
)

# full_color <- c(
#     rep("grey", 15),
#     rep("tomato", 197),
#     rep("steelblue", 58),
#     rep("yellowgreen", 28),
#     
#     rep("blueviolet", 8),
#     rep("yellow", 3),
#     rep("darkgoldenrod", 3),
#     rep("bisque", 2),
#     rep("gold", 2),
#     rep("lemonchiffon", 2),
#     rep("forestgreen", 4),
#     rep("darkolivegreen", 4),
#     rep("darkgreen", 2)
# )

full_color <- c(
    rep("grey", 15),
    rep("red", 99),
    rep("#ED7117", 58),
    rep("#FFA500", 28),
    
    rep("lightblue", 8),
    rep("darkblue", 3),
    rep("darkblue", 3),
    rep("darkblue", 2),
    rep("darkblue", 2),
    rep("darkblue", 2),
    rep("turquoise", 4),
    rep("turquoise", 4),
    rep("turquoise", 2)
)

# org$branch1[11:18] <- "ptc > 0 (No BL or Acute TCMR)"
# org$branch1[49:52] <- "ptcml > 0 "
# org$branch1[45:48] <- "cg > 0 "

options(warn = -1) # suppress warning messages caused by NA coercion
toMatch <- c("Possible Chronic Active TCMR II", "Possible Acute TCMR IIA", "Possible Acute TCMR IIB", "Possible Acute TCMR III")

other_diagnoses_pattern <- "None|Suspicious for CNI toxicity|Diabetic nephropathy"

C4d_stain <- 1:2
AAMR <- 3:26
CAAMR <- 27:44
CIAMR <- 45:52

BOR <- 53:54
ATCMRIA <- 55
ATCMRIB <- 56
ATCMRIIA <- 57
ATCMRIIB <- 58
ATCMRIII <- 59
CATCMRIA <- 60
CATCMRIB <- 61
CATCMRII <- 62

grade1_belong <- 1
grade2_belong <- 2
grade3_belong <- 3
normal_IFTA <- 4

class1_belong <- 1
class2_belong <- 2
class3_belong <- 3
normal_BK <- 4

# a list for coloring the tree diagram
tmpls <- readRDS("./colorLOCAL_updated.rds")
# tmpls <- read_rds("Banff_diagnosis_shiny2/colorLOCAL_updated.rds")
# tmpls[[93]] <- c(rep(0, 10), 335)

# a dataframe for coloring the tree diagram
org <- readRDS("./banff_df3.rds")

org[org == "ATI = 0"] <- "No ATI"
org[org == "ATI = 1"] <- "ATI"
org[org == "TMA = 0"] <- "No TMA"
org$branch3[grepl("TMA", org$branch3, ignore.case = T)] <- "TMA unrelated to AMR"
org$branch1[grepl("TMA", org$branch1, ignore.case = T)] <- "TMA related to AMR"
org$branch2[grepl("TMA", org$branch2, ignore.case = T)] <- "No TMA or related to AMR"

# org <- readRDS("Banff_diagnosis_shiny2/banff_df3.rds")
# org <- org %>% 
#     mutate_all(funs(str_replace(., "PTCML", "ptcml")))
# org[83, 3] <- "i = 1"
# org[org == "i-IFTA < 2"] <- "i-IFTA < 2 or ti < 2"



# Define UI for Banff Automation Application
ui <- fluidPage(theme = shinytheme("paper"),
                shinyjs::useShinyjs(),
                navbarPage("Banff Diagnosis", collapsible = T,
                           tabPanel("Previsualization",
                                    # titlePanel(strong("Banff individual lesions scores")),
                                    fluidRow(
                                        column(2,
                                               fluidRow(h3(strong("Banff individual lesions scores")),
                                                   column(6, 
                                                          selectInput("ABO", "ABO incompatibility", choices = c("", NA, "No", "Yes")),
                                                          selectInput("C4d", "C4d", choices = c("", NA, 0:3)),
                                                          selectInput("C4d_type", "C4d type", choices = c("", NA, "IF", "IHC")),
                                                          selectInput("t", "t", choices = c("", 0:3)),
                                                          selectInput("i", "i", choices = c("", 0:3)),
                                                          selectInput("v", "v", choices = c("", NA, 0:3)),
                                                          selectInput("g", "g", choices = c("", 0:3)),
                                                          selectInput("ptc", "ptc", choices = c("", 0:3)),
                                                          selectInput("cg", "cg", choices = c("", "NA", "0", "1a", "1b", "2", "3")),
                                                          selectInput("ci", "ci", choices = c("", 0:3)),
                                                          selectInput("ct", "ct", choices = c("", 0:3))
                                                   ),
                                                   column(6, 
                                                          selectInput("i_IFTA", "i-IFTA", choices = c("", 0:3)),
                                                          selectInput("t_IFTA", "t-IFTA", choices = c("", NA, 0:3)),
                                                          selectInput("cv", "cv", choices = c("", NA, 0:3, 
                                                                                              "new onset/prolif. cv1", 
                                                                                              "new onset/prolif. cv2", 
                                                                                              "new onset/prolif. cv3")),
                                                          selectInput("ti", "ti", choices = c("", 0:3)),
                                                          selectInput("TMA", "TMA", 
                                                                      # choices = c("", NA, 0:1),
                                                                      choices = c("", NA,
                                                                                  "No TMA" = 0,
                                                                                  "TMA unrelated to AMR" = 1,
                                                                                  "TMA related to AMR" = 2)
                                                                      ),
                                                          selectInput("ATI", "ATI", 
                                                                      choices = c("", NA,
                                                                                  "No ATI" = 0,
                                                                                  "ATI" = 1)),
                                                          selectInput("ptcml", "ptcml", choices = c("", NA, 0:3)),
                                                          selectInput("PVL", "PVL", choices = c("", NA, 0:3)),
                                                          selectInput("DSA", "Circulating anti-HLA DSA", choices = c("", NA, "No", "Yes")),
                                                          selectInput("mmarker", "Molecular marker", choices = c("", NA, "No", "AMR Molecular Marker")),
                                                          selectInput("prior", "Prior diagnosis", choices = c("", NA, "No", "Active AMR or Chronic active AMR"))
                                                          # selectInput("recurrent", "Recurrent disease", choices = c("", NA, "No", "Yes")),
                                                          # selectizeInput("", "Non rejection diagnoses (multiple choices allowed)",
                                                          #               )
                                                          # textOutput("textResult_test"),
                                                          # textOutput("textResult_test2"),
                                                   )),
                                               
                                               fluidRow(tags$div(
                                                   tags$h3(strong("Other diagnoses")),
                                                   "(multiple choices allowed)"
                                                   ),
                                                   # h3(strong("Non rejection diagnoses\n(multiple choices allowed)")),
                                                        column(12,
                                                               pickerInput(
                                                                   inputId = "other_diagnoses",
                                                                   label = h4(strong("")),
                                                                   choices = list(None = list("None"),
                                                                                  `Rejection diagnoses` = other_diagnoses_ls
                                                                   ),
                                                                   selected = "None",
                                                                   options = pickerOptions(
                                                                       actionsBox = F,
                                                                       size = 10,
                                                                       selectedTextFormat = "count",
                                                                       noneSelectedText = "Select"
                                                                   ),
                                                                   multiple = TRUE
                                                               ),
                                                               
                                                               conditionalPanel(
                                                                   condition = "output.cond == true",
                                                                   textAreaInput("free_text", "Please describe in details atypical lesions.", 
                                                                                 placeholder = "Please type here.")),
                                                               helpText("Please fill in every blank then click the SUBMIT button."),
                                                               actionBttn("action", "SUBMIT", style = "pill")
                                                               )
                                                        )
                                        ),
                                        column(10,
                                               br(), br(), br(), br(), br(),
                                               h3(textOutput("caption_diagnosis")), 
                                               verbatimTextOutput("textResult1"),
                                               h4(textOutput("caption_notes")), 
                                               verbatimTextOutput("textResult2"),
                                               # dataTableOutput("debug"),
                                               # verbatimTextOutput("debug2"),
                                               # tags$video(src = "egfr_traj.mp4", type = "video/mp4", autoplay = T, controls = T, width = 900, height = 500),
                                               collapsibleTreeOutput("graph", height = "1100px")
                                        )
                                    )
                           ),
                           tabPanel("Automated Report Generator",
                                    titlePanel("Automated Banff Report"),
                                    sidebarLayout(
                                        sidebarPanel(
                                            div(a("Example file (xlsm)", target = "_blank", href = "test_biopsies.xlsm"), style = "font-size: 20px"),
                                            tags$hr(),
                                            # Input: Select a file
                                            uiOutput("file1_ui"),
                                            # fileInput("file_excel", "Choose xls/xlsx/xlsm File", multiple = F),
                                            radioButtons(
                                                inputId = "type_file",
                                                label = "Please select a type of file for the report you desire",
                                                choiceNames = list("PDF report", "Excel report"),
                                                choiceValues = list("pdf", "xlsx"), 
                                                inline = F
                                            ),
                                            
                                            
                                            # Horizontal line
                                            tags$hr(),
                                            conditionalPanel(condition = "output.report_sing == true",
                                                             downloadButton("report.single", "Generate report")),
                                            conditionalPanel(condition = "output.report_mul == true",
                                                             downloadButton("report", "Generate report")),
                                            conditionalPanel(condition = "output.report_sing == false & output.report_mul == false",
                                                             downloadButton("report_tab", "Generate report"))
                                            # 
                                            # conditionalPanel(condition = "output.report_sing == true",
                                            #                  shinyjs::disabled(downloadButton("report.single", "Generate report"))),
                                            # conditionalPanel(condition = "output.report_mul == true",
                                            #                  shinyjs::disabled(downloadButton("report", "Generate report"))),
                                            # conditionalPanel(condition = "output.report_sing == false & output.report_mul == false",
                                            #                  shinyjs::disabled(downloadButton("report_tab", "Generate report")))
                                        ), 
                                        mainPanel(
                                            # textOutput("text"),
                                            verbatimTextOutput("instruction"),
                                            tags$head(tags$style("#instruction{color:cornflowerblue; font-size:20px; overflow-y:scroll; background: sepia;}"))
                                            # tableOutput("table")
                                        )
                                    )
                           ),
                           
                           tabPanel("REFERENCES",
                                    verbatimTextOutput("text1"),
                                    uiOutput("ref1"),
                                    uiOutput("ref2"),
                                    uiOutput("ref3"),
                                    uiOutput("ref4"),
                                    uiOutput("ref5"),
                                    uiOutput("ref6"),
                                    uiOutput("ref7"),
                                    uiOutput("ref8")
                           )
                )
)



# Backend functions
server <- function(input, output, session) {
    
    C4d <- eventReactive(input$action, {
        input$C4d
    })
    
    C4d_type <- eventReactive(input$action, {
        input$C4d_type
    })
    
    t <- eventReactive(input$action, {
        input$t
    })
    
    i <- eventReactive(input$action, {
        input$i
    })
    
    ci <- eventReactive(input$action, {
        input$ci
    })
    
    ct <- eventReactive(input$action, {
        input$ct
    })
    
    v <- eventReactive(input$action, {
        input$v
    })
    
    g <- eventReactive(input$action, {
        input$g
    })
    
    ptc <- eventReactive(input$action, {
        input$ptc
    })
    
    cg <- eventReactive(input$action, {
        input$cg
    })
    
    i_IFTA <- eventReactive(input$action, {
        input$i_IFTA
    })
    
    t_IFTA <- eventReactive(input$action, {
        input$t_IFTA
    })
    
    cv <- eventReactive(input$action, {
        input$cv
    })
    
    ti <- eventReactive(input$action, {
        input$ti
    })
    
    TMA <- eventReactive(input$action, {
        input$TMA
    })
    
    ATI <- eventReactive(input$action, {
        input$ATI
    })
    
    ptcml <- eventReactive(input$action, {
        input$ptcml
    })
    
    other_diagnoses <- eventReactive(input$action, {
        input$other_diagnoses
    })
    
    PVL <- eventReactive(input$action, {
        input$PVL
    })
    
    DSA <- eventReactive(input$action, {
        input$DSA
    })
    
    mmarker <- eventReactive(input$action, {
        input$mmarker
    })
    
    prior <- eventReactive(input$action, {
        input$prior
    })
    
    ABO <- eventReactive(input$action, {
        input$ABO
    })
    
    
    output$report_sing <- reactive({
        if (is.null(input$file_excel$datapath)) {
            data_origin <- data.frame()
        } else {
            data_origin <- as.data.frame(read_excel(input$file_excel$datapath))    
        }
        
        nrow(data_origin) == 1 & input$type_file %in% "pdf"
    })
    outputOptions(output, "report_sing", suspendWhenHidden = F)
    
    output$report_mul <- reactive({
        if (is.null(input$file_excel$datapath)) {
            data_origin <- data.frame()
        } else {
            data_origin <- as.data.frame(read_excel(input$file_excel$datapath))    
        }
        nrow(data_origin) > 1 & input$type_file %in% "pdf" | nrow(data_origin) %in% 0
    })
    outputOptions(output, "report_mul", suspendWhenHidden = F)
    
    output$report_tab <- reactive({
        input$type_file %in% "xlsx"
    })
    outputOptions(output, "report_tab", suspendWhenHidden = F)
    
    
    other_diagnoses <- eventReactive(input$action, {
        input$other_diagnoses
    })
    
    free <- eventReactive(input$action, {
        input$free_text
    })
    
    
    
    
    output$cond <- reactive({
        length(input$other_diagnoses) > 0 & any(input$other_diagnoses %in% other_diagnoses_ls)
    })
    outputOptions(output, "cond", suspendWhenHidden = F)
    
    # output$textResult_test2 <- renderText(length(input$non_rejection))
    make_original_DF <- eventReactive(input$action, {
        mmarker <- ifelse(input$mmarker == "No", 0, 
                          ifelse(input$mmarker == "AMR Molecular Marker", 1, NA))
        DSA <- ifelse(input$DSA == "No", 0,
                      ifelse(input$DSA == "Yes", 1, NA))
        prior <- ifelse(input$prior == "No", 0,
                        ifelse(input$prior == "Active AMR or Chronic active AMR", 1, NA))
        v <- ifelse(input$v == "0", 0,
                    ifelse(input$v == "1", 1,
                           ifelse(input$v == "2", 2,
                                  ifelse(input$v == "3", 3, NA))))
        # cv <- ifelse(input$cv %in% c(NA, "NA", "0", "1", "2", "3"), 0,
        #              ifelse(input$cv %in% c("new onset/prolif. cv1", "new onset/prolif. cv2", "new onset/prolif. cv3"), 1, NA))
        cg <- ifelse(input$cg == "0", 0,
                     ifelse(input$cg == "1a", 1,
                            ifelse(input$cg == "1b", 1,
                                   ifelse(input$cg == "2", 2,
                                          ifelse(input$cg == "3", 3, NA)))))
        ptcml <- ifelse(input$ptcml == "0", 0,
                        ifelse(input$ptcml == "1", 1,
                               ifelse(input$ptcml == "2", 2,
                                      ifelse(input$ptcml == "3", 3, NA))))
        TMA <- ifelse(input$TMA == "0", 0,
                      ifelse(input$TMA == "1", 1,
                             ifelse(input$TMA == "2", 2, NA)))
        ATI <- ifelse(input$ATI == "0", 0,
                      ifelse(input$ATI == "1", 1, NA))
        ABO <- ifelse(input$ABO %in% "No", 0, 
                      ifelse(input$ABO %in% "Yes", 1, NA))
        
        
        tmpDF <- data.frame(C4d = as.numeric(input$C4d), C4d_type = input$C4d_type, t = as.numeric(input$t), i = as.numeric(input$i), v = v,
                            g = as.numeric(input$g), ptc = as.numeric(input$ptc), cg = cg, ci = as.numeric(input$ci), ct = as.numeric(input$ct),
                            i_IFTA = as.numeric(input$i_IFTA), t_IFTA = as.numeric(input$t_IFTA), PVL = as.numeric(input$PVL),
                            cv = input$cv, ti = as.numeric(input$ti), TMA = TMA, ATI = ATI, ptcml = ptcml,
                            DSA = DSA, mmarker = mmarker, prior_diagnosis = prior, ABO = ABO, stringsAsFactors = F)
        tmpDF
    })
    
    
    
    makeDF <- eventReactive(input$action, {
        
        mmarker <- ifelse(input$mmarker == "No", 0, 
                          ifelse(input$mmarker == "AMR Molecular Marker", 1, NA))
        DSA <- ifelse(input$DSA == "No", 0,
                      ifelse(input$DSA == "Yes", 1, NA))
        prior <- ifelse(input$prior == "No", 0,
                        ifelse(input$prior == "Active AMR or Chronic active AMR", 1, NA))
        v <- ifelse(input$v == "0", 0,
                    ifelse(input$v == "1", 1,
                           ifelse(input$v == "2", 2,
                                  ifelse(input$v == "3", 3, 0))))
        cv <- ifelse(input$cv %in% c(NA, "NA", "0", "1", "2", "3"), 0,
                     ifelse(input$cv %in% c("new onset/prolif. cv1", "new onset/prolif. cv2", "new onset/prolif. cv3"), 1, NA))
        cg <- ifelse(input$cg == "0", 0,
                     ifelse(input$cg == "1a", 1,
                            ifelse(input$cg == "1b", 1,
                                   ifelse(input$cg == "2", 2,
                                          ifelse(input$cg == "3", 3, 0)))))
        ptcml <- ifelse(input$ptcml == "0", 0,
                        ifelse(input$ptcml == "1", 1,
                               ifelse(input$ptcml == "2", 2,
                                      ifelse(input$ptcml == "3", 3, 0))))
        TMA <- ifelse(input$TMA == "0", 0,
                      ifelse(input$TMA == "1", 1,
                             ifelse(input$TMA == "2", 2, 0)))
        ATI <- ifelse(input$ATI == "0", 0,
                      ifelse(input$ATI == "1", 1, 0))
        ABO <- ifelse(is.na(input$ABO) | input$ABO == "" | input$ABO %in% "NA" | input$ABO %in% "No", 0, 1)
        
        
        tmpDF <- data.frame(C4d = as.numeric(input$C4d), C4d_type = input$C4d_type, t = as.numeric(input$t), i = as.numeric(input$i), v = v,
                            g = as.numeric(input$g), ptc = as.numeric(input$ptc), cg = cg, ci = as.numeric(input$ci), ct = as.numeric(input$ct),
                            i_IFTA = as.numeric(input$i_IFTA), t_IFTA = as.numeric(input$t_IFTA), PVL = as.numeric(input$PVL), 
                            cv = cv, ti = as.numeric(input$ti), TMA = TMA, ATI = ATI, ptcml = ptcml,
                            DSA = DSA, mmarker = mmarker, prior_diagnosis = prior, ABO = ABO, stringsAsFactors = F)
        tmpDF
    })
    
    #### Function for main computation ####
    makeGraph <- eventReactive(input$action, {
        
        tmpDF <- makeDF()
        tmpDF <- 
            tmpDF %>% 
            mutate(C4d = case_when(
                ABO %in% 1 ~ NA_real_,
                TRUE ~ C4d
            ))
        
        tmpDF <- tmpDF %>%
            mutate(g_cal = replace(g, is.na(g), 0),
                   ptc_cal = replace(ptc, is.na(ptc), 0)) %>%
            
            mutate(diagnosis_AMR = case_when(
                C4d > 1 & C4d_type == "IF" & t == 0 & v == 0 & g == 0 & ptc == 0 & cg == 0 & cv == 0 & TMA == 0 & ATI == 0 & ptcml == 0 ~ 1,
                C4d > 0 & C4d_type == "IHC" & t == 0 & v == 0 & g == 0 & ptc == 0 & cg == 0 & cv == 0 &  TMA == 0 & ATI == 0 & ptcml == 0 ~ 2,
                
                g > 0 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 3,
                g > 0 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 4,
                g > 0 & cg == 0 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & DSA == 1 ~ 5,
                g > 0 & cg == 0 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & mmarker == 1 ~ 6,
                
                g > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 7,
                g > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 8,
                g > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & DSA == 1 ~ 9,
                g > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & mmarker == 1 ~ 10,
                
                ptc > 0 & !(i > 0 & t > 0) & cg == 0 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 11,
                ptc > 0 & !(i > 0 & t > 0) & cg == 0 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 12,
                ptc > 0 & !(i > 0 & t > 0) & cg == 0 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & DSA == 1 ~ 13,
                ptc > 0 & !(i > 0 & t > 0) & cg == 0 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & mmarker == 1 ~ 14,

                ptc > 0 & !(i > 0 & t > 0) & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 15,
                ptc > 0 & !(i > 0 & t > 0) & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 16,
                ptc > 0 & !(i > 0 & t > 0) & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & DSA == 1 ~ 17,
                ptc > 0 & !(i > 0 & t > 0) & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & mmarker == 1 ~ 18,
                
                v > 0 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 19,
                v > 0 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 20,
                v > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 21,
                v > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 22,
                
                TMA == 2 & cg %in% c(0, NA, "NA") & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 23,
                TMA == 2 & cg %in% c(0, NA, "NA") & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 24,
                
                ATI == 1 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 25,
                ATI == 1 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 26,
                
                cg > 0 & TMA %in% c(0, 2) & C4d > 1 & C4d_type == "IF" ~ 27,
                cg > 0 & TMA %in% c(0, 2) & C4d > 0 & C4d_type == "IHC" ~ 28,
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & DSA == 1 ~ 29,
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & mmarker == 1 ~ 30,
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & g >= 1 & (i > 0 & t > 0) & DSA == 1 ~ 31,
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & g >= 1 & (i > 0 & t > 0) & mmarker == 1 ~ 32,
                
                cv > 0 & C4d > 1 & C4d_type == "IF" ~ 33,
                cv > 0 & C4d > 0 & C4d_type == "IHC" ~ 34,
                cv > 0 & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & DSA == 1 ~ 35,
                cv > 0 & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & mmarker == 1 ~ 36,
                cv > 0 & g_cal + ptc_cal >= 2 & (i > 0 & t > 0) & g >= 1 &  DSA == 1 ~ 37,
                cv > 0 & g_cal + ptc_cal >= 2 & (i > 0 & t > 0) & g >= 1 & mmarker == 1 ~ 38,
                
                ptcml > 0 & C4d > 1 & C4d_type == "IF" ~ 39,
                ptcml > 0 & C4d > 0 & C4d_type == "IHC" ~ 40,
                ptcml > 0 & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & DSA == 1 ~ 41,
                ptcml > 0 & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & mmarker == 1 ~ 42,
                ptcml > 0 & g_cal + ptc_cal >= 2 & (i > 0 & t > 0) & g >= 1 &  DSA == 1 ~ 43,
                ptcml > 0 & g_cal + ptc_cal >= 2 & (i > 0 & t > 0) & g >= 1 & mmarker == 1 ~ 44,
                
                cg > 0 & g_cal + ptc_cal <= 1 & !(is.na(g) & is.na(ptc)) & C4d <= 1 & C4d_type == "IF" & DSA == 1 ~ 45,
                cg > 0 & g_cal + ptc_cal <= 1 & !(is.na(g) & is.na(ptc)) & C4d <= 1 & C4d_type == "IF" & prior_diagnosis == 1 ~ 46,
                cg > 0 & g_cal + ptc_cal <= 1 & !(is.na(g) & is.na(ptc)) & C4d == 0 & C4d_type == "IHC" & DSA == 1 ~ 47,
                cg > 0 & g_cal + ptc_cal <= 1 & !(is.na(g) & is.na(ptc)) & C4d == 0 & C4d_type == "IHC" & prior_diagnosis == 1 ~ 48,
                
                ptcml > 0 & g_cal + ptc_cal <= 1 & !is.na(g) & !is.na(ptc) & C4d <= 1 & C4d_type == "IF" & DSA == 1 ~ 49,
                ptcml > 0 & g_cal + ptc_cal <= 1 & !is.na(g) & !is.na(ptc) & C4d <= 1 & C4d_type == "IF" & prior_diagnosis == 1 ~ 50,
                ptcml > 0 & g_cal + ptc_cal <= 1 & !is.na(g) & !is.na(ptc) & C4d == 0 & C4d_type == "IHC" & DSA == 1 ~ 51,
                ptcml > 0 & g_cal + ptc_cal <= 1 & !is.na(g) & !is.na(ptc) & C4d == 0 & C4d_type == "IHC" & prior_diagnosis == 1 ~ 52,
                
                TRUE ~ 63)) %>%
            
            mutate(diagnosis_TCMR = case_when(
                
                # AMR, CATCMR IA/IB + II or not  --> graph CATCMR IA/IB , text add possible
                diagnosis_AMR %in% c(CAAMR, CIAMR) & (t == 3 | t_IFTA == 3) & ti >= 2 & i_IFTA >= 2 ~ 61,
                diagnosis_AMR %in% c(CAAMR, CIAMR) & (t == 2 | t_IFTA == 2) & ti >= 2 & i_IFTA >= 2 ~ 60,
                # cv > 0 & conditions for CATCMR IA/IB & AMR 93 --> both graph text
                # other cases should be Possible. No graph
                cv > 0 ~ 62,
                
                (t == 3 | t_IFTA == 3) & ti >= 2 & i_IFTA >= 2 ~ 61,
                (t == 2 | t_IFTA == 2) & ti >= 2 & i_IFTA >= 2 ~ 60,
                
                TRUE ~ 63
            ),
            
            
            diagnosis_TCMR_nonCh = case_when(
                (t > 0 & i == 1 & v == 0) | (t > 0 & i == 1 & v > 0 & !diagnosis_AMR %in% c(C4d_stain, 63)) ~ 53,
                (t == 1 & i >= 1 & v == 0) | (t == 1 & i >= 1 & v > 0 & !diagnosis_AMR %in% c(C4d_stain, 63)) ~ 54,
                
                # regardless of i or t, thus should be priotized over Acute TCMR IA and IB
                # but in case of AMR and TCMR IA, IB
                diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) & t == 2 & i >= 2 ~ 55,
                diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) & t == 3 & i >= 2 ~ 56,
                
                v == 1 ~ 57,
                v == 2 ~ 58,
                v == 3 ~ 59,
                
                t == 2 & i >= 2 ~ 55,
                t == 3 & i >= 2 ~ 56,
                
                TRUE ~ 63
            )) %>%
            
            mutate(diagnosis_IFTA = case_when(
                (ci == 1 & ct <= 1) | (ci <= 1 & ct == 1) ~ 1,
                (ci == 2 & ct <= 2) | (ci <= 2 & ct == 2) ~ 2,
                (ci == 3 & ct <= 3) | (ci <= 3 & ct == 3) ~ 3,
                TRUE ~ 4
            )) %>%
            
            mutate(diagnosis_BK = case_when(
                PVL == 1 & ci <= 1 ~ 1,
                (PVL == 1 & ci >= 2) | PVL == 2 | (PVL == 3 & ci <= 1) ~ 2,
                PVL == 3 & ci >= 2 ~ 3,
                TRUE ~ 4
            ))
        
        
        return(tmpDF[, c("diagnosis_AMR", "diagnosis_TCMR", "diagnosis_TCMR_nonCh", "diagnosis_IFTA", "diagnosis_BK")])
        
    })
    
    
    chooseColor <- eventReactive(input$action, {
        
        if (input$C4d == "" | input$C4d_type == "" | input$t == "" | input$i == "" |
            input$v == "" | input$g == "" | input$ptc == "" | input$cg == "" | input$ci == "" | input$ct == "" | input$i_IFTA == "" |
            input$cv == "" | input$ti == "" | input$TMA == "" | input$ATI == "" | input$ptcml == "" | 
            input$ABO == "" |
            input$PVL == "" | input$DSA == "" | input$mmarker == "" | input$prior == "" | input$t_IFTA == "") {
            final_color <- full_color
        }
        
        else {
            
            # diagnosis_AMR <- 22
            # diagnosis_TCMR_nonCh <- 57
            # diagnosis_TCMR <- 63
            
            diagnosis_AMR <- makeGraph()[, "diagnosis_AMR"]
            diagnosis_TCMR <- makeGraph()[, "diagnosis_TCMR"]
            diagnosis_TCMR_nonCh <- makeGraph()[, "diagnosis_TCMR_nonCh"]
            
            AMR_color <- ifelse(diagnosis_AMR %in% C4d_stain, "grey",
                                ifelse(diagnosis_AMR %in% AAMR, "red",
                                       ifelse(diagnosis_AMR %in% CAAMR, "#ED7117",
                                              ifelse(diagnosis_AMR %in% CIAMR, "#FFA500", "white"))))
            
            TCMR_color_noCh <- ifelse(diagnosis_TCMR_nonCh %in% BOR, "lightblue",
                                 ifelse(diagnosis_TCMR_nonCh %in% ATCMRIA, "darkblue",
                                        ifelse(diagnosis_TCMR_nonCh %in% ATCMRIB, "darkblue",
                                               ifelse(diagnosis_TCMR_nonCh %in% ATCMRIIA, "darkblue",
                                                      ifelse(diagnosis_TCMR_nonCh %in% ATCMRIIB, "darkblue",
                                                             ifelse(diagnosis_TCMR_nonCh %in% ATCMRIII, "darkblue", "white"))))))
            
            TCMR_color <- ifelse(diagnosis_TCMR %in% CATCMRIA, "turquoise",
                                      ifelse(diagnosis_TCMR %in% CATCMRIB, "turquoise", 
                                             ifelse(diagnosis_TCMR %in% CATCMRII, "turquoise", "white")))


            tmpDF <- textBasic()
            TCMR_text <- tmpDF$TCMR_text
            TCMR_text_nonCh <- tmpDF$TCMR_text_nonCh
            if (TCMR_text %in% toMatch) {
                TCMR_color <- "white"
            }
            
            if (TCMR_text_nonCh %in% toMatch) {
                TCMR_color_noCh <- "white"
            }
            
            
            color1 <- c(
                rep("white", tmpls[[diagnosis_AMR]][1]),
                rep(AMR_color, tmpls[[diagnosis_AMR]][2]),
                rep("white", tmpls[[diagnosis_AMR]][3]),
                rep(AMR_color, tmpls[[diagnosis_AMR]][4]),
                rep("white", tmpls[[diagnosis_AMR]][5]),
                rep(AMR_color, tmpls[[diagnosis_AMR]][6]),
                rep("white", tmpls[[diagnosis_AMR]][7]),
                rep(AMR_color, tmpls[[diagnosis_AMR]][8]),
                rep("white", tmpls[[diagnosis_AMR]][9])
            )
            
            color2 <- c(
                rep("white", tmpls[[diagnosis_TCMR]][1]),
                rep(TCMR_color, tmpls[[diagnosis_TCMR]][2]),
                rep("white", tmpls[[diagnosis_TCMR]][3]),
                rep(TCMR_color, tmpls[[diagnosis_TCMR]][4]),
                rep("white", tmpls[[diagnosis_TCMR]][5]),
                rep(TCMR_color, tmpls[[diagnosis_TCMR]][6]),
                rep("white", tmpls[[diagnosis_TCMR]][7]),
                rep(TCMR_color, tmpls[[diagnosis_TCMR]][8]),
                rep("white", tmpls[[diagnosis_TCMR]][9])
            )
            
            color3 <- c(
                rep("white", tmpls[[diagnosis_TCMR_nonCh]][1]),
                rep(TCMR_color_noCh, tmpls[[diagnosis_TCMR_nonCh]][2]),
                rep("white", tmpls[[diagnosis_TCMR_nonCh]][3]),
                rep(TCMR_color_noCh, tmpls[[diagnosis_TCMR_nonCh]][4]),
                rep("white", tmpls[[diagnosis_TCMR_nonCh]][5]),
                rep(TCMR_color_noCh, tmpls[[diagnosis_TCMR_nonCh]][6]),
                rep("white", tmpls[[diagnosis_TCMR_nonCh]][7]),
                rep(TCMR_color_noCh, tmpls[[diagnosis_TCMR_nonCh]][8]),
                rep("white", tmpls[[diagnosis_TCMR_nonCh]][9])
            )
            
            
            final_color <- ifelse(color1 != "white", color1,
                                  ifelse(color2 != "white", color2, 
                                         ifelse(color3 != "white", color3, "white")))
            
        }
        
        final_color
        
    })
    
    
    output$graph <- renderCollapsibleTree({
        
        # final_color <- full_color_test
        final_color <- full_color
        if (input$action > 0) {
            final_color <- chooseColor()
        }
        
        collapsibleTree(org, hierarchy = names(org), collapsed = F,
                        fontSize = 11, tooltip = T, root = "Biopsy", fillByLevel = F, linkLength = 140,
                        fill = c("chocolate",
                                 final_color)
                        , zoomable = T)
        
    })
    
    textBasic <- eventReactive(input$action, {
        diagnosis_AMR <- makeGraph()[, "diagnosis_AMR"]
        diagnosis_TCMR <- makeGraph()[, "diagnosis_TCMR"]
        diagnosis_TCMR_nonCh <- makeGraph()[, "diagnosis_TCMR_nonCh"]
        diagnosis_IFTA <- makeGraph()[, "diagnosis_IFTA"]
        diagnosis_BK <- makeGraph()[, "diagnosis_BK"]
        tmp_df <- makeDF()
        
        tmp_df <- 
            tmp_df %>% 
            mutate(C4d = case_when(
                ABO %in% 1 ~ NA_real_,
                TRUE ~ C4d
            ))
        
        tmp_result <- 
            data.frame(diagnosis_AMR = diagnosis_AMR, diagnosis_TCMR = diagnosis_TCMR, 
                       diagnosis_TCMR_nonCh = diagnosis_TCMR_nonCh,
                       diagnosis_IFTA = diagnosis_IFTA, diagnosis_BK = diagnosis_BK) %>%
            cbind(tmp_df)
        
        tmp_result <- 
            tmp_result %>% 
            mutate(g_cal = replace(g, is.na(g), 0),
                   ptc_cal = replace(ptc, is.na(ptc), 0)) %>% 
            mutate(AMR_text = case_when(
                diagnosis_AMR %in% C4d_stain ~ "C4d Staining Without Evidence of Rejection",
                # diagnosis_AMR %in% 23:24 & input$ATI == "1" ~ "Equivocal for diagnosis of Active AMR (other causes of TMA and ATI should be ruled out)",
                # diagnosis_AMR %in% 23:24 ~ "Equivocal for diagnosis of Active AMR (other causes of TMA should be ruled out)",
                diagnosis_AMR %in% 25:26 ~ "Equivocal for diagnosis of Active AMR (other causes of ATI should be ruled out)",
                ############## change
                g_cal + ptc_cal >= 2 & g_cal >= 1 & diagnosis_AMR %in% 63 ~ "Equivocal for diagnosis of AMR",
                
                diagnosis_AMR %in% AAMR ~ "Active AMR",
                diagnosis_AMR %in% CAAMR ~ "Chronic active AMR",
                diagnosis_AMR %in% CIAMR ~ "Chronic inactive AMR",
                TRUE ~ "No evidence of AMR")
            )
        
        
        
        tmp_result <- tmp_result %>% 
            mutate(diagnosis_TCMR2 = case_when(
                
                # AMR, CATCMR IA/IB + II or not  --> graph CATCMR IA/IB , text add possible
                diagnosis_TCMR %in% CATCMRIA & cv > 0 ~ "Chronic Active TCMR IA; Possible Chronic Active TCMR II",
                diagnosis_TCMR %in% CATCMRIA ~ "Chronic Active TCMR IA",
                diagnosis_TCMR %in% CATCMRIB & cv > 0 ~ "Chronic Active TCMR IB; Possible Chronic Active TCMR II",
                diagnosis_TCMR %in% CATCMRIB ~ "Chronic Active TCMR IB",
                # cv > 0 & conditions for CATCMR IA/IB & AMR 93 or C4d stain --> both graph text
                # other cases should be Possible. No graph
                diagnosis_TCMR %in% CATCMRII & diagnosis_AMR %in% c(C4d_stain, 63) & ti >= 2 & i_IFTA >= 2 & (t >= 2 | t_IFTA >= 2) ~ "Chronic Active TCMR II",
                diagnosis_TCMR %in% CATCMRII ~ "Possible Chronic Active TCMR II",
                TRUE ~ NA_character_),
                
                diagnosis_TCMR_nonCh = case_when(
                    diagnosis_TCMR_nonCh %in% BOR & v %in% 1 & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Suspicious (Borderline) for Acute TCMR; Possible Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% BOR & v %in% 2 & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Suspicious (Borderline) for Acute TCMR; Possible Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% BOR & v %in% 3 & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Suspicious (Borderline) for Acute TCMR; Possible Acute TCMR III",
                    diagnosis_TCMR_nonCh %in% BOR ~ "Suspicious (Borderline) for Acute TCMR",
                    
                    diagnosis_TCMR_nonCh %in% ATCMRIA & v == 1 ~ "Acute TCMR IA; Possible Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% ATCMRIA & v == 2 ~ "Acute TCMR IA; Possible Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% ATCMRIA & v == 3 ~ "Acute TCMR IA; Possible Acute TCMR III",
                    diagnosis_TCMR_nonCh %in% ATCMRIA ~ "Acute TCMR IA",
                    
                    diagnosis_TCMR_nonCh %in% ATCMRIB & v == 1 ~ "Acute TCMR IB; Possible Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% ATCMRIB & v == 2 ~ "Acute TCMR IB; Possible Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% ATCMRIB & v == 3 ~ "Acute TCMR IB; Possible Acute TCMR III",
                    diagnosis_TCMR_nonCh %in% ATCMRIB ~ "Acute TCMR IB",
                    
                    diagnosis_TCMR_nonCh %in% ATCMRIIA & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Possible Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% ATCMRIIA ~ "Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% ATCMRIIB & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Possible Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% ATCMRIIB ~ "Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% ATCMRIII & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Possible Acute TCMR III",
                    diagnosis_TCMR_nonCh %in% ATCMRIII ~ "Acute TCMR III",
                    TRUE ~ NA_character_
                ),
                
                diagnosis_IFTA2 = case_when(
                    diagnosis_IFTA %in% grade1_belong ~ "Grade I",
                    diagnosis_IFTA %in% grade2_belong ~ "Grade II", 
                    diagnosis_IFTA %in% grade3_belong ~ "Grade III",
                    TRUE ~ "No IFTA"),
                
                diagnosis_BK2 = case_when(
                    diagnosis_BK %in% class1_belong ~ "Class 1",
                    diagnosis_BK %in% class2_belong ~ "Class 2", 
                    diagnosis_BK %in% class3_belong ~ "Class 3", 
                    diagnosis_BK %in% normal_BK & tmp_df$PVL > 0 ~ "ci is needed for classification", 
                    diagnosis_BK %in% normal_BK & is.na(tmp_df$PVL) ~ "NA",
                    TRUE ~ "No PVN")
            )
        
        AMR_text <- tmp_result$AMR_text
        TCMR_text <- tmp_result$diagnosis_TCMR2
        TCMR_text_nonCh <- tmp_result$diagnosis_TCMR_nonCh
        IFTA_text <- tmp_result$diagnosis_IFTA2
        BK_text <- tmp_result$diagnosis_BK2
        
        
        tmpDF <- data.frame(diagnosis_AMR = diagnosis_AMR, diagnosis_TCMR = diagnosis_TCMR, diagnosis_TCMR_nonCh = diagnosis_TCMR_nonCh,
                            diagnosis_IFTA = diagnosis_IFTA, diagnosis_BK = diagnosis_BK, 
                            
                            AMR_text = AMR_text, TCMR_text = TCMR_text, TCMR_text_nonCh = TCMR_text_nonCh,
                            IFTA_text = IFTA_text, BK_text = BK_text,
                            stringsAsFactors = F)
        tmpDF
    })
    
    #### Diagnosis text ####
    output$textResult1 <- renderText({
        # isolate({
        #     df <- tibble(C4d = input$C4d,
        #                  C4d_type = input$C4d_type,
        #                  t = input$t,
        #                  i = input$i,
        #                  ci = input$ci,
        #                  ct = input$ct,
        #                  v = input$v,
        #                  g = input$g,
        #                  ptc = input$ptc,
        #                  cg = input$cg,
        #                  i_IFTA = input$i_IFTA,
        #                  t_IFTA = input$t_IFTA,
        #                  cv = input$cv,
        #                  ti = input$ti,
        #                  TMA = input$TMA,
        #                  ATI = input$ATI,
        #                  ptcml = input$ptcml,
        #                  other_diagnoses = is.null(input$other_diagnoses),
        #                  PVL = input$PVL,
        #                  DSA = input$DSA,
        #                  mmarker = input$mmarker,
        #                  prior = input$prior,
        #                  ABO = input$ABO)
        # })
        # validate(
        #     need(!any(df == ""), "Please fill in every blank then click the SUBMIT button.")
        # ) 
        
        C4d <- C4d(); C4d_type <- C4d_type(); t <- t(); i <- i(); ci <- ci(); ct <- ct();
        cv <- cv(); ti <- ti(); ABO <- ABO(); TMA <- TMA(); ATI <- ATI(); ptcml <- ptcml();
        v <- v(); g <- g(); ptc <- ptc(); cg <- cg(); i_IFTA <- i_IFTA(); t_IFTA <- t_IFTA(); 
        PVL <- PVL(); DSA <- DSA(); mmarker <- mmarker(); prior <- prior()
        other_diagnoses <- other_diagnoses()
        
        validate(
            isolate(need(!(C4d == "" | C4d_type == "" | input$t == "" | i == "" | ci == "" | ct == "" |
                               v == "" | g == "" | ptc == "" | cg == "" | i_IFTA == "" | t_IFTA == "" |
                               cv == "" | ti == ""  | TMA == "" | ATI == "" | ptcml == "" |
                               PVL == "" | DSA == "" | mmarker == "" | prior == "" | ABO == "" | is.null(other_diagnoses)),
                               "Please fill in every blank then click the SUBMIT button."))
        )
        
        make_original_DF <- make_original_DF()
        tmpDF <- textBasic()
        other_diagnoses <- other_diagnoses()
        free_text <- free()
        
        AMR_text <- tmpDF$AMR_text
        TCMR_text <- tmpDF$TCMR_text
        TCMR_text_nonCh <- tmpDF$TCMR_text_nonCh
        IFTA_text <- tmpDF$IFTA_text
        BK_text <- tmpDF$BK_text
        TMA_result <- make_original_DF$TMA
        ATI_result <- make_original_DF$ATI
        
        TMA_ATI_result <- ifelse(TMA_result %in% 1 & ATI_result %in% 1, "TMA unrelated to AMR, ATI.",
                                 ifelse(TMA_result %in% 1 & ATI_result %in% c(0, NA), "TMA unrelated to AMR",
                                        ifelse(TMA_result %in% 2 & ATI_result %in% 1, "TMA related to AMR, ATI.",
                                               ifelse(TMA_result %in% 2 & ATI_result %in% c(0, NA), "TMA related to AMR",
                                                      ifelse(TMA_result %in% c(0, NA) & ATI_result %in% 1, "ATI",
                                                             ifelse(TMA_result %in% c(0, NA) & ATI_result %in% c(0, NA), "", ""))))))
        
        text <- paste0("Other diagnoses: ", paste0(other_diagnoses, collapse = "/"), sep = "\n")
        if (!(free_text == "" | other_diagnoses == "None")) {
            text <- paste0(text,
                           "Details: ", free_text, sep = "\n")
        }
        if (any(!other_diagnoses %in% c("None", "Suspicious for CNI toxicity", "Diabetic nephropathy"))) {
            text <- paste0(text, "\nRejection diagnosis should be interpreted regarding associated other diagnoses.")
        }
        if (AMR_text == "No evidence of AMR" & is.na(TCMR_text) & is.na(TCMR_text_nonCh) & IFTA_text == "No IFTA" & (BK_text == "No PVN" | BK_text == "NA")) {
            if (TMA_ATI_result != "") {
                text <- paste0(text, "\n", TMA_ATI_result)
            } else {
                text <- paste0(text, "\nNo specific lesions")
            }
        } else if (is.na(TCMR_text) & is.na(TCMR_text_nonCh)) {
            text <- paste0(text,
                           "\nYour diagnosis for AMR: ", AMR_text,
                           "\nYour diagnosis for TCMR: ", "No evidence of TCMR",
                           "\nYour IFTA: ", IFTA_text,
                           "\nYour Polyomavirus Nephropathy: ", BK_text
            )
        } else if (is.na(TCMR_text_nonCh)) {
            text <- paste0(text,
                           "\nYour diagnosis for AMR: ", AMR_text,
                           "\nYour diagnosis for TCMR: ", TCMR_text,
                           "\nYour IFTA: ", IFTA_text,
                           "\nYour Polyomavirus Nephropathy: ", BK_text
            )
        } else if (is.na(TCMR_text)) {
            text <- paste0(text,
                           "\nYour diagnosis for AMR: ", AMR_text,
                           "\nYour diagnosis for TCMR: ", TCMR_text_nonCh,
                           "\nYour IFTA: ", IFTA_text,
                           "\nYour Polyomavirus Nephropathy: ", BK_text
            )
        } else {
            text <- paste0(text,
                           "\nYour diagnosis for AMR: ", AMR_text,
                           "\nYour first diagnosis for TCMR: ", TCMR_text,
                           "\nYour second diagnosis for TCMR: ", TCMR_text_nonCh,
                           "\nYour IFTA: ", IFTA_text,
                           "\nYour Polyomavirus Nephropathy: ", BK_text
            )
        }
        
        # df
        text
        # }
    })
    
    #### Recommendation text ####
    observeEvent(input$action, {
        output$textResult2 <- renderText({
            if (isolate(input$C4d) == "" | isolate(input$C4d_type) == "" | isolate(input$t) == "" | isolate(input$i) == "" |
                isolate(input$v) == "" | isolate(input$g) == "" | isolate(input$ptc) == "" | isolate(input$cg) == "" | isolate(input$i_IFTA) == "" |
                isolate(input$cv) == "" | isolate(input$ti) == "" | isolate(input$TMA) == "" | isolate(input$ATI) == "" | isolate(input$ptcml) == "" | is.null(isolate(input$other_diagnoses)) |
                isolate(input$PVL) == "" | isolate(input$DSA) == "" | isolate(input$mmarker) == "" | isolate(input$prior) == "" |
                isolate(input$ABO) == "") return(NULL)
            
            else {
                
                tmpDF2 <- makeDF()
                # tmpDF2 <- 
                #     tmpDF2 %>% 
                #     mutate(C4d = case_when(
                #         ABO %in% 1 ~ NA_real_,
                #         TRUE ~ C4d
                #     ))
                
                tmpDF <- textBasic()
                tmpDF_original <- make_original_DF()
                diagnosis_AMR <- tmpDF$diagnosis_AMR
                diagnosis_TCMR <- tmpDF$diagnosis_TCMR
                diagnosis_TCMR_nonCh <- tmpDF$diagnosis_TCMR_nonCh
                diagnosis_IFTA <- tmpDF$diagnosis_IFTA
                diagnosis_BK <- tmpDF$diagnosis_BK
                
                AMR_text <- tmpDF$AMR_text
                TCMR_text <- tmpDF$TCMR_text
                TCMR_text_nonCh <- tmpDF$TCMR_text_nonCh
                IFTA_text <- tmpDF$IFTA_text
                BK_text <- tmpDF$BK_text
                
                TMA_result <- tmpDF_original$TMA
                ATI_result <- tmpDF_original$ATI
                
                TMA_ATI_result <- ifelse(TMA_result %in% 1 & ATI_result %in% 1, "TMA unrelated to AMR, ATI.",
                                         ifelse(TMA_result %in% 1 & ATI_result %in% c(0, NA), "TMA unrelated to AMR",
                                                ifelse(TMA_result %in% 2 & ATI_result %in% 1, "TMA related to AMR, ATI.",
                                                       ifelse(TMA_result %in% 2 & ATI_result %in% c(0, NA), "TMA related to AMR",
                                                              ifelse(TMA_result %in% c(0, NA) & ATI_result %in% 1, "ATI",
                                                                     ifelse(TMA_result %in% c(0, NA) & ATI_result %in% c(0, NA), "", ""))))))
                
                
                text <- ""
                ### C4d type warning
                if (!is.na(tmpDF2$C4d) & (is.na(tmpDF2$C4d_type) | tmpDF2$C4d_type == "NA") ) {
                    text <- paste0(text, "\nPlease fill C4d type in")
                }
                
                ### ABO incompatibility
                if (tmpDF2$ABO %in% 1 & !is.na(tmpDF2$C4d) & tmpDF2$C4d > 0) {
                    text <- paste0(text, "\nIn the context of ABO incompatibility, positive C4d is not interpretable")
                }
                
                ### ATI / TMA note
                
                if (TMA_ATI_result != "" & !(AMR_text == "No evidence of AMR" & is.na(TCMR_text) & is.na(TCMR_text_nonCh) & IFTA_text == "No IFTA" & (BK_text == "No PVN" | BK_text == "NA"))) {
                    text <- paste0(text, "\n", TMA_ATI_result)
                }
                
                if (tmpDF2$TMA %in% c(2) & tmpDF2$ATI %in% 1) {
                    text <- paste0(text, "\nMake sure other causes of TMA or ATI are ruled out")
                } else if (tmpDF2$TMA %in% c(2)) {
                    text <- paste0(text, "\nMake sure other causes of TMA are ruled out")
                } else if (tmpDF2$ATI %in% 1) {
                    text <- paste0(text, "\nMake sure other causes of ATI are ruled out")
                }
                
                ### cg & TMA & Active AMR
                if (diagnosis_AMR %in% c(7:10, 15:18, 21:22) & tmpDF2$TMA %in% 1) {
                    text <- paste0(text, "\ncg is not considered because it could be due to TMA")
                }
                
                
                #### DSA
                if ((diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) | (grepl("equivocal", AMR_text, ignore.case = T))) & is.na(tmpDF2$DSA)) {
                    text <- paste0(text,
                                   "\nTest for Anti-HLA DSA")
                } else if ((diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) | (grepl("equivocal", AMR_text, ignore.case = T))) & tmpDF2$DSA %in% 0) {
                    text <- paste0(text,
                                   "\nTest for non Anti-HLA Antibodies\nTest for gene transcripts/classifiers associated with AMR")
                } else if (is.na(tmpDF2$C4d) | is.na(tmpDF2$DSA)) {
                    if (is.na(tmpDF2$C4d) & is.na(tmpDF2$DSA)) {
                        text <- paste0(text, "\nDSA and C4d tests are recommended for AMR diagnosis")
                    } else if (is.na(tmpDF2$C4d)) {
                        text <- paste0(text, "\nC4d test is recommended for AMR diagnosis")
                    } else if (is.na(tmpDF2$DSA)) {
                        text <- paste0(text, "\nDSA test is recommended for AMR diagnosis")
                    }
                }
                
                # note for i-IFTA
                if (tmpDF2$i_IFTA > 0 & is.na(tmpDF2$PVL)) {
                    text <- paste0(text,
                                   "\nNote for i-IFTA: make sure other known causes of i-IFTA such as pyelonephritis and polyomavirus nephropathy ruled out\ni-IFTA: Test for gene transcripts/classifiers associated with AMR and TCMR, virus-specific transcripts")
                } else if (tmpDF2$i_IFTA > 0) {
                    text <- paste0(text, 
                                   "\ni-IFTA: Test for gene transcripts/classifiers associated with AMR and TCMR, virus-specific transcripts")
                }
                
                # note for DSA positive, C4d neg, g+ptc == 1
                if (tmpDF2$DSA %in% 1 & tmpDF2$C4d %in% 0 & tmpDF2$g + tmpDF2$ptc == 1) {
                    text <- paste0(text,
                                   "\nDSA positive, C4d negative, g + ptc = 1: Test for gene transcripts/classifiers associated with AMR")
                }
                
                # note for C4d stains
                if (diagnosis_AMR %in% C4d_stain) {
                    text <- paste0(text,
                                   "\nC4d Staining: Test for gene transcripts/classifiers associated with AMR")
                }
                # isolated v
                if (tmpDF2$v > 0 & tmpDF2$g + tmpDF2$ptc <= 1 & tmpDF2$i + tmpDF2$t <= 1) {
                    text <- paste0(text,
                                   "\nisolated v: Test for gene transcripts/classifiers associated with AMR and TCMR")
                }
                
                # note for all AMR with Possible TCMR / Mixed
                toMatch <- c("Possible Chronic Active TCMR II", "Possible Acute TCMR IIA", "Possible Acute TCMR IIB", "Possible Acute TCMR III")
                # matched <- grepl(paste(toMatch, collapse = "|"), TCMR_text, ignore.case = T)
                # matched
                tcmr_count <- c(diagnosis_TCMR, diagnosis_TCMR_nonCh) %in% c(ATCMRIA, ATCMRIB, ATCMRIIA, ATCMRIIB, ATCMRIII, CATCMRIA, CATCMRIB, CATCMRII)
                possible_tcmr_count <- c(TCMR_text, TCMR_text_nonCh) %in% toMatch
                
                if (diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) & sum(tcmr_count) > sum(possible_tcmr_count) ) {
                    text <- paste0(text,
                                   "\nMixed rejection: Test for gene transcripts/classifiers associated with AMR and TCMR")
                } else if (diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) & any(possible_tcmr_count) ) {
                    text <- paste0(text,
                                   "\nPossible Mixed rejection: Test for gene transcripts/classifiers associated with AMR and TCMR")
                }
                # note for BOR
                if (diagnosis_TCMR_nonCh %in% BOR) {
                    text <- paste0(text,
                                   "\nBorderline for acute TCMR: Test for gene transcripts/classifiers associated with TCMR")
                }
                # note for at least one equivocal
                if (grepl("Equivocal", AMR_text, ignore.case = T)) {
                    text <- paste0(text,
                                   "\nEquivocal for diagnosis of AMR: Test for gene transcripts/classifiers associated with AMR")
                }
                
                # note for Chronic inactive AMR
                if (diagnosis_AMR %in% CIAMR) {
                    text <- paste0(text,
                                   "\nChronic inactive AMR: Test for gene transcripts/classifiers associated with AMR")
                }
                # note for electron microscopy for cg and ptcml
                if ((tmpDF_original$cg == "" | tmpDF_original$cg == "NA" | is.na(tmpDF_original$cg)) & (tmpDF_original$ptcml == "" | tmpDF_original$ptcml == "NA" | is.na(tmpDF_original$ptcml))) {
                    text <- paste0(text,
                                   "\nIf you have an evidence for active or chronic AMR, you could perform electron microscopy to precise the diagnostic (cg, ptcml)")
                }
                
                if (!(is.na(tmpDF_original$cv) | tmpDF_original$cv %in% c("NA", "0")) ) {
                    text <- paste0(text,
                                   "\ncv > 0: please make sure if new onset or proliferative")
                }
                
                if (tmpDF2$PVL > 0 & !is.na(tmpDF2$PVL)) {
                    text <- paste0(text,
                                   "\nPVN: Diagnosis of TCMR might be mistaken")
                }
                
                text
            }
        })
    })
        
    
    # output$debug <- renderDataTable({
    #     tmpDF2 <- makeGraph()
    #     tmpDF2
    #     
    # })
    # 
    # output$debug2 <- renderText({
    #     # tmpDF2 <- makeGraph()
    #     tmpDF <- makeDF()
    #     tmpDF <- 
    #         tmpDF %>% 
    #         mutate(C4d = case_when(
    #             ABO %in% 1 ~ NA_real_,
    #             TRUE ~ C4d
    #         ))
    #     
    #     tmpDF <- tmpDF %>%
    #         mutate(g_cal = replace(g, is.na(g), 0),
    #                ptc_cal = replace(ptc, is.na(ptc), 0))
    #     
    #     tmpDF$ptc > 0 & !tmpDF$diagnosis_TCMR_nonCh %in% c(53:59) & tmpDF$cg == 0 & tmpDF$cv == 0 & tmpDF$ptcml == 0 & tmpDF$C4d > 1 & tmpDF$C4d_type == "IF"
    #     # c(tmpDF2$diagnosis_AMR)
    #     
    # })
    
    observeEvent(input$action, {
        output$caption_diagnosis <- renderText({
            
            "Banff Diagnosis"
            
            # if (input$C4d == "" | input$C4d_type == "" | input$t == "" | input$i == "" | input$ci == "" | input$ct == "" |
            #     input$v == "" | input$g == "" | input$ptc == "" | input$cg == "" | input$i_IFTA == "" | 
            #     # input$recurrent == "" |
            #     input$cv == "" | input$ti == "" | input$TMA == "" | input$ATI == "" | input$ptcml == "" | input$t_IFTA == "" |
            #     input$PVL == "" | input$DSA == "" | input$mmarker == "" | input$prior == "") return(NULL)
            # else {
            #     "Banff Diagnosis"
            # }
        })
    })
    
    observeEvent(input$action, {
        output$caption_notes <- renderText({
            "Notes and Suggestions"
            # if (input$C4d == "" | input$C4d_type == "" | input$t == "" | input$i == "" | input$ci == "" | input$ct == "" |
            #     input$v == "" | input$g == "" | input$ptc == "" | input$cg == "" | input$i_IFTA == "" | 
            #     # input$recurrent == "" |
            #     input$cv == "" | input$ti == "" | input$TMA == "" | input$ATI == "" | input$ptcml == "" | input$t_IFTA == "" |
            #     input$PVL == "" | input$DSA == "" | input$mmarker == "" | input$prior == "") return(NULL)
            # else {
            #     "Notes and Suggestions"
            # }
        })
    })
    
    
    url1 <- a("The Banff 2017 Kidney Meeting Report: Revised diagnostic
              criteria for chronic active T cell–mediated rejection, antibody‐mediated rejection,
              and prospects for integrative endpoints for next‐generation clinical trials",
              href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5817248/")
    url2 <- a("The Banff 2019 Kidney Meeting Report (I): Updates on and clarification of criteria for T cell– and antibody‐mediated rejection", 
              href = "https://onlinelibrary.wiley.com/doi/full/10.1111/ajt.15898")
    url3 <- a("The Banff Working Group Classification of Definitive Polyomavirus Nephropathy: Morphologic Definitions and Clinical Correlations",
              href = "https://jasn.asnjournals.org/content/29/2/680.long")
    url4 <- a("The Banff 2015 Kidney Meeting Report: Current Challenges in Rejection Classification and Prospects for Adopting Molecular Pathology",
              href = "https://onlinelibrary.wiley.com/doi/full/10.1111/ajt.14107")
    url5 <- a("Molecular Diagnosis of Antibody‐Mediated Rejection in Human Kidney Transplants",
              href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/ajt.12150")
    url6 <- a("NK Cell Transcripts and NK Cells in Kidney Biopsies from Patients with Donor‐Specific Antibodies: Evidence for NK Cell Involvement in Antibody‐Mediated Rejection",
              href = "https://onlinelibrary.wiley.com/doi/full/10.1111/j.1600-6143.2010.03201.x")
    url7 <- a("Molecular Diagnosis of T Cell‐Mediated Rejection in Human Kidney Transplant Biopsies",
              href = "https://onlinelibrary.wiley.com/doi/full/10.1111/ajt.12079")
    url8 <- a("Molecular Phenotypes of Acute Kidney Injury in Kidney Transplants",
              href = "https://jasn.asnjournals.org/content/23/5/948.short")
    
    output$text1 <- renderText({
        paste("DSA: Donor-specific antibody",
              "TMA: Acute Thrombotic Microangiopathy in the absence of any other cause",
              "ATI: Acute Tubular Injury in the absence of any other apparent cause",
              "ptcml: Peritubular Capillary Basement Membrane Multilayering",
              "BL: Borderline for Acute TCMR",
              "PVL: Polyomavirus load",
              "PVL 0: 0% positive tubules/ducts",
              "PVL 1: 0% ~ 1% positive tubules/ducts", 
              "PVL 2: 1% ~ 10% positive tubules/ducts",
              "PVL 3: > 10% positive tubules/ducts",
              "CNI: calcineurin inhibitor",
              # "IgA: recurrent IgA nephropathy",
              # "FSGS: Recurrence of focal segmental glomerulosclerosis",
              # "MPGN: Membranoproliferative glomerulonephritis",
              # "aHUS: Atypical hemolytic uremic syndrome",
              # "MGN: Membranous glomerulonephritis",
              sep = "\n")
    })
    
    
    
    output$ref1 <- renderUI({
        tagList("1. ", url1)
    })
    output$ref2 <- renderUI({
        tagList("2. ", url2)
    })
    output$ref3 <- renderUI({
        tagList("3. ", url3)
    })
    output$ref4 <- renderUI({
        tagList("4. ", url4)
    })
    output$ref5 <- renderUI({
        tagList("5. ", url5)
    })
    output$ref6 <- renderUI({
        tagList("6. ", url6)
    })
    output$ref7 <- renderUI({
        tagList("7. ", url7)
    })
    output$ref8 <- renderUI({
        tagList("8. ", url8)
    })
    
    
    #### generate report ####
    
    
    generateReport <- reactive({
        data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
        
        data_origin2 <- data_origin
        
        for (i in 1:ncol(data_origin2)) {
            if (class(data_origin2[, i]) == "character") {
                data_origin2[which(data_origin2[, i] == "NA"), i] <- NA
            }
        }
        
        
        data_origin2 <-
            data_origin2 %>% 
            rename(ABO = ABO_incompatibility) %>% 
            mutate(ABO = ifelse(is.na(ABO) | ABO %in% c("", "NA", "No"), 0, 1),
                   mmarker = case_when(mmarker == "No" ~ 0,
                                       mmarker == "AMR Molecular Marker" ~ 1),
                   DSA = case_when(DSA %in% c("No", 0) ~ 0,
                                   DSA %in% c("Yes", 1) ~ 1),
                   prior_diagnosis = case_when(prior_diagnosis == "No" ~ 0,
                                               prior_diagnosis == "Active AMR or Chronic active AMR" ~ 1),
                   v = case_when(is.na(v) ~ 0,
                                 TRUE ~ as.numeric(v)),
                   cv_original = cv,
                   cv = case_when(cv %in% c("new onset/prolif. cv1", "new onset/prolif. cv2", "new onset/prolif. cv3") ~ 1,
                                  TRUE ~ 0),
                   cg_original = cg,
                   cg = case_when(cg == "0" | cg == 0 ~ 0,
                                  cg == "1a" | cg == 1 ~ 1,
                                  cg == "1b" | cg == 1 ~ 1,
                                  cg == "2" | cg == 2 ~ 2,
                                  cg == "3" | cg == 3 ~ 3,
                                  TRUE ~ 0),
                   ptcml = case_when(is.na(ptcml) ~ 0,
                                     TRUE ~ as.numeric(ptcml)),
                   i_IFTA = case_when(is.na(i_IFTA) ~ 0,
                                      TRUE ~ as.numeric(i_IFTA)),
                   TMA = case_when(is.na(TMA) | TMA %in% "No TMA" ~ 0,
                                   TMA %in% "TMA unrelated to AMR" ~ 1,
                                   TMA %in% "TMA related to AMR" ~ 2,
                                   TRUE ~ as.numeric(TMA)),
                   ATI = case_when(is.na(ATI) | ATI %in% "No ATI" ~ 0,
                                   ATI %in% "ATI" ~ 1,
                                   TRUE ~ as.numeric(ATI)),
                   other_diagnoses = ifelse(other_diagnoses %in% "NA", NA, other_diagnoses)
            )
        
        tmp_ls <- str_split(data_origin2$other_diagnoses, pattern = "/")
        
        col_num <- c("C4d", "t", "i", "v", "g", "ptc", "cg", "ci", "ct", "i_IFTA", "PVL", "ah", "mm", "cv",
                     "ti", "TMA", "ATI", "ptcml", "DSA", "mmarker", "prior_diagnosis", "number_glomeruli", "number_artery")
        data_origin2[col_num] <- lapply(data_origin2[col_num], as.numeric)
        
        data_origin2 <- 
            data_origin2 %>% 
            mutate(C4d = case_when(
                ABO %in% 1 ~ NA_real_,
                TRUE ~ C4d
            ))
        
        data <- data_origin2
        
        data <- data %>%
            mutate(g_cal = replace(g, is.na(g), 0),
                   ptc_cal = replace(ptc, is.na(ptc), 0)) %>%
            
            mutate(diagnosis_AMR = case_when(
                C4d > 1 & C4d_type == "IF" & t == 0 & v == 0 & g == 0 & ptc == 0 & cg == 0 & cv == 0 & TMA == 0 & ATI == 0 & ptcml == 0 ~ 1,
                C4d > 0 & C4d_type == "IHC" & t == 0 & v == 0 & g == 0 & ptc == 0 & cg == 0 & cv == 0 &  TMA == 0 & ATI == 0 & ptcml == 0 ~ 2,
                
                g > 0 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 3,
                g > 0 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 4,
                g > 0 & cg == 0 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & DSA == 1 ~ 5,
                g > 0 & cg == 0 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & mmarker == 1 ~ 6,
                
                g > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 7,
                g > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 8,
                g > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & DSA == 1 ~ 9,
                g > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & mmarker == 1 ~ 10,
                
                ptc > 0 & !(i > 0 & t > 0) & cg == 0 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 11,
                ptc > 0 & !(i > 0 & t > 0) & cg == 0 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 12,
                ptc > 0 & !(i > 0 & t > 0) & cg == 0 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & DSA == 1 ~ 13,
                ptc > 0 & !(i > 0 & t > 0) & cg == 0 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & mmarker == 1 ~ 14,
                
                ptc > 0 & !(i > 0 & t > 0) & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 15,
                ptc > 0 & !(i > 0 & t > 0) & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 16,
                ptc > 0 & !(i > 0 & t > 0) & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & DSA == 1 ~ 17,
                ptc > 0 & !(i > 0 & t > 0) & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & g_cal + ptc_cal >= 2 & mmarker == 1 ~ 18,
                
                v > 0 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 19,
                v > 0 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 20,
                v > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 21,
                v > 0 & cg > 0 & TMA == 1 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 22,
                
                TMA == 2 & cg %in% c(0, NA, "NA") & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 23,
                TMA == 2 & cg %in% c(0, NA, "NA") & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 24,
                
                ATI == 1 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 1 & C4d_type == "IF" ~ 25,
                ATI == 1 & cg == 0 & cv == 0 & ptcml == 0 & C4d > 0 & C4d_type == "IHC" ~ 26,
                
                cg > 0 & TMA %in% c(0, 2) & C4d > 1 & C4d_type == "IF" ~ 27,
                cg > 0 & TMA %in% c(0, 2) & C4d > 0 & C4d_type == "IHC" ~ 28,
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & (is.na(i) | is.na(t)) & g >= 1 & DSA == 1 ~ 29, # in case when i or t are NA but all other criteria are met
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & DSA == 1 ~ 29,
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & mmarker == 1 ~ 30,
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & (is.na(i) | is.na(t)) & g >= 1 & mmarker == 1 ~ 30, # in case when i or t are NA but all other criteria are met
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & g >= 1 & (i > 0 & t > 0) & DSA == 1 ~ 31,
                cg > 0 & TMA %in% c(0, 2) & g_cal + ptc_cal >= 2 & g >= 1 & (i > 0 & t > 0) & mmarker == 1 ~ 32,
                
                cv > 0 & C4d > 1 & C4d_type == "IF" ~ 33,
                cv > 0 & C4d > 0 & C4d_type == "IHC" ~ 34,
                cv > 0 & g_cal + ptc_cal >= 2 & (is.na(i) | is.na(t)) & g >= 1 & DSA == 1 ~ 35, # in case when i or t are NA but all other criteria are met
                cv > 0 & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & DSA == 1 ~ 35,
                cv > 0 & g_cal + ptc_cal >= 2 & (is.na(i) | is.na(t)) & g >= 1 & mmarker == 1 ~ 36, # in case when i or t are NA but all other criteria are met
                cv > 0 & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & mmarker == 1 ~ 36,
                cv > 0 & g_cal + ptc_cal >= 2 & (i > 0 & t > 0) & g >= 1 & DSA == 1 ~ 37,
                cv > 0 & g_cal + ptc_cal >= 2 & (i > 0 & t > 0) & g >= 1 & mmarker == 1 ~ 38,
                
                ptcml > 0 & C4d > 1 & C4d_type == "IF" ~ 39,
                ptcml > 0 & C4d > 0 & C4d_type == "IHC" ~ 40,
                ptcml > 0 & g_cal + ptc_cal >= 2 & (is.na(i) | is.na(t)) & g >= 1 & DSA == 1 ~ 41, # in case when i or t are NA but all other criteria are met
                ptcml > 0 & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & DSA == 1 ~ 41,
                ptcml > 0 & g_cal + ptc_cal >= 2 & (is.na(i) | is.na(t)) & g >= 1 & mmarker == 1 ~ 42, # in case when i or t are NA but all other criteria are met
                ptcml > 0 & g_cal + ptc_cal >= 2 & !(i > 0 & t > 0) & mmarker == 1 ~ 42,
                ptcml > 0 & g_cal + ptc_cal >= 2 & (i > 0 & t > 0) & g >= 1 &  DSA == 1 ~ 43,
                ptcml > 0 & g_cal + ptc_cal >= 2 & (i > 0 & t > 0) & g >= 1 & mmarker == 1 ~ 44,
                
                cg > 0 & g_cal + ptc_cal <= 1 & !(is.na(g) & is.na(ptc)) & C4d <= 1 & C4d_type == "IF" & DSA == 1 ~ 45,
                cg > 0 & g_cal + ptc_cal <= 1 & !(is.na(g) & is.na(ptc)) & C4d <= 1 & C4d_type == "IF" & prior_diagnosis == 1 ~ 46,
                cg > 0 & g_cal + ptc_cal <= 1 & !(is.na(g) & is.na(ptc)) & C4d == 0 & C4d_type == "IHC" & DSA == 1 ~ 47,
                cg > 0 & g_cal + ptc_cal <= 1 & !(is.na(g) & is.na(ptc)) & C4d == 0 & C4d_type == "IHC" & prior_diagnosis == 1 ~ 48,
                
                ptcml > 0 & g_cal + ptc_cal <= 1 & !is.na(g) & !is.na(ptc) & C4d <= 1 & C4d_type == "IF" & DSA == 1 ~ 49,
                ptcml > 0 & g_cal + ptc_cal <= 1 & !is.na(g) & !is.na(ptc) & C4d <= 1 & C4d_type == "IF" & prior_diagnosis == 1 ~ 50,
                ptcml > 0 & g_cal + ptc_cal <= 1 & !is.na(g) & !is.na(ptc) & C4d == 0 & C4d_type == "IHC" & DSA == 1 ~ 51,
                ptcml > 0 & g_cal + ptc_cal <= 1 & !is.na(g) & !is.na(ptc) & C4d == 0 & C4d_type == "IHC" & prior_diagnosis == 1 ~ 52,
                
                TRUE ~ 63)) %>%
            
            mutate(diagnosis_TCMR = case_when(
                
                # AMR, CATCMR IA/IB + II or not  --> graph CATCMR IA/IB , text add possible
                diagnosis_AMR %in% c(CAAMR, CIAMR) & (t == 3 | t_IFTA == 3) & ti >= 2 & i_IFTA >= 2 ~ 61,
                diagnosis_AMR %in% c(CAAMR, CIAMR) & (t == 2 | t_IFTA == 2) & ti >= 2 & i_IFTA >= 2 ~ 60,
                # cv > 0 & conditions for CATCMR IA/IB & AMR 93 --> both graph text
                # other cases should be Possible. No graph
                cv > 0 ~ 62,
                
                (t == 3 | t_IFTA == 3) & ti >= 2 & i_IFTA >= 2 ~ 61,
                (t == 2 | t_IFTA == 2) & ti >= 2 & i_IFTA >= 2 ~ 60,
                
                TRUE ~ 63
            ),
            diagnosis_TCMR_nonCh = case_when(
                (t > 0 & i == 1 & v == 0) | (t > 0 & i == 1 & v > 0 & !diagnosis_AMR %in% c(C4d_stain, 63)) ~ 53,
                (t == 1 & i >= 1 & v == 0) | (t == 1 & i >= 1 & v > 0 & !diagnosis_AMR %in% c(C4d_stain, 63)) ~ 54,
                
                # regardless of i or t, thus should be priotized over Acute TCMR IA and IB
                # but in case of AMR and TCMR IA, IB
                diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) & t == 2 & i >= 2 ~ 55,
                diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) & t == 3 & i >= 2 ~ 56,
                
                v == 1 ~ 57,
                v == 2 ~ 58,
                v == 3 ~ 59,
                
                t == 2 & i >= 2 ~ 55,
                t == 3 & i >= 2 ~ 56,
                
                TRUE ~ 63
            )) %>%
            
            mutate(diagnosis_IFTA = case_when(
                (ci == 1 & ct <= 1) | (ci <= 1 & ct == 1) ~ 1,
                (ci == 2 & ct <= 2) | (ci <= 2 & ct == 2) ~ 2,
                (ci == 3 & ct <= 3) | (ci <= 3 & ct == 3) ~ 3,
                TRUE ~ 4
            )) %>%
            
            mutate(diagnosis_BK = case_when(
                PVL == 1 & ci <= 1 ~ 1,
                (PVL == 1 & ci >= 2) | PVL == 2 | (PVL == 3 & ci <= 1) ~ 2,
                PVL == 3 & ci >= 2 ~ 3,
                TRUE ~ 4
            ))
        
        
        data <- 
            data %>% 
            mutate(AMR_text = case_when(
                diagnosis_AMR %in% C4d_stain ~ "C4d Staining Without Evidence of Rejection",
                # diagnosis_AMR %in% 23:24 & ATI %in% 1 ~ "Equivocal for diagnosis of Active AMR (other causes of TMA and ATI should be ruled out)",
                # diagnosis_AMR %in% 23:24 ~ "Equivocal for diagnosis of Active AMR (other causes of TMA should be ruled out)",
                diagnosis_AMR %in% 25:26 ~ "Equivocal for diagnosis of Active AMR (other causes of ATI should be ruled out)",
                ############## change
                g_cal + ptc_cal >= 2 & g_cal >= 1 & diagnosis_AMR %in% 63 ~ "Equivocal for diagnosis of AMR",
                
                diagnosis_AMR %in% AAMR ~ "Active AMR",
                diagnosis_AMR %in% CAAMR ~ "Chronic active AMR",
                diagnosis_AMR %in% CIAMR ~ "Chronic inactive AMR",
                TRUE ~ "No evidence of AMR")
                   
            )
        

        
        
        data <- data %>% 
            mutate(
                TCMR_text = case_when(
                    
                    # AMR, CATCMR IA/IB + II or not  --> graph CATCMR IA/IB , text add possible
                    diagnosis_TCMR %in% CATCMRIA & cv > 0 ~ "Chronic Active TCMR IA; Possible Chronic Active TCMR II",
                    diagnosis_TCMR %in% CATCMRIA ~ "Chronic Active TCMR IA",
                    diagnosis_TCMR %in% CATCMRIB & cv > 0 ~ "Chronic Active TCMR IB; Possible Chronic Active TCMR II",
                    diagnosis_TCMR %in% CATCMRIB ~ "Chronic Active TCMR IB",
                    # cv > 0 & conditions for CATCMR IA/IB & AMR 93 or C4d stain --> both graph text
                    # other cases should be Possible. No graph
                    diagnosis_TCMR %in% CATCMRII & diagnosis_AMR %in% c(C4d_stain, 63) & ti >= 2 & i_IFTA >= 2 & (t >= 2 | t_IFTA >= 2) ~ "Chronic Active TCMR II",
                    diagnosis_TCMR %in% CATCMRII ~ "Possible Chronic Active TCMR II",
                    TRUE ~ NA_character_),
                
                TCMR_text_nonCh = case_when(
                    diagnosis_TCMR_nonCh %in% BOR & v %in% 1 & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Suspicious (Borderline) for Acute TCMR; Possible Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% BOR & v %in% 2 & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Suspicious (Borderline) for Acute TCMR; Possible Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% BOR & v %in% 3 & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Suspicious (Borderline) for Acute TCMR; Possible Acute TCMR III",
                    diagnosis_TCMR_nonCh %in% BOR ~ "Suspicious (Borderline) for Acute TCMR",
                    
                    diagnosis_TCMR_nonCh %in% ATCMRIA & v == 1 ~ "Acute TCMR IA; Possible Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% ATCMRIA & v == 2 ~ "Acute TCMR IA; Possible Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% ATCMRIA & v == 3 ~ "Acute TCMR IA; Possible Acute TCMR III",
                    diagnosis_TCMR_nonCh %in% ATCMRIA ~ "Acute TCMR IA",
                    diagnosis_TCMR_nonCh %in% ATCMRIB & v == 1 ~ "Acute TCMR IB; Possible Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% ATCMRIB & v == 2 ~ "Acute TCMR IB; Possible Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% ATCMRIB & v == 3 ~ "Acute TCMR IB; Possible Acute TCMR III",
                    diagnosis_TCMR_nonCh %in% ATCMRIB ~ "Acute TCMR IB",
                    diagnosis_TCMR_nonCh %in% ATCMRIIA & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Possible Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% ATCMRIIA ~ "Acute TCMR IIA",
                    diagnosis_TCMR_nonCh %in% ATCMRIIB & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Possible Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% ATCMRIIB ~ "Acute TCMR IIB",
                    diagnosis_TCMR_nonCh %in% ATCMRIII & !diagnosis_AMR %in% c(C4d_stain, 63) ~ "Possible Acute TCMR III",
                    diagnosis_TCMR_nonCh %in% ATCMRIII ~ "Acute TCMR III",
                    TRUE ~ NA_character_
                ),
                
                TCMR_text = case_when(
                    # is.na(TCMR_text) & is.na(TCMR_text_nonCh) & t >= 2 & (ti >= 2 | cv_original %in% c("2", "3", "new onset/prolif. cv2", "new onset/prolif. cv3")) ~ "Possible Chronic Active TCMR",
                    TRUE ~ TCMR_text
                ),
                
                IFTA_text = case_when(
                    diagnosis_IFTA %in% grade1_belong ~ "Grade I",
                    diagnosis_IFTA %in% grade2_belong ~ "Grade II", 
                    diagnosis_IFTA %in% grade3_belong ~ "Grade III",
                    ### only for report
                    diagnosis_IFTA %in% normal_IFTA & (is.na(ci) | is.na(ct)) ~ "ci or ct are missing",
                    TRUE ~ "No IFTA"),
                
                BK_text = case_when(
                    diagnosis_BK %in% class1_belong ~ "Class 1",
                    diagnosis_BK %in% class2_belong ~ "Class 2", 
                    diagnosis_BK %in% class3_belong ~ "Class 3", 
                    diagnosis_BK %in% normal_BK & PVL > 0 & !is.na(PVL) ~ "ci is needed for classification",
                    diagnosis_BK %in% normal_BK & is.na(PVL) ~ "NA",
                    TRUE ~ "No PVN"),
                
                
                # note for ABO, C4d
                note_ABO = case_when(
                    ABO %in% 1 & !(data_origin$C4d == "NA" | is.na(data_origin$C4d)) & data_origin$C4d > 0 ~ "In the context of ABO incompatibility, positive C4d is not interpretable",
                    TRUE ~ NA_character_
                ),
                
                # # not for missing C4d type
                # note_C4d_type = case_when(
                #     !(data_origin$C4d == "NA" | is.na(data_origin$C4d)) & (is.na(C4d_type) | C4d_type == "NA") ~ "Please fill C4d type in",
                #     TRUE ~ NA_character_
                # ),
                
                # note for TMA, ATI
                note_TMA_ATI = case_when(
                    TMA %in% 2 & ATI %in% 1 ~ "Make sure other causes of TMA or ATI are ruled out",
                    TMA %in% 2 ~ "Make sure other causes of TMA are ruled out",
                    ATI %in% 1 ~ "Make sure other causes of ATI are ruled out",
                    TRUE ~ NA_character_
                ),
                
                # note for cg, TMA, AMR
                note_cg_TMA_AAMR = case_when(
                    diagnosis_AMR %in% c(7:10, 15:18, 21:22) & TMA %in% 1 ~ "cg is not considered because it could be due to TMA",
                    TRUE ~ NA_character_
                ),
                
                # note for non DSA
                note_DSA = case_when(
                    (diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) | grepl("Equivocal", AMR_text, ignore.case = T)) & is.na(DSA) ~ "Test for Anti-HLA DSA",
                    (diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) | grepl("Equivocal", AMR_text, ignore.case = T)) & DSA %in% 0 ~ "Test for non Anti-HLA Antibodies. Test for gene transcripts/classifiers associated with AMR",
                    (is.na(data_origin$C4d) | data_origin$C4d == "NA") & is.na(DSA) ~ "DSA and C4d tests are recommended for AMR diagnosis",
                    is.na(data_origin$C4d) | data_origin$C4d == "NA" ~ "C4d test is recommended for AMR diagnosis",
                    is.na(DSA) ~ "DSA test is recommended for AMR diagnosis",
                    TRUE ~ NA_character_
                ),
                
                # note for i-IFTA
                note_iIFTA = case_when(
                    i_IFTA > 0 & is.na(PVL) ~ "Make sure other known causes of i-IFTA such as pyelonephritis and polyomavirus nephropathy ruled out; test for gene transcripts/classifiers associated with AMR and TCMR, virus-specific transcripts",
                    i_IFTA > 0 ~ "i-IFTA: Test for gene transcripts/classifiers associated with AMR and TCMR, virus-specific transcripts"),
                # note for TMA
                # note_TMA = case_when(
                #     TMA == 1 & diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) ~ "TMA: test molecular markers for AMR"),
                
                # note for DSA pos, C4d neg, g + ptc == 1
                note_DSA_C4d_g_ptc = case_when(
                    DSA == 1 & C4d == 0 & g + ptc == 1 ~ "DSA positive, C4d negative, g + ptc = 1: Test for gene transcripts/classifiers associated with AMR",
                    TRUE ~ NA_character_),
                # note for isolated v
                note_isolated_v = case_when(
                    v > 0 & g + ptc <= 1 & i + t <= 1 ~ "isolated v: Test for gene transcripts/classifiers associated with AMR and TCMR",
                    TRUE ~ NA_character_),
                # note for C4d stains
                note_C4d = case_when(
                    diagnosis_AMR %in% C4d_stain ~ "C4d Staining: Test for gene transcripts/classifiers associated with AMR"),
                
                # note for all AMR with Possible TCMR / Mixed
                note_AMR_PTCMR_Mixed = case_when(
                    diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) & 
                    (diagnosis_TCMR %in% c(ATCMRIA, ATCMRIB, ATCMRIIA, ATCMRIIB, ATCMRIII, CATCMRIA, CATCMRIB, CATCMRII) + diagnosis_TCMR_nonCh %in% c(ATCMRIA, ATCMRIB, ATCMRIIA, ATCMRIIB, ATCMRIII, CATCMRIA, CATCMRIB, CATCMRII)) >
                        (TCMR_text %in% toMatch + TCMR_text_nonCh %in% toMatch) ~ "Mixed rejection: Test for gene transcripts/classifiers associated with AMR and TCMR",
                    diagnosis_AMR %in% c(AAMR, CAAMR, CIAMR) & (TCMR_text %in% toMatch + TCMR_text_nonCh %in% toMatch) > 0 ~ "Possible Mixed rejection: Test for gene transcripts/classifiers associated with AMR and TCMR"),
                    
                # note for BOR
                note_BOR = case_when(
                    diagnosis_TCMR_nonCh %in% BOR ~ "Borderline for acute TCMR: Test for gene transcripts/classifiers associated with TCMR"),
                # note for at least one equivocal
                note_suspicious = case_when(
                    grepl("Equivocal", AMR_text, ignore.case = T) ~ "Equivocal for diagnosis of AMR: Test for gene transcripts/classifiers associated with AMR"),
                # note for Chronic inactive AMR
                note_chronic_inactiveAMR = case_when(
                    diagnosis_AMR %in% CIAMR ~ "Chronic inactive AMR: Test for gene transcripts/classifiers associated with AMR"),
                # note for electron microscopy
                note_micro = case_when(
                    (is.na(data_origin$cg) | data_origin$cg == "NA") & (is.na(data_origin$ptcml) | data_origin$ptcml == "NA") ~ "If you have an evidence for active or chronic AMR, you could perform electron microscopy to precise the diagnostic (cg, ptcml)"),
                #note for adequacy
                note_adequate = case_when(
                    number_glomeruli >= 10 & number_artery >= 2 ~ 1,
                    number_glomeruli >= 7 & number_artery >= 1 ~ 2,
                    number_glomeruli < 7 | number_artery < 1 ~ 3,
                    TRUE ~ 4
                ),
                # note cv lesion
                note_cv = case_when(
                    data_origin$cv %in% c(1:3, "1", "2", "3", "new onset/prolif. cv1", "new onset/prolif. cv2", "new onset/prolif. cv3") ~ "cv > 0: please make sure if new onset or proliferative"
                ),
                # note recurrent disease
                other_diagnoses_warning = case_when(
                    any(!other_diagnoses %in% c("None", "Suspicious for CNI toxicity", "Diabetic nephropathy")) ~ "Rejection diagnosis should be interpreted regarding associated other diagnoses."
                ),
                # note PVN
                note_PVN = case_when(
                    PVL > 0 ~ "PVN: Diagnosis of TCMR might be mistaken"
                ),
                # note t or i NA
                note_ti_NA = case_when(
                    diagnosis_AMR %in% c(29, 30, 35, 36, 41, 42) & (is.na(t) | is.na(i)) ~ "t or i missing: Diagnosis of Borderline cannot be concluded"
                )
            )
        
        data$other_diagnoses_warning <-
            sapply(1:length(tmp_ls), function(x){
            any(!grepl(other_diagnoses_pattern, tmp_ls[[x]])) & !all(is.na(tmp_ls[[x]]))
        })
        
        data <-
            data %>% 
            mutate(
                other_diagnoses_warning = ifelse(other_diagnoses_warning == T, "Rejection diagnosis should be interpreted regarding associated other diagnoses.",
                                               NA)
            )
        data
    })
    
    #  chooseColor_report <- reactive({
    #     data <- generateReport()
    #     final_color <- list()
    #     
    #     for (i in 1:nrow(generateReport())) {
    #         each_biopsy <- data[i, ]
    #         each_AMR <- each_biopsy[, "diagnosis_AMR"]
    #         each_TCMR <- each_biopsy[, "diagnosis_TCMR"]
    #         each_TCMR_nonCh <- each_biopsy[, "diagnosis_TCMR_nonCh"]
    #         
    #         AMR_color <- ifelse(each_AMR %in% C4d_stain, "grey",
    #                             ifelse(each_AMR %in% AAMR, "red",
    #                                    ifelse(each_AMR %in% CAAMR, "#ED7117",
    #                                           ifelse(each_AMR %in% CIAMR, "#FFA500", "white"))))
    #         
    #         TCMR_color_noCh <- ifelse(each_TCMR_nonCh %in% BOR, "lightblue",
    #                                   ifelse(each_TCMR_nonCh %in% ATCMRIA, "darkblue",
    #                                          ifelse(each_TCMR_nonCh %in% ATCMRIB, "darkblue",
    #                                                 ifelse(each_TCMR_nonCh %in% ATCMRIIA, "darkblue",
    #                                                        ifelse(each_TCMR_nonCh %in% ATCMRIIB, "darkblue",
    #                                                               ifelse(each_TCMR_nonCh %in% ATCMRIII, "darkblue", "white"))))))
    #         
    #         TCMR_color <- ifelse(each_TCMR %in% CATCMRIA, "turquoise",
    #                              ifelse(each_TCMR %in% CATCMRIB, "turquoise", 
    #                                     ifelse(each_TCMR %in% CATCMRII, "turquoise", "white")))
    #         
    #         
    #         TCMR_text <- data$TCMR_text[i]
    #         TCMR_text_nonCh <- data$TCMR_text_nonCh[i]
    #         if (TCMR_text %in% toMatch) {
    #             TCMR_color <- "white"
    #         }
    #         if (TCMR_text_nonCh %in% toMatch) {
    #             TCMR_color_noCh <- "white"
    #         }
    #         
    #         
    #         color1 <- c(
    #             rep("white", tmpls[[each_AMR]][1]),
    #             rep(AMR_color, tmpls[[each_AMR]][2]),
    #             rep("white", tmpls[[each_AMR]][3]),
    #             rep(AMR_color, tmpls[[each_AMR]][4]),
    #             rep("white", tmpls[[each_AMR]][5]),
    #             rep(AMR_color, tmpls[[each_AMR]][6]),
    #             rep("white", tmpls[[each_AMR]][7]),
    #             rep(AMR_color, tmpls[[each_AMR]][8]),
    #             rep("white", tmpls[[each_AMR]][9])
    #             )
    #         
    #         color2 <- c(
    #             rep("white", tmpls[[each_TCMR]][1]),
    #             rep(TCMR_color, tmpls[[each_TCMR]][2]),
    #             rep("white", tmpls[[each_TCMR]][3]),
    #             rep(TCMR_color, tmpls[[each_TCMR]][4]),
    #             rep("white", tmpls[[each_TCMR]][5]),
    #             rep(TCMR_color, tmpls[[each_TCMR]][6]),
    #             rep("white", tmpls[[each_TCMR]][7]),
    #             rep(TCMR_color, tmpls[[each_TCMR]][8]),
    #             rep("white", tmpls[[each_TCMR]][9])
    #             )
    #         
    #         color3 <- c(
    #             rep("white", tmpls[[each_TCMR_nonCh]][1]),
    #             rep(TCMR_color_noCh, tmpls[[each_TCMR_nonCh]][2]),
    #             rep("white", tmpls[[each_TCMR_nonCh]][3]),
    #             rep(TCMR_color_noCh, tmpls[[each_TCMR_nonCh]][4]),
    #             rep("white", tmpls[[each_TCMR_nonCh]][5]),
    #             rep(TCMR_color_noCh, tmpls[[each_TCMR_nonCh]][6]),
    #             rep("white", tmpls[[each_TCMR_nonCh]][7]),
    #             rep(TCMR_color_noCh, tmpls[[each_TCMR_nonCh]][8]),
    #             rep("white", tmpls[[each_TCMR_nonCh]][9])
    #             )
    #         
    #         final_color_tmp <- ifelse(color1 != "white", color1,
    #                                   ifelse(color2 != "white", color2,
    #                                          ifelse(color3 != "white", color3, "white")))
    #         
    #         final_color[[i]] <- final_color_tmp
    #     }
    #     final_color
    # })
    
    # graph_report <- reactive({
    #     # req(input$file)
    #     if (!is.null(input$file_excel$datapath)) {
    #         data <- generateReport()
    #         final_color <- chooseColor_report()    
    #     }
    #     # data <- generateReport()
    #     # final_color <- chooseColor_report()
    #     graph_list <- list()
    # 
    #     for (i in 1:nrow(data)) {
    #         tmp <- quote(collapsibleTree(org, hierarchy = names(org), collapsed = F,
    #                         fontSize = 11, tooltip = T, root = "Biopsy", fillByLevel = F, linkLength = 140,
    #                         fill = c("chocolate",
    #                                  final_color[[i]])
    #                         , zoomable = T))
    #         graph_list[[i]] <- tmp
    #     }
    #     graph_list
    # })
    
    
    
    output$instruction <- renderText({
        "1. Please download the example file on the left hand side then open the file.
        \n2. Please Enable the Macros.
        \n3. Update the information using the drop-down lists. You do not need to use the example file but please use the same format. You can either choose xls/xlsx/xlsm file as an input file format.
        3a. Please keep the column names as they are.
        3b. You might add extra columns with different names.
        3c. Please use the drop-down lists as inputs. Otherwise, it might not generate a report properly.
        3d. In case of missing data, you can leave the blanks as NA or just empty.
        3e. Please fill in C4d type in case there is C4d score.
        \n4. Upload the excel file into the application.
        \n5. Please choose a type of the report you desire. PDF report(s) might take few seconds.
        \n6. Click 'Generate report'.
        \ncv > 0: please make sure if it is new onset or proliferative as the example file's dropdown.
        \nDepends on how many biopsies you upload, the processing time will vary.\nThe output file will be generated either as an Excel, a PDF, or a ZIP file that contain multiple PDF files.\nIt is not necessary to use the example file. However, please use the same format, same columns, and the same column names as the example file; they will be used to generate a report.\nPlease use the same format of each variable as used in the example file. If the data misses a required column, please make one with empty cells."
    })
    

    
    output$report_tab <- downloadHandler(
        filename = "report.xlsx",
        
        content = function(file) {
            data <- generateReport()
            data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
            
            
            scle_glom_text <- ifelse(data$sclerotic_glomeruli > 1, " are ", " is ")
            numb_glo_text <- ifelse(data$number_glomeruli > 1, " glomeruli ", " glomerule ")
            
            glomeruli_sclerotic <- ifelse(!(is.na(data$sclerotic_glomeruli) | is.na(data$number_glomeruli)), 
                                          paste0(" (", data$number_glomeruli, numb_glo_text, "of which ",
                                                 data$sclerotic_glomeruli, scle_glom_text, "sclerotic)"), "")
            
            data <-
                data %>% 
                select(Patient_id:other_diagnoses_free_note,
                       AMR_text, TCMR_text_nonCh, TCMR_text,
                       IFTA_text:BK_text, 
                       note_ABO:note_ti_NA) %>% 
                mutate(note_adequate = ifelse(note_adequate == 1, paste0("Adequate biopsy, diagnosis can be retained with confidence", glomeruli_sclerotic, "."),
                                             ifelse(note_adequate == 2, paste0("Narrow biopsy, diagnosis should be retained with caution", glomeruli_sclerotic, "."),
                                                    ifelse(note_adequate == 3, paste0("Inadequate biopsy, diagnosis cannot be retained", glomeruli_sclerotic, "."),
                                                           "No information of number of glomeruli or artery to determine the adequacy.")))) %>% 
                unite("Note", note_ABO:note_ti_NA, na.rm = T, remove = T, sep = "/")
            
            data_origin <-
                data_origin %>% 
                mutate(AMR_diagnosis = data$AMR_text,
                       TCMR_Acute_diagnosis = data$TCMR_text_nonCh,
                       TCMR_Chronic_diagnosis = data$TCMR_text,
                       IFTA = data$IFTA_text,
                       PVN = data$BK_text,
                       Note = data$Note) %>% 
                mutate(Note = gsub("/NA$", Note, replacement = ""),
                       Note = gsub("/NA", Note, replacement = "")) %>% glimpse()
            
            # data_origin[data_origin == "NA"] <- NA
            
            write_xlsx(data_origin, file)
        }
    )
    
    
    
    output$report.single <- downloadHandler(
        
        filename = "report.pdf",
        content = function(file) {
            
            
            if (!is.null(input$file_excel$datapath)) {
                data <- generateReport()
                # graph_color <- chooseColor_report()
                data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
                data_origin <- data_origin %>% mutate(Patient_id = as.character(Patient_id))
            }
            
            # src <- normalizePath("test1.Rmd")
            # owd <- setwd(tempdir())
            # on.exit(setwd(owd))
            # tempReport = file.path(tempdir(), "test1.Rmd")
            # file.copy(src, "test1.Rmd", overwrite = T)
            
            params = list(
                # graph_color = graph_color[[1]],
                Patient_id = data$Patient_id[1],
                date_transplant = data$date_transplant[1],
                date_biopsy = data$date_biopsy[1],
                C4d = data$C4d[1],
                C4d_type = data$C4d_type[1],
                t = data$t[1],
                i = data$i[1],
                v = data_origin$v[1],
                g = data$g[1],
                ptc = data$ptc[1],
                cg = data_origin$cg[1],
                ci = data$ci[1],
                ct = data$ct[1],
                i_IFTA = data_origin$i_IFTA[1],
                t_IFTA = data_origin$t_IFTA[1],
                ah = data_origin$ah[1],
                mm = data_origin$mm[1],
                PVL = data$PVL[1],
                cv = data_origin$cv[1],
                ti = data$ti[1],
                TMA = data_origin$TMA[1],
                ATI = data_origin$ATI[1],
                ptcml = data_origin$ptcml[1],
                DSA = data$DSA[1],
                mmarker = data$mmarker[1],
                prior_diagnosis = data$prior_diagnosis[1],
                AMR_text = data$AMR_text[1],
                TCMR_text = data$TCMR_text[1],
                TCMR_text_nonCh = data$TCMR_text_nonCh[1],
                IFTA_text = data$IFTA_text[1],
                BK_text = data$BK_text[1],
                note_ABO = data$note_ABO[1],
                # note_C4d_type = data$note_C4d_type[1],
                note_TMA_ATI = data$note_TMA_ATI[1],
                note_cg_TMA_AAMR = data$note_cg_TMA_AAMR[1],
                note_iIFTA = data$note_iIFTA[1],
                note_DSA_C4d_g_ptc = data$note_DSA_C4d_g_ptc[1],
                note_isolated_v = data$note_isolated_v[1],
                note_chronic_inactiveAMR = data$note_chronic_inactiveAMR[1],
                note_C4d = data$note_C4d[1],
                note_AMR_PTCMR_Mixed = data$note_AMR_PTCMR_Mixed[1],
                note_BOR = data$note_BOR[1],
                note_suspicious = data$note_suspicious[1],
                note_cv = data$note_cv[1],
                note_micro = data$note_micro[1],
                note_DSA = data$note_DSA[1],
                note_adequate = data$note_adequate[1],
                note_PVN = data$note_PVN[1],
                other_diagnoses = data_origin$other_diagnoses[1],
                other_diagnoses_warning = data$other_diagnoses_warning[1],
                other_diagnoses_free_note = data_origin$other_diagnoses_free_note[1],
                sclerotic_glomeruli = data_origin$sclerotic_glomeruli[1],
                number_glomeruli = data_origin$number_glomeruli[1],
                note_ti_NA = data$note_ti_NA[1]
            )
            
            withProgress(message = "Writing a report to Disk. Please wait...", value = 0, {
                for(i in 1:nrow(data)) {
                    incProgress(1 / nrow(data))
                    out <- render("test1.Rmd", params = params, output_file = file,
                                  envir = new.env(parent = globalenv()))
                }
            })
            
            # withProgress(message = "Writing a report to Disk. Please wait...", value = 0, {
            #     for(i in 1:nrow(data)) {
            #         incProgress(1 / nrow(data))
            #         Sys.sleep(35)
            #     }
            # })
            
            file.rename(out, file)
        }
    )
    
    
    output$report <- downloadHandler(
        
        
        filename = "report.zip",
        
        content = function(file) {
            
            fs <- c()
            if (!is.null(input$file_excel$datapath)) {
                data <- generateReport()
                # graph_color <- chooseColor_report()
                data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
                data_origin <- data_origin %>% mutate(Patient_id = as.character(Patient_id))
            }
            
            
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport = file.path(tempdir(), "test1.Rmd")
            file.copy("test1.Rmd", tempReport, overwrite = T)
            
            render_report = function(tmp_data, i) {
                rmarkdown::render(
                    "test1.Rmd", params = list(
                        # graph_color = graph_color[[i]],
                        Patient_id = data$Patient_id[i],
                        date_transplant = data$date_transplant[i],
                        date_biopsy = data$date_biopsy[i],
                        C4d = data_origin$C4d[i],
                        C4d_type = data$C4d_type[i],
                        t = data$t[i],
                        i = data$i[i],
                        v = data_origin$v[i],
                        g = data$g[i],
                        ptc = data$ptc[i],
                        cg = data_origin$cg[i],
                        ci = data$ci[i],
                        ct = data$ct[i],
                        i_IFTA = data_origin$i_IFTA[i],
                        t_IFTA = data_origin$t_IFTA[i],
                        ah = data_origin$ah[i],
                        mm = data_origin$mm[i],
                        PVL = data$PVL[i],
                        cv = data_origin$cv[i],
                        ti = data$ti[i],
                        TMA = data_origin$TMA[i],
                        ATI = data_origin$ATI[i],
                        ptcml = data_origin$ptcml[i],
                        DSA = data$DSA[i],
                        mmarker = data$mmarker[i],
                        prior_diagnosis = data$prior_diagnosis[i],
                        AMR_text = data$AMR_text[i],
                        TCMR_text = data$TCMR_text[i],
                        TCMR_text_nonCh = data$TCMR_text_nonCh[i],
                        IFTA_text = data$IFTA_text[i],
                        BK_text = data$BK_text[i],
                        note_ABO = data$note_ABO[i],
                        # note_C4d_type = data$note_C4d_type[i],
                        note_TMA_ATI = data$note_TMA_ATI[i],
                        note_cg_TMA_AAMR = data$note_cg_TMA_AAMR[i],
                        note_iIFTA = data$note_iIFTA[i],
                        note_DSA_C4d_g_ptc = data$note_DSA_C4d_g_ptc[i],
                        note_isolated_v = data$note_isolated_v[i],
                        note_chronic_inactiveAMR = data$note_chronic_inactiveAMR[i],
                        note_C4d = data$note_C4d[i],
                        note_cv = data$note_cv[i],
                        note_AMR_PTCMR_Mixed = data$note_AMR_PTCMR_Mixed[i],
                        note_BOR = data$note_BOR[i],
                        note_suspicious = data$note_suspicious[i],
                        note_micro = data$note_micro[i],
                        note_DSA = data$note_DSA[i],
                        note_adequate = data$note_adequate[i],
                        note_PVN = data$note_PVN[i],
                        other_diagnoses = data_origin$other_diagnoses[i],
                        other_diagnoses_warning = data$other_diagnoses_warning[i],
                        other_diagnoses_free_note = data_origin$other_diagnoses_free_note[i],
                        sclerotic_glomeruli = data_origin$sclerotic_glomeruli[i],
                        number_glomeruli = data_origin$number_glomeruli[i],
                        note_ti_NA = data$note_ti_NA[i]
                        
                    ),
                    output_file = paste0(data$Patient_id[i], ".report", ".pdf"), envir = new.env(parent = globalenv()),
                )
            }
            
            withProgress(message = "Writing Files to Disk. Please wait...", value = 0, {
                for(i in 1:nrow(data)) {
                    incProgress(1 / nrow(data))
                    fs <- c(fs, render_report(data, i))
                }
            })
            
            zip(zipfile = file, files = fs)
            
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app)
        }
        # contentType = "application/zip"
    )
    
    
    output$file1_ui <- renderUI({
        input$myconfirmation1
        fileInput("file_excel", "Choose xls/xlsx/xlsm File", multiple = F)
    })
    
    observeEvent(input$file_excel, {
        # data_origin <- as.data.frame(read_excel("~/Downloads/cedars_for_auto_banff.xlsx"))
        data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
        
        any_missing <- any(is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA")
        ind <- which(is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA")
        any_c4d <- any(na.omit(data_origin$C4d[ind])[!na.omit(data_origin$C4d[ind]) %in% "NA"] >= 0) # remove C4d in case of NA or "NA"
        template_names <- c("Patient_id", "date_transplant", "date_biopsy", 
                            "sclerotic_glomeruli", "number_glomeruli", "number_artery", 
                            "ABO_incompatibility", "C4d", "C4d_type", "t", "i", "v", "g",
                            "ptc", "cg", "ci", "ct", "i_IFTA", "t_IFTA", "PVL",
                            "ah", "mm", "cv", "ti", "TMA", "ATI", "ptcml", "DSA",
                            "mmarker", "prior_diagnosis", "other_diagnoses", "other_diagnoses_free_note")
        
       
        
        ABO_incompatibility_template <- c("Yes", "No", NA, "NA")
        C4d_template <- c(0:3, NA, "NA")
        C4d_type_template <- c("IF", "IHC", NA, "NA")
        t_template <- c(0, 1, 2, 3, NA, "NA")
        i_template <- c(0:3, NA, "NA")
        g_template <- c(0:3, NA, "NA")
        ptc_template <- c(0:3, NA, "NA")
        cg_template <- c(0:3, "1a", "1b", 2, 3, NA, "NA")
        v_template <- c(0:3, NA, "NA")
        ci_template <- c(0:3, NA, "NA")
        ct_template <- c(0:3, NA, "NA")
        i_IFTA_template <- c(0:3, NA, "NA")
        t_IFTA_template <- c(0:3, NA, "NA")
        PVL_template <- c(0:3, NA, "NA")
        ah_template <- c(0:3, NA, "NA")
        mm_template <- c(0:3, NA, "NA")
        cv_template <- c(0:3, "new onset/prolif. cv1", "new onset/prolif. cv2", "new onset/prolif. cv3", NA, "NA")
        ti_template <- c(0:3, NA, "NA")
        TMA_template <- c("No TMA", "TMA unrelated to AMR", "TMA related to AMR", NA, "NA")
        ATI_template <- c("No ATI", "ATI", NA, "NA")
        ptcml_template <- c(0:3, NA, "NA")
        DSA_template <- c("Yes", "No", NA, "NA", 1, 0)
        mmarker_template <- c("AMR Molecular Marker", "No", NA, "NA")
        prior_diagnosis_template <- c("Active AMR or Chronic active AMR", "No", NA, "NA")
        other_diagnoses_template <- c("None",
                                      "Suspicious for CNI toxicity",
                                      "Suspicious for pyelonephritis",
                                      "Immune complex nephropathy, IgA",
                                      "Immune complex nephropathy, C3",
                                      "Immune complex nephropathy, membranous",
                                      "Immune complex nephropathy, lupus",
                                      "Focal Segmental Glomerulosclerosis (FSGS)",
                                      "Diabetic nephropathy",
                                      "Post-transplant lymphoproliferative disease (PTLD)",
                                      "Drug-induced interstitial nephritis",
                                      "Neoplasia", 
                                      "Others",
                                      NA, "NA")
        
        
        other_diag <- data_origin$other_diagnoses %>% str_split("/") %>% unlist()
        
        data_names <- names(data_origin)
        tmp <- template_names[!template_names %in% data_names]
        
        
        text <- ifelse(!all(template_names %in% names(data_origin)), paste0("Names of the required columns (variables) are mismatching or missing.\n", paste0(str_c(tmp, collapse = ", "), " is (are) missing or mismatched.")), 
                       ifelse(any_missing & any_c4d, "Please fill in C4d type when you have C4d score.",
                              ifelse(!all(data_origin$ABO_incompatibility %in% ABO_incompatibility_template), "Please correctly fill in 'ABO_incompatibility'.",
                                     ifelse(!all(data_origin$C4d %in% C4d_template), "Please correctly fill in 'C4d' lesion.",
                                            ifelse(!all(data_origin$C4d_type %in% C4d_type_template), "Please correctly fill in 'C4d_type'.",
                                                   ifelse(!all(data_origin$t %in% t_template), "Please correctly fill in 't' lesion.",
                                                          ifelse(!all(data_origin$i %in% i_template), "Please correctly fill in 'i' lesion.",
                                                                 ifelse(!all(data_origin$v %in% v_template), "Please correctly fill in 'v' lesion.",
                                                                        ifelse(!all(data_origin$g %in% g_template), "Please correctly fill in 'g' lesion.",
                                                                               ifelse(!all(data_origin$ptc %in% ptc_template), "Please correctly fill in 'ptc' lesion.",
                                                                                      ifelse(!all(data_origin$cg %in% cg_template), "Please correctly fill in 'cg' lesion.",
                                                                                             ifelse(!all(data_origin$ci %in% ci_template), "Please correctly fill in 'ci' lesion.",
                                                                                                    ifelse(!all(data_origin$ct %in% ct_template), "Please correctly fill in 'ct' lesion.",
                                                                                                           ifelse(!all(data_origin$i_IFTA %in% i_IFTA_template), "Please correctly fill in 'i_IFTA' lesion.",
                                                                                                                  ifelse(!all(data_origin$t_IFTA %in% t_IFTA_template), "Please correctly fill in 't_IFTA' lesion.",
                                                                                                                         ifelse(!all(data_origin$PVL %in% PVL_template), "Please correctly fill in 'PVL' lesion.",
                                                                                                                                ifelse(!all(data_origin$ah %in% ah_template), "Please correctly fill in 'ah' lesion.",
                                                                                                                                       ifelse(!all(data_origin$mm %in% mm_template), "Please correctly fill in 'mm' lesion.",
                                                                                                                                              ifelse(!all(data_origin$cv %in% cv_template), "Please correctly fill in 'cv' lesion.",
                                                                                                                                                     ifelse(!all(data_origin$ti %in% ti_template), "Please correctly fill in 'ti' lesion.",
                                                                                                                                                            ifelse(!all(data_origin$TMA %in% TMA_template), "Please correctly fill in 'TMA' lesion.",
                                                                                                                                                                   ifelse(!all(data_origin$ATI %in% ATI_template), "Please correctly fill in 'ATI' lesion.",
                                                                                                                                                                          ifelse(!all(data_origin$ptcml %in% ptcml_template), "Please correctly fill in 'ptcml' lesion.",
                                                                                                                                                                                 ifelse(!all(data_origin$DSA %in% DSA_template), "Please correctly fill in 'DSA' lesion.",
                                                                                                                                                                                        ifelse(!all(data_origin$mmarker %in% mmarker_template), "Please correctly fill in 'mmarker'.",
                                                                                                                                                                                               ifelse(!all(data_origin$prior_diagnosis %in% prior_diagnosis_template), "Please correctly fill in 'prior_diagnosis'.",
                                                                                                                                                                                                      ifelse(!all(other_diag %in% other_diagnoses_template), "Please correctly fill in 'other_diagnoses'.", NA_character_)))))))))))))))))))))))))))
        
        if (
            (!all(template_names %in% names(data_origin))) | 
            (any_missing & any_c4d) |
            (!all(data_origin$t %in% t_template)) |
            (!all(data_origin$ABO_incompatibility %in% ABO_incompatibility_template)) |
            (!all(data_origin$C4d %in% C4d_template)) |
            (!all(data_origin$C4d_type %in% C4d_type_template)) |
            (!all(data_origin$t %in% t_template)) |
            (!all(data_origin$i %in% i_template)) |
            (!all(data_origin$v %in% v_template)) |
            (!all(data_origin$g %in% g_template)) |
            (!all(data_origin$ptc %in% ptc_template)) |
            (!all(data_origin$cg %in% cg_template)) |
            (!all(data_origin$ci %in% ci_template)) |
            (!all(data_origin$ct %in% ct_template)) |
            (!all(data_origin$i_IFTA %in% i_IFTA_template)) |
            (!all(data_origin$t_IFTA %in% t_IFTA_template)) |
            (!all(data_origin$PVL %in% PVL_template)) |
            (!all(data_origin$ah %in% ah_template)) |
            (!all(data_origin$mm %in% mm_template)) |
            (!all(data_origin$cv %in% cv_template)) |
            (!all(data_origin$ti %in% ti_template)) |
            (!all(data_origin$TMA %in% TMA_template)) |
            (!all(data_origin$ATI %in% ATI_template)) |
            (!all(data_origin$ptcml %in% ptcml_template)) |
            (!all(data_origin$DSA %in% DSA_template)) |
            (!all(data_origin$mmarker %in% mmarker_template)) |
            (!all(data_origin$prior_diagnosis %in% prior_diagnosis_template)) |
            !all(other_diag %in% other_diagnoses_template)
        ) {
            confirmSweetAlert(
                session = session,
                inputId = "myconfirmation1",
                type = "warning",
                title = text,
                btn_labels = c("Okay"),
                danger_mode = T
            )
        }
    })
    
    # observeEvent(input$file_excel, {
    #     data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
    #     template_names <- c("Patient_id", "date_transplant", "date_biopsy", 
    #                      "sclerotic_glomeruli", "number_glomeruli", "number_artery", 
    #                      "ABO_incompatibility", "C4d", "C4d_type", "t", "i", "v", "g",
    #                      "ptc", "cg", "ci", "ct", "i_IFTA", "t_IFTA", "PVL",
    #                      "ah", "mm", "cv", "ti", "TMA", "ATI", "ptcml", "DSA",
    #                      "mmarker", "prior_diagnosis", "other_diagnoses", "other_diagnoses_free_note")
    #     text <- ifelse(!all(template_names %in% names(data_origin)), "Names of the required columns (variables) are mismatching or missing.", "ExampleText")
    #     
    #     
    #     if (!all(template_names %in% names(data_origin))) {
    #         confirmSweetAlert(
    #             session = session,
    #             inputId = "warning_message",
    #             type = "warning",
    #             title = text,
    #             btn_labels = c("Okay"),
    #             danger_mode = T
    #         )
    #     }
    # })
    
    
    # observeEvent(input$file_excel, {
    #     data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
    #     any_missing <- any(is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA")
    #     ind <- which(is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA")
    #     any_c4d <- any(na.omit(data_origin$C4d[ind]) >= 0)
    # 
    #     if (any_missing & any_c4d) {
    #         shinyjs::disable("report_tab")
    #     } else {
    #         shinyjs::enable("report_tab")
    #     }
    # })
    # 
    # observeEvent(input$file_excel, {
    #     data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
    #     if ( (is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA") & data_origin$C4d >= 0) {
    #         shinyjs::disable("report.single")
    #     } else {
    #         shinyjs::enable("report.single")
    #     }
    # })
    # 
    # observeEvent(input$file_excel, {
    #     data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
    #     any_missing <- any(is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA")
    #     ind <- which(is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA")
    #     any_c4d <- any(na.omit(data_origin$C4d[ind]) >= 0)
    # 
    #     if (any_missing & any_c4d) {
    #         shinyjs::disable("report")
    #     } else {
    #         shinyjs::enable("report")
    #     }
    # })
    
    # output$text <- renderText({
    #     data_origin <- as.data.frame(read_excel(input$file_excel$datapath))
    #     any_missing <- any(is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA")
    #     ind <- which(is.na(data_origin$C4d_type) | data_origin$C4d_type == "NA")
    #     # any_missing & any(data_origin$C4d[ind] > 0)
    #     any_c4d <- any(na.omit(data_origin$C4d[ind]) >= 0)
    #     print(paste(any_missing & any_c4d))
    # 
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)

