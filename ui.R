library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)

dashboardPage(skin = "blue",

  dashboardHeader(title = "Estimation of FTE by Discipline", titleWidth = "390px"),


  dashboardSidebar(width = "390px",
      selectInput(inputId = "year",label = h5(tags$b("Please select year"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                  choices = c("2013","2014","2015"),selected = c("2015"),multiple = F),

      tags$hr(),
      
      h4(tags$b("Time taken for one case (mins)"), style = "font-family: Georgia, Times, Times New Roman, serif;color: #000000;"),
      
      numericInput(inputId = "InpDisch", label = h5(tags$b("Inpatient Discharges"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 5),
      
      numericInput(inputId = "ALOS", label = h5(tags$b("Average Length of Stay"),style = "font-family: Georgia, Times, Times New Roman, serif;color: #000000;"),
                   value = 6),
      
      numericInput(inputId = "OP", label = h5(tags$b("Outpatient"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 15),
      
      numericInput(inputId = "DS", label = h5(tags$b("Day Surgeries"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 30),
      
      numericInput(inputId = "InpSurg", label = h5(tags$b("Inpatient Surgeries"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 120),
      
      tags$hr(),
      
      h4(tags$b("Time period of on call"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
      
      numericInput(inputId = "Oncall", label = h5(tags$b("No. of on call hours per consultant per day"),style = "font-family: Georgia, Times, Times New Roman, serif;color: #000000;"),
                   value = 16),
      
      numericInput(inputId = "NoDr", label = h5(tags$b("No. of consultants on call per day"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 1),
      
      tags$hr(),
      
      h4(tags$b("Actual working days for each FTE per year"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
      
      numericInput(inputId = "AL", label = h5(tags$b("No. of days for annual leave per staff"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 24),
      
      numericInput(inputId = "PH", label = h5(tags$b("No. of days for public holiday"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 11),
      
      numericInput(inputId = "Admin", label = h5(tags$b("No. of days for research/training/teaching/admin"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 11),
      
      tags$hr(),
      
      h4(tags$b("No. of mins per working day"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
      
      numericInput(inputId = "HourPerWk", label = h5(tags$b("No. of hours per working week"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 42),
      
      numericInput(inputId = "DayPerWk", label = h5(tags$b("No. of days per week"),style = "font-family: Georgia, Times, Times New Roman, serif; color: #000000;"),
                   value = 5)
      
      ),
    
  
    dashboardBody(
      
      # Change the font of dashboard title
      tags$head(tags$style(HTML('
      .main-header .logo {
                                font-family: "Arial", Helvetica, sans-serif;
                                font-weight: bold;
                                font-size: 20px;
                                }
                                '))),
      # Change the background color of dashboardSidebar panel
      tags$style(HTML('.skin-blue .main-sidebar {
                        background-color: #D3D3D3;
                      }')
                 ),
      
      tabsetPanel(
        
        tabPanel(h4("Neurosurgery"), tags$br(),tags$br(),
        
                fluidRow(
          
                  infoBox(h2(tags$b(textOutput("Neuro"))), title = "No. of FTE needed",icon = icon("list", lib = "font-awesome")
                  ),
                  
                  infoBox(h2(tags$b(textOutput("Neuro_1"))), title = "No. of FTE needed (Up 20%)", icon = icon("list", lib = "font-awesome")
                  ),
                  
                  infoBox(h2(tags$b(textOutput("Neuro_2"))), title = "No. of FTE needed (If reach 15% Market Share)", icon = icon("list", lib = "font-awesome")
                  )), tags$br(), tags$br(),
                  
                  box(DT::dataTableOutput("Neurotable"),width = "500px", title = tags$b("Workload Raw Data"), status = "primary", solidHeader = TRUE, height = "320px")
                 ),
        
        tabPanel(h4("General Surgery"), tags$br(),tags$br(),
                 
                 fluidRow(
                   
                   infoBox(h2(tags$b(textOutput("Gensurg"))), title = "No. of FTE needed", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("Gensurg_1"))), title = "No. of FTE needed (Up 20%)", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("Gensurg_2"))), title = "No. of FTE needed (If reach 15% Market Share)", icon = icon("list", lib = "font-awesome")
                   )), tags$br(), tags$br(),
                 
                   box(DT::dataTableOutput("Gensurgtable"),width = "500px", title = tags$b("Workload Raw Data"), status = "primary", solidHeader = TRUE, height = "320px")
                  ),
        
        tabPanel(h4("Orthopaedic Surgery"), tags$br(),tags$br(),
                 
                 fluidRow(
                   
                  infoBox(h2(tags$b(textOutput("Ortho"))), title = "No. of FTE needed", icon = icon("list", lib = "font-awesome")
                   ),
                   
                  infoBox(h2(tags$b(textOutput("Ortho_1"))), title = "No. of FTE needed (Up 20%)", icon = icon("list", lib = "font-awesome")
                   ),
                   
                  infoBox(h2(tags$b(textOutput("Ortho_2"))), title = "No. of FTE needed (If reach 15% Market Share)", icon = icon("list", lib = "font-awesome")
                   )), tags$br(), tags$br(),
                 
                   box(DT::dataTableOutput("Orthotable"),width = "500px", title = tags$b("Workload Raw Data"), status = "primary", solidHeader = TRUE, height = "320px")
                ),
        
        tabPanel(h4("Urology"), tags$br(),tags$br(),
                 
                 fluidRow(
                   
                   infoBox(h2(tags$b(textOutput("Uro"))), title = "No. of FTE needed", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("Uro_1"))), title = "No. of FTE needed (Up 20%)", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("Uro_2"))), title = "No. of FTE needed (If reach 15% Market Share)", icon = icon("list", lib = "font-awesome")
                   )), tags$br(), tags$br(),
                 
                   box(DT::dataTableOutput("Urotable"),width = "500px", title = tags$b("Workload Raw Data"), status = "primary", solidHeader = TRUE, height = "320px")
                  ),
        
        tabPanel(h4("EYE"), tags$br(),tags$br(),
                 
                 fluidRow(
                   
                   infoBox(h2(tags$b(textOutput("Eye"))), title = "No. of FTE needed", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("Eye_1"))), title = "No. of FTE needed (Up 20%)", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("Eye_2"))), title = "No. of FTE needed (If reach 15% Market Share)", icon = icon("list", lib = "font-awesome")
                   )), tags$br(), tags$br(),
                 
                   box(DT::dataTableOutput("Eyetable"),width = "500px", title = tags$b("Workload Raw Data"), status = "primary", solidHeader = TRUE, height = "320px")
                  ),
        
        tabPanel(h4("ENT"), tags$br(),tags$br(),
                 
                 fluidRow(
                   
                   infoBox(h2(tags$b(textOutput("ENT"))), title = "No. of FTE needed", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("ENT_1"))), title = "No. of FTE needed (Up 20%)", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("ENT_2"))), title = "No. of FTE needed (If reach 15% Market Share)", icon = icon("list", lib = "font-awesome")
                   )), tags$br(), tags$br(),
                 
                   box(DT::dataTableOutput("ENTtable"),width = "500px", title = tags$b("Workload Raw Data"), status = "primary", solidHeader = TRUE, height = "320px")
                  ),
        
        tabPanel(h4("General Medicine"), tags$br(),tags$br(),
                 
                 fluidRow(
                   
                   infoBox(h2(tags$b(textOutput("GenMed"))), title = "No. of FTE needed", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("GenMed_1"))), title = "No. of FTE needed (Up 20%)", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("GenMed_2"))), title = "No. of FTE needed (If reach 15% Market Share)", icon = icon("list", lib = "font-awesome")
                   )), tags$br(), tags$br(),
                 
                   box(DT::dataTableOutput("GenMedtable"),width = "500px", title = tags$b("Workload Raw Data"), status = "primary", solidHeader = TRUE, height = "320px")
                  ),
        
        tabPanel(h4("Geriatric Medicine"), tags$br(),tags$br(),
                 
                 fluidRow(
                   
                   infoBox(h2(tags$b(textOutput("GeriMed"))), title = "No. of FTE needed", icon = icon("list", lib = "font-awesome")
                   ),
                   
                   infoBox(h2(tags$b(textOutput("GeriMed_1"))), title = "No. of FTE needed (Up 20%)", icon = icon("list", lib = "font-awesome")
                   ),
          
      
                   infoBox(h2(tags$b(textOutput("GeriMed_2"))), title = "No. of FTE needed (If reach 15% Market Share)", icon = icon("list", lib = "font-awesome")
                   )), tags$br(), tags$br(),
                 
                   box(DT::dataTableOutput("GeriMedtable"),width = "500px", title = tags$b("Workload Raw Data"), status = "primary", solidHeader = TRUE, height = "320px")
                  )
        
      )
   )
)
    
