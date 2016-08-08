library(shiny)
library(magrittr)
library(dplyr)

Workload <- read.csv("Workload Data/Workload.csv")
Workload$Year <- as.character(Workload$Year)
Workload$NoCase <- as.numeric(as.character(Workload$NoCase))
Workload$NoCase_15Percent <- as.numeric(as.character(Workload$NoCase_15Percent))

shinyServer(function(input, output) {
  
  
  dataset <- reactive({

    Workload %>% filter(
      Year == input$year
    )

  })
  
  output$Neuro <- renderText({
    
    Neurosurgery <- filter(dataset(), Specialty == "Neurosurgery")
   
    if (TRUE %in% is.na(Neurosurgery$NoCase)) {
      return ("You have missing value!")
    }
    
    # Total time taken (working days)
    a <- sum(Neurosurgery$NoCase[Neurosurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
              Neurosurgery$NoCase[Neurosurgery$WorkloadDriver == "Outpatient"]*input$OP,
              Neurosurgery$NoCase[Neurosurgery$WorkloadDriver == "Day Surgery"]*input$DS,
              Neurosurgery$NoCase[Neurosurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
              input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    # Number of working days for each staff per year
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$Neuro_1 <- renderText({
    
    Neurosurgery <- filter(dataset(), Specialty == "Neurosurgery")
    if (TRUE %in% is.na(Neurosurgery$NoCase)) {
      return ("You have missing value!")
    }
    a <- sum(Neurosurgery$NoCase[Neurosurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Neurosurgery$NoCase[Neurosurgery$WorkloadDriver == "Outpatient"]*input$OP,
             Neurosurgery$NoCase[Neurosurgery$WorkloadDriver == "Day Surgery"]*input$DS,
             Neurosurgery$NoCase[Neurosurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b*1.2,1)
  })
  
  output$Neuro_2 <- renderText({
    
    Neurosurgery <- filter(dataset(), Specialty == "Neurosurgery")
    
    if (TRUE %in% is.na(Neurosurgery$NoCase_15Percent)) {
      return ("You have missing value!")
    }
    a <- sum(Neurosurgery$NoCase_15Percent[Neurosurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Neurosurgery$NoCase_15Percent[Neurosurgery$WorkloadDriver == "Outpatient"]*input$OP,
             Neurosurgery$NoCase_15Percent[Neurosurgery$WorkloadDriver == "Day Surgery"]*input$DS,
             Neurosurgery$NoCase_15Percent[Neurosurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$Neurotable <- DT::renderDataTable({
    filter(dataset(), Specialty == "Neurosurgery")
  })
  
  
  output$Gensurg <- renderText({
    
    Generalsurgery <- filter(dataset(), Specialty == "General Surgery")
    
    if (TRUE %in% is.na(Generalsurgery$NoCase)) {
      return ("You have missing value!")
    }
    
    # Total time taken (working days)
    a <- sum(Generalsurgery$NoCase[Generalsurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Generalsurgery$NoCase[Generalsurgery$WorkloadDriver == "Outpatient"]*input$OP,
             Generalsurgery$NoCase[Generalsurgery$WorkloadDriver == "Day Surgery"]*input$DS,
             Generalsurgery$NoCase[Generalsurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    # Number of working days for each staff per year
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$Gensurg_1 <- renderText({
    
    Generalsurgery <- filter(dataset(), Specialty == "General Surgery")
    
    if (TRUE %in% is.na(Generalsurgery$NoCase)) {
      return ("You have missing value!")
    }
  
    a <- sum(Generalsurgery$NoCase[Generalsurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Generalsurgery$NoCase[Generalsurgery$WorkloadDriver == "Outpatient"]*input$OP,
             Generalsurgery$NoCase[Generalsurgery$WorkloadDriver == "Day Surgery"]*input$DS,
             Generalsurgery$NoCase[Generalsurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b*1.2,1)
  })
  
  output$Gensurg_2 <- renderText({
    
    Generalsurgery <- filter(dataset(), Specialty == "General Surgery")
    
    if (TRUE %in% is.na(Generalsurgery$NoCase_15Percent)) {
      return ("You have missing value!")
    }
    
    a <- sum(Generalsurgery$NoCase_15Percent[Generalsurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Generalsurgery$NoCase_15Percent[Generalsurgery$WorkloadDriver == "Outpatient"]*input$OP,
             Generalsurgery$NoCase_15Percent[Generalsurgery$WorkloadDriver == "Day Surgery"]*input$DS,
             Generalsurgery$NoCase_15Percent[Generalsurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b,1)
  })
  
  output$Gensurgtable <- DT::renderDataTable({
    filter(dataset(), Specialty == "General Surgery")
  })
  
  output$Ortho <- renderText({
    
    Orthosurgery <- filter(dataset(), Specialty == "Orthopaedic Surgery")
    
    if (TRUE %in% is.na(Orthosurgery$NoCase)) {
      return ("You have missing value!")
    }
    
    # Total time taken (working days)
    a <- sum(Orthosurgery$NoCase[Orthosurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Orthosurgery$NoCase[Orthosurgery$WorkloadDriver == "Outpatient"]*input$OP,
             Orthosurgery$NoCase[Orthosurgery$WorkloadDriver == "Day Surgery"]*input$DS,
             Orthosurgery$NoCase[Orthosurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    # Number of working days for each staff per year
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$Ortho_1 <- renderText({
    
    Orthosurgery <- filter(dataset(), Specialty == "Orthopaedic Surgery")
    
    if (TRUE %in% is.na(Orthosurgery$NoCase)) {
      return ("You have missing value!")
    }
    
    a <- sum(Orthosurgery$NoCase[Orthosurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Orthosurgery$NoCase[Orthosurgery$WorkloadDriver == "Outpatient"]*input$OP,
             Orthosurgery$NoCase[Orthosurgery$WorkloadDriver == "Day Surgery"]*input$DS,
             Orthosurgery$NoCase[Orthosurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)

    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b*1.2,1)
  })
  
  output$Ortho_2 <- renderText({
    
    Orthosurgery <- filter(dataset(), Specialty == "Orthopaedic Surgery")
    
    if (TRUE %in% is.na(Orthosurgery$NoCase_15Percent)) {
      return ("You have missing value!")
    }
    
    a <- sum(Orthosurgery$NoCase_15Percent[Orthosurgery$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Orthosurgery$NoCase_15Percent[Orthosurgery$WorkloadDriver == "Outpatient"]*input$OP,
             Orthosurgery$NoCase_15Percent[Orthosurgery$WorkloadDriver == "Day Surgery"]*input$DS,
             Orthosurgery$NoCase_15Percent[Orthosurgery$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b,1)
  })
  
  output$Orthotable <- DT::renderDataTable({
    filter(dataset(), Specialty == "Orthopaedic Surgery")
  })

  output$Uro <- renderText({
    
    Urology <- filter(dataset(), Specialty == "Urology")
    
    if (TRUE %in% is.na(Urology$NoCase)) {
      return ("You have missing value!")
    }
    
    # Total time taken (working days)
    a <- sum(Urology$NoCase[Urology$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Urology$NoCase[Urology$WorkloadDriver == "Outpatient"]*input$OP,
             Urology$NoCase[Urology$WorkloadDriver == "Day Surgery"]*input$DS,
             Urology$NoCase[Urology$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    # Number of working days for each staff per year
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$Uro_1 <- renderText({
    
    Urology <- filter(dataset(), Specialty == "Urology")
    
    if (TRUE %in% is.na(Urology$NoCase)) {
      return ("You have missing value!")
    }
 
    a <- sum(Urology$NoCase[Urology$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Urology$NoCase[Urology$WorkloadDriver == "Outpatient"]*input$OP,
             Urology$NoCase[Urology$WorkloadDriver == "Day Surgery"]*input$DS,
             Urology$NoCase[Urology$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)

    b <- 365 - (input$AL+input$PH+input$Admin)

    round(a/b*1.2,1)
  })
  
  output$Uro_2 <- renderText({
    
    Urology <- filter(dataset(), Specialty == "Urology")
    
    if (TRUE %in% is.na(Urology$NoCase_15Percent)) {
      return ("You have missing value!")
    }
    
    a <- sum(Urology$NoCase_15Percent[Urology$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Urology$NoCase_15Percent[Urology$WorkloadDriver == "Outpatient"]*input$OP,
             Urology$NoCase_15Percent[Urology$WorkloadDriver == "Day Surgery"]*input$DS,
             Urology$NoCase_15Percent[Urology$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b,1)
  })
  
  output$Urotable <- DT::renderDataTable({
    filter(dataset(), Specialty == "Urology")
  })
  
  output$Eye <- renderText({
    
    Eye <- filter(dataset(), Specialty == "EYE")
    
    if (TRUE %in% is.na(Eye$NoCase)) {
      return ("You have missing value!")
    }
    
    # Total time taken (working days)
    a <- sum(Eye$NoCase[Eye$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Eye$NoCase[Eye$WorkloadDriver == "Outpatient"]*input$OP,
             Eye$NoCase[Eye$WorkloadDriver == "Day Surgery"]*input$DS,
             Eye$NoCase[Eye$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    # Number of working days for each staff per year
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$Eye_1 <- renderText({
    
    Eye <- filter(dataset(), Specialty == "EYE")
    
    if (TRUE %in% is.na(Eye$NoCase)) {
      return ("You have missing value!")
    }
    

    a <- sum(Eye$NoCase[Eye$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Eye$NoCase[Eye$WorkloadDriver == "Outpatient"]*input$OP,
             Eye$NoCase[Eye$WorkloadDriver == "Day Surgery"]*input$DS,
             Eye$NoCase[Eye$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)

    b <- 365 - (input$AL+input$PH+input$Admin)

    round(a/b*1.2,1)
  })
  
  output$Eye_2 <- renderText({
    
    Eye <- filter(dataset(), Specialty == "EYE")
    
    if (TRUE %in% is.na(Eye$NoCase_15Percent)) {
      return ("You have missing value!")
    }
    
    
    a <- sum(Eye$NoCase_15Percent[Eye$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             Eye$NoCase_15Percent[Eye$WorkloadDriver == "Outpatient"]*input$OP,
             Eye$NoCase_15Percent[Eye$WorkloadDriver == "Day Surgery"]*input$DS,
             Eye$NoCase_15Percent[Eye$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b,1)
  })
  
  output$Eyetable <- DT::renderDataTable({
    filter(dataset(), Specialty == "EYE")
  })
  
  output$ENT <- renderText({
    
    ENT <- filter(dataset(), Specialty == "ENT")
    
    if (TRUE %in% is.na(ENT$NoCase)) {
      return ("You have missing value!")
    }
    
    
    # Total time taken (working days)
    a <- sum(ENT$NoCase[ENT$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             ENT$NoCase[ENT$WorkloadDriver == "Outpatient"]*input$OP,
             ENT$NoCase[ENT$WorkloadDriver == "Day Surgery"]*input$DS,
             ENT$NoCase[ENT$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    # Number of working days for each staff per year
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$ENT_1 <- renderText({
    
    ENT <- filter(dataset(), Specialty == "ENT")
    
    if (TRUE %in% is.na(ENT$NoCase)) {
      return ("You have missing value!")
    }
    

    a <- sum(ENT$NoCase[ENT$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             ENT$NoCase[ENT$WorkloadDriver == "Outpatient"]*input$OP,
             ENT$NoCase[ENT$WorkloadDriver == "Day Surgery"]*input$DS,
             ENT$NoCase[ENT$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)

    b <- 365 - (input$AL+input$PH+input$Admin)

    round(a/b*1.2,1)
  })
  
  output$ENT_2 <- renderText({
    
    ENT <- filter(dataset(), Specialty == "ENT")
    
    if (TRUE %in% is.na(ENT$NoCase_15Percent)) {
      return ("You have missing value!")
    }
    
    
    a <- sum(ENT$NoCase_15Percent[ENT$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             ENT$NoCase_15Percent[ENT$WorkloadDriver == "Outpatient"]*input$OP,
             ENT$NoCase_15Percent[ENT$WorkloadDriver == "Day Surgery"]*input$DS,
             ENT$NoCase_15Percent[ENT$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b,1)
  })
  
  output$ENTtable <- DT::renderDataTable({
    filter(dataset(), Specialty == "ENT")
  })
  
  output$GenMed <- renderText({
    
    GenMed <- filter(dataset(), Specialty == "General Medicine")
    
    if (TRUE %in% is.na(GenMed$NoCase)) {
      return ("You have missing value!")
    }
    
    
    # Total time taken (working days)
    a <- sum(GenMed$NoCase[GenMed$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             GenMed$NoCase[GenMed$WorkloadDriver == "Outpatient"]*input$OP,
             GenMed$NoCase[GenMed$WorkloadDriver == "Day Surgery"]*input$DS,
             GenMed$NoCase[GenMed$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    # Number of working days for each staff per year
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$GenMed_1 <- renderText({
    
    GenMed <- filter(dataset(), Specialty == "General Medicine")
    
    if (TRUE %in% is.na(GenMed$NoCase)) {
      return ("You have missing value!")
    }
    
    a <- sum(GenMed$NoCase[GenMed$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             GenMed$NoCase[GenMed$WorkloadDriver == "Outpatient"]*input$OP,
             GenMed$NoCase[GenMed$WorkloadDriver == "Day Surgery"]*input$DS,
             GenMed$NoCase[GenMed$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b*1.2,1)
  })
  
  output$GenMed_2 <- renderText({
    
    GenMed <- filter(dataset(), Specialty == "General Medicine")
    
    if (TRUE %in% is.na(GenMed$NoCase_15Percent)) {
      return ("You have missing value!")
    }
    
    a <- sum(GenMed$NoCase_15Percent[GenMed$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             GenMed$NoCase_15Percent[GenMed$WorkloadDriver == "Outpatient"]*input$OP,
             GenMed$NoCase_15Percent[GenMed$WorkloadDriver == "Day Surgery"]*input$DS,
             GenMed$NoCase_15Percent[GenMed$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b,1)
  })
  
  output$GenMedtable <- DT::renderDataTable({
    filter(dataset(), Specialty == "General Medicine")
  })
  
  output$GeriMed <- renderText({
    
    GeriMed <- filter(dataset(), Specialty == "Geriatric Medicine")
    
    if (TRUE %in% is.na(GeriMed$NoCase)) {
      return ("You have missing value!")
    }
    
    # Total time taken (working days)
    a <- sum(GeriMed$NoCase[GeriMed$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             GeriMed$NoCase[GeriMed$WorkloadDriver == "Outpatient"]*input$OP,
             GeriMed$NoCase[GeriMed$WorkloadDriver == "Day Surgery"]*input$DS,
             GeriMed$NoCase[GeriMed$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    # Number of working days for each staff per year
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    # No. of FTE needed
    round(a/b,1)
  })
  
  output$GeriMed_1 <- renderText({
    
    GeriMed <- filter(dataset(), Specialty == "Geriatric Medicine")
    
    if (TRUE %in% is.na(GeriMed$NoCase)) {
      return ("You have missing value!")
    }
    
    a <- sum(GeriMed$NoCase[GeriMed$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             GeriMed$NoCase[GeriMed$WorkloadDriver == "Outpatient"]*input$OP,
             GeriMed$NoCase[GeriMed$WorkloadDriver == "Day Surgery"]*input$DS,
             GeriMed$NoCase[GeriMed$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b*1.2,1)
  })
  
  output$GeriMed_2 <- renderText({
    
    GeriMed <- filter(dataset(), Specialty == "Geriatric Medicine")
    
    if (TRUE %in% is.na(GeriMed$NoCase_15Percent)) {
      return ("You have missing value!")
    }
    
    a <- sum(GeriMed$NoCase_15Percent[GeriMed$WorkloadDriver == "Inpatient Discharges"]*input$InpDisch*input$ALOS, 
             GeriMed$NoCase_15Percent[GeriMed$WorkloadDriver == "Outpatient"]*input$OP,
             GeriMed$NoCase_15Percent[GeriMed$WorkloadDriver == "Day Surgery"]*input$DS,
             GeriMed$NoCase_15Percent[GeriMed$WorkloadDriver == "Inpatient Surgery"]*input$InpSurg,
             input$Oncall*60*input$NoDr*365)/(input$HourPerWk*60/input$DayPerWk)
    
    b <- 365 - (input$AL+input$PH+input$Admin)
    
    round(a/b,1)
  })
  
  output$GeriMedtable <- DT::renderDataTable({
    filter(dataset(), Specialty == "Geriatric Medicine")
  })
})
