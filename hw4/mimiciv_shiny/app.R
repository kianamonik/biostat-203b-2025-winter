library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(bigrquery)
library(DBI)
library(dbplyr)

get_patient_details <- function(subject_id) {
  satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"
  bq_auth(path = satoken)
  
  con_bq <- dbConnect(
    bigrquery::bigquery(),
    project = "biostat-203b-2025-winter",
    dataset = "mimiciv_3_1",
    billing = "biostat-203b-2025-winter"
  )
  
  patients <- tbl(con_bq, "patients") %>%
    filter(subject_id == !!subject_id) %>%
    collect()
  
  admissions <- tbl(con_bq, "admissions") %>%
    filter(subject_id == !!subject_id) %>%
    collect()
  
  transfers <- tbl(con_bq, "transfers") %>%
    filter(subject_id == !!subject_id) %>%
    collect()
  
  icustays <- tbl(con_bq, "icustays") %>%
    filter(subject_id == !!subject_id) %>%
    arrange(subject_id, hadm_id, stay_id) %>%
    collect()
  
  labevents <- tbl(con_bq, "labevents") %>%
    filter(subject_id == !!subject_id) %>%
    collect()
  
  procedures <- tbl(con_bq, "procedures_icd") %>%
    filter(subject_id == !!subject_id) %>%
    collect()
  
  diagnoses <- tbl(con_bq, "diagnoses_icd") %>%
    filter(subject_id == !!subject_id) %>%
    collect()
  
  chartevents <- tbl(con_bq, "chartevents") %>%
    filter(subject_id == !!subject_id) %>%
    collect()
  
  d_items <- tbl(con_bq, "d_items") %>%
    collect()
  
  d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses") %>%
    collect()
  
  d_icd_procedures <- tbl(con_bq, "d_icd_procedures") %>%
    collect()

  return(list(
    patients = patients,
    admissions = admissions,
    transfers = transfers,
    icustays = icustays,
    labevents = labevents,
    procedures = procedures,
    diagnoses = diagnoses,
    chartevents = chartevents,
    d_items = d_items,
    d_icd_diagnoses = d_icd_diagnoses,
    d_icd_procedures = d_icd_procedures
  ))
}

setwd("~/Desktop/BIOSTAT/BIOSTAT 203B/203B-hw")
mimic_icu_cohort <- read_rds("hw4/mimiciv_shiny/mimic_icu_cohort.rds")

ui <- fluidPage(
  titlePanel("ICU Cohort Data"),
  tabsetPanel(
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_group", "Variable Group:",
                             choices = c("demo", "vitals", "lab_measure")),
                 uiOutput("var_selector")
               ),
               mainPanel(
                 plotOutput("summary_plot")
               )
             )
    ),
    tabPanel("Patient Info",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "subject_id", 
                   "Patient ID", 
                   choices = sort(unique(mimic_icu_cohort$subject_id)),
                   selected = 10001217, 
                   selectize = TRUE
                 ),
                 actionButton("submit", "Submit"),
                 selectInput("plot_type", "Select a plot:",
                             choices = c("ADT", "ICU"))
               ),
               mainPanel(
                 plotOutput("patient_plot"),
                 verbatimTextOutput("query_error")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  output$var_selector <- renderUI({
    vars <- switch(input$var_group,
                   "demo" = c("race", "insurance", "marital_status",
                              "gender", "age_at_intime"),
                   "vitals" = c("heart_rate",
                                "non_invasive_blood_pressure_systolic"),
                   "lab_measure" = c("bicarbonate", "hematocrit"))
    selectInput("variable", "Variable:", choices = vars)
  })
  
  output$summary_plot <- renderPlot({
    req(input$variable)
    data <- mimic_icu_cohort %>%
      filter(!is.na(!!sym(input$variable)))
    if (input$variable == "race") {
      data <- data %>%
        mutate(
          race = fct_collapse(
            race,
            ASIAN = c("ASIAN", "ASIAN - VIETNAMESE", "ASIAN - CHINESE", 
                      "ASIAN - FILIPINO", "ASIAN - OTHER",
                      "ASIAN - SOUTH EAST ASIAN",
                      "ASIAN - KOREAN", "ASIAN - ASIAN INDIAN"),
            BLACK = c("BLACK/AFRICAN AMERICAN", "BLACK/CAPE VERDEAN",
                      "BLACK/HAITIAN",
                      "BLACK/AFRICAN", "BLACK/CARIBBEAN ISLAND"),
            HISPANIC = c("HISPANIC OR LATINO", "HISPANIC/LATINO - PUERTO RICAN", 
                         "HISPANIC/LATINO - DOMINICAN",
                         "HISPANIC/LATINO - CUBAN", 
                         "HISPANIC/LATINO - CENTRAL AMERICAN", 
                         "HISPANIC/LATINO - SOUTH AMERICAN",
                         "HISPANIC/LATINO - MEXICAN",
                         "HISPANIC/LATINO - SALVADORAN",
                         "HISPANIC/LATINO - GUATEMALAN",
                         "HISPANIC/LATINO - HONDURAN",
                         "HISPANIC/LATINO - COLUMBIAN"),
            WHITE = c("WHITE", "WHITE - RUSSIAN", "WHITE - BRAZILIAN", 
                      "WHITE - OTHER EUROPEAN", "WHITE - EASTERN EUROPEAN"),
            Other = c("AMERICAN INDIAN/ALASKA NATIVE",
                      "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
                      "MULTIPLE RACE/ETHNICITY", "UNABLE TO OBTAIN", "UNKNOWN",
                      "PATIENT DECLINED TO ANSWER", "SOUTH AMERICAN", "OTHER",
                      "PORTUGUESE")
          )
        )
    }
    
    if (is.numeric(data[[input$variable]])) {
      data <- data %>%
        filter(between(!!sym(input$variable),
                       quantile(!!sym(input$variable), 0.01), 
                       quantile(!!sym(input$variable), 0.99)))
      ggplot(data, aes(x = !!sym(input$variable))) +
        geom_histogram(binwidth = 5, fill = "gray", color = "black") +
        theme_minimal()
    } else {
      ggplot(data, 
             aes(x = !!sym(input$variable), fill = !!sym(input$variable))) +
        geom_bar() +
        theme_minimal()
    }
  })
  
  observeEvent(input$submit, {
    subject_id <- as.numeric(input$subject_id)

    patient_details <- get_patient_details(subject_id)

    my_ids <- c(224072, 224733, 223988, 224773, 220048,
                228395, 220210, 225054, 223794, 228411)
    
    cat("\nItem details:\n")
    print(
      patient_details$d_items %>%
        filter(itemid %in% my_ids) %>%
        select(itemid, label, abbreviation, unitname, param_type) %>%
        head(50)
    )
    
    
    output$query_error <- renderText({
      if (is.null(patient_details$patients) || 
          nrow(patient_details$patients) == 0)
        "No patient data found." else ""
    })
    
    output$patient_plot <- renderPlot({
      req(patient_details)
      
      if (input$plot_type == "ADT") {
        # ADT Plot (adapted from HW3)
        patients <- patient_details$patients
        transfers <- patient_details$transfers
        labevents <- patient_details$labevents
        procedures <- patient_details$procedures
        diagnoses <- patient_details$diagnoses
        admissions <- patient_details$admissions
        d_icd_diagnoses <- patient_details$d_icd_diagnoses
        d_icd_procedures <- patient_details$d_icd_procedures

        diagnoses <- diagnoses %>%
          mutate(icd_code = str_pad(icd_code, width = 5, pad = "0")) %>%
          left_join(d_icd_diagnoses, by = c("icd_code", "icd_version"))
        
        long_title_col <- grep("long_title", colnames(diagnoses), value = TRUE)
        if ("long_title" %in% long_title_col) {
          diagnoses <- diagnoses %>% rename(diagnosis_name = long_title)
        } else if ("long_title.x" %in% long_title_col) {
          diagnoses <- diagnoses %>% rename(diagnosis_name = long_title.x)
        } else if ("long_title.y" %in% long_title_col) {
          diagnoses <- diagnoses %>% rename(diagnosis_name = long_title.y)
        } else {
          stop("long_title column not found in diagnoses")
        }
        
        top_diagnoses <- diagnoses %>% 
          filter(!is.na(diagnosis_name)) %>% 
          count(diagnosis_name, sort = TRUE) %>%  
          head(3) %>% 
          pull(diagnosis_name)
        
        top_diagnoses_text <- ifelse(length(top_diagnoses) > 0,
                                     paste(top_diagnoses, collapse = "\n"), "")
        
        patient_summary <- paste(
          "Patient", subject_id,
          ifelse(is.na(patients$gender[1]), "", patients$gender[1]),
          ifelse(is.na(patients$anchor_age[1]), "",
                 paste(patients$anchor_age[1], "years old")),
          ifelse(is.na(admissions$race[1]), "", admissions$race[1])
        ) %>% str_squish()
        
        transfers <- transfers %>%
          mutate(intime = as.POSIXct(intime, format="%Y-%m-%d %H:%M:%S"),
                 outtime = as.POSIXct(outtime, format="%Y-%m-%d %H:%M:%S")) %>%
          filter(!is.na(outtime))
        
        labevents <- labevents %>%
          mutate(chartdate = as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%S"))
        
        procedures <- procedures %>%
          mutate(chartdate = as.POSIXct(chartdate,
                                        format="%Y-%m-%d %H:%M:%S")) %>%
          left_join(d_icd_procedures, by = c("icd_code", "icd_version"))
        
        procedure_name_cols <- grep("long_title", colnames(procedures),
                                    value = TRUE)
        if (length(procedure_name_cols) > 1) {
          procedures <- procedures %>% 
            select(-one_of(procedure_name_cols[-1])) %>% 
            rename(procedure_name = procedure_name_cols[1])
        } else if (length(procedure_name_cols) == 1) {
          procedures <- procedures %>% 
            rename(procedure_name = procedure_name_cols[1])
        } else {
          stop("procedure_name column not found in procedures")
        }
        procedures <- procedures %>% filter(!is.na(procedure_name))
        
        care_unit_colors <- c("Emergency Department" = "red", 
                              "Medicine" = "green", 
                              "Neurology" = "cyan", 
                              "Surgical Intensive Care Unit (SICU)" = "purple")
        
        plot <- ggplot() +
          geom_segment(data = transfers, 
                       aes(x = intime, xend = outtime,
                           y = "ADT", yend = "ADT", color = careunit),
                       linewidth = 3) +
          geom_point(data = labevents, 
                     aes(x = chartdate, y = "Lab"), shape = 3, size = 3) +
          geom_point(data = procedures, 
                     aes(x = chartdate, y = "Procedure",
                         shape = procedure_name), size = 5) +
          scale_color_manual(values = care_unit_colors) +
          scale_shape_manual(values = setNames(seq(15, 15 + 
                            length(unique(procedures$procedure_name)) - 1),
                                        unique(procedures$procedure_name)),
                             drop = FALSE) +
          theme_minimal() +
          labs(title = patient_summary, 
               subtitle = top_diagnoses_text, 
               x = "Calendar Time", 
               y = NULL, 
               color = "Care Unit", 
               shape = "Procedure")
        print(plot)
        
      } else {
        icustays <- patient_details$icustays
        chartevents <- patient_details$chartevents
        d_items <- patient_details$d_items
      
        vital_ids <- c(220045, 220179, 220181, 223761, 220210)

        subject_stays <- icustays %>%
          filter(subject_id == subject_id) %>%
          select(stay_id, intime, outtime)
     
        chartevents_filtered <- chartevents %>%
          filter(subject_id == subject_id,
                 itemid %in% vital_ids) %>%
          inner_join(subject_stays, by = "stay_id") %>%
          filter(charttime >= intime & charttime <= outtime) %>%
          select(stay_id, itemid, charttime, valuenum) %>%
          filter(!is.na(valuenum))  
        chartevents_with_labels <- chartevents_filtered %>%
          inner_join(d_items %>% select(itemid, abbreviation),
                     by = "itemid") %>%
          mutate(charttime = as_datetime(charttime))
        
        ggplot(chartevents_with_labels, aes(x = charttime, y = valuenum,
                                            color = abbreviation)) +
          geom_point(size = 1.2) +
          geom_line(size = 0.8) +
          facet_grid(abbreviation ~ stay_id, scales = "free") +
          labs(
            title = paste("Patient", subject_id, "ICU stays - Vitals"),
            x = "Time",
            y = "Vital Value"
          ) +
          scale_x_datetime(
            breaks = seq(
              floor_date(min(chartevents_with_labels$charttime, na.rm = TRUE),
                         unit = "6 hours"),
              ceiling_date(max(chartevents_with_labels$charttime, na.rm = TRUE),
                           unit = "6 hours"),
              by = "6 hours"
            ),
            date_labels = "%b %d %H:%M"
          ) +
          theme_minimal() +
          theme(
            legend.position = "none",
            strip.text = element_text(size = 12, face = "bold",
                                      color = "white"),
            strip.background = element_rect(fill = "darkgrey",
                                            color = "darkgrey"),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.grid.major = element_line(size = 0.5, linetype = "dotted",
                                            color = "gray"),
            panel.grid.minor = element_blank()
          )
        
      }
    })
  })
}

shinyApp(ui, server)
