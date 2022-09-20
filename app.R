# app to create EQ2a logbook reports

# FIXME obstetrics summary by supervision and ASA
# TODO TIVA summary by supervision and ASA
# TODO procedures summary

library(shiny)
library(tidyverse)
library(readxl)
library(DT)

# define report dropdown
logs <- c(
    "Anaesthetic Cases", 
    "Paediatric Cases", 
    "Regional Cases", 
    "Obstetric Cases ASA 3+",
    "General ASA 3+",
    "Paediatric Summary",
    "Regional Summary", 
#        "Obstetric Summary",
    "Sessions Summary",
    "TIVA Cases",
    "Sedation"
    )

ui <- fluidPage(
    fileInput("upload", NULL, accept = ".xlsx"),
    selectInput("report", "Choose a report", logs),
    DTOutput("table")
)

server <- function(input, output, session) {

    # load and clean anaesthetic cases sheet
    anaesthetic_cases <- reactive({
        req(input$upload)

        # read input file
        data <- read_excel(input$upload$datapath, sheet = 1)
        # correct colnmaes
        colnames(data) <- str_replace_all(colnames(data), " ", "_")
        # create age columns
        data <- data %>%
        separate(Age, c("age_value", "age_units"))
        # set age value to integer
        data$age_value <- as.integer(data$age_value)
        # output final df
        data
    })

    # filter for paediatric cases and categorise under/over 5yrs
    paediatric_cases <- reactive({
        paeds_cases <- anaesthetic_cases() %>%
        filter(age_units %in% c("days", "months") | age_value <= 16) %>% 
        mutate(Age_Category = if_else(
            age_units %in% c("days", "months") | age_value < 5, "Under 5yrs", "5yrs or over"
            )
        )
        paeds_cases
    })

    # filter for regional cases
    regional_cases <- reactive({
        anaesthetic_cases() %>%
            select(contains("Regional"), -contains("Notes")) %>%
            drop_na()
    })

    # filter for Obstetric cases
    obstetric_cases <- reactive({
        anaesthetic_cases() %>%
            filter(Primary_Specialty == "Obstetrics")
    })

    # filter for Obstetric ASA 3+ cases
    obstetric_complex <- reactive({
        anaesthetic_cases() %>%
            filter(Primary_Specialty == "Obstetrics", ASA %in% c("asa-3", "asa-4"))
    })

    # GA ASA 3+ cases
    general_complex <- reactive({
        anaesthetic_cases() %>%
            filter(ASA %in% c("asa-3", "asa-4", "asa-5", "donor"))
    })

    # filter for TIVA cases
    tiva <- reactive({
        anaesthetic_cases() %>%
            filter(Procedure_Type %in% c("tci", "tiva"))
    })

    # filter for sedation cases
    sedation <- reactive({
        anaesthetic_cases() %>%
            filter(Mode_of_Anaesthesia == "sedation")
    })
    
    # summarise regional cases
    regional_summary <- reactive({
        regional_cases() %>%
            group_by(Regional_Type, Regional_Supervision) %>%
            summarise(Number = n()) %>%
            pivot_wider(
                names_from = Regional_Supervision,
                values_from = Number,
                values_fill = 0
                ) %>%
            mutate(total = distant + immediate + local + solo)
    })

    # summarise paeds cases
    paediatric_summary <- reactive({
        paediatric_cases() %>% 
            group_by(Age_Category, Supervision) %>% 
            summarise(Number = n()) %>% 
            pivot_wider(
                names_from = Supervision,
                values_from = Number,
                values_fill = 0
                ) %>% 
            mutate(Total = Distant + Immediate + Local)
    })

    # summarise obstetric cases # FIXME
    obstetric_summary <- reactive({
        obstetric_cases() %>% 
            group_by(ASA, Supervision) %>% 
            summarise(Number = n())
            pivot_wider(
                names_from = Supervision,
                values_from = Number,
                values_fill = 0
                )
    })

    # load and clean sessions sheet
    sessions <- reactive({
        req(input$upload)

        data <- read_excel(input$upload$datapath, sheet = 2)
        colnames(data) <- str_replace_all(colnames(data), " ", "_")
        data
    })

    # summarise sessions
    sessions_summary <- reactive({
        sessions() %>%
            group_by(Activity) %>%
            summarise(Sessions = n())
    })

    # load and clean procedures sheet
    procedures <- reactive({
        req(input$upload)

        data <- read_excel(input$upload$datapath, sheet = 3)
        colnames(data) <- str_replace_all(colnames(data), " ", "_")
        colnames(data) <- str_replace_all(colnames(data), "[()]", "")
        data
    })

    # load and clean icm cases sheet
    icm_cases <- reactive({
        req(input$upload)

        data <- read_excel(input$upload$datapath, sheet = 4)
        colnames(data) <- str_replace_all(colnames(data), " ", "_")
        data
    })

    output$table <- renderDT(
        switch(input$report,
            "Anaesthetic Cases" = anaesthetic_cases(),
            "Paediatric Cases" = paediatric_cases(),
            "Regional Cases" = regional_cases(),
            "Paediatric Summary" = paediatric_summary(),
            "Regional Summary" = regional_summary(),
#                "Obstetric Summary" = obstetric_summary(),
            "Sessions Summary" = sessions_summary(),
            "Obstetric Cases ASA 3+" = obstetric_complex(),
            "General ASA 3+" = general_complex(),
            "TIVA Cases" = tiva(),
            "Sedation" = sedation()
        )
    )
}
shinyApp(ui = ui, server = server)
