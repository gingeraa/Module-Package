# Module UI
  
#' @title   mod_table_ui
#' @description  UI of Shiny module which provides a table summary of the simulation.
#' @rdname mod_table_ui
#' @param id shiny id
#' @keywords internal 

mod_table_ui <- function(id) {
# Define namespace function for UI IDs
  ns <- NS(id)
  
# Create tagList to be used in the UI
  tagList(
    hr(),
    h4("Single Dose"),
    tableOutput(ns("table1")),
    h4("Steady State"),
    tableOutput(ns("table2"))
  )  # tagList together 
  
}  # mod_table_ui

# Module Server
#' @title mod_table_server
#' @description  Server of Shiny module which provides a table summary of the simulation.   
#' @rdname mod_table_server
#' @param input internal
#' @param output internal
#' @param session internal
#' @param simOutput internal
#' @param doseInput internal
#' @keywords internal

mod_table_server <- function(input, output, session, simOutput, doseInput) {
# Define namespace function for UI IDs
  ns <- session$ns
  
  
  plotOutput <- reactive({
    # Only run if pre-requisite inputs exist
    # Note: Using shiny:: namespace to prevent clashes with mrgsolve
    
    # Define summary functions
    ci90lo <- function(x) quantile(x, prob = 0.05, na.rm = TRUE)
    ci90hi <- function(x) quantile(x, prob = 0.95, na.rm = TRUE)
    
    shiny::req(simOutput())
    simdf <- simOutput()
    
    
    shiny::req(doseInput())
    dosein <- doseInput()
    
    
      NID <- max(simdf$ID)
      DOSEFRQ <- (24/dosein$ii)
      # Script Outputs 
      pksum024 <- simdf %>%
        group_by(ID) %>%
        summarise(
          AUCtau = pk.calc.auc.last(CP, time, interval = c(0, DOSEFRQ)),
          Cmax = pk.calc.cmax(CP),
          Ctrough = pk.calc.ctrough(CP, time, end = DOSEFRQ)) %>%
          mutate(Cave = pk.calc.cav(AUCtau, start = 0, end = DOSEFRQ)) %>%
          summarise_at(vars(-ID),  list(Median = median, Mean = mean, SD = sd, UL = max, LL = min, ci90lo = ci90lo, ci90hi = ci90hi)) %>%
          summarise_all(signif, digits = 3) %>%
          pivot_longer(everything()) %>%
          separate(name, c("Metric", "stat"), sep = "_") %>%
          pivot_wider(id_cols = Metric, names_from = "stat", values_from = "value") %>%
          unite("90% PI", ci90lo, ci90hi, sep = " - ") %>%
        purrr::when(
          NID == 1 ~ mutate(., `90% PI` = "N/A"),
          NID != 1 ~ .
        )
      
      
      pksum024
    })

  output$table1 <- renderTable(plotOutput())
  
  
  plotOutput2 <- reactive({
    # Only run if pre-requisite inputs exist
    # Note: Using shiny:: namespace to prevent clashes with mrgsolve
    
    # Define summary functions
    ci90lo <- function(x) quantile(x, prob = 0.05, na.rm = TRUE)
    ci90hi <- function(x) quantile(x, prob = 0.95, na.rm = TRUE)
    
    
    
    shiny::req(simOutput())
    simdf <- simOutput()
    
    
    shiny::req(doseInput())
    dosein <- doseInput()
    
    
    NID <- max(simdf$ID)
    DOSEFRQ <- (24/dosein$ii)
    DOSEDUR <- (1 + dosein$addl)
    # Script Outputs 
    pksumSS <- simdf %>%
      #filter(evid == 0 & time >= (24*DOSEDUR - DOSEFRQ) & time <= 24*DOSEDUR) %>%
      group_by(ID) %>%
      summarise(
        AUCtau = pk.calc.auc.last(CP, time, interval = c(24*DOSEDUR - DOSEFRQ, 24*DOSEDUR)),
        Cmax = pk.calc.cmax(CP),
        Ctrough = pk.calc.ctrough(CP, time, end = 24*DOSEDUR)) %>%
      mutate(Cave = pk.calc.cav(AUCtau, start = 24*DOSEDUR - DOSEFRQ, end = 24*DOSEDUR)) %>%
      summarise_at(vars(-ID), list(Median = median, Mean = mean, SD = sd, UL = max, LL = min, ci90lo = ci90lo, ci90hi = ci90hi)) %>%
      summarise_all(signif, digits = 3) %>%
      pivot_longer(everything()) %>%
      separate(name, c("Metric", "stat"), sep = "_") %>%
      pivot_wider(id_cols = Metric, names_from = "stat", values_from = "value") %>%
      unite("90% PI", ci90lo, ci90hi, sep = " - ") %>%
      purrr::when(
        NID == 1 ~ mutate(., `90% PI` = "N/A"),
        NID != 1 ~ .
      )
    
    
    
    pksumSS
  })
  
  output$table2 <- renderTable(plotOutput2())

}  # mod_table_server
