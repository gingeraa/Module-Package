# Module UI
  
#' @title   mod_multiDoseInput_ui
#' @description  UI of Shiny module which handles multidose input.
#' @rdname mod_multiDoseInput_ui
#' @param id shiny id
#' @keywords internal


mod_multiDoseInput_ui <- function(id, ...) {
# Define namespace function for UI IDs
  ns <- NS(id)
  
# Create tagList to be used in the UI
  tagList(
    uiOutput(ns("regimen")),
    fluidRow(style = "margin-bottom:10px;", 
      column(6, actionButton(ns("add"), 
        label = "Add", title = "Add additional row of doses.",
        width = "100%", ...)),
      column(6, actionButton(ns("rem"), 
        label = "Remove", title = "Remove last row of doses.",
        width = "100%", ...))
    )  # fluidRow
  )  # tagList
  
}  # mod_multiDoseInput_ui

# Module Server
#' @title   mod_multiDoseInput_server
#' @description  Server of Shiny module which handles multidose input.    
#' @rdname mod_multiDoseInput_server
#' @param input internal
#' @param output internal
#' @param session internal
#' @param initial internal
#' @param units internal
#' @param relativeValue internal
#' @param nrow internal
#' @param maxrow internal
#' @keywords internal

mod_multiDoseInput_server <- function(input, output, session, initial, 
  units = c("mg", "hours", "days"), relativeValue = c(1, 1, 24), 
  nrow = 1, maxrow = 3
) {
# Define namespace function for UI IDs
  ns <- session$ns

# Provide session info as output
# Create reactiveValues object to store renderUI input values
# * n is the number of dosing regimens 
#     + it dictates the number of rendered input boxes as well as how many
#       amt, int and dur values are used when passing to the model
# * amt, int and dur are vectors for the dose, interval and duration for the
#   first, second and third dosing regimen
#     + if no second or third dosing regimen exists, values in those positions
#       are not output
  rv <- reactiveValues(
    n = nrow,  # number of rendered input boxes (min: 1, max: 3)
    amt = rep(initial[[1]], maxrow),
    int = rep(initial[[2]], maxrow),
    dur = rep(initial[[3]], maxrow)
  )
  
# Define reactive function for dynamic input box ui
# * map used for creating the same set of input boxes `rv$n` times
# * ilab and padding are defined so that only one label is rendered for all
#   `rv$n` input boxes
# * fluidRow wrapped in column to remove padding issues
# * div used to define column function with width that applies to any size screen
# * inputId given standardised name depending on the row of input boxes it is in
# * value equals corresponding `rv` value indexed using input box row
  Rui <- reactive({
    purrr::map(seq(1, rv$n, by = 1), function(i) {
      if (i == 1) { 
        ilab <- c("Dose:", "Interval:", "Duration:") 
        padding <- "padding-top:25px; padding-left:0px"
      } else { 
        ilab <- NULL 
        padding <- "padding-left:0px"
      }
      fluidRow(
        div(class = "col-xs-3",
          numericInput(ns(paste0("amt", i)), 
            label = ilab[[1]], 
            value = rv$amt[[i]]
          )  # numericInput
        ), # div
        div(class = "col-xs-1",
          style = padding,
          p(strong(paste0(units[[1]], ", every")))
        ), # column
        div(class = "col-xs-3",
          numericInput(ns(paste0("int", i)), 
            label = ilab[[2]], 
            value = rv$int[[i]]
          )  # numericInput)
        ), # column
        div(class = "col-xs-1",
          style = padding,
          p(strong(paste0(units[[2]], ", for")))
        ), # column
        div(class = "col-xs-3",
          numericInput(ns(paste0("dur", i)), 
            label = ilab[[3]], 
            value = rv$dur[[i]]
          )  # numericInput
        ), # column
        div(class = "col-xs-1",
          style = padding,
          p(strong(units[[3]]))
        )  # column
      )  # fluidRow
    })  # map
  })  # reactive
  
# Observe add button for dynamic ui
# * Update values in `rv[[names]]` using existing input boxes using `index`
# * Add 1 to the `rv$n` if below the maximum (3)
  observeEvent(input$add, {
    names <- c("amt", "int", "dur")
    index <- seq(1, rv$n, by = 1)
    purrr::walk(names, function(x) { 
      rv[[x]][index] <- purrr::map_dbl(index, function(i) input[[paste0(x, i)]])
    })
    if (rv$n < 3) { rv$n <- rv$n + 1 }
  })  # observeEvent
  
# Observe remove button for dynamic ui
# * Update values in `rv[[names]]` using existing input boxes using `index`
# * Subtract 1 from the `rv$n` if above the minimum (1)
  observeEvent(input$rem, {
    names <- c("amt", "int", "dur")
    index <- seq(1, rv$n, by = 1)
    purrr::walk(names, function(x) { 
      rv[[x]][index] <- purrr::map_dbl(index, function(i) input[[paste0(x, i)]])
    })
    if (rv$n > 1) { rv$n <- rv$n - 1 }
  })  # observeEvent
  
# Define output for dynamic ui
  output$regimen <- renderUI(Rui())
  
# Define reactive function that processes the dosing information
  moduleOutput <- reactive({
  # Only run if pre-requisite inputs exist
    inputNames <- c("amt", "int", "dur")
    index <- seq(1, rv$n, by = 1)
  # If simulation is requested prior to dosage UI being rendered
    if (is.null(input[[paste0("amt", 1)]])) {
    # Extract default dose from reactiveValues
      doseInput <- purrr::map(inputNames, function(x) { 
        purrr::map_dbl(index, function(i) rv[[x]][[i]])
      })  # map
  # Otherwise, if dosage UI has been rendered at least once
    } else {
    # Require dosage UI for desired number of doses to be rendered
      shiny::req(input[[paste0("amt", rv$n)]])
    # Extract user dose from input  
      doseInput <- purrr::map(inputNames, function(x) { 
        purrr::map_dbl(index, function(i) input[[paste0(x, i)]])
      })  # map
    }  # if/else
    names(doseInput) <- inputNames
  # Process dose input into mrgsolve::ev input
    doseEvents <- purrr::map_dfr(index, function(i) {
    # Convert values to units required for simulation
      amt <- relativeValue[[1]]*doseInput$amt[[i]]
      int <- relativeValue[[2]]*doseInput$int[[i]]
      dur <- relativeValue[[3]]*doseInput$dur[[i]]
    # Compute time after first dose
      time <- ifelse(i == 1, 0, 
        relativeValue[[3]]*sum(doseInput$dur[seq(1, (i - 1), by = 1)])
      )  # ifelse
    # Perform error handling
      if (int > dur) {
        dur <- int
        doseInput$dur[[i]] <<- dur/relativeValue[[3]]
        showNotification(paste("The dosing duration must be equal or longerer",  
          "than the dosing interval. Dosing duration", i, "was extended to", 
          doseInput$dur[[i]], units[[3]], "prior to simulation."), 
          type = "error", duration = 30
        )  # showNotification
      }  # if
      if (dur %% int != 0) {
        dur <- dur - (dur %% int)
        doseInput$dur[[i]] <<- dur/relativeValue[[3]]
        showNotification(paste("The dosing duration must be divisible by the",  
          "dosing interval. Dosing duration", i, "was reduced to", 
          doseInput$dur[[i]], units[[3]], "prior to simulation."), 
          type = "error", duration = 30
        )  # showNotification
      }  # if
    # Return data.frame for use with mrgsolve::ev
      data.frame(stringsAsFactors = FALSE,
        amt = amt,
        time = time,
        ii = int,
        addl = dur/int - 1
      )  # data.frame
    })  # map_dfr
    attr(doseEvents, "regimenUnits") <- units
    attr(doseEvents, "relativeValue") <- relativeValue
    return(doseEvents)
  })  # reactive
  
# Add bookmark handling
# Exclude actionButton values from bookmarking
# Prevents associated events triggering on restore
  setBookmarkExclude(c("add", "rem"))
  
# Save values in state$values when bookmark occurs
# Dev note: Module namespace is handled automatically by shiny
  onBookmark(function(state) {
    state$values$n <- rv$n
    state$values$amt <- rv$amt
    state$values$int <- rv$int
    state$values$dur <- rv$dur
  })

# Read values from state$values when restore from bookmark occurs
  onRestore(function(state) {
    rv$n <- state$values$n
    rv$amt <- state$values$amt
    rv$int <- state$values$int
    rv$dur <- state$values$dur
  })
  
# Return the processed dosing information to the parent
  return(moduleOutput)
  
}  # mod_multiDoseInput_server
