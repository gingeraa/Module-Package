# Module UI
  
#' @title mod_mrgSimulate_ui
#' @description  UI of Shiny module which simulates the pharmacokinetic model with the input from the human app user.
#' @rdname mod_mrgSimulate_ui
#' @param id shiny id
#' @param label internal
#' @keywords internal


mod_mrgSimulate_ui <- function(id, label, saveButton = FALSE, ...) {
# Define namespace function for IDs
  ns <- shiny::NS(id)
  
# Create tagList to be used in the UI
  if (saveButton) {
    if (length(label) == 1) {
      warning(
        "The label argument for mod_mrgSimulate_ui requires a vector of ", 
        "three character strings when saveButton equals TRUE.\n",
        "Providing default labels for Save and Clear buttons."
      )
      label[2:3] <- c("Save Current Output", "Clear Saved Output")
    }
    if (length(label) != 3) {
      stop(
        "The label argument for mod_mrgSimulate_ui requires a vector of ", 
        "three character strings when saveButton equals TRUE.\n",
        "Please provide labels for the Simulate, Save and Clear buttons."
      )
    }
    out <- tagList(
      fluidRow(
        column(12, 
          actionButton(ns("sim"), 
            label = label[[1]], ..., 
            title = paste(
              "Runs the simulation with the current inputs updating both plots", 
              "and tables."
            )  # paste
          ),  # actionButton
          style = "margin-bottom: 10px;"
        )  # column
      ),  # fluidRow
      fluidRow(
        column(6,
          actionButton(ns("save"), 
            label = label[[2]], ..., 
            title = paste(
              "Saves current output and displays saved output in plots and", 
              "tables. Simulations from updated inputs can then be compared", 
              "with the saved simulation."
            )  # paste
          ),  # actionButton
          style = "padding-right: 5px;"
        ),  # column
        column(6,
          actionButton(ns("clear"), 
            label = label[[3]], ...,
            title = "Removes the saved output from a previous simulation."
          ),  # actionButton
          style = "padding-left: 5px;"
        )  # column
      )  # fluidRow
    )  # tagList
  } else {
    out <- fluidRow(
      column(12, 
        actionButton(ns("sim"), 
          label = label[[1]], ..., 
          title = paste(
            "Runs the simulation with the current inputs updating both plots", 
            "and tables."
          )  # paste
        )  # actionButton
      )  # column
    )  # fluidRow
  }  # if/else
  
  return(out)
  
}  # mod_mrgSimulate_ui

# Module Server

#' @title mod_mrgSimulate_server
#' @description  Server of Shiny module which simulates the pharmacokinetic model with the input from the human app user.
#' @rdname mod_mrgSimulate_server
#' @param input internal
#' @param output internal
#' @param session internal
#' @param parentInput internal
#' @param model internal
#' @param nsim internal
#' @param dose internal
#' @param covariates internal
#' @keywords internal


mod_mrgSimulate_server <- function(input, output, session, parentInput,
  model, nsim, dose, covariates, 
  runOnStart = FALSE, mrgOptions = list()
) {
# Create reactiveValues object to store simulation output
# * `rsave$input` contains the saved input for the mrgsolve simulation
# * `rsave$output` contains the saved output from the mrgsolve simulation
  rsave <- shiny::reactiveValues(
    input = NULL,
    output = NULL
  )
  
  mrgInput <- shiny::eventReactive(input$sim, ignoreNULL = !runOnStart, {
  # Only run if pre-requisite inputs exist
  # Note: Using shiny:: namespace to prevent clashes with mrgsolve
    shiny::req(dose())
    covInput <- lapply(covariates, function(cov) as.double(parentInput[[cov]]))
    names(covInput) <- names(covariates)
  # Return mrgsolve inputs as a list of arguments for fct_mrgSimulate
    list(
      nsimInput = parentInput[[nsim]],
      doseInput = dose(),
      covInput = covInput,
      mrgOptions = mrgOptions
    )
  })  # eventReactive

# Observe
  mrgOutput <- shiny::reactive({
  # Only run if pre-requisite inputs exist
    shiny::req(mrgInput())
  # Run simulation
    simOptions <- mrgInput()
    simOptions$mod <- model
    do.call(fct_mrgSimulate, simOptions)
  })
  
# Observe
  shiny::observeEvent(input$save, {
  # Only run if pre-requisite inputs exist
    shiny::req(mrgOutput())
    rsave$input <- mrgInput()
    rsave$output <- mrgOutput()
  })
  
  shiny::observeEvent(input$clear, {
  # Only run if pre-requisite inputs exist
    shiny::req(rsave$output)
    rsave$input <- NULL
    rsave$output <- NULL
  })
  
# Add bookmark handling
# Exclude actionButton values from bookmarking
# Prevents associated events triggering on restore
# Not removing "sim" as trigger renders plot even if runOnStart = FALSE
  shiny::setBookmarkExclude(c("save", "clear"))
  
# Save values in state$values when bookmark occurs
# Only input is saved, as simulated output can be very large
# Dev note: Module namespace is handled automatically by shiny
  shiny::onBookmark(function(state) {
    state$values$input <- rsave$input
  })

# Read values from state$values after bookmark restore has occurs
# Occurs after bookmark restore as it requires 
  shiny::onRestore(function(state) {
    rsave$input <- state$values$input
    simOptions <- rsave$input
    simOptions$mod <- model
    rsave$output <- do.call(fct_mrgSimulate, simOptions)
  })
  
# Define module output
  moduleOutput <- shiny::reactive({
  # Only run if pre-requisite inputs exist
    shiny::req(mrgOutput())
  # Add simulation type identifier to current simulation for use by outputs
    mrgOutput <- mrgOutput()
    mrgOutput$simType <- "Current"
    attr(mrgOutput, "currentInput") <- mrgInput()
  # If data is saved bind saved and original data together
    if (!is.null(rsave$input)) {
      shiny::req(rsave$output)
    # Add simulation type identifier to saved simulation for use by outputs
      savedOutput <- rsave$output
      savedOutput$simType <- "Saved"
      mrgOutput <- rbind(mrgOutput, savedOutput)
      attr(mrgOutput, "savedInput") <- rsave$input
    }
    return(mrgOutput)
  })
  
# Return module output to parent environment
  return(moduleOutput)
  
}  # mod_mrgSimulate_server

#' @title   fct_mrgSimulate
#' @description  mod_mrgSimulate function which actually uses mrgsolve to simulate.
#' @rdname fct_mrgSimulate
#' @param mod
#' @param nsimInput
#' @param doseInput
#' @param covInput
#' @param mrgOptions
#' @keywords internal


fct_mrgSimulate_random <- function(mod, nsimInput, doseInput, covInput, mrgOptions) {
# Error recovery
  if (nsimInput < 1) {
    nsimInput <- 1
    shiny::showNotification(paste("The number of simulated individuals must be",  
      "greater than zero. Number of simulated individuals was changed to 1."), 
      type = "error", duration = 30)
  }
# Update model
  if (nsimInput != 1) {
    simModel <- mod
  } else {
    mrgOmega <- mrgsolve::omat(simModel, make = TRUE)
    newOmega <- diag(rep(0, nrow(mrgOmega)))
    simModel <- mrgsolve::omat(simModel, newOmega)
  }
  eventInput <- do.call(what = mrgsolve::ev, args = doseInput)
# Add default options if user has not already specified
  if (!any(c("start", "end", "delta", "tgrid") %in% names(mrgOptions))) {
    mrgOptions$start <- head(doseInput$time, 1)
    mrgOptions$end <- with(tail(doseInput, 1), time + ii*(addl + 4))
    mrgOptions$delta <- 0.5
  }
  if (!"obsonly" %in% names(mrgOptions)) { mrgOptions$obsonly <- TRUE }
# Simulate
  do.call(args = mrgOptions, what = function(...) {
    mrgsolve::mrgsim_df(
      x = simModel,
      param = covInput, 
      events = eventInput, 
      nid = nsimInput,
      ...  # additional parameters passed from module server function
    )  # mrgsim_df
  })  # do.call
}  # fct_mrgSimulate

fct_mrgSimulate <- shiny::repeatable(fct_mrgSimulate_random)