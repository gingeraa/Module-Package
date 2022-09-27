# Module UI
  
#' @title   mod_plot_CT_ui
#' @description  UI of Shiny module which provides visualization of the outputted simulations concentration-time curve. 
#' @rdname mod_plot_CT_ui
#' @param id shiny id
#' @keywords internal


mod_plot_CT_ui <- function(id) {
# Define namespace function for UI IDs
  ns <- NS(id)
  
# Create tagList to be used in the UI
  tagList(
    plotOutput(ns("plot1"))
  )  # tagList
  
}  # mod_plot_CT_ui

# Module Server
#' @title   mod_plot_CT_server
#' @description  Server of Shiny module which provides visualization of the outputted simulations concentration-time curve. 
#' @rdname mod_plot_CT_server
#' @param input internal
#' @param output internal
#' @param session internal
#' @param simOutput internal
#' @param histfeatures1 internal
#' @param histfeatures2 internal
#' @param CTfeatures1 internal
#' @param CTfeatures2 internal
#' @keywords internal


mod_plot_CT_server <- function(input, output, session, simOutput, CTfeatures1, CTfeatures2,histfeatures1, histfeatures2) {
# Define namespace function for UI IDs
  ns <- session$ns
  

  # Define plot output for a mean conconcentration-time curve with upper and lower 95% prediction intervals 
  plotOutput <- reactive({
    # Only run if pre-requisite inputs exist
    # Note: Using shiny:: namespace to prevent clashes with mrgsolve
    shiny::req(simOutput())
    simData <- simOutput()
    themeOptions <- list(legend.position = "bottom")
    if (!"Saved" %in% simData$simType) { themeOptions$legend.position <- "none" }
    
    

    d1a2a <- ddply(simData, .(time), Mean.DV = mean(CP), low = quantile(CP, CTfeatures2[[1]]), high = quantile(CP, CTfeatures2[[2]]),
                   summarize)
    
    
    g <- function(x){
    p <- NULL
    p <- ggplot(aes(x = time, y = Mean.DV), data = d1a2a)
    p <- p + geom_line(color=CTfeatures1[[1]])
    p <- p + geom_ribbon(aes(x = time, ymin = low, ymax = high), fill = CTfeatures1[[2]], alpha = 0.2)
    p <- p + do.call(ggplot2::theme, themeOptions)
    p <- p + xlab(CTfeatures1[[3]]) # for the x axis label 
    p <- p + ylab(CTfeatures1[[4]]) # for the y axis label
    suppressWarnings(p)      }
    
    do.call("g", list(CTfeatures1))
  })

  # Render plot output to Shiny UI
  output$plot1 <- renderPlot(plotOutput())
}  # mod_plot_CT_server
