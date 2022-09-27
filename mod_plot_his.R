# Module UI
  
#' @title   mod_plot_his_ui
#' @description  UI of Shiny module which provides a histogram visualization.
#' @rdname mod_plot_his_ui
#' @param id shiny id
#' @keywords internal


mod_plot_his_ui <- function(id) {
# Define namespace function for UI IDs
  ns <- NS(id)
  
# Create tagList to be used in the UI
  tagList(
    plotOutput(ns("plot1"))
  )  # tagList
  
}  # mod_plot_his_ui

# Module Server
#' @title   mod_plot_his_server
#' @description  Server of Shiny module which provides a histogram visualization.  
#' @rdname mod_plot_his_server
#' @param input internal
#' @param output internal
#' @param session internal
#' @param simOutput internal
#' @param histfeatures1 internal
#' @param histfeatures2 internal
#' @param CTfeatures1 internal
#' @param CTfeatures2 internal
#' @keywords internal

mod_plot_his_server <- function(input, output, session, simOutput, histfeatures1, histfeatures2, CTfeatures1, CTfeatures2) {
# Define namespace function for UI IDs
  ns <- session$ns


  
#Define plot output for a concentration histogram
    plotOutput <- reactive({
      # Only run if pre-requisite inputs exist
      # Note: Using shiny:: namespace to prevent clashes with mrgsolve
      shiny::req(simOutput())
      simData <- simOutput()
      d <- simData[!duplicated(simData$ID),]

      
      themeOptions <- list(legend.position = "bottom")
      if (!"Saved" %in% simData$simType) { themeOptions$legend.position <- "none" }
      p <- NULL
      data(extran3)
      extran3
      
      g <- function(x){
        par(mfcol=c(1,3))

        hist(d$VCi,
             main= histfeatures1[[1]],
             xlab= histfeatures1[[2]],
             ylab= histfeatures1[[3]],
             col= histfeatures1[[4]],
             ylim = c(0,histfeatures2[[1]]),
             freq=TRUE
        )
      
        hist(d$KAi,
             main=histfeatures1[[5]],
             xlab=histfeatures1[[6]],
             ylab=histfeatures1[[7]],
             col=histfeatures1[[8]],
             ylim = c(0,histfeatures2[[2]]),
             freq=TRUE
        )
        
        hist(d$CLi,
             main=histfeatures1[[9]],
             xlab=histfeatures1[[10]],
             ylab=histfeatures1[[11]],
             col=histfeatures1[[12]],
             ylim = c(0,histfeatures2[[3]]),
             freq=TRUE
        )
        
        
      }
      
      do.call("g", list(histfeatures1))
        })
    
  # Render plot output to Shiny UI
  output$plot1 <- renderPlot(plotOutput())
}  # mod_plot_his_server
