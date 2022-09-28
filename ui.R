app_ui <- function(request) {
  navbarPage("App Title", id = "page",
             theme = shinythemes::shinytheme("cerulean"),
             tabPanel("Simulation",
                      sidebarLayout(
                        sidebarPanel(width = "6 col-lg-4",  # width 6 on small screens, 4 on large
                                     h4("Patient Population", style = "margin-top:0px;"),
                                     numericInput("nid", 
                                                  label = "Simulated Population Size",
                                                  value = 100
                                     ),  # numericInput
                                     numericInput("bwt", 
                                                  label = "Typical Baseline Body Weight (kg)",
                                                  value = 70
                                     ),  # numericInput
                                     selectInput("sex", 
                                                 label = "Sex",
                                                 choices = list(
                                                   "Male" = 0,
                                                   "Female" = 1),
                                                 selected = 0
                                     ),  # selectInput
                                     hr(),
                                     h4("Dosing Regimen"),
                                     mod_DoseInput_ui("dose1", class = "btn-primary"),
                                     hr(),
                                     mod_mrgSimulate_ui("sim1",
                                                        label = c(
                                                          sim = "Update Population and Dosing Regimen"
                                                        ),
                                                        #saveButton = TRUE, 
                                                        width = "100%", 
                                                        class = "btn-primary"
                                     ),  # mod_mrgSimulate_ui
                                     br()
                        ),  # sidebarPanel
                        mainPanel(width = "6 col-lg-8",  # width 6 on small screens, 8 on large
                                  h4("Simulation Output"),
                                  div(
                                    mod_plot_ui("plot1")
                                  )  # div
                        )  # mainPanel
                      )  # sidebarLayout
             )  # tabPanel
  )  # navbarPage
}  # app_ui
