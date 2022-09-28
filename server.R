app_server <- function(input, output, session) {
  # Simulate Tab
  # Call dosing input module; doseInput
  doseInput <- callModule(mod_DoseInput_server, 
                          id = "dose1", initial = c(500, 24, 3), 
                          units = c("mg", "hours", "days"), relativeValue = c(1, 1, 24)
  )  # callModule
  
  # Call simulation module for mrgsolve; mrgSimulate
  simOutput <- callModule(mod_mrgSimulate_server, parentInput = input, 
                          id = "sim1", model = mrgModel, nsim = "nid", dose = doseInput,  
                          covariates = c(WT = "bwt",  SEX = "sex"),
                          runOnStart = FALSE
  )  # callModule
  
  # Call plot module
  plotOutput <- callModule(mod_plot_server, id = "plot1", simOutput = simOutput, 
                           CTfeatures1 = c(linecolor = "black", fill = "grey40", conctimexaxis = "Time After Dose (hours)", "Concentration (ng/mL)"), CTfeatures2 =c(0.05,0.95), #(CI low boundary,CI high boundary)
                           histfeatures1 = c(Vtitle = "Volume of distribution (L)", Vxlab = "Volume (L)", Vylab = "Count", VFill = "darkblue", KAtitle = "Absorption rate constant (1/hr)", KAxlab = "KA (1/hr)", 
                                             KAylab = "Count", KAFill = "darkblue", CLtitle = "Clearance  (L/hr)", CLxlab= "Clearance  (L/hr)", CLylab = "Count",
                                             CLFill = "darkblue"), 
                           histfeatures2 = c(50,50,50)  #yaxis upper limit for V, KA, and Cl
  )  # callModule
  
}  # app_server