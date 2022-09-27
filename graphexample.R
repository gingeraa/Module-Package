# Clear objects from environment
rm(list = ls(all = TRUE))

# Load package dependencies
library(shiny)
library(ggplot2)
library(mrgsolve)
library(plyr)
library(dplyr)
`%>%` <- magrittr::`%>%`



# Source function and module dependencies (stored in ~/R directory)

source("/home/desktop/Users/ANDERG23/RFiles/12.2PlotModuleFiles/mod_mrgSimulate.R")
source("/home/desktop/Users/ANDERG23/RFiles/12.2PlotModuleFiles/mod_DoseInput.R")
source("/home/desktop/Users/ANDERG23/RFiles/12.2PlotModuleFiles/server.R")
source("/home/desktop/Users/ANDERG23/RFiles/12.2PlotModuleFiles/ui.R")
source("/home/desktop/Users/ANDERG23/RFiles/12.2PlotModuleFiles/mod_plot_CT1.13.R") #to load concentration-time plot



# Set ggplot2 theme
theme_bw2 <- ggplot2::theme_set(ggplot2::theme_bw(base_size = 16))
ggplot2::theme_update(plot.margin = ggplot2::unit(c(1, 1.5, 1, 1), "lines"))

ci90lo <- function(x) quantile(x, prob = 0.05, na.rm = TRUE)
ci90hi <- function(x) quantile(x, prob = 0.95, na.rm = TRUE)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   
# Model Definition (non-reactive) ---------------------------------------------

# Load model either from current directory or preferably ~/data
code <- '
$PROB PF-house

$PLUGIN base

$PARAM @annotated
CL   : 1.5    : Clearance  (L/hr)
VC   : 22   : Volume of distribution (L)
KA   : 1.5  : Absorption rate constant (1/hr)
F1   : 1.0  : Bioavailability fraction (.)
D1   : 1.5  : Infusion duration (hr)
WT   : 70   : Weight (kg)
SEX  : 0    : Covariate female sex
WTCL : 0.75 : Exponent WT on CL
WTVC : 1.00 : Exponent WT on VC
SEXCL: 0.7  : Prop cov effect on CL
SEXVC: 0.85 : Prop cov effect on VC
KIN  : 110  : Resp prod rate constant (1/hr)
KOUT : 2    : Resp elim rate constant (1/hr)
IC50 : 12   : Conc giving 50% max resp (ng/ml)

$CMT @annotated
GUT  : Dosing compartment (mg)
CENT : Central compartment (mg)
RESP : Response (unitless)

$OMEGA @labels ECL EVC EKA EKOUT
0.25 0.25 0.1 0.3

$SIGMA @labels EXPO
0.45

$SET end=120, delta=0.25


$GLOBAL
#define CP (CENT/VCi)
#define INH (CP/(IC50+CP))

typedef double localdouble;

$MAIN
F_GUT = F1;
D_CENT = D1;

double CLi   = exp(log(CL)   + WTCL*log(WT/70) + log(SEXCL)*SEX + ECL);
double VCi   = exp(log(VC)   + WTVC*log(WT/70) + log(SEXVC)*SEX + EVC);
double KAi   = exp(log(KA)   + EKA);
double KOUTi = exp(log(KOUT) + EKOUT);

RESP_0 = KIN/KOUTi;

$ODE
dxdt_GUT = -KAi*GUT;
dxdt_CENT = KAi*GUT - (CLi/VCi)*CENT;
dxdt_RESP = KIN*(1-INH) - KOUTi*RESP;

$TABLE
double DV = CP*exp(EXPO);


$CAPTURE @annotated
DV: Dependent variable (ng/ml)
CP: Plasma concentration (ng/ml)
VCi: Volume
KAi: Absorption 
CLi: Clearance
'


mod <- mcode("house",code)

# Load model either from current directory or preferably ~/data
mrgModel <- mod




shinyApp(ui = app_ui, server = app_server)


