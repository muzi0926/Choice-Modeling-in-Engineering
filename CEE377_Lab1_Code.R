# ################################################################# #
#### CEE 377: LAB 1                                              ####
# ################################################################# #

### Instructions:
### Download data file from Canvas
### Run this code line by line
### Work through error messages
### Examine output 

# ################################################################# #
#### STEP 1: LOAD LIBRARY AND DEFINE CORE SETTINGS               ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Install Apollo 
if(!suppressPackageStartupMessages(require('apollo')))	install.packages('apollo')

### Load Apollo library
library(apollo)

### Initialize code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="Lab1_BL_04202022",
  modelDescr ="Binary logit model on evacuation choice data",
  indivID   ="INDIV_ID" #, #Uncomment comma for random parameter model  
  #mixing    = TRUE, #Uncomment line for random parameter model
  #nCores    = 3 #Uncomment line for random parameter model
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

setwd("~/Desktop/Study/22-2 Spring/Behavioral Choice Modeling in Engineering/Week 5_Lab 1") #This will depend on where on your computer the data file is located
database = read.csv("CEE377_Lab1_Data.csv",header=TRUE)
names(database)[1]<-'INDIV_ID'
database$AGE_TRAVELTIME_DISTANCE = database$AGE * database$TRAVELTIME * database$DISTANCE
database$DISABILITY_TRAVELTIME_DISTANCE = database$DISABILITY * database$TRAVELTIME * database$DISTANCE
#database$ELDERLY_DISTANCE_TRAVELTIME = database$ELDERLY * database$DISTANCE * database$TRAVELTIME
#database$HH_SIZE_VEHICLES = database$HH_SIZE * database$VEHICLES
#database$URBAN=ifelse(database$AREA==1,1,0)
#database$COSTOVERINCOME = database$COST * database$INCOME
#database$FEMALE = ifelse(database$GENDER==2,1,0)
#database$ttfemale=database$TRAVELTIME*database$FEMALE
# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

### This is where you will define betas for additional model parameters 
### Start with zero, then update with estimate for shorter run times (optional)
### Make sure there is a comma at the end of each line but the last 

apollo_beta = c(
                
                ### Alternative specific constants:
                asc_go                       = -0.72709, 
                asc_stay                     = 0,
                
                ### Ride attributes: 
                #v_traveltime                 = -0.035472,
                #v_costincome                = 0,
                #v_income                    = 0,
                
                v_waittime                  = -0.036309, 
                #v_vehicles                  = 0,
                v_cost                       = 0.01634,#Comment line for random parameter model
                v_urgency                  = 0,
                v_distance                   = 0,
                v_private                    = 0,
                v_peerchoice_stay            = 0,
                #v_COSTOVERINCOME             = 0,
                #v_ELDERLY_DISTANCE_TRAVELTIME           = 0,
                #v_hh_size_vehicles           = 0,
                #mean_cost                   = -27.9991, #Uncomment line for random parameter model
                #stdv_cost_inter             = 9.4330, #Uncomment line for random parameter model
                
                ### Sociodemographics 
                #v_FEMALE                    = 0,
                #v_ttfemale                  = 0,
                #v_hh_size                   = 0,
                #v_elderly                   = 0,
                #v_URBAN                     = 0,
                v_AGE_TRAVELTIME_DISTANCE    = 0,
                v_DISABILITY_TRAVELTIME_DISTANCE = 0,
                #v_DISABILITY                 = 0,
                v_pets                      = 0.721259
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_stay")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS (OPTIONAL)                         ####
# ################################################################# #

### Set parameters for generating draws

#Uncomment the following 13 ines for random parameter model (using “control + shift + C”):

# apollo_draws = list(
#   interDrawsType = "halton",
#   interNDraws    = 100, #increase to 500 or 1000
# 
#   interUnifDraws = c(),
#   interNormDraws = c("N_draws_cost_inter"),
# 
#   intraDrawsType = "halton",
#   intraNDraws    = 100,
#   intraUnifDraws = c(),
# 
#   intraNormDraws = c()
# )

### Create random parameters


#Uncomment the following 8 lines for random parameter model:

# apollo_randCoeff = function(apollo_beta, apollo_inputs){
#   randcoeff = list()
# 
#   randcoeff[["v_cost"]] = -exp( mean_cost
#                                  + stdv_cost_inter      * N_draws_cost_inter ) 
# 
#   return(randcoeff)
# }

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### List of utilities: these must use the same names used above, order is irrelevant
  V = list()
  
  ### This is where you will add ride attributes, sociodemographics, etc.
  V[['alt1']] = asc_go + v_cost*COST  + v_waittime*WAITTIME   +  v_urgency*URGENCY + v_private*PRIVATE #+ v_COSTOVERINCOME*COSTOVERINCOME # + v_traveltime*TRAVELTIME + v_hh_size_vehicles*HH_SIZE_VEHICLES #+ v_URBAN*URBAN #+ v_hh_size*HH_SIZE # + v_income*INCOME + v_costincome*costincome  + v_FEMALE*FEMALE + v_ttfemale*ttfemale
  V[['alt2']] = asc_stay  + v_pets*PETS + v_distance*DISTANCE + v_peerchoice_stay*PEERCHOICE_STAY + v_AGE_TRAVELTIME_DISTANCE * AGE_TRAVELTIME_DISTANCE + v_DISABILITY_TRAVELTIME_DISTANCE * DISABILITY_TRAVELTIME_DISTANCE #+ v_DISABILITY*DISABILITY#+ v_ELDERLY_DISTANCE_TRAVELTIME * ELDERLY_DISTANCE_TRAVELTIME #+ v_elderly*ELDERLY #+ v_vehicles * VEHICLES
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = list(alt1=1, alt2=1),
    choiceVar     = ALT,
    V             = V
  )

  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)

  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ### Average across inter-individual draws
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality) #Uncomment line for random parameter model

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
  
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, 
                        apollo_inputs, 
                        list(writeIter=FALSE)) #Comment line for random parameter model 
                        #estimate_settings=list(hessianRoutine="maxLik")) #Uncomment line for random parameter model

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model, list(printPVal = TRUE))

PETS_data=database$PETS
avgPETS =mean(database$PETS )
sdPETS =sd(database$PETS )
minPETS =min(database$PETS )
maPETSX =max(database$PETS )
PETS <-c(avgPETS ,sdPETS ,minPETS ,maPETSX )


