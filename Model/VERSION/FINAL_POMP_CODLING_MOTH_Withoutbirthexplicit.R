#########################
###THE FULL POMP MODEL###
#########################
########################
###Coded by Damie Pak ##
########################
###Packages required#
#####################

library(pomp)#ENSURE THAT YOU ARE USING POMP2 NOT POMP
#library(here)
library(ggplot2) #This is ensuring for the plotting
library(dplyr) #This is to match up the original data and the simulation
library(viridis)
####################################################################
###THE COVARIATES THAT INCLUDE THE DAILY TEMPERATURE (TT), and   ###
###THE DIFFERENCE IN PHOTOPERIOD (PP) AND THE REAL TRAP DATA (ACT)##
####################################################################
####################################################################

##################################################################################################   
###NOTE: the original covar has the averaged trap data when it's more appropriate to use         #
### the MARSS time series ########################################################################
###number#########################################################################################
##################################################################################################

###Just load this RData file that has the data.frame- 
###originally, the covar's ARC is the averaged abundance and NOT
###the MARSS Data, which has to be loaded separately 
load(here("Data","covariates","covar.RData"))
###This is the MARSS abundance- must load!
load(here("Data","covariates","NEW_CM_ABUNDANCE.RData"))

###New pomp packages requires you to use the covariate-table- so run this 

###We replace the old ARC wih the new ARC
covartable$ARC <- NEW_CM_ABUNDANCE$Abund
COVAR <- covariate_table(covartable,times = 'time')
##########################################################################
###MORE FOR FITTING PURPOSES##############################################
##########################################################################


###covartable_fit <- covartable[1:4018,]
###COVAR_FIT <- covariate_table(covartable_fit,times = 'time')

###########################################################################################
##This is for any analysis where you want to explore hot climate situation-just copy and ##
###paste the covartable and add 4 degrees to the temperature ##############################
###########################################################################################
covartable_hot <- covartable
covartable_hot $TT <- covartable_hot$TT + 4
COVAR_HOT <- covariate_table(covartable_hot,times = 'time')
#############################################################################################

#############################################################
#################THE POMP MODEL BELOW #######################
#############################################################

######################################################
###THE NUMBER OF SUBCOMPARMENTS FOR EACH LIFE-STAGE##
######################################################
##############################
###Have to run this first ###
#############################
###Remember as the number of compartment increases- the variability
###in development rate decreases and vice versa.


nE <- 50 #Egg
nL1 <-30 #First Instar Larvae
nL2 <- 30 #Second Instar Larvae
nL3 <-30 #Third Instar Larvae
nL4 <-30 #Fourth Instar Larvae
nL5 <-30 #Fifth Instar Larvae
nP <-25 #Pupae
nAr <-25#Reproductive adult
nDL5  <-25#Diapausing fifth instar larvae

###This is what you put in the pomp so this just says 
###Hey C, I need these integers that will set the number
### of subcompartments 
globs <- c(paste0("static int nE = ",nE,";"),
           paste0("static int nL1 = ",nL1,";"),
           paste0("static int nL2 = ",nL2,";"),
           paste0("static int nL3 = ",nL3,";"),
           paste0("static int nL4 = ",nL4,";"),
           paste0("static int nL5 = ",nL5,";"),
           paste0("static int nDL5 = ",nDL5,";"),
           paste0("static int nP = ",nP,";"),
           paste0("static int nAr = ",nAr,";"))

################################################################################
#############DETERMINISTIC SKELETON ############################################
#################################################################################


DIA_SKEL <- Csnippet("/////////////////////////////////////////////////////////////////////////////////
                     //I need this function for the birth rate and diapause rate                //////
                     // Example:the number of eggs is dependent on all the reproductive adults  //////
                     // in the subcompartments so I have to sum it up                           //////
                     // C doesn't have a function for that so I have to create it and I name it //////
                     // sum                                                                     //////
                     ///////////////////////////////////////////////////////////////////////////////////
                  
                     const double  sum(const double arr[], int n) 
                     { 
                     double  sum = 0.0;  
                     for (int i = 0; i < n; i++) 
                     sum += arr[i]; 
                     
                     return sum; 
                     } 

                      //////////////////////////////
                     //Subcompartments in stages//
                     /////////////////////////////


                     /////////////////////////////////////////////////////////////////////////
                     // Here are the states that will require pointers to form        ////////
                     // an array of vectors. For all stages except for senescent adult;///////
                     // check King's pomp documentation and there's more information there////
                      /////////////////////////////////////////////////////////////////////////
                   
                     double  *E = &E1; //EGG
                     double *DE = &DE1;
                     
                     double *L1 = &L11; //LARVAE1
                     double *DL1 = &DL11;
                     
                     double *L2 = &L21; //LARVAE2
                     double *DL2 = &DL21;
                     
                     double *L3 = &L31; //LARVAE3
                     double *DL3 = &DL31;
                     
                     double  *L4= &L41; //LARVAE4
                     double *DL4 = &DL41;
                     
                     double *L5= &L51; //LARVAE5
                     double *DL5 = &DL51;
                     
                     double *DIAL5= &DIAL51; //DIAPAUSING LARVAE 5
                     double *DDIAL5 = &DDIAL51;
                     
                     double *P= &P1; //PUPAE
                     double *DP = &DP1;
                     
                     double *Ar= &Ar1; //REPRODUCTIVE ADULT
                     double *DAr = &DAr1;
                     
                     int i; 


                     /////////////
                     //Birth Rate/
                    //////////////
                     long double B;
                     

                     //If you check the supp- you would see that the birth rate is a Gaussian Function
                     B =  10.52 * exp(-0.50* powl ((TT- 24.629), 2.0) / powl (3.427 , 2.0));
                     
                    ///////////////////////////////////////
                     //Development Rate of all the stages//
                    ///////////////////////////////////////

                     double alphaE, alphaL1, alphaL2, alphaL3, 
                     alphaL4, alphaL5, alphaP, alphaA, alphaDL;
                     
                     
                     //Sigmoidal functions- the warmer it is, the faster one develops 
                     alphaE  = alphaE_a/ (1+exp(-alphaE_b*(TT - alphaE_c)));
                     alphaL1  = alphaL1_a/(1+exp(-alphaL1_b*(TT - alphaL1_c)));
                     alphaL2 =  alphaL2_a/(1+exp(- alphaL2_b * (TT - alphaL2_c)));
                     alphaL3 = alphaL3_a/(1+exp(- alphaL3_b*(TT - alphaL3_c)));
                     alphaL4 = alphaL4_a/(1+exp(- alphaL4_b*(TT - alphaL4_c)));
                     alphaL5 = alphaL5_a/(1+exp(- alphaL5_b*(TT - alphaL5_c)));
                     alphaP = alphaP_a/(1+ exp(-alphaP_b*(TT - alphaP_c)));
                     alphaA = alphaA_a/(1+exp(-alphaA_b*(TT - alphaA_c)));
                    
                    //////////////////////////////////////////////////////////////////////////////////
                    //The diapause development rate is reverse where 'cooler temperature' increases //
                    //the development rate up to a point                                            //
                    //////////////////////////////////////////////////////////////////////////////////
                     
                      alphaDL = alphaDL_a/(1+exp(alphaDL_b *(TT + alphaDL_c)));
                     

                    /////////////////////////////////////
                    ///Mortality Rate of all the stages//
                    /////////////////////////////////////
                     double muE, muL, muP, muA, muDL;
                     
                
                     //Wang mortality functions
                     muE = 1- (1/(exp((1+exp(-(TT-T_E_OPT)/MORT_E_A))*(1+exp(-(T_E_OPT- TT)/MORT_E_A ))*MORT_E_B)));
                     muL = 1- (1/(exp((1+exp(-(TT-T_L_OPT)/MORT_L_A))*(1+exp(-(T_L_OPT- TT)/MORT_L_A))*MORT_L_B)));
                     muP =  1- (1/(exp((1+exp(-(TT-T_P_OPT)/MORT_P_A))*(1+exp(-(T_P_OPT- TT)/MORT_P_A))*MORT_P_B )));
                     muA =  1- (1/(exp((1+exp(-(TT-T_A_OPT)/MORT_DL_A))*(1+exp(-(T_A_OPT- TT)/MORT_DL_A))*MORT_A_B )));
                     muDL = 1- (1/(exp((1+exp(-(TT-T_DL_OPT)/MORT_DL_A))*(1+exp(-(T_DL_OPT- TT)/MORT_DL_A))*MORT_DL_B )));
                     
                     
                     //////////////////////
                     //Diapause induction//
                     //////////////////////

                     long double DIA_INDUC_L5;
                     
                     //Sigmoidal function
                     DIA_INDUC_L5 =DIA_A/( DIA_B+exp(DIA_C *PP_DIFF));
                     
                     ///////////////////////
                     //BALANCE THE EQUATION/
                     ////////////////////////
                     
                     
                     //General structure is shown below :

                     //dS/dt = (recruitment in due to birth,
                    // maturation or diapause termination)  - (recruitment out due to maturation or diapause) -
                    //background mortality 
                      
                      //////
                     //EGG//
                     ///////
                    ///////////////////////////////////////////////////////////////////////////////////
                     // Incorporating Allee effect on the birth rate and the use of the sum function/
                     /////////////////////////////////////////////////////////////////////////////////
                    DE[0] = 0.5*B*exp(-C*sum(Ar,nAr))*sum(Ar,nAr)  - (nE * alphaE* E[0]) - (muE* E[0]);
                     for ( i = 1; i  < nE;  i ++){
                     DE[i] = nE * alphaE * (E[i-1]- E[i]) - muE * E[i];}
                    
                      /////////////
                     // LARVAE 1//
                     /////////////
                     DL1[0] = nE * alphaE * E[nE-1] - (nL1 * alphaL1  * L1[0])-  (muL*L1[0]);
                     for ( i = 1;  i < nL1; i++){
                     DL1[i] = nL1 * alphaL1 * (L1[i-1]- L1[i])- muL * L1[i];}
                     
                      ////////////
                     //LARVAE 2//
                     ////////////
                     DL2[0] =  nL1 * alphaL1 * L1[nL1-1]  - (nL2 * alphaL2 * L2[0])- (muL*L2[0]);
                     for (i = 1;  i < nL2 ;  i++){
                     DL2[i] = nL2 * alphaL2 * (L2[i-1]- L2[i])- muL*L2[i];}
                     
                     ////////////
                     //LARVAE 3//
                     ////////////
                     DL3[0] =  nL2 * alphaL2 * L2[nL2-1] -  (nL3 * alphaL3 * L3[0])- (muL*L3[0]);
                     for (i = 1; i < nL3; i++){
                     DL3[i]= nL3 * alphaL3 *(L3[i-1]- L3[i])- muL* L3[i];
                     }

                     ////////////
                     //LARVAE 4//
                     ////////////
                     DL4[0] =  nL3 * alphaL3 * L3[nL3-1] -  (nL4 * alphaL4 * L4[0])-
                     (muL*L4[0]);
                     for (i = 1; i < nL4; i++){
                     DL4[i]= nL4 * alphaL4 *(L4[i-1]- L4[i])- muL* L4[i];
                     }
                     
                    ////////////
                    //LARVAE 5//
                    ////////////
                    /////////////////////////////////////////////////////////////////////////////////
                    // NOTE: Larvae 5 stage acts a bit differently than the other 4 larval instars///
                    // There is now recruitment in not only due to the                            /// 
                    //fourth instar larvae but also diapausing larvae                             ///
                    // that successfully broke diapause also                                      ///
                    //recruitment out is due to larvae being induced into           /////////////////
                    //diapause as well as maturation into the pupal stage and ///////////////////////
                    //the background mortality                                            ///////////
                    /////////////////////////////////////////////////////////////////////////////////
                    
                     DL5[0] = (nL4 * alphaL4 * L4[nL4-1]) +
                     (nDL5 * alphaDL * DIAL5[nDL5-1]) -           
                     (nL5 * alphaL5 *L5[0])  -
                     (DIA_INDUC_L5 * L5[0] ) - 
                     (muL*L5[0]);
                     
                      for ( i = 1;  i < nL5; i++){
                     DL5[i]=  nL5 * alphaL5 * (L5[i-1] -L5[i])  -
                     (DIA_INDUC_L5 * L5[i]) - 
                     (muL *L5[i]);
                     }
                    //////////////////////////////
                    //DIAPAUSING LARVAL STAGE 5 //
                    //////////////////////////////

                    ///////////////////////////////////////////////////////
                    //Diapausing larval five have all fifth instar larvae//
                    //being able to be induced into diapause and         //
                    //the diapausing larvae are 'developing' through their/
                    //diapause                                            /
                    ///////////////////////////////////////////////////////
                     
                     DDIAL5[0] =DIA_INDUC_L5*(sum(L5,nL5))  - 
                     (nDL5 * alphaDL*DIAL5[0])- 
                     (muDL + (COMP * sum(DL5,nDL5))) * DIAL5[0];
                     
                     for ( i = 1;  i <  nDL5; i++){
                     DDIAL5[i]=  nDL5* alphaDL*(DIAL5[i-1] - DIAL5[i]) - 
                     (muDL + (COMP * sum(DL5,nDL5))) * DIAL5[i];
                     }
                     /////////
                     //PUPAE//
                     /////////

                     DP[0] = nL5 * alphaL5 * (L5[nL5-1]) - nP * alphaP *  P[0] - muP * P[0] ;
                     for ( i = 1;  i <nP; i++){
                     DP[i] =  nP * alphaP * (P[i-1] - P[i]) - muP*P[i];
                     }
                     //////////////////////
                     //REPRODUCTIVE ADULT//
                     //////////////////////

                     DAr[0] = nP * alphaP * P[nP-1] -nAr * alphaA*  Ar[0] - muA * Ar[0] ;
                     
                     for ( i = 1;  i < nAr; i++){
                     DAr[i] =  nAr * alphaA * (Ar[i-1] - Ar[i]) - muA*Ar[i];
                     }
                     ///////////////////
                     //SENESCENT ADULT//
                     ///////////////////
                    //////////////////////////////////////////////////////////////////////
                    //I'm assuming they're just flying about and just in                //
                    // process of dying- not important except to be the catching 'state'//
                    //////////////////////////////////////////////////////////////////////
                     DAs =  nAr * alphaA * Ar[nAr-1] - 0.62 * As;
                     
                     
                     ")

#################
###INITIALIZER###
#################
###This initializes the function. 
###I wrote this is as an R code instead of a Csnippet
init <-function(params,t0,...)
{
  c(E = rep(0, nE),
    L1 = rep(0,nL1),
    L2 = rep(0,nL2),
    L3 = rep(0,nL3),
    L4 = rep(0,nL4),
    L5 = rep(0,nL5),
    DIAL5 =rep(30,nDL5), #Starting out with 30 diapausing larvae in each nDL5 compartment
    P = rep(0,nP),
    Ar = rep(0,nAr),
    As = 0)}

###################################
###THIS IS THE FULL POMP MACHINE###
###################################


###THE DMEASURE IS IN A DIFFERENT SCRIPT#
###FOR RUNNING SIMULATION YOU DON'T NEED IT SO I PUT IT AS NULL#########

POMP_CM<- pomp(covartable,
               times="time",
               t0=1,
               globals=globs,
               dmeasure = NULL,
               covar =COVAR,
               covarnames = c("TT", "PP_DIFF"),
               obsnames = c("ARC"),
               rinit = init ,
               skeleton = vectorfield(DIA_SKEL),
                     paramnames= 
                       c("alphaE_a",
                         "alphaE_b",
                         "alphaE_c",
                         "alphaL1_a",
                         "alphaL1_b",
                         "alphaL1_c",
                         "alphaL2_a",
                         "alphaL2_b",
                         "alphaL2_c",
                         "alphaL3_a",
                         "alphaL3_b",
                         "alphaL3_c",
                         "alphaL4_a",
                         "alphaL4_b",
                         "alphaL4_c",
                         "alphaL5_a",
                         "alphaL5_b",
                         "alphaL5_c",
                         "alphaP_a",
                         "alphaP_b",
                         "alphaP_c",
                         "alphaDL_a",
                         "alphaDL_b",
                         "alphaDL_c",
                         "alphaA_a",
                         "alphaA_b",
                         "alphaA_c",
                         "MORT_E_A","MORT_E_B",'T_E_OPT',
                         "MORT_L_A","MORT_L_B",'T_L_OPT',
                         "MORT_P_A","MORT_P_B","T_P_OPT",
                         "MORT_A_A","MORT_A_B","T_A_OPT",
                         "MORT_DL_A","MORT_DL_B",'T_DL_OPT',
                         "DIA_A","DIA_B","DIA_C","C",'COMP'),
                      statenames=c(sprintf("E%1d",seq_len(nE)),
                                  sprintf("L1%1d",seq_len(nL1)),
                                  sprintf("L2%1d",seq_len(nL2)),
                                  sprintf("L3%1d",seq_len(nL3)),
                                  sprintf("L4%1d",seq_len(nL4)),
                                  sprintf("L5%1d",seq_len(nL5)),
                                  sprintf("DIAL5%1d",seq_len(nDL5)),
                                  sprintf("P%1d",seq_len(nP)),
                                  sprintf("Ar%1d",seq_len(nAr)),
                                  "As"))
####################################
#THE PARAMETERS AND THEIR VALUES ###
####################################

PARAMETERS<-   c(alphaE_a=0.1927, #Egg Development rate
                 alphaE_b=0.3039, #...
                 alphaE_c= 18.5929, #...
                 
                 alphaL1_a=0.30, #Instar 1 Development rate
                 alphaL1_b=0.327,#...
                 alphaL1_c= 17.60, #...
                 
                 alphaL2_a=0.457, #Instar 2 Development rate
                 alphaL2_b=0.2301,#...
                 alphaL2_c=21.23, #...
                 
                 alphaL3_a= 0.338, #Instar 3 Development rate
                 alphaL3_b=0.350, #...
                 alphaL3_c=18.45, #...
                 
                 alphaL4_a=0.305, #Instar 4 Development rate
                 alphaL4_b=0.429, #...
                 alphaL4_c= 21.54, #...
                 
                 alphaL5_a= 0.33, #Instar 5 Development rate
                 alphaL5_b=0.2144, #...
                 alphaL5_c= 20.94, #...
                
                 alphaP_a=0.09287, #Pupae Development rate
                 alphaP_b=0.28966, #...
                 alphaP_c=18.48736,#...
                 
                 alphaA_a=0.13, #Adult development rate
                 alphaA_b=0.18, #...
                 alphaA_c= 20.2206,#...
                 
                 alphaDL_a=0.0716149819  , #Diapause development
                 alphaDL_b=.0899064656,
                 alphaDL_c= 0.53 ,
                 
                 MORT_E_A =2.063e+00, #Egg Mortality rate
                 MORT_E_B=1.306e-04 ,  #...
                 T_E_OPT =1.357e+01 , #...
                 
                 MORT_L_A=  5.0000000000 , #Larval Mortality rate
                 MORT_L_B= 0.0004022344 , #...
                 T_L_OPT= 16.24,#...
                 
                 MORT_P_A=2.862e+00 , #Pupal mortality rate
                 MORT_P_B=1.036e-05 , #...
                 T_P_OPT=5.461e+00  ,#...
                 
                 MORT_A_A= 2.862e+00, #Adult mortality rate
                 MORT_A_B=1.306e-04, #...
                 T_A_OPT= 20, #...
                 
                 MORT_DL_A= 6.491816, #Diapause larvae mortality rate
                 MORT_DL_B= 0.000321, #...
                 T_DL_OPT= 3.725070    ,#...
                 
                 DIA_A=0.500 , #Diapause induction rate
                 DIA_B=0.5,#...
                 DIA_C= 220, #...
                 
                 C = 0.0061202543, #Allee constant ,
                 COMP =0.0093028802) #Density dependent )



###################################################################
###This runs the desolver and we integrate- TRAJ_MODEL IS WHAT I USE #
### FOR ALL ANALYSIS##################################################
TRAJ_MODEL<- trajectory(POMP_CM,
                        PARAMETERS
                        ,times=seq(1,12054),
                        format = 'data.frame',method = 'bdf')

save(TRAJ_MODEL, file = 'TRAJ_MODEL.RData')
######################################################################
###WE NEED TO GIVE THE DATE SO THAT WE CAN COMBINE THE REAL LIFE DATA#
###SIMULATION                                                        #
######################################################################
DATE <- seq.Date(as.Date('1/1/1984', format = '%m/%d/%Y'),
                 as.Date('12/31/2016', format = '%m/%d/%Y'),
                 'days')
                                                                  

##############################################################
###To get the total number of individuals within a life-stage# 
###We have to sum up the whole subcompartments ###############
##############################################################
##########################################################################
###To get the total abundance of egg we have to sum up all the indviduals#
###in the compartments together E = nE1 + nE2 and so on                   #
###Isn't there a more efficient way of doing this? Yes ###################  
##########################################################################
###EGG
EGG<-data.frame(DATE, Abund= rowSums(TRAJ_MODEL [,1:(nE)]), id = 'Egg')

###ALL LARVAL STAGES
LARVAE1<- data.frame(DATE, Abund = rowSums(TRAJ_MODEL [,(nE+1):(nE+nL1)]), id= 'L1')

LARVAE2<- data.frame(DATE, Abund = rowSums(TRAJ_MODEL[,(nE+nL1+1):(nE+nL1+nL2)]), id = 'L2')

LARVAE3<- data.frame(DATE, Abund = rowSums(TRAJ_MODEL[,(nE+nL1+nL2+1):(nE+nL1+nL2+nL3)]), id = 'L3')

LARVAE4<- data.frame(DATE, Abund = rowSums(TRAJ_MODEL[,(nE+nL1+nL2+nL3+1):(nE+nL1+nL2+nL3+nL4)]), id = 'L4')

LARVAE5<- data.frame(DATE, Abund = rowSums(TRAJ_MODEL[,(nE+nL1+nL2+nL3+nL4+1):(nE+nL1+nL2+nL3+nL4+nL5)]), id = 'L5')

###The Diapausing Larval Stage

DIAL5<- data.frame(DATE, Abund = rowSums(TRAJ_MODEL[,(nE+nL1+nL2+nL3+nL4+nL5+1):(nE+nL1+nL2+nL3+nL4+nL5+nDL5)]),
                   id = 'DL5')


###Pupal Stage
Pupae<-data.frame(DATE, Abund = 
                    rowSums(TRAJ_MODEL[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+1):(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP)]),
                  id = 'Pupae')
###Reproductive Adult
R.adult<- data.frame(DATE, Abund = rowSums(TRAJ_MODEL[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                   (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)]), id = 'R.adult')

###This is for matching up with the actual pheromone trap data
R.adult_2 = rowSums(TRAJ_MODEL[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                             (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)])
### Senescent Adult
S.adult<- data.frame(DATE, Abund= TRAJ_MODEL[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr+1)], id = 'S.Adult')

#################################################################
#######################################################################
###THE FULL MODEL IS NOW ALL THE TOTAL INDIVIDUALS IN A SINGLE DF #####
#######################################################################
FULL_Model <- rbind.data.frame(EGG, LARVAE1, LARVAE2, LARVAE3, LARVAE4, LARVAE5,DIAL5, 
                               Pupae, R.adult, S.adult)

###Putting in the year and the day of year
FULL_Model$Year <- as.numeric(format(FULL_Model$DATE, '%Y')) 
FULL_Model$Julian <- as.numeric(format(FULL_Model$DATE, '%j'))



###If you want to see a plot of all the life-stages together 
###Just for the sake of making sure nothing is negative 
ggplot(FULL_Model,
  aes (x = Julian, y = log(Abund+1),color = id))+
  geom_line(size =1.2)+
  facet_wrap(~Year)+
  scale_color_viridis(discrete=TRUE)+
  theme_bw()

###CREATING A DATAFRAME TO MERGE BOTH THE ACTUAL DATA (ARC) AND
###THE SIMULATED DATA
                                                                  
                                                                
SIM_DF<- cbind.data.frame(DATE ,SIM =(R.adult_2))
ABUND_DF <- cbind.data.frame(DATE, ACT = covartable$ARC[1:12054] )

SIM_ABUND_DF <- left_join(SIM_DF, ABUND_DF, by=c("DATE"))
SIM_ABUND_DF$YEAR <- as.numeric(format(SIM_ABUND_DF$DATE,'%Y'))
SIM_ABUND_DF$julian <- as.numeric(format(SIM_ABUND_DF$DATE,'%j'))


ggplot(na.omit(SIM_ABUND_DF), aes(x = julian, y =(ACT)))+
  geom_line(size = 1,col='#fa1e3f',alpha = 1)+
  geom_point( aes(x =julian, y = (ACT)),col='#fa1e3f', size=1)+
  geom_line(data = SIM_ABUND_DF, aes(x= julian, y = log((0.5*SIM)+1)),size =1)+
  facet_wrap(~YEAR)+ 
  xlab("Day of Year")+
  ylab("Abundance of adult moths (log)")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16))

#########################################################################################
#####################THIS IS FOR SIMULATING UNDER A HOTTER TEMPERATURE REGIME############
##########################################################################################


POMP_CM_HOT<- pomp(covartable_hot,
               times="time",
               t0=1,
               globals=globs,
               dmeasure = NULL,
               covar =COVAR_HOT,
               covarnames = c("TT", "PP_DIFF"),
               obsnames = c("ARC"),
               rinit = init ,
               skeleton = vectorfield(DIA_SKEL),
               paramnames= 
                 c("alphaE_a",
                   "alphaE_b",
                   "alphaE_c",
                   "alphaL1_a",
                   "alphaL1_b",
                   "alphaL1_c",
                   "alphaL2_a",
                   "alphaL2_b",
                   "alphaL2_c",
                   "alphaL3_a",
                   "alphaL3_b",
                   "alphaL3_c",
                   "alphaL4_a",
                   "alphaL4_b",
                   "alphaL4_c",
                   "alphaL5_a",
                   "alphaL5_b",
                   "alphaL5_c",
                   "alphaP_a",
                   "alphaP_b",
                   "alphaP_c",
                   "alphaDL_a",
                   "alphaDL_b",
                   "alphaDL_c",
                   "alphaA_a",
                   "alphaA_b",
                   "alphaA_c",
                   "MORT_E_A","MORT_E_B",'T_E_OPT',
                   "MORT_L_A","MORT_L_B",'T_L_OPT',
                   "MORT_P_A","MORT_P_B","T_P_OPT",
                   "MORT_A_A","MORT_A_B","T_A_OPT",
                   "MORT_DL_A","MORT_DL_B",'T_DL_OPT',
                   "DIA_A","DIA_B","DIA_C","C",'COMP'),
               statenames=c(sprintf("E%1d",seq_len(nE)),
                            sprintf("L1%1d",seq_len(nL1)),
                            sprintf("L2%1d",seq_len(nL2)),
                            sprintf("L3%1d",seq_len(nL3)),
                            sprintf("L4%1d",seq_len(nL4)),
                            sprintf("L5%1d",seq_len(nL5)),
                            sprintf("DIAL5%1d",seq_len(nDL5)),
                            sprintf("P%1d",seq_len(nP)),
                            sprintf("Ar%1d",seq_len(nAr)),
                            "As"))
TRAJ_MODEL_HOT<- trajectory(POMP_CM_HOT,
                        PARAMETERS
                        ,times=seq(1,12054),
                        format = 'data.frame',method = 'bdf')


R.adult_2_HOT = rowSums(TRAJ_MODEL_HOT[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                 (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)])

SIM_DF_HOT<- cbind.data.frame(DATE ,SIM_HOT =(R.adult_2_HOT))

HOT_ABUND_DF <-left_join(SIM_DF,SIM_DF_HOT)
HOT_ABUND_DF$YEAR <- as.numeric(format(HOT_ABUND_DF$DATE,'%Y'))
HOT_ABUND_DF$julian <- as.numeric(format(HOT_ABUND_DF$DATE,'%j'))

HOT_ABUND_DF_MELT <- melt(HOT_ABUND_DF, id.vars= list("DATE",
                                                      'YEAR','julian'))


levels(HOT_ABUND_DF_MELT$variable)[levels(HOT_ABUND_DF_MELT$variable)=="SIM"] <- "Historical temperature"
levels(HOT_ABUND_DF_MELT$variable)[levels(HOT_ABUND_DF_MELT$variable)=="SIM_HOT"] <- "Historical temperature + 4 °C"
names(HOT_ABUND_DF_MELT)[names(HOT_ABUND_DF_MELT)=="variable"]  <- "Scenario"

ggplot(HOT_ABUND_DF_MELT, aes(x = julian, y =log((0.5*value)+1), color=Scenario))+
  geom_line(size = 1)+  facet_wrap(~YEAR)+ theme_bw()+
  scale_color_manual(values = c('navyblue','#fa1e3f'))+
  xlab("Day of Year")+
  ylab("Abundance of adult moths (log)")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        legend.position="top")

save(HOT_ABUND_DF,file = "SIMULATION_HISTORICALANDHOT.RData")


