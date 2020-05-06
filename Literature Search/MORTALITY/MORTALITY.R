######################################################
###THIS IS FOR FITTING THE MORTALITY DATA AND ########
###GETTING THE MORTALITY PARAMETERS###################
######################################################

#############################################################################
####This is the code for calculating the mortality rate of the codling moth##
###We use nls to estimate the parameters                                   ## 
#############################################################################
#######################################################################
##Here I use the Wang function for all life stages for mortality rates#
#######################################################################

##############
###PACKAGES###
##############
library(ggplot2)
library(minpack.lm)
###nls is part of the stats package which should already be loaded?
##################
##################

##############################
###This is the data for it####
###############################
MORT_CM_2 <- read.csv("MORT_CM_2.csv")

##############################################
###The temperature range we are interested in#
##############################################
TT = seq(-35,35,length = 1000)

#########################
###EGG MORTALITY RATE####
#########################

###GOOD DATA SO WILL USE THIS TO FIX.

EGG_MORT <- subset(MORT_CM_2, MORT_CM_2$Stage == 'Egg')
#############
###THE NLS###
#############
WANG_EGG <- nls(Per_cap_mort~ 1 -(1/(exp((1+exp(-(TEMP-TEMPO)/B))*
                (1+exp(-(TEMPO-TEMP)/B))*H))),
                start = list( B =4, H = 0.0011,TEMPO= 15),
                 data = EGG_MORT)

#MORT_E_A (B) =2.063e+00, 
#MORT_E_B (H) =1.306e-04 
#T_E_OPT ((TEMPO) =1.357e+01 \


NEWTEMP_EGGMORT<-data.frame(Temperature= seq(-35,35, length=1000),
                    Mort_egg=predict(WANG_EGG, newdata =
                    data.frame(TEMP = seq(-35,35,length=1000))))

###THIS MAKES THE PLOT ABOUT THE MORTALITY RATE
WANG_EGG_GG<-ggplot(NEWTEMP_EGGMORT, aes(x=Temperature,
                                         y= Mort_egg))+
        geom_line(size = 1.1,alpha = 0.5,color='blue')+
        geom_point(data = EGG_MORT, aes( x= TEMP, y= Per_cap_mort),size = 1.5)+
        theme_classic()+
        ggtitle("Egg")
        
#####################################################################################
######################################################################################

########################################
###LARVAL MORTALITY RATE################
########################################

LARV_MORT <-  subset(MORT_CM_2, MORT_CM_2$Stage == 'Larvae')


###I used maximum likelihood to fit the mortality rate of the first two parameters
###B and H while I fix optimum temperature to about 16.24 (This is from a 
###previous fit)

###From the maximum likelihood fit- I got
###

MORT_L_A=  5.0000000000 
MORT_L_B= 0.0004022344
T_L_OPT= 16.24



NEWTEMP_LARVMORT <- data.frame(Temperature=TT,
                               Mort_larv=1 -
                                 (1/(exp((1+exp(-(TT-T_L_OPT)/MORT_L_A))*
                                  (1+exp(-(T_L_OPT-TT)/MORT_L_A))*MORT_L_B))))

WANG_LARV_GG <- ggplot(NEWTEMP_LARVMORT, aes(x = Temperature, y= Mort_larv))+
  geom_line(size = 1.1,alpha = 0.5,color='blue')+
  geom_point(data = LARV_MORT, aes( x= TEMP, y= Per_cap_mort),size = 1.5)+
  theme_classic()+
  ggtitle("Larvae")


#############################
###PUPAL STAGE MORTALITY ####
#############################

PUP_MORT <- subset(MORT_CM_2, MORT_CM_2$Stage == 'Pupae')

################
###VERY MESSY###
################

##########################################################
##########################################################
######ORIENTAL FRUIT MOTH HAD CLEANER DATA AND VERY ######
######CLOSELY RELATED TO THE CODLING MOTH#################
#########################################################
##########################################################

PUP_MORT_OFM <- subset(MORT_CM_2, MORT_CM_2$Stage == 'OFMP')

plot(PUP_MORT_OFM$TEMP, PUP_MORT_OFM$Per_cap_mort)

PUP_MORT = nlsLM(Per_cap_mort~ 1 - (1/(exp((1+exp(-(TEMP-TEMPO)/B))*
                     (1+exp(-(TEMPO- TEMP)/B))*H))),
      start = list(B =2, H =1e-04,TEMPO=17),
      data = PUP_MORT_OFM)

WANG_PUP_NEW<- data.frame(Temperature= TT,
             Mort_pup= predict(PUP_MORT ,list(TEMP= TT)))

WANG_PUP_GG <- ggplot(WANG_PUP_NEW, aes(x = Temperature, y= Mort_pup))+
  geom_line(size = 1.1,alpha = 0.5,color='blue')+
  geom_point(data = PUP_MORT_OFM, aes( x=TEMP, y= Per_cap_mort),size = 1.5)+
  theme_classic()+
  ggtitle("Pupae")


######################################
###DIAPAUSING LARVAE MORTALITY RATE###
######################################

###removing the sixth entry to make my fitting work- nls being picky!
DL_MORT <- subset(MORT_CM_2, 
                   MORT_CM_2$Stage == 'Diapausing_Larv')[-6,]

DIA_MORT <- 
  nls(Per_cap_mort~1 - (1/(exp((1+exp(-(TEMP-TEMPO)/B))*
                       (1+exp(-(TEMPO- TEMP)/B))*H))),
                      start = list(B =5.5, H= 4e-04,TEMPO= 0.36),
                       data = DL_MORT, algorithm = 'port',
                        control= list(maxiter = 1000,warnOnly=TRUE
                      ))

WANG_DIA_NEW<- data.frame(Temperature= TT,
                          Mort_dia= predict(DIA_MORT ,list(TEMP= TT)))



WANG_DIA_GG <- ggplot(WANG_DIA_NEW, aes(x = Temperature, y= Mort_dia))+
  geom_line(size = 1.1,alpha = 0.5,color='blue')+
  geom_point(data = DL_MORT, aes( x=TEMP, y= Per_cap_mort),size = 1.5)+
  theme_classic()+ylim(0,1)+
  ggtitle("Diapausing Larvae")

##########################
###ADULT MORTALITY RATE###
###########################
#################################################################
###I'm assuming not a lot of adults die like pupae- except i ####
###increased otpimum temperature for survivorship for adult######
#################################################################

MORT_A_A= 2.862e+00
MORT_A_B=1.306e-04
T_A_OPT= 20




NEWTEMP_ADULTMORT <- data.frame(Temperature=TT,
                       Mort_adult=1 -
                         (1/(exp((1+exp(-(TT-T_A_OPT)/MORT_A_A))*
                                   (1+exp(-(T_A_OPT-TT)/MORT_A_A))*MORT_A_B))))

WANG_ADULT_GG <- ggplot(NEWTEMP_ADULTMORT, aes(x = Temperature, y= Mort_adult))+
  geom_line(size = 1.1,alpha = 0.5,color='blue')+
  theme_classic()+
  ggtitle("Reproductive adult")+ ylim(0,1)


(WANG_EGG_GG + WANG_LARV_GG)/
  (WANG_PUP_GG + WANG_DIA_GG)/
 ( WANG_ADULT_GG + plot_spacer())



