###Development rate of the different stages and using 
###nls for parameter estimation 

###############
###Packages ###
###############
library(ggplot2)
library(gridExtra)
############################################################################################
#######This is the main_data that have all the development rates from previous literature## 
###########################################################################################
MAIN_DAT <- read.csv("DEV_CM.csv")

#################
###EGG STAGE ####
#################
EGG_DAT <- subset(MAIN_DAT, MAIN_DAT$Stage == 'Egg')


###This is the temperature range that we are interested in all stages
TEMP = seq(10,35)
###################################################################
###We are fitting a sigmoid function to the egg development rate:
###Remember the sigmoid function is a/(1+exp(-b*(Tempeature -C))###
####################################################################
EGG_DAT_NLS <- nls(Dev_Rate~ a/(1+exp(-b*(Temperature-c))),data=EGG_DAT,
                   start= list(a = 0.2, b= 0.3, c = 18))

EGG_EQUATION <- cbind.data.frame(TEMP, 
                DEV_PRED =0.19269/ (1+exp(- 0.30388*(TEMP-18.59282))))

##########################
###Here is the graph#####
#########################

EGG_GRAPH <-
  ggplot(EGG_DAT, aes(x = Temperature, y = Dev_Rate)) + 
  geom_point(size = 3) +
  geom_line(
    data = EGG_EQUATION,
    aes(x = TEMP, y = DEV_PRED),
    size = 1.2,
    alpha = 0.3,
    color = 'blue') + 
  theme_classic() +
      ylab("Development Rate") +
  theme(panel.border = element_rect(fill = NA, colour = "black")) + 
  ylim(0, 0.5)
  
###########################
###LARVAL STAGE (1-5) ####
###########################
#####################################################
#We subset the data to include all the larval stages#
#####################################################
LARV_DAT <- subset(MAIN_DAT, MAIN_DAT$Stage == 'Larvae.1'|
                      MAIN_DAT$Stage== 'Larvae.2'|
                      MAIN_DAT$Stage == 'Larvae.3'|
                      MAIN_DAT$Stage == 'Larvae.4'|
                      MAIN_DAT$Stage == 'Larvae.5')

###Parameter estimates for the larvae 

LARV1_DAT_NLS <- nls(Dev_Rate~ a/(1+exp(-b*(Temperature-c))),data=subset(LARV_DAT, LARV_DAT$Stage == 'Larvae.1'),
                   start= list(a = 0.5, b= 0.5, c = 18))

LARV2_DAT_NLS <- nls(Dev_Rate~ a/(1+exp(-b*(Temperature-c))),data=subset(LARV_DAT, LARV_DAT$Stage == 'Larvae.2'),
                     start= list(a = 0.5, b= 0.5, c = 18))
LARV3_DAT_NLS <- nls(Dev_Rate~ a/(1+exp(-b*(Temperature-c))),data=subset(LARV_DAT, LARV_DAT$Stage == 'Larvae.3'),
                     start= list(a = 0.5, b= 0.5, c = 18))
LARV4_DAT_NLS <- nls(Dev_Rate~ a/(1+exp(-b*(Temperature-c))),data=subset(LARV_DAT, LARV_DAT$Stage == 'Larvae.4'),
                     start= list(a = 0.5, b= 0.5, c = 18))
LARV5_DAT_NLS <- nls(Dev_Rate~ a/(1+exp(-b*(Temperature-c))),data=subset(LARV_DAT, LARV_DAT$Stage == 'Larvae.5'),
                     start= list(a = 0.5, b= 0.5, c = 18))

LARV1_EQUATION <- cbind.data.frame(TEMP, DEV_PRED = 0.3058/ (1+exp(- 0.3271*(TEMP-17.6028 ))),
                                   ID = 'Larvae.1')
LARV2_EQUATION <- cbind.data.frame(TEMP, DEV_PRED =0.4517/ (1+exp(- 0.2301*(TEMP-21.2385))),
                                   ID = 'Larvae.2')
LARV3_EQUATION <- cbind.data.frame(TEMP, DEV_PRED =0.3380 / (1+exp(- 0.3501*(TEMP-18.4555 ))),
                                   ID = 'Larvae.3')
LARV4_EQUATION <- cbind.data.frame(TEMP, DEV_PRED = 0.3058/ (1+exp(- 0.4299 *(TEMP-21.5401  ))),
                                   ID = 'Larvae.4')
LARV5_EQUATION <- cbind.data.frame(TEMP, DEV_PRED = 0.3316/ (1+exp(- 0.2144 *(TEMP-20.9499 ))),
                                   ID = 'Larvae.5')

LARV_PRED <- rbind(LARV1_EQUATION,LARV2_EQUATION, LARV3_EQUATION, LARV4_EQUATION, LARV5_EQUATION)


LARV1_GRAPH <-ggplot(subset(LARV_DAT ,LARV_DAT$Stage == 'Larvae.1'), aes(x= Temperature, y= Dev_Rate))+geom_point(size=3)+
  geom_line(data =LARV1_EQUATION, aes(x= TEMP, y = DEV_PRED),size = 1.2,alpha=0.3,
            color='blue')+theme_classic()+ylab("Development Rate")+
  theme(panel.border = element_rect(fill=NA,colour = "black"))+ylim(0,0.5)
        
LARV2_GRAPH <-ggplot(subset(LARV_DAT ,LARV_DAT$Stage == 'Larvae.2'), aes(x= Temperature, y= Dev_Rate))+geom_point(size=3)+
  geom_line(data =LARV2_EQUATION, aes(x= TEMP, y = DEV_PRED),size = 1.2,alpha=0.3,
            color='blue')+theme_classic()+ylab("Development Rate")+
  theme(panel.border = element_rect(fill=NA,colour = "black"))+ylim(0,0.5)

LARV3_GRAPH <-ggplot(subset(LARV_DAT ,LARV_DAT$Stage == 'Larvae.3'), aes(x= Temperature, y= Dev_Rate))+geom_point(size=3)+
  geom_line(data =LARV3_EQUATION, aes(x= TEMP, y = DEV_PRED),size = 1.2,alpha=0.3,
            color='blue')+theme_classic()+ylab("Development Rate")+
  theme(panel.border = element_rect(fill=NA,colour = "black"))+ylim(0,0.5)

LARV4_GRAPH <-ggplot(subset(LARV_DAT ,LARV_DAT$Stage == 'Larvae.4'), aes(x= Temperature, y= Dev_Rate))+geom_point(size=3)+
  geom_line(data =LARV4_EQUATION, aes(x= TEMP, y = DEV_PRED),size = 1.2,alpha=0.3,
            color='blue')+theme_classic()+ylab("Development Rate")+
  theme(panel.border = element_rect(fill=NA,colour = "black"))+ylim(0,0.5)

LARV5_GRAPH <-ggplot(subset(LARV_DAT ,LARV_DAT$Stage == 'Larvae.5'), aes(x= Temperature, y= Dev_Rate))+geom_point(size=3)+
  geom_line(data =LARV5_EQUATION, aes(x= TEMP, y = DEV_PRED),size = 1.2,alpha=0.3,
            color='blue')+theme_classic()+ylab("Development Rate")+
  theme(panel.border = element_rect(fill=NA,colour = "black"))+ylim(0,0.5)

grid.arrange(LARV1_GRAPH, LARV2_GRAPH, LARV3_GRAPH, LARV4_GRAPH,LARV5_GRAPH,ncol=3)



#####################
###PUPAL STAGE ####
####################
PUP_DAT <- subset(MAIN_DAT, MAIN_DAT$Stage == 'Pupae')


PUP_DAT_NLS <- nls(Dev_Rate~ a/(1+exp(-b*(Temperature-c))),data=PUP_DAT ,
                     start= list(a = 0.5, b= 0.5, c = 18))

PUP_EQUATION <- cbind.data.frame(TEMP, DEV_PRED =  0.09287/ (1+exp(- 0.3271*(TEMP-17.6028 ))),
                                   ID = 'Pupae')
PUPAE_GRAPH <-
  ggplot(PUP_DAT , aes(x = Temperature, y = Dev_Rate)) + geom_point(size =3) +
  geom_line(
    data = PUP_EQUATION,
    aes(x = TEMP, y = DEV_PRED),
    size = 1.2,
    alpha = 0.3,
    color = 'blue'
  ) + theme_classic() + ylab("Development Rate") +
  theme(panel.border = element_rect(fill = NA, colour = "black")) + ylim(0, 0.5)

#######################
###Reproductive Adult##
#######################

ADULT_DAT <- subset(MAIN_DAT, MAIN_DAT$Stage == 'Reproductive adult')


ADULT_DAT_NLS <- nls(Dev_Rate~ a/(1+exp(-b*(Temperature-c))),data=ADULT_DAT ,
                   start= list(a = 0.5, b= 0.5, c = 18))

ADULT_EQUATION <- cbind.data.frame(TEMP, DEV_PRED =  0.1707/ (1+exp(- 0.1830*(TEMP-20.2206 ))),
                                 ID = 'Adult')

ADULT_GRAPH <-
  ggplot(ADULT_DAT , aes(x = Temperature, y = Dev_Rate)) + 
  geom_point(size = 3) +
  geom_line(
    data = ADULT_EQUATION,
    aes(x = TEMP, y = DEV_PRED),
    size = 1.2,
    alpha = 0.3,
    color = 'blue') +
  theme_classic() + ylab("Development Rate") +
  theme(panel.border = element_rect(fill = NA, colour = "black")) + 
  ylim(0, 0.5)

####################################################################
##THIS MAKES THE FIGURE FOR DEVELOPMENT RATE ACROSS ALL LIFE STAGES#
####################################################################
grid.arrange(EGG_GRAPH, LARV_ALL, 
             PUPAE_GRAPH, ADULT_GRAPH)
