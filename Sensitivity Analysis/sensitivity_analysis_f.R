###############################
###SENSITIVITY ANALYSIS#######
############################
library(pracma)
library(pomp2)

################################################
###I'm running it only for the first 10 Years###
################################################
#############################
###Smoother and peak finders#
#############################
###10 years
#Jan 1st, 1984 - Dec 31st, 1993

DATES <- seq.Date(as.Date('01-01-1984',format = '%m-%d-%Y'),
                  as.Date('12-31-2016', format = '%m-%d-%Y'),'days')


# Sensitivity analysis for the phenology ----------------------------------



##THE CONTROL, assuming no changes to the parameters-
###again only running it for 10 years
CONTROL_MODEL <-  trajectory(
  POMP_CM,
  PARAMETERS,
  times = seq(1, 12054),
  format = 'data.frame',
  method = 'bdf'
)

#Only looking at the phenology of adults
CONTROL_R.Adult <- log((0.5*(rowSums(CONTROL_MODEL
                            [,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                            (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)]))+1))

###MAKING IT INTO A DATAFRAME WITH YEARS
CONTROL_MODEL_R.Adult = cbind.data.frame(DATES,CONTROL_R.Adult )
CONTROL_MODEL_R.Adult$Year <- as.numeric(format(CONTROL_MODEL_R.Adult$DATES,'%Y'))
SPLITTED_CONTROL <- split(CONTROL_MODEL_R.Adult, CONTROL_MODEL_R.Adult$Year)




#############################

###THIS IS A FOR LOOP THAT CALCULATES THE PEAK
DOY_FIRST_PEAK_CONTROL = NULL #This is the collector 
for(k in seq(1,33)){ #For every year
  tmp <-SPLITTED_CONTROL [[k]] # THIS GETS THE ORIGINAL (UNCHANGED)
  tmp$day <- as.numeric(format(tmp$DATES,'%j')) # get the DOY
  
  tmp2<- subset(tmp, tmp$day > 100)
  
  tmp_smooth <- smooth.spline(x=tmp2$day, y=tmp2$CONTROL_R.Adult, spar = 0.5)
  
  df_smooth  <- cbind.data.frame(doy=   tmp_smooth $x,abund =   tmp_smooth $y)
  
  
  peak =data.frame(findpeaks( df_smooth$abund,npeaks=3, 
                                  minpeakheight=2,
                                  zero='+')) 
  index_peak  <-  peak $X2
  
  
  
  DOY_PEAK= data.frame(tmp2 [index_peak ,])
  #this is where the package comes in
  
  closest=DOY_PEAK[which.min(abs(150 -  DOY_PEAK$day)),]
  
  DOY_FIRST_PEAK_CONTROL [[k]] = cbind.data.frame( day =closest$day,
                                            year =unique(tmp$Year))
  
  plot(tmp$day,tmp$CONTROL_R.Adult,main=unique(tmp$Year))
  abline(v= closest$day,col='red')
  
}

DOY_FIRST_PEAK_CONTROL_F <- do.call(rbind,DOY_FIRST_PEAK_CONTROL)

###n=10
###plot(as.numeric(format(SPLITTED_CONTROL[[n]]$DATES,'%j')),
###     log(SPLITTED_CONTROL[[n]]$CONTROL_R.Adult+1),type='l')
###abline(v=DOY_FIRST_PEAK_CONTROL [[n]]);DOY_FIRST_PEAK_CONTROL[[n]]
###abline(v = DOY_SECOND_PEAK_CONTROL[[n]]);DOY_SECOND_PEAK_CONTROL[[n]]

###Looks goods- 

PARAMS_START <- PARAMETERS


TRAJ <- NULL
DOY_PEAK_PARAMS <- NULL

  for ( i in seq(1,50)){
  ###HERE YOU GET THE 0.95 of the Parameter i
  PARAMS_2_95 <- PARAMS_START
  PARAMS_2_95[[i]] <- PARAMS_START[[i]]*0.95

  
  PARAMS_2_105 <- PARAMS_START
  PARAMS_2_105[[i]] <- PARAMS_START[[i]] * 1.05
  
  
  TRAJ_MODEL_95 <- trajectory(POMP_CM,
                          PARAMS_2_95 ,times=seq(1,12054),
                          format = 'data.frame')
  
  
  TRAJ_MODEL_105 <- trajectory(POMP_CM,  PARAMS_2_105 ,
                               times = seq(1,12054),
                               format = 'data.frame')
  
  
  R.adult_95<- log((0.5*rowSums(TRAJ_MODEL_95[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                 (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)]+1)))
  
  R.adult_105<- log((0.5*rowSums(TRAJ_MODEL_105[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                       (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)]+1)))
  
  
  NEW_95 <-  cbind.data.frame(DATES,R.adult_95)
  NEW_105 <- cbind.data.frame(DATES, R.adult_105)
  NEW_95$Year <- as.numeric(format( NEW_95$DATES,'%Y'))
  NEW_105 $Year <- as.numeric(format( NEW_105 $DATES,'%Y'))
  
  TRAJ_MODELS <- cbind(NEW_95[,-3], NEW_105[,-c(1)])
  
  TRAJ[[i]] <- TRAJ_MODELS
  
  
  
  
  SPLITTED_95 <- split(  NEW_95,  NEW_95$Year)
  SPLITTED_105 <- split(  NEW_105,  NEW_105$Year)
  
  
  DOY_PEAK =NULL
  for(k in seq(1,33)){
    tmp_95 <-SPLITTED_95[[k]]
    tmp_105 <- SPLITTED_105[[k]]
    tmp_95$day <- as.numeric(format(tmp_95$DATES,'%j'))
    tmp_105$day <- as.numeric(format(tmp_105$DATES,'%j'))
               
    
                   
    tmp_95_100 <- subset(tmp_95, tmp_95$day > 100)
    tmp_105_100 <- subset(tmp_105, tmp_105$day > 100)
    
    a_95 <- smooth.spline(x=tmp_95_100$day, y=tmp_95_100$R.adult, spar = 0.5)
    a_105 <- smooth.spline(x=tmp_105_100$day, y=tmp_105_100$R.adult, spar = 0.5)
    df_a_95 <- cbind.data.frame(doy= a_95$x,abund = a_95$y)
    df_a_105 <- cbind.data.frame(doy= a_105$x,abund = a_105$y)
    
    
    
    
    
    ind_95_DF =data.frame(findpeaks(df_a_95 $abund,npeaks=3, 
                                    minpeakheight=2,
                                    zero='+'))
    ind_105_DF=data.frame(findpeaks( df_a_105 $abund,npeaks=3, 
                                    minpeakheight=2,
                                    zero='+'))
    
    index_95 <- ind_95_DF$X2
    index_105 <- ind_105_DF$X2
    
    

      DOY_PEAK_95   =    data.frame(tmp_95_100 [index_95,], id ='95', param =names(PARAMS_2_95[i]))
    colnames(DOY_PEAK_95)[2]='adu'
        DOY_PEAK_105  =    data.frame(tmp_105_100[index_105,], id ='105', param =names(PARAMS_2_95[i]))
        colnames(DOY_PEAK_105)[2]='adu'
        
      DOY_PEAK[[k]]  <- rbind.data.frame(DOY_PEAK_95, 
                                  DOY_PEAK_105)
      
      
    
}
  DOY_PEAK_PARAMS[[i]] = DOY_PEAK
  }

PEAK_FINDER_F <- NULL
for (m in seq(1, length(DOY_PEAK_PARAMS))){
  tmp= DOY_PEAK_PARAMS[[m]]
  split = split(TRAJ[[m]],TRAJ[[m]]$Year)
  
  
  PEAK_FINDER <- NULL
  for (n in seq(1, 33)){

  temp_95 = subset(tmp[[n]], tmp[[n]]$id==95)
  temp_105=  subset(tmp[[n]], tmp[[n]]$id==105)
  
  
  closest95=temp_95[which.min(abs(150 -   temp_95$day)),]
  closest105=temp_105[which.min(abs(150 -   temp_105$day)),]

  
  
  
  
  
  PEAK_FINDER[[n]]=cbind.data.frame(d_95 =  
                                      closest95$day[1], d_105=closest105$day[1],
                                    year= unique( temp_95$Year),
                   param= unique( temp_95$param))
  

  }
  
  PEAK_FINDER_F[[m]] = PEAK_FINDER
}


# for (k in seq(1,15)){
#  # plot(SPLITTED_Y[[k]]$Day, SPLITTED_Y[[k]]$R.adult_95,
#  #      main = unique(SPLITTED_Y[[k]]$Year))
#  # abline(v=PEAK_PARAMS$d_95[[k]])
#  # points(SPLITTED_Y[[k]]$Day, SPLITTED_Y[[k]]$R.adult_105,col='blue')
#  # abline(v=PEAK_PARAMS$d_105[[k]],col='blue')
#  # title(outer = TRUE, unique(PEAK_PARAMS$param))
# 

PEAK_FINDER_F[[24]][[9]]$d_95 <- 168
PEAK_FINDER_F[[24]][[9]]$d_105 <- 168

PEAK_FINDER_F[[24]][[14]]$d_95 <- 170
PEAK_FINDER_F[[24]][[14]]$d_105 <- 170



PERCENT_CHANGE=NULL
for(l in seq(1,50)){
  PEAK_PARAMS <- do.call(rbind,PEAK_FINDER_F[[l]])
  Current_Traj_Param <- TRAJ[[l]]
  Current_Traj_Param$Day <- as.numeric(format(Current_Traj_Param$DATES,'%j'))
  
  SPLITTED_Y <- split(Current_Traj_Param, Current_Traj_Param$Year)

  PERCENT_CHANGE [[l]]= mean((PEAK_PARAMS[,2]- PEAK_PARAMS[,1])/ (0.10*(DOY_FIRST_PEAK_CONTROL_F 
$day)))}
  
  # 
  # for (k in seq(1,15)){
  #  plot(SPLITTED_Y[[k]]$Day, SPLITTED_Y[[k]]$R.adult_95,
  #       main = unique(SPLITTED_Y[[k]]$Year))
  #  abline(v=PEAK_PARAMS$d_95[[k]])
  #  points(SPLITTED_Y[[k]]$Day, SPLITTED_Y[[k]]$R.adult_105,col='blue')
  #  abline(v=PEAK_PARAMS$d_105[[k]],col='blue')
  #  title(outer = TRUE, unique(PEAK_PARAMS$param))

}
}


###Trying to find peak can be a bit picky at times- 
###Eying it manually to make sure nothing is going wrong 

#alphaP_C is wonky...(24th parameter) 1992 and 1997

###1992 - 9th year
##1997-14th year



###MANUALLY FIX THESE

  ###HERE YOU GET THE 0.95 of the Parameter i
  PARAMS_FIX_95 <- PARAMS_START
  PARAMS_FIX_95[[24]] <- PARAMS_FIX_95[[24]]*0.95
  
  PARAMS_FIX_105 <- PARAMS_START
  PARAMS_FIX_105[[24]] <- PARAMS_FIX_105[[24]]*1.05
  
  
  TRAJ_MODEL_FIX_95 <- trajectory(POMP_CM,
                                  PARAMS_FIX_95 ,times=seq(1,12054),
                              format = 'data.frame')
  
  
  TRAJ_MODEL_FIX_105 <- trajectory(POMP_CM, PARAMS_FIX_105,
                               times = seq(1,12054),
                               format = 'data.frame')
  
  
  R.adult_95_FIX<- log((0.5*rowSums(TRAJ_MODEL_95[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                                (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)]+1)))
  
  R.adult_105_FIX<- log((0.5*rowSums(TRAJ_MODEL_105[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                                  (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)]+1)))
  
  
  NEW_95_F <-  cbind.data.frame(DATES,R.adult_95_FIX)
  NEW_105_F <- cbind.data.frame(DATES, R.adult_105_FIX)
  NEW_95_F$Year <- as.numeric(format( NEW_95_F$DATES,'%Y'))
  NEW_105_F$Year <- as.numeric(format( NEW_105_F $DATES,'%Y'))
  
  TRAJ_MODELS_FIX <- cbind(NEW_95_F[,-3], NEW_105_F[,-c(1)])
  
  TRAJ_MODEL_1992 <- subset(TRAJ_MODELS_FIX,
                            TRAJ_MODELS_FIX$Year==1992)
  TRAJ_MODEL_1997 <- subset(TRAJ_MODELS_FIX,
                            TRAJ_MODELS_FIX$Year ==1997)
  

  

  TRAJ_MODEL_1992$day <- as.numeric(format(TRAJ_MODEL_1992$DATES,'%j'))
  TRAJ_MODEL_1997$day <- as.numeric(format(TRAJ_MODEL_1997$DATES,'%j'))
    
    
    
  ###ONLY THE 95th need to be fixed 
  TRAJ_MODEL_1992_100<- subset(TRAJ_MODEL_1992,
                               TRAJ_MODEL_1992$day > 100)
  TRAJ_MODEL_1997_100<- subset(TRAJ_MODEL_1997,
                               TRAJ_MODEL_1997$day > 100)    
    
    a_95_1992 <- smooth.spline(x=TRAJ_MODEL_1992_100$day, y=TRAJ_MODEL_1992_100$R.adult_95_FIX, spar = 0.5)
    a_105_1992 <-smooth.spline(x=TRAJ_MODEL_1992_100$day, y =TRAJ_MODEL_1992_100$R.adult_105_FIX, spar = 0.5)
    a_95_1997 <- smooth.spline(x=TRAJ_MODEL_1997_100$day, y=TRAJ_MODEL_1997_100$R.adult_95_FIX, spar = 0.5)
    a_105_1997 <-smooth.spline(x=TRAJ_MODEL_1997_100$day, y =TRAJ_MODEL_1997_100$R.adult_105_FIX, spar = 0.5)
    
    
    
    df_a_95_92 <- cbind.data.frame(doy=  a_95_1992 $x,abund =  a_95_1992 $y)
    df_a_105_92 <- cbind.data.frame(doy =  a_105_1992 $x,abund =  a_105_1992 $y)
    df_a_95_97 <- cbind.data.frame(doy=  a_95_1997 $x,abund =  a_95_1997$y)
    df_a_105_97 <- cbind.data.frame(doy=  a_105_1997 $x,abund =  a_105_1997$y)
    
    ind_95_92_DF =data.frame(findpeaks(df_a_95_92  $abund,npeaks=3, 
                                    minpeakheight=2,
                                    zero='+'))
    ind_105_92_DF =data.frame(findpeaks(df_a_105_92  $abund,npeaks=3, 
                                       minpeakheight=2,
                                       zero='+'))
    points(a_105_1992,col='blue')
    
    abline(v=df_a_105_92[ind_105_92_DF$X2[2],]$doy,col='blue')
    
    
    ###SO 95 for 1992 is DOY 168
    ###SO 105 for 1992 is DOY 168
    
    
    ind_95_97_DF =data.frame(findpeaks(df_a_95_97  $abund,npeaks=3, 
                                       minpeakheight=2,
                                       zero='+'))
    ind_105_97_DF =data.frame(findpeaks(df_a_105_97  $abund,npeaks=3, 
                                        minpeakheight=2,
                                        zero='+'))
  
    ###SO 95 for 1997 is DOY 170
    ###SO 105 for 1992 is DOY 170
    

#SENSITIVTY INDEX

SENSITIVITY_DEVELOPMENT_FIRST = 
  cbind.data.frame(name = names(PARAMETERS),sensitivity=  PERCENT_CHANGE )

SENSITIVITY_DEVELOPMENT_FIRST$Function <- c(rep('birth',3),
                                        rep('dev_e',3),
                                        rep('dev_l1', 3),
                                        rep('dev_l2',3),
                                        rep('dev_l3',3),
                                        rep('dev_l4',3),
                                        rep('dev_l5',3),
                                        rep('dev_p',3),
                                        rep('dev_a',3),
                                        rep('dev_dl',3),
                                        rep('mort_e', 3),
                                        rep('mort_l',3),
                                        rep('mort_p',3),
                                        rep('mort_a',3),
                                        rep('mort_dl',3),
                                        rep('dia_induc',3),
                                        'C','COMP')

SENSITIVITY_DEVELOPMENT_FIRST$Function_G <- c(rep('birth',3),
                                            rep('dev',3),
                                            rep('dev', 3),
                                            rep('dev',3),
                                            rep('dev',3),
                                            rep('dev',3),
                                            rep('dev',3),
                                            rep('dev',3),
                                            rep('dev',3),
                                            rep('dia',3),
                                            rep('mort', 3),
                                            rep('mort',3),
                                            rep('mort',3),
                                            rep('mort',3),
                                            rep('mort',3),
                                            rep('dia',3),
                                            'birth','mort')

#############################
#Parameters related to Birth#
#############################
SENSITIVITY_DEVELOPMENT_FIRST_BIRTH_PARAMETERS <- 
  subset(SENSITIVITY_DEVELOPMENT_FIRST, SENSITIVITY_DEVELOPMENT_FIRST$Function_G =='birth')
SENSITIVITY_DEVELOPMENT_FIRST_BIRTH_PARAMETERS$name <- as.character(SENSITIVITY_DEVELOPMENT_FIRST_BIRTH_PARAMETERS$name)




BIRTH <- ggplot(SENSITIVITY_DEVELOPMENT_FIRST_BIRTH_PARAMETERS,
       aes(x= 1, y = name, fill = abs(sensitivity)))+
        geom_tile(color='black')+coord_equal()+
  scale_y_discrete(limits =
     rev(SENSITIVITY_DEVELOPMENT_FIRST_BIRTH_PARAMETERS$name))+
      theme_classic()+
      theme(axis.text.x=element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.line = element_blank())+
         scale_fill_viridis(limits =c(0,0.6),guide=FALSE)+
           ggtitle("Fecundity")


###################################
#Parameters related to Development#
###################################
SENSITIVITY_DEVELOPMENT_FIRST_DEV_PARAMETERS <- 
  subset(SENSITIVITY_DEVELOPMENT_FIRST, SENSITIVITY_DEVELOPMENT_FIRST$Function_G =='dev')
SENSITIVITY_DEVELOPMENT_FIRST_DEV_PARAMETERS$name2 <- 
  c(rep("E",3),
    rep("L1",3),
    rep("L2",3),
    rep("L3",3),
    rep("L4",3),
    rep("L5",3),
    rep("P",3),
    rep("A",3))
SENSITIVITY_DEVELOPMENT_FIRST_DEV_PARAMETERS$name2 <- factor(SENSITIVITY_DEVELOPMENT_FIRST_DEV_PARAMETERS$name2, 
                                                             levels=rev(c("E", "L1", "L2","L3","L4","L5","P","A")))


SENSITIVITY_DEVELOPMENT_FIRST_DEV_PARAMETERS$param <-rep(c('a','b','c'),8)

DEV<- ggplot(SENSITIVITY_DEVELOPMENT_FIRST_DEV_PARAMETERS,
             aes(x= param, y = name2, fill = abs(sensitivity)))+
  geom_tile(color='black')+coord_equal()+
  
  theme_classic()+
  theme(
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank())+
  scale_fill_viridis(limits =c(0,0.6),guide=FALSE)+
  ggtitle("Development")

###################################
#Parameters related to MORTALITY#
###################################
SENSITIVITY_DEVELOPMENT_FIRST_MORT_PARAMETERS <- 
  subset(SENSITIVITY_DEVELOPMENT_FIRST, SENSITIVITY_DEVELOPMENT_FIRST$Function_G =='mort')


SENSITIVITY_DEVELOPMENT_FIRST_MORT_PARAMETERS$name2 <- 
  c(rep("E",3),
    rep("L",3),
    rep("P",3),
    rep("A",3),
    rep("DL",3),
    "COMP")
SENSITIVITY_DEVELOPMENT_FIRST_MORT_PARAMETERS$name2 <- factor(SENSITIVITY_DEVELOPMENT_FIRST_MORT_PARAMETERS$name2, 
                                                              levels=rev(c("E", "L","P","A","DL","COMP")))


SENSITIVITY_DEVELOPMENT_FIRST_MORT_PARAMETERS$param <-c(rep(c('a','b','c'),5),'b')

MORT<- ggplot(SENSITIVITY_DEVELOPMENT_FIRST_MORT_PARAMETERS,
              aes(x= param, y = name2, fill = abs(sensitivity)))+
  geom_tile(color='black')+coord_equal()+
  
  theme_classic()+
  theme(
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank())+
  scale_fill_viridis( limits =c(0,0.6),guide=FALSE)+
  ggtitle("Mortality")



###################################
#Parameters related to DIAPAUSE#
###################################
SENSITIVITY_DEVELOPMENT_FIRST_DIA_PARAMETERS <- 
  subset(SENSITIVITY_DEVELOPMENT_FIRST, SENSITIVITY_DEVELOPMENT_FIRST$Function_G =='dia')

SENSITIVITY_DEVELOPMENT_FIRST_DIA_PARAMETERS$name2 <- 
  c(rep("DIA_1",3),
    rep("DIA_2",3))
SENSITIVITY_DEVELOPMENT_FIRST_DIA_PARAMETERS$param <-rep(c('a','b','c'),2)

DIA<- ggplot(SENSITIVITY_DEVELOPMENT_FIRST_DIA_PARAMETERS,
             aes(x= param, y = name2, fill = abs(sensitivity )))+
  geom_tile(color='black')+coord_equal()+
  
  theme_classic()+
  theme(
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank())+
  scale_fill_viridis(limits =c(0,0.6))+
  ggtitle("Development")

BIRTH + DEV + MORT+ DIA
#########################################################################
#########################################################################
########################
###ABUNDANCE- EGG#####
########################
###CONTROL 


# EGG- ABUNDANCE ----------------------------------------------------------


CONTROL_MODEL<- trajectory(POMP_CM,
                           PARAMETERS ,times=seq(1,12054),
                           format = 'data.frame',method = 'bdf')

CONTROL_EGG <- (rowSums(CONTROL_MODEL [,1:(nE)]))
CONTROL_MODEL_EGG = cbind.data.frame(DATES,Egg=log((0.5*CONTROL_EGG)+1))
CONTROL_MODEL_EGG$Year <- as.numeric(format(CONTROL_MODEL_EGG$DATES,'%Y'))


CONTROL_MODEL_EGG_SPLIT <- split(CONTROL_MODEL_EGG,CONTROL_MODEL_EGG$Year)

SUMMED_SPLIT <-unlist(lapply(CONTROL_MODEL_EGG_SPLIT, function(x) sum(x$Egg )))


###Looks goods- 

PARAMS_START <- PARAMETERS

PERCENT_CHANGE_EGG_ABUNDANCE<- NULL


for ( i in seq(1,50)){
  PARAMS_2_95 <- PARAMS_START
  PARAMS_2_95[[i]] <- PARAMS_START[[i]]*0.95
  
  
  PARAMS_2_105 <- PARAMS_START
  PARAMS_2_105[[i]] <- PARAMS_START[[i]] * 1.05
  
  
  TRAJ_MODEL_95 <- trajectory(POMP_CM,
                              
                              PARAMS_2_95 ,times=seq(1,12054),
                              format = 'data.frame')
  
  
  TRAJ_MODEL_105 <- trajectory(POMP_CM,  PARAMS_2_105 ,
                               times = seq(1,12054),
                               format = 'data.frame')
  
  #EGGS-0.95
  E_95<- data.frame(Date = DATES,
                    E_95 =log((0.5*rowSums(TRAJ_MODEL_95[,1:(nE)]))+1))
  E_95$Year <- as.numeric(format(E_95$Date, format = '%Y'))
  #EGGS-1.05
  E_105<- data.frame(Date = DATES,
                     E_105=log((0.5*rowSums(TRAJ_MODEL_105[,1:(nE)]))+1))
  E_105$Year <- as.numeric(format(E_105$Date, format = '%Y'))
  ###SPLIT- 0.95
  E_95_SPLIT <- split(E_95, E_95$Year)
  
  SUMMED_SPLIT_95 <-unlist(lapply(  E_95_SPLIT , function(x) sum(x$E_95 )))
  
  #SPLIT-1.05
  E_105_SPLIT <- split(E_105, E_105$Year)
  
  SUMMED_SPLIT_105 <-unlist(lapply(  E_105_SPLIT , function(x) sum(x$E_105 )))
  
  PERCENT_CHANGE_EGG_ABUNDANCE [[i]]= mean((SUMMED_SPLIT_105-SUMMED_SPLIT_95)/ (0.10*( 
    SUMMED_SPLIT )))
}



SENSITIVITY_EGG_FIRST = 
  cbind.data.frame(name = names(PARAMETERS),sensitivity=  PERCENT_CHANGE_EGG_ABUNDANCE)

SENSITIVITY_EGG_FIRST $Name <- c(rep('E',3),
                                        rep('E',3),
                                        rep('L1', 3),
                                        rep('L2',3),
                                        rep('L3',3),
                                        rep('L4',3),
                                        rep('L5',3),
                                        rep('P',3),
                                        rep('A',3),
                                        rep('DL',3),
                                        rep('E', 3),
                                        rep('L',3),
                                        rep('P',3),
                                        rep('A',3),
                                        rep('DL',3),
                                        rep('DIA',3),
                                        'C','COMP')

ggplot(SENSITIVITY_EGG_FIRST, aes(y=name , x= 1, 
                                 label =round(sensitivity,digits=2)))+
  geom_tile(aes(fill =abs((sensitivity)),width = 1,height =1),  size = 0.8,
            color='black')+facet_grid(.~Name)+
  scale_fill_viridis()+coord_equal()



save(SENSITIVITY_EGG_FIRST, file = 'Sens_EGG_Abund.RData')


####################################################################
####################################################################

#########################
#######################
###DIAPAUSING LARVAE#####
#########################
#########################


CONTROL_MODEL<- trajectory(POMP_CM,
                           PARAMETERS ,times=seq(1,12054),
                           format = 'data.frame',method = 'bdf')

CONTROL_DL <- (rowSums(CONTROL_MODEL [,(nE+nL1+nL2+nL3+nL4+nL5+1): 
                                        (nE+nL1+nL2+nL3+nL4+nL5+nDL5)]))
CONTROL_MODEL_DL = cbind.data.frame(DATES,DL=log((0.5*CONTROL_DL)+1))
CONTROL_MODEL_DL$Year <- as.numeric(format(CONTROL_MODEL_DL$DATES,'%Y'))


CONTROL_MODEL_DL_SPLIT <- split(CONTROL_MODEL_DL,CONTROL_MODEL_DL$Year)

SUMMED_SPLIT_DL <-unlist(lapply(CONTROL_MODEL_DL_SPLIT, function(x) sum(x$DL)))


###Looks goods- 

PARAMS_START <- PARAMETERS

PERCENT_CHANGE_DL_ABUNDANCE<- NULL


for ( i in seq(1,50)){
  PARAMS_2_95 <- PARAMS_START
  PARAMS_2_95[[i]] <- PARAMS_START[[i]]*0.95
  
  
  PARAMS_2_105 <- PARAMS_START
  PARAMS_2_105[[i]] <- PARAMS_START[[i]] * 1.05
  
  
  TRAJ_MODEL_95 <- trajectory(POMP_CM,
                              
                              PARAMS_2_95 ,times=seq(1,12054),
                              format = 'data.frame')
  
  
  TRAJ_MODEL_105 <- trajectory(POMP_CM,  PARAMS_2_105 ,
                               times = seq(1,12054),
                               format = 'data.frame')
  
  DL_95<- data.frame(Date = DATES,
                    DL_95 =log((0.5*rowSums(TRAJ_MODEL_95[,(nE+nL1+nL2+nL3+nL4+nL5+1): 
                      (nE+nL1+nL2+nL3+nL4+nL5+nDL5)]))+1))
  DL_95$Year <- as.numeric(format(DL_95$Date, format = '%Y'))
  DL_105<- data.frame(Date = DATES,
                     DL_105=log((0.5*rowSums(TRAJ_MODEL_105[,(nE+nL1+nL2+nL3+nL4+nL5+1): 
                      (nE+nL1+nL2+nL3+nL4+nL5+nDL5)]))+1))
  DL_105$Year <- as.numeric(format(DL_105$Date, format = '%Y'))
  DL_95_SPLIT <- split(DL_95, DL_95$Year)
  
  SUMMED_SPLIT_95_DL <-unlist(lapply(DL_95_SPLIT , function(x) sum(x$DL_95 )))
  
  DL_105_SPLIT <- split(DL_105, DL_105$Year)
  
  SUMMED_SPLIT_105_DL <-unlist(lapply(DL_105_SPLIT , function(x) sum(x$DL_105 )))
  
  PERCENT_CHANGE_DL_ABUNDANCE [[i]]= mean((SUMMED_SPLIT_105_DL-SUMMED_SPLIT_95_DL)/ (0.10*( 
    SUMMED_SPLIT_DL)))
}



SENSITIVITY_DL_FIRST = 
  cbind.data.frame(name = names(PARAMETERS),sensitivity=  PERCENT_CHANGE_DL_ABUNDANCE)

SENSITIVITY_DL_FIRST $Name <- c(rep('E',3),
                                 rep('E',3),
                                 rep('L1', 3),
                                 rep('L2',3),
                                 rep('L3',3),
                                 rep('L4',3),
                                 rep('L5',3),
                                 rep('P',3),
                                 rep('A',3),
                                 rep('DL',3),
                                 rep('E', 3),
                                 rep('L',3),
                                 rep('P',3),
                                 rep('A',3),
                                 rep('DL',3),
                                 rep('DIA',3),
                                 'C','COMP')


ggplot(SENSITIVITY_DL_FIRST, aes(y=name , x= 1, 
                                  label =round(sensitivity,digits=2)))+
  geom_tile(aes(fill =abs((sensitivity)),width = 1,height =1),  size = 0.8,
            color='black')+facet_grid(.~Name)+
  scale_fill_viridis()+coord_equal()



###############################
###############################
#############ADULT#############
###############################
################################

CONTROL_MODEL<- trajectory(POMP_CM,
                           PARAMETERS ,times=seq(1,12054),
                           format = 'data.frame',method = 'bdf')

CONTROL_A <-  rowSums(CONTROL_MODEL[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                     (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)])
CONTROL_MODEL_A = cbind.data.frame(DATES,A=log((0.5*CONTROL_A)+1))
CONTROL_MODEL_A$Year <- as.numeric(format(CONTROL_MODEL_A$DATES,'%Y'))


CONTROL_MODEL_A_SPLIT <- split(CONTROL_MODEL_A,CONTROL_MODEL_A$Year)

SUMMED_SPLIT_A <-unlist(lapply(CONTROL_MODEL_A_SPLIT, function(x) sum(x$A)))


PARAMS_START <-PARAMETERS

PERCENT_CHANGE_A_ABUNDANCE<- NULL


for ( i in seq(1,50)){
  PARAMS_2_95 <- PARAMS_START
  PARAMS_2_95[[i]] <- PARAMS_START[[i]]*0.95
  
  
  PARAMS_2_105 <- PARAMS_START
  PARAMS_2_105[[i]] <- PARAMS_START[[i]] * 1.05
  
  
  TRAJ_MODEL_95 <- trajectory(POMP_CM,
                              
                              PARAMS_2_95 ,times=seq(1,12054),
                              format = 'data.frame')
  
  
  TRAJ_MODEL_105 <- trajectory(POMP_CM,  PARAMS_2_105 ,
                               times = seq(12054),
                               format = 'data.frame')
  
  
  A_95<- rowSums(TRAJ_MODEL_95[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                  (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)])
  
  A_105<- rowSums(TRAJ_MODEL_105[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                    (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)])
  A_95<- data.frame(Date = DATES,
                    A_95 =log((0.5* rowSums(TRAJ_MODEL_95[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                              (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)]))+1))
  A_95$Year <- as.numeric(format(A_95$Date, format = '%Y'))
  #EGGS-1.05
  A_105<- data.frame(Date = DATES,
                      A_105=log((0.5*rowSums(TRAJ_MODEL_105[,(nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+1):
                                             (nE+nL1+nL2+nL3+nL4+nL5+nDL5+nP+nAr)]))+1))
  A_105$Year <- as.numeric(format(A_105$Date, format = '%Y'))
  ###SPLIT- 0.95
  A_95_SPLIT <- split(A_95, A_95$Year)
  
  SUMMED_SPLIT_95_A <-unlist(lapply(A_95_SPLIT , function(x) sum(x$A_95 )))
  
  #SPLIT-1.05
  A_105_SPLIT <- split(A_105,A_105$Year)
  
  SUMMED_SPLIT_105_A <-unlist(lapply(A_105_SPLIT , function(x) sum(x$A_105 )))
  
  PERCENT_CHANGE_A_ABUNDANCE [[i]]= mean((SUMMED_SPLIT_105_A-SUMMED_SPLIT_95_A)/ (0.10*( 
    SUMMED_SPLIT_A)))
  
}



SENSITIVITY_A_FIRST = 
  cbind.data.frame(name = names(PARAMETERS),sensitivity= 
                     PERCENT_CHANGE_A_ABUNDANCE)
SENSITIVITY_A_FIRST $Name <- c(rep('E',3),
                                rep('E',3),
                                rep('L1', 3),
                                rep('L2',3),
                                rep('L3',3),
                                rep('L4',3),
                                rep('L5',3),
                                rep('P',3),
                                rep('A',3),
                                rep('DL',3),
                                rep('E', 3),
                                rep('L',3),
                                rep('P',3),
                                rep('A',3),
                                rep('DL',3),
                                rep('DIA',3),
                                'C','COMP')


ggplot(SENSITIVITY_A_FIRST, aes(y=name , x= 1, 
                                 label =round(sensitivity,digits=2)))+
  geom_tile(aes(fill =abs((sensitivity)),width = 1,height =1),  size = 0.8,
            color='black')+facet_grid(.~Name)+
  scale_fill_viridis()+coord_equal()


save(SENSITIVITY_A_FIRST, file = 'Sens_A_Abund.RData')
