#######################
###Circular Variance###
#######################

################################################################
###PACKAGES######################################################
#################################################################
library(ggplot2)
library(reshape2)
library(patchwork)
library(plotrix) #just I'm lazy and didn't want to write my own
###standard error function (crazy, how it's not in base R)

#################################################################
#The Dates#######################################################
#################################################################
DATE <- seq.Date(as.Date('1/1/1984', format = '%m/%d/%Y'),
                 as.Date('12/31/2016',format = '%m/%d/%Y'),'days')
################################################################
################################################################
#########################################################################################################
###The circular variance is calculated here and this function is nested with a function that normalizes##
###the data output further down the lines                                                             ##
#########################################################################################################

########################################################################################################
###!!!IF YOU'RE SPECIFICALLY JUST LOOKING FOR THE CIRCULAR VARIANCE FUNCTION- THIS IS IT HERE!!!!!!#####
#########################################################################################################
cvar=function(y,theta.start,theta.end){
  
  ###The theta.start and theta.end is provided by the user and is determined by the number of stages
  ### or subcompartments in my cases
  p=(theta.end-theta.start)/sum(theta.end-theta.start);
  theta.start=c(0,2*pi*cumsum(p[-length(p)]));
  theta.end=2*pi*cumsum(p) #normalize theta for missing stages
  
  if(length(dim(y))==0){
    y=matrix(y,nrow=1,ncol=length(y))}
  
  diff.theta=matrix(theta.end-theta.start,
                    nrow=dim(y)[1],
                    ncol=dim(y)[2],
                    byrow=T)
  
  cos.theta1=matrix(cos(theta.start),nrow=dim(y)[1],ncol=dim(y)[2],byrow=T);
  cos.theta2=matrix(cos(theta.end),nrow=dim(y)[1],ncol=dim(y)[2],byrow=T)
  
  sin.theta1=matrix(sin(theta.start),nrow=dim(y)[1],ncol=dim(y)[2],byrow=T);              
  sin.theta2=matrix(sin(theta.end),nrow=dim(y)[1],ncol=dim(y)[2],byrow=T)
  
  ###THIS IS HOW YOU CALCULATE THE Resultant Vector 
  R=sqrt(rowSums((y/diff.theta)*(sin.theta2-sin.theta1))^2+rowSums((y/diff.theta)*(cos.theta1-cos.theta2))^2)/rowSums(y)
  
  ###You then return the Circular Variance 
  return(1-R)
  
}
################################################################
####You can look further, but it's mostly for my data figures###
################################################################

##########################################################################
###This fixes up the kind of data i get from the desolve/pomp output    #
### I just take the output from the pomp output and put it through here-#
###if you get an error message, check you're producing enough theta #####
#########################################################################

#################################################################################################
###I call this the circular variance noramlizer, because you put in the data and it noramlizes,##
###calculates thet theta, and circular variance together.                                      ##
#################################################################################################
cvar_norm<- function(dat, descriptor){
  
###(This gets rid of the columns that you don't need-specifically the time columns )
TRAJ_MODEL_FIXED <- dat[,-c(ncol(dat)-2,
                                   ncol(dat)-1,
                                         ncol(dat))]

###You calculate the number of theta based on the column length
theta = seq(0, 2*pi, length = length(colnames(TRAJ_MODEL_FIXED))+1)
###calculate the starting theta for each stage
theta.start <- theta[-length(theta)]
###calculate the ending theta for each stage
theta.end <- theta[-1]

###Normalize the data
y=TRAJ_MODEL_FIXED/matrix(colMeans(TRAJ_MODEL_FIXED),
                   nrow=dim(TRAJ_MODEL_FIXED)[1],
                  ncol=dim(TRAJ_MODEL_FIXED)[2],byrow=T) #normalize
###Then you get the information back
var.loc.NORM= cvar(y,theta.start,theta.end)


###For simplicty, this function calculate the year and julian
tmp =  data.frame(DATE, var.loc.NORM, descriptor)
tmp$year <- as.numeric(format(tmp$DATE, '%Y'))
tmp$julian <- as.numeric(format(tmp$DATE, '%j'))

return(tmp)
}



##################################
###MAKING FIGURE 10 and FIGURE 11#
###################################

###For my sanity, I decide to make data-files early on by manually changing the values and
###Running the trajectory (If you want to see the files look at "./Circular Variance/Data_output")

##The data file you need is "FULL_UNCHANGED_MODEL.RData" and "FULL_HOT_UNCHANGED_MODEL.RData
###The whole point is that you calculate the circular variance for 33 years and then aggregate it by 
###day of year (1-365 days)

FULL_UNCHANGED_MODEL_CIRC<- cvar_norm(TRAJ_MODEL,"FULL_UNCHANGED")
FULL_HOT_UNCHANGED_MODEL_CIRC <- cvar_norm(TRAJ_MODEL_HOT,"FULL_HOT_UNCHANGED")

Unchanged_Agg <- do.call(data.frame,aggregate(FULL_UNCHANGED_MODEL_CIRC$var.loc.NORM,
                             by=list(FULL_UNCHANGED_MODEL_CIRC$julian),FUN = function(x)
                               c(mean = mean(x),se=std.error(x)))[-366,])
Unchanged_Agg$id <- "Control"
Unchanged_Hot_Agg <- do.call(data.frame,aggregate(FULL_HOT_UNCHANGED_MODEL_CIRC $var.loc.NORM,
                                              by=list(FULL_HOT_UNCHANGED_MODEL_CIRC $julian),FUN = function(x)
                                                c(mean = mean(x),se=std.error(x)))[-366,])
Unchanged_Hot_Agg$id <- "Hot"


Full_Unchanged_cont_hot <- rbind.data.frame(Unchanged_Agg, Unchanged_Hot_Agg)



levels(Full_Unchanged_cont_hot$id)[levels(Full_Unchanged_cont_hot$id)=="Control"] <- "Historical temperature"
levels(Full_Unchanged_cont_hot$id)[levels(Full_Unchanged_cont_hot$id)=="Hot"] <- "Historical temperature + 4 °C"
names(Full_Unchanged_cont_hot)[names(Full_Unchanged_cont_hot)=="id"]  <- "Scenario"

###Here is it to make Figure 10
ALL<-ggplot(Full_Unchanged_cont_hot, 
       aes( x= as.numeric(Group.1), y= as.numeric(x.mean), color= Scenario,group=Scenario))+
        geom_line(size =1)+
  geom_ribbon(aes(ymin = as.numeric(x.mean)- as.numeric(x.se),
                 ymax = as.numeric(x.mean)+as.numeric(x.se), fill = Scenario),alpha = 0.3)+
                 scale_color_manual(values = c('navyblue','#fa1e3f'))+
                scale_fill_manual(values = c('navyblue','#fa1e3f'))+ theme_bw()+
          theme(panel.grid.major = 
          element_blank(), panel.grid.minor = element_blank(),
          text=element_text(size = 16),
          legend.position="top")+
  xlab("Day of year")+
  ylab("Circular variance")+ylim(0,1)

#################################################################################################################
##################################################################################################################

###This is for Figure 11 where we can compare the circular variance for different number of substages for pupae
###or diapausing larvae (NOTE: ONE OR THE OTHER, DO NOT CHANGE BOTH)

###Again these data-ouptuts are all in the folder and should be pretty understandable 
###We are comparing this to control (Where we are using historical temperature and there no changes to 
###the number of subcompartment for the nDL or nP )
###########################################################
###THIS IS FOR THE PUPAE###################################
###########################################################

FULL_UNCHANGED <- cvar_norm(TRAJ_MODEL_HOT,"HOT_UNCHANGED")
FULL_HOT_P5_CIRC<- cvar_norm(TRAJ_MODEL_HOT_P_5,"HOT_P5")
FULL_HOT_P10_CIRC<- cvar_norm(TRAJ_MODEL_HOT_P_10,"HOT_P10")

############
###nP = 5###
############
FULL_HOT_P5_Agg <- do.call(data.frame,aggregate(FULL_HOT_P5_CIRC$var.loc.NORM,
                           by=list(FULL_HOT_P5_CIRC$julian),FUN = function(x)
                         c(mean = mean(x),se=std.error(x)))[-366,])
FULL_HOT_P5_Agg$id <- "HOT_P5"

############
###nP = 10###
############
FULL_HOT_P10_Agg <- do.call(data.frame,aggregate(FULL_HOT_P10_CIRC$var.loc.NORM,
                                                by=list(FULL_HOT_P10_CIRC$julian),FUN = function(x)
                                                  c(mean = mean(x),se=std.error(x)))[-366,])
FULL_HOT_P10_Agg$id <- "HOT_P10"


#############
###nP = 25###
##############
FULL_UNCHANGED_Agg <- do.call(data.frame,aggregate(FULL_UNCHANGED$var.loc.NORM,
                                                 by=list(FULL_UNCHANGED$julian),FUN = function(x)
                                                   c(mean = mean(x),se=std.error(x)))[-366,])
FULL_UNCHANGED_Agg$id <- "HOT"
############
###FULL P###
############
FULL_P_HOT <- rbind.data.frame(FULL_HOT_P5_Agg, 
                               FULL_HOT_P10_Agg, 
                               FULL_UNCHANGED_Agg,Unchanged_Agg)


################################################################################################
#############################################################################################


###THIS MAKES THE DIAPAUSING LARVAE PART OF FIGURE 11
HOT_PUP_2 <- ggplot(FULL_P_HOT, aes(x = Group.1,
                                    y = x.mean, color = id)) +
  geom_line(size = 1.1) + scale_color_manual(values =
           c('navyblue', '#fa1e3f', '#E17F12', '#FFE300'),
           guide=FALSE) +
  theme_bw() + theme(
    panel.grid.major =
      element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16))+
  ylim(0, 1)+
  xlab("Day of year")+
  ylab("Circular variance")+ggtitle('Pupae')

###THIS IS FOR THE PUPAE###################################
FULL_HOT_DL5_CIRC<- cvar_norm(TRAJ_MODEL_HOT_DL_5,"HOT_DL5")
FULL_HOT_DL10_CIRC<- cvar_norm(TRAJ_MODEL_HOT_DL_10,"HOT_DL10")

#############
###nDL = 5###
#############
FULL_HOT_DL5_Agg <- do.call(data.frame,aggregate(FULL_HOT_DL5_CIRC$var.loc.NORM,
                                                by=list(FULL_HOT_DL5_CIRC$julian),FUN = function(x)
                                                  c(mean = mean(x),se=std.error(x)))[-366,])
FULL_HOT_DL5_Agg$id <- "HOT_DL5"

#############
###nDL = 10###
#############
FULL_HOT_DL10_Agg <- do.call(data.frame,aggregate(FULL_HOT_DL10_CIRC$var.loc.NORM,
                                                 by=list(FULL_HOT_DL10_CIRC$julian),FUN = function(x)
                                                   c(mean = mean(x),se=std.error(x)))[-366,])
FULL_HOT_DL10_Agg$id <- "HOT_DL10"

FULL_DL_HOT <- rbind.data.frame(FULL_HOT_DL5_Agg, FULL_HOT_DL10_Agg, FULL_UNCHANGED_Agg,Unchanged_Agg)


###THIS MAKES THE DIAPAUSING LARVAE PART OF FIGURE 11
HOT_DL_2<-  ggplot(FULL_DL_HOT, aes( x= Group.1, y = x.mean, color = id))+
  geom_line(size = 1.1) + scale_color_manual(values =
                                               c('navyblue', '#fa1e3f', '#E17F12', '#FFE300')) +
  theme_bw() + theme(
    panel.grid.major =
      element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16))+
  ylim(0, 1)+
  xlab("Day of year")+
  ylab("Circular variance")+ggtitle("Diapausing Larvae")



###THIS MAKES THE FIGURE 11 GRAPH

ALL/(HOT_PUP_2 | HOT_DL_2)
