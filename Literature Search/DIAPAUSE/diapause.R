###############################################
############DIAPAUSE FIGURES###################
###############################################

###DIAPAUSE TERMINATION RATE ####

alphaDL_a=0.0716149819  
alphaDL_b=.0899064656
alphaDL_c= 0.53 

TT=seq(-35,35,)

DIA_DEV <- data.frame(Temperature=TT,
             DEV = alphaDL_a/(1+exp(alphaDL_b *(TT + alphaDL_c))))
          
DIA_DEV_GG <- ggplot(DIA_DEV, aes(x = Temperature, y = DEV))+
  geom_line(size =1.2,color='navyblue') + theme_bw()+
  xlab("Temperature °C")+
  ylab("Diapause termination rate (1/day)")+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 20))
             
###DIAPAUSE INDUCTION RATE ####

DIA_A=0.525 #Diapause induction rate
DIA_B=0.5#...
DIA_C= 225#.

PP_DIFF <- (covartable$PP_DIFF[1:364])

DIA_INDUC <- data.frame(DOY =seq(1,364),
             INDUC =DIA_A/( DIA_B+exp(DIA_C *PP_DIFF)))

DIA_INDUC_GG <- ggplot(DIA_INDUC, aes(x = DOY, y = INDUC))+
  geom_line(size =1.2,color='navyblue') + theme_bw()+
  xlab("Day of year")+
  ylab("Diapause induction rate (1/day)")+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 20))

DIA_DEV_GG + DIA_INDUC_GG
