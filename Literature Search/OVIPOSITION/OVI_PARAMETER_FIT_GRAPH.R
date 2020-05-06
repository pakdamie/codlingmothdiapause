

####This is the code for calculating the oviposition rate of the codling moth
###We use nls to estimate the parameters

library(ggplot2)

###This is the data for it
OVI <- read.csv("ovi_cm.csv")

TEMP = seq(-10,35)

EGG_DAT_NLS <- nls(EGG_DAY_1~ a*(exp(-0.5*(Temp-b)^2)/(c^2)),data=OVI  ,
                     start= list(a = 10, b= 25, c =0))


plot(TEMP,10.52*exp(-0.5*(TEMP-24.62)^2/(3.42^2)))

OVI_EQUATION <- cbind.data.frame(TEMP, OVI_PRED = 10.52*exp(-0.5*(TEMP-24.62)^2/(3.42^2)))

ggplot(OVI, aes(x = Temp, y = EGG_DAY)) + 
  geom_point() + geom_line(data = OVI_EQUATION,
  aes(x = TEMP, y = OVI_PRED),
  size = 1.2,
  alpha = 0.3,
  color = 'blue') + 
  theme_classic() + ylab("Per capita birth rate") +
  theme(panel.border = element_rect(fill = NA, colour = "black"))


