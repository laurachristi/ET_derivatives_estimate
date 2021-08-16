library(readxl)
library(tidyr)
library(tidyverse)

setwd("~/Modena Data")

corn_252=read_excel("Allen/Soil Moisture/2020/Corn 252.xlsx")
corn_245=read_excel("Allen/Soil Moisture/2020/Corn 245.xlsx")
corn_215=read_excel("Allen/Soil Moisture/2020/Corn 215.xlsx")
alf_216=read_excel("Allen/Soil Moisture/2020/Alfalfa 216.xlsx")
alf_226=read_excel("Allen/Soil Moisture/2020/Alfalfa 226.xlsx")
alf_263=read_excel("Allen/Soil Moisture/2020/Alfalfa 263.xlsx")

##Corn 252 Soil Moisture 
plot(corn_252$`Timestamp (UTC)`, corn_252$`1 Acclima`, ylim=c(10, 30), type='l', col='coral4', main='Corn 252 Soil Moisture', xlab='2020', ylab='Soil Moisture Content (%)') 
lines(corn_252$`Timestamp (UTC)`, corn_252$`2 Acclima`, type='l', pch=20, col='steelblue4')
lines(corn_252$`Timestamp (UTC)`, corn_252$`3 Acclima`, type='l', pch=20, col='gold4')
lines(corn_252$`Timestamp (UTC)`, corn_252$`4 Acclima`, type='l', pch=20, col='olivedrab4')
lines(corn_252$`Timestamp (UTC)`, corn_252$`5 Acclima`, type='l', pch=20, col='coral2')
lines(corn_252$`Timestamp (UTC)`, corn_252$`6 Acclima`, type='l', pch=20, col='steelblue1')
lines(corn_252$`Timestamp (UTC)`, corn_252$`7 Acclima`, type='l', pch=20, col='gold2')
lines(corn_252$`Timestamp (UTC)`, corn_252$`8 Acclima`, type='l', pch=20, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('s1', 's2','s3','s4','3 in','3 in','6 in','6 in','s5','s6','s7','s8','1 ft','2 ft','3 ft','4 ft'), col = c(NA,NA,NA,NA,'coral4','steelblue4','gold4','olivedrab4',NA,NA,NA,NA,'coral2','steelblue1','gold2','olivedrab3'), ncol=4)

##Corn 245 Soil Moisture
plot(corn_245$`Timestamp (UTC)`, corn_245$`1 Acclima`, ylim=c(10, 35), type='l', col='coral4', main='Corn 245 Soil Moisture', xlab='2020', ylab='Soil Moisture Content (%)') 
lines(corn_245$`Timestamp (UTC)`, corn_245$`2 Acclima`, type='l', pch=20, col='steelblue4')
lines(corn_245$`Timestamp (UTC)`, corn_245$`3 Acclima`, type='l', pch=20, col='gold4')
lines(corn_245$`Timestamp (UTC)`, corn_245$`4 Acclima`, type='l', pch=20, col='olivedrab4')
lines(corn_245$`Timestamp (UTC)`, corn_245$`5 Acclima`, type='l', pch=20, col='coral2')
lines(corn_245$`Timestamp (UTC)`, corn_245$`6 Acclima`, type='l', pch=20, col='steelblue1')
lines(corn_245$`Timestamp (UTC)`, corn_245$`7 Acclima`, type='l', pch=20, col='gold2')
lines(corn_245$`Timestamp (UTC)`, corn_245$`8 Acclima`, type='l', pch=20, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('s1', 's2','s3','s4','3 in','3 in','6 in','6 in','s5','s6','s7','s8','1 ft','2 ft','3 ft','4 ft'), col = c(NA,NA,NA,NA,'coral4','steelblue4','gold4','olivedrab4',NA,NA,NA,NA,'coral2','steelblue1','gold2','olivedrab3'), ncol=4)

##Corn 215 Soil Moisture
plot(corn_215$`Timestamp (UTC)`, corn_215$`1 Acclima`, ylim=c(0, 40), type='l', col='coral4', main='Corn 215 Soil Moisture', xlab='2020', ylab='Soil Moisture Content (%)') 
lines(corn_215$`Timestamp (UTC)`, corn_215$`2 Acclima`, type='l', pch=20, col='steelblue4')
lines(corn_215$`Timestamp (UTC)`, corn_215$`3 Acclima`, type='l', pch=20, col='gold4')
lines(corn_215$`Timestamp (UTC)`, corn_215$`4 Acclima`, type='l', pch=20, col='olivedrab4')
lines(corn_215$`Timestamp (UTC)`, corn_215$`5 Acclima`, type='l', pch=20, col='coral2')
lines(corn_215$`Timestamp (UTC)`, corn_215$`6 Acclima`, type='l', pch=20, col='steelblue1')
lines(corn_215$`Timestamp (UTC)`, corn_215$`7 Acclima`, type='l', pch=20, col='gold2')
lines(corn_215$`Timestamp (UTC)`, corn_215$`8 Acclima`, type='l', pch=20, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('s1', 's2','s3','s4','3 in','3 in','6 in','6 in','s5','s6','s7','s8','1 ft','2 ft','3 ft','4 ft'), col = c(NA,NA,NA,NA,'coral4','steelblue4','gold4','olivedrab4',NA,NA,NA,NA,'coral2','steelblue1','gold2','olivedrab3'), ncol=4)

##Alfalfa 216 Soil Moisture
plot(alf_216$`Timestamp (UTC)`, alf_216$`1 Acclima`, ylim=c(5, 35), type='l', col='coral4', main='Alfalfa 216 Soil Moisture', xlab='2020', ylab='Soil Moisture Content (%)') 
lines(alf_216$`Timestamp (UTC)`, alf_216$`2 Acclima`, type='l', pch=20, col='steelblue4')
lines(alf_216$`Timestamp (UTC)`, alf_216$`3 Acclima`, type='l', pch=20, col='gold4')
lines(alf_216$`Timestamp (UTC)`, alf_216$`4 Acclima`, type='l', pch=20, col='olivedrab4')
lines(alf_216$`Timestamp (UTC)`, alf_216$`5 Acclima`, type='l', pch=20, col='coral2')
lines(alf_216$`Timestamp (UTC)`, alf_216$`6 Acclima`, type='l', pch=20, col='steelblue1')
lines(alf_216$`Timestamp (UTC)`, alf_216$`7 Acclima`, type='l', pch=20, col='gold2')
legend('bottom', lty=1, lwd=3, legend=c('s1', 's2','s3','s4','3 in','6 in','1 ft','2 ft','s5','s6','s7',NA,'3 ft','35 in','45 in'), col = c(NA,NA,NA,NA,'coral4','steelblue4','gold4','olivedrab4',NA,NA,NA,NA,'coral2','steelblue1','gold2'), ncol=4)


##Alfalfa 226 Soil Moisture
plot(alf_226$`Timestamp (UTC)`, alf_226$`1 Acclima`, ylim=c(5, 30), type='l', col='coral4', main='Alfalfa 226 Soil Moisture', xlab='2020', ylab='Soil Moisture Content (%)') 
lines(alf_226$`Timestamp (UTC)`, alf_226$`2 Acclima`, type='l', pch=20, col='steelblue4')
lines(alf_226$`Timestamp (UTC)`, alf_226$`3 Acclima`, type='l', pch=20, col='gold4')
lines(alf_226$`Timestamp (UTC)`, alf_226$`4 Acclima`, type='l', pch=20, col='olivedrab4')
lines(alf_226$`Timestamp (UTC)`, alf_226$`5 Acclima`, type='l', pch=20, col='coral2')
lines(alf_226$`Timestamp (UTC)`, alf_226$`6 Acclima`, type='l', pch=20, col='steelblue1')
lines(alf_226$`Timestamp (UTC)`, alf_226$`7 Acclima`, type='l', pch=20, col='gold2')
legend('bottom', lty=1, lwd=3, legend=c('s1', 's2','s3','s4','3 in','6 in','1 ft','2 ft','s5','s6','s7',NA,'3 ft','4 ft','5 ft'), col = c(NA,NA,NA,NA,'coral4','steelblue4','gold4','olivedrab4',NA,NA,NA,NA,'coral2','steelblue1','gold2'), ncol=4)


##Alfalfa 263 Soil Moisture
plot(alf_263$`Timestamp (UTC)`, alf_263$`1 Acclima`, ylim=c(5, 30), type='l', col='coral4', main='Alfalfa 263 Soil Moisture', xlab='2020', ylab='Soil Moisture Content (%)') 
lines(alf_263$`Timestamp (UTC)`, alf_263$`2 Acclima`, type='l', pch=20, col='steelblue4')
lines(alf_263$`Timestamp (UTC)`, alf_263$`3 Acclima`, type='l', pch=20, col='gold4')
lines(alf_263$`Timestamp (UTC)`, alf_263$`4 Acclima`, type='l', pch=20, col='olivedrab4')
lines(alf_263$`Timestamp (UTC)`, alf_263$`5 Acclima`, type='l', pch=20, col='coral2')
lines(alf_263$`Timestamp (UTC)`, alf_263$`6 Acclima`, type='l', pch=20, col='steelblue1')
lines(alf_263$`Timestamp (UTC)`, alf_263$`7 Acclima`, type='l', pch=20, col='gold2')
legend('bottom', lty=1, lwd=3, legend=c('s1', 's2','s3','s4','3 in','6 in','1 ft','2 ft','s5','s6','s7',NA,'3 ft','4 ft','5 ft'), col = c(NA,NA,NA,NA,'coral4','steelblue4','gold4','olivedrab4',NA,NA,NA,NA,'coral2','steelblue1','gold2'), ncol=4)
