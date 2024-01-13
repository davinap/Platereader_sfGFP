setwd("C:/Users/davin/Documents/PhD/Writing/Heterogeneity/Supp_Data/YTK_platecurver_sfGFP/311020_sfGFP/")
library(readxl)
library(growthcurver)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(plater)
library(directlabels)
library(gtools)
library(extrafont)

#plater import
plated <- read_plate(
  file = "311020_layoutgfp.csv",
  well_ids_column = "Wells")

#take away OD column from the plater tibble and transpose
platerbind <- plated[,1:2] %>% t()

#####SORTFILES#####
#folder for files containing OD
sortfiles<-mixedsort(list.files(pattern = '.xlsx'))
sortfiles

#####FUNCTIONS#####
#extract OD function
#For GFP signal, changed C27:N34 to C28:N35
extractify.OD <- function(f){
  excel <-read_excel(f, col_names=FALSE, 
                     range = "C28:N35", col_types = c("numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric"))
  transv <- as.vector((t(excel)))
  return(transv)}

#tidy up function - REMEMBER TO CHANGE TIME POINTS
tidy.up<-function(newseries){
  Time<-c("0","1", "2","3","4","5","6","7","8","8.5","10","11","11.25","12","13","23.25","24","25",
          "25.5","26","27","48","49","50","51","52","53","54","55","55.5","56")
  tns<-t(newseries)
  nice<-rbind(platerbind[2,], tns)
  samples<-as.data.frame(nice[c(1,3:nrow(nice)),], col.names=TRUE, stringsAsFactors=FALSE)
  colnames(samples)<-samples[1,]
  samples_clean<-samples[2:nrow(samples),]
  merged<-cbind(Time,samples_clean, stringsAsFactors=FALSE) 
  fornum <- merged[!is.na(names(merged))]
  num<- fornum %>% mutate_if(sapply(fornum, is.character), as.numeric)
  write.csv(num, "tidy_new.csv", row.names=FALSE) #######CHANGE FILE NAME
}

length(sortfiles)
#check extractify
##length(sortfiles)
##extractify.OD(sortfiles[[22]])


#series dataframe for loop
series <- data.frame(well=c("A01","A02","A03","A04","A05","A06","A07","A08","A09","A10","A11","A12",
                            "B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12",
                            "C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12",
                            "D01","D02","D03","D04","D05","D06","D07","D08","D09","D10","D11","D12",
                            "E01","E02","E03","E04","E05","E06","E07","E08","E09","E10","E11","E12",
                            "F01","F02","F03","F04","F05","F06","F07","F08","F09","F10","F11","F12",
                            "G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12",
                            "H01","H02","H03","H04","H05","H06","H07","H08","H09","H10","H11","H12"))

#loop
for(f in sortfiles){series[[f]]<-cbind(extractify.OD(f))}
tidy.up(series)
tidied<-read.csv("tidy_new.csv", header = T, stringsAsFactors = F)
View(tidied)

####Extraction, blank-correction and plotting#####---------------------------------

#####extraction#####
#extract ypd1, ypdx1, sm1, yepg1 - not renaming blank columns as will not use growthcurver
ypd1<- select(tidied,1,contains("ypd1"))
ypdx1<- select(tidied,1,contains("ypdx1"))
sm1<- select(tidied,1,contains("sm1"))
yepg1<- select(tidied,1,contains("yepg1"))


#####blank-correction#####
#note: [,3:ncol(df)-1] means 2nd column to 2nd to last column. [,3:(ncol(df)-1)] means 3rd column to 2nd to last column
ypd1_bc<-ypd1
ypd1_bc[,2:(ncol(ypd1_bc)-1)] = ypd1_bc[,2:(ncol(ypd1_bc)-1)] - ypd1_bc[,ncol(ypd1_bc)]
View(ypd1_bc) #good

ypdx1_bc<-ypdx1
ypdx1_bc[,2:(ncol(ypdx1_bc)-1)] = ypdx1_bc[,2:(ncol(ypdx1_bc)-1)] - ypdx1_bc[,ncol(ypdx1_bc)]
sm1_bc<-sm1
sm1_bc[,2:(ncol(sm1_bc)-1)] = sm1_bc[,2:(ncol(sm1_bc)-1)] - sm1_bc[,ncol(sm1_bc)]
yepg1_bc<-yepg1
yepg1_bc[,2:(ncol(yepg1_bc)-1)] = yepg1_bc[,2:(ncol(yepg1_bc)-1)] - yepg1_bc[,ncol(yepg1_bc)]

#####SAVE######
#save blank-corrected tables as csvs
write.csv(ypd1_bc, "311020_ypd1bc.csv")
write.csv(ypdx1_bc, "311020_ypdXbc.csv")
write.csv(sm1_bc, "311020_sm1bc.csv")
write.csv(yepg1_bc, "311020_yepg1bc.csv")


#melting
ypd1_melt <- melt(ypd1_bc, id.vars = "Time")
ypdx1_melt <- melt(ypdx1_bc, id.vars = "Time")
sm1_melt <- melt(sm1_bc, id.vars = "Time")
yepg1_melt <- melt(yepg1_bc, id.vars = "Time")


####ggplotting####
#save formatting as a theme
mytheme<-theme(axis.text.x =element_text(size=12),
               axis.text.y =element_text(size=12),
               axis.title =element_text(size=16),
               axis.line = element_line(size=1.2),
               axis.ticks = element_line(size=1.2),
               legend.position = "none")

#####functionise plotting#####
ggplotify.gfp<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected RFU") +
    ## geom_dl(aes(label=variable), method="last.points") +
    facet_wrap(~variable, ncol=4) +
    theme_minimal() +
    mytheme +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(limits = c(-2000,44000), breaks = seq(-2000, 44000, by = 10000))}

#plot all in one space
ggplotify.gfp.comb<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected RFU") +
    geom_dl(aes(label=variable), method="last.points") +
   ## facet_wrap(~variable, ncol=4) +
    theme_minimal() +
    mytheme +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(limits = c(-2000,44000), breaks = seq(-2000, 44000, by = 10000))}

ggplotify.gfp.ypdx<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected RFU") +
    ## geom_dl(aes(label=variable), method="last.points") +
    facet_wrap(~variable, ncol=4) +
    theme_minimal() +
    mytheme +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(limits = c(-2000,88000), breaks = seq(-2000, 88000, by = 10000))}

#plot all in one space
ggplotify.gfp.comb.ypdx<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected RFU") +
    geom_dl(aes(label=variable), method="last.points") +
    ## facet_wrap(~variable, ncol=4) +
    theme_minimal() +
    mytheme +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(limits = c(-2000,88000), breaks = seq(-2000, 88000, by = 10000))}


###copy these ggplotify functions for later editions
ggplotify.gfp2<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected RFU") +
    ## geom_dl(aes(label=variable), method="last.points") +
    facet_wrap(~variable, ncol=4) +
    theme_minimal() +
    mytheme +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10))}
 ##   scale_y_continuous(limits = c(-2000,44000), breaks = seq(-2000, 44000, by = 10000))}

ggplotify.gfp2c<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected RFU") +
    geom_dl(aes(label=variable), method="last.points") +
   ## facet_wrap(~variable, ncol=4) +
    theme_minimal() +
    mytheme +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10))}
##   scale_y_continuous(limits = c(-2000,44000), breaks = seq(-2000, 44000, by = 10000))}


#plotting - first two lines are for facet plots. Last two are for combined 'c' plots
ypd1_plot<-ggplotify.gfp(ypd1_melt)
ypd1_plot
ypd1_plot_c<-ggplotify.gfp.comb(ypd1_melt)
ypd1_plot_c

ypdx1_plot<-ggplotify.gfp.ypdx(ypdx1_melt)
ypdx1_plot
ypdx1_plot_c<-ggplotify.gfp.comb.ypdx(ypdx1_melt)
ypdx1_plot_c

sm1_plot<-ggplotify.gfp2(sm1_melt)
sm1_plot
sm1_plotc<-ggplotify.gfp2c(sm1_melt)
sm1_plotc

yepg1_plot<-ggplotify.gfp2(yepg1_melt)
yepg1_plot
yepg1_plotc<-ggplotify.gfp2c(yepg1_melt)
yepg1_plotc

####-----------------------------END OF GFP PLOTTING----------------------------#####



#Determining max expression levels in all
class(tidied)
max_value <- apply(tidied,2,max)
print(max_value)


allmax<-write.csv(max_value, "allmax311020.csv")



####RFU with OD normalisation####
###DID NOT USE BLANK-CORRECTED VALUES###
ods<-read.csv("tidy_ods.csv", header = T, stringsAsFactors = F)
View(ods)

#divide the gfp table with the od table - gfp/OD for normalisation
View(tidied)
norm_gfp <- tidied/ods
View(norm_gfp) #good

#same as above but excluding the time column
norm_gfp2 <- tidied[,2:ncol(tidied)]/ods[,2:ncol(ods)]
norm_gfp3 <- cbind(tidied$Time, norm_gfp2)
colnames(norm_gfp3)[which(names(norm_gfp3) == "tidied$Time")] <- "Time"
View(norm_gfp3) #good


#plot graphs
#extractionof different media conditions#
ypdn<- select(norm_gfp3,1,contains("ypd1"))
ypdxn<- select(norm_gfp3,1,contains("ypdx1"))
smn<- select(norm_gfp3,1,contains("sm1"))
yepgn<- select(norm_gfp3,1,contains("yepg1"))

View(ypdn)
head(melt(ypdn, id.vars = "Time"))


#melt above for ggplot
mnorm_ypd <- melt(ypdn, id.vars = "Time")
mnorm_ypdx <- melt(ypdxn, id.vars = "Time")
mnorm_sm <- melt(smn, id.vars = "Time")
mnorm_yepg <- melt(yepgn, id.vars = "Time")

#test looks weird - v. high gfp at beginning of run and then less gfp until stationary phase - not good
ggplotify.gfp.norm(mnorm_ypd) +  geom_line(color="#939393") + geom_point(color="#b5c6cb")


####Normalisation ggplot themes and functions####
#plot with ggplotify.gfp.norm function
mytheme<-theme(axis.text.x =element_text(size=12,family="Calibri Light"),
               axis.text.y =element_text(size=12, family="Calibri Light"),
               axis.title =element_text(size=14,family="Calibri Light"),
               axis.line = element_line(size=0.9),
               axis.ticks = element_line(size=1),
               legend.position = "none",
               panel.grid.minor.y = element_blank())

ggplotify.gfp.norm<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
  ##  geom_point() + 
  ##  geom_line() +
    xlab("Hours") + 
    ylab("OD-normalised RFU") +
    ## geom_dl(aes(label=variable), method="last.points") +
    facet_wrap(~variable, ncol=4) +
    theme_minimal() +
    mytheme +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10))}
##   scale_y_continuous(limits = c(-2000,44000), breaks = seq(-2000, 44000, by = 10000))}



####Normalisation WITH blank-corrected GFP and OD####
#blank-corrected gfp divided by blank-corrected OD
#blank-corrected gfp = 'media_bc'
#blank-corrected od =  'media_bc_od'

#import blank-corrected ODs from 311020_od.R script
#select removes the first column which has row numbers and removes control wells for flow
ypdx_bcod<-read.csv("311020_ypdXbc_od.csv", header = T, stringsAsFactors = F) %>% select(2:22)
ypd_bcod<-read.csv("311020_ypd1bc_od.csv", header = T, stringsAsFactors = F) %>% select(2:22)
sm_bcod<-read.csv("311020_sm1bc_od.csv", header = T, stringsAsFactors = F) %>% select(2:22)
yepg_bcod<-read.csv("311020_yepg1bc_od.csv", header = T, stringsAsFactors = F) %>% select(2:22)

view(ypdx_bcod)#same size as ypdx1_bc now
view(ypd_bcod) #good

#removing control wells from blank-corrected gfp table
#this is to make blank corrected gfp tables the same size as the blank corrected od tables
ypdx_bcg <- select(ypdx1_bc, 1:21)
ypd_bcg <- select(ypd1_bc, 1:21)
sm_bcg <- select(sm1_bc, 1:21)
yepg_bcg <- select(yepg1_bc, 1:21)

View(ypdx_bcg) #good
view(ypd_bcg) #good


#FOR YPDX -------------
#divide blank corrected gfps by blank-corrected ODs - output = norm2
norm1 <- ypdx_bcg[,2:ncol(ypdx_bcg)]/ypdx_bcod[,2:ncol(ypdx_bcod)]
norm2 <- cbind(ypdx_bcg$Time, norm1)
colnames(norm2)[which(names(norm2) == "ypdx_bcg$Time")] <- "Time"
View(norm2) #good

mbc_ypdx <- melt(norm2, id.vars = "Time")
head(mbc_ypdx)
ggplotify.gfp.norm(mbc_ypdx) +  geom_line(color="#939393") + geom_point(color="#5d8eb1")


#FOR YPD -------------
norm3 <- ypd_bcg[,2:ncol(ypd_bcg)]/ypd_bcod[,2:ncol(ypd_bcod)]
norm4 <- cbind(ypd_bcg$Time, norm3)
colnames(norm4)[which(names(norm4) == "ypd_bcg$Time")] <- "Time"
View(norm4) #good - values checked by hand

mbc_ypd <- melt(norm4, id.vars = "Time")
ggplotify.gfp.norm(mbc_ypd) +  geom_line(color="#939393") + geom_point(color="#b5c6cb")


#FOR SM ---------------
norm5 <- sm_bcg[,2:ncol(sm_bcg)]/sm_bcod[,2:ncol(sm_bcod)]
norm6 <- cbind(sm_bcg$Time, norm5)
colnames(norm6)[which(names(norm6) == "sm_bcg$Time")] <- "Time"

mbc_sm <- melt(norm6, id.vars = "Time")
ggplotify.gfp.norm(mbc_sm) +  geom_line(color="#939393") + geom_point(color="#d4818e")


#FOR YEPG ---------------
View(yepg_bcg)
View(yepg_bcod)
norm7 <- yepg_bcg[,2:ncol(yepg_bcg)]/yepg_bcod[,2:ncol(yepg_bcod)]
norm8 <- cbind(yepg_bcg$Time, norm7)
colnames(norm8)[which(names(norm8) == "yepg_bcg$Time")] <- "Time"

mbc_yepg <- melt(norm8, id.vars = "Time")
ggplotify.gfp.norm(mbc_yepg) +  geom_line(color="#939393") + geom_point(color="#dec193")





####Extracting max expression in all media using blank-corrected normalised values####
max_ypdx <- apply(norm2,2,max)
write.csv(max_ypdx, "ypdx_max311020.csv")

max_ypd <- apply(norm4,2,max)
write.csv(max_ypd, "ypd_max311020.csv")

max_sm <- apply(norm6,2,max)
write.csv(max_sm, "sm_max311020.csv")

max_yepg <- apply(norm8,2,max)
write.csv(max_yepg, "yepg_max311020.csv")


####extract normalised GFPs from timepoints of flow experiments####
#In 311020 run, timepoints were: 
#8.5h for YPD and YPD10 exponential
#11.25h for SM exponential
#26h for YEPG exponential
#55.5 for YEPG stationary

####FOR YPDX####
#Timepoints = 8.5h exp, 26h stat
view(norm2)

#subset by time 8.5h and t26h in the Time column
#=8.5 does not work but ==8.5 does. the = operator is used for assignment, 
#while the == operator is used for comparison. 
#You should use = when you want to assign a value to a variable or named argument, 
#and use == when you want to compare the equality of two values
ypdx_filt <- norm2[norm2$Time==8.5|norm2$Time==26.0, ] #gets both 8.5 and 26h timepoints 
View(ypdx_filt) #works
#- I only want 1 so I can combine the other replicates of each growth phase together later

ypdx_exp <- norm2[norm2$Time==8.5,] 
View(ypdx_exp) #good

ypdx_sta <- norm2[norm2$Time==26.0,] 
ypdx_sta

#write
write.csv(ypdx_exp, "311020_ypdx_expgfp.csv")
write.csv(ypdx_sta, "311020_ypdx_stagfp.csv")

####FOR YPD####
ypd_exp <- norm4[norm4$Time==8.5,] 
ypd_exp
ypd_sta <- norm4[norm4$Time==26.0,]
ypd_sta

write.csv(ypd_exp, "311020_ypd_expgfp.csv")
write.csv(ypd_sta, "311020_ypd_stagfp.csv")

####FOR SM####
sm_exp <- norm6[norm6$Time==11.25,]
sm_exp
sm_sta <- norm6[norm6$Time==26.0,]
sm_sta

write.csv(sm_exp, "311020_sm_expgfp.csv")
write.csv(sm_sta, "311020_sm_stagfp.csv")

####FOR YEPG####
yepg_exp <- norm8[norm8$Time==26.0,]
yepg_exp
yepg_sta <- norm8[norm8$Time==55.5,]
yepg_sta

write.csv(yepg_exp, "311020_yepg_expgfp.csv")
write.csv(yepg_sta, "311020_yepg_stagfp.csv")


