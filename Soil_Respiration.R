######################################################
###Biotron Root physiology experiment
##Kayla C. Mathes
###Start Data: October 19th, 2021
#######################################################

####Soil Respiration Data

#Load Library
library(ggplot2)
library(dplyr)
library(googledrive)
library(plotrix) ##For std.error()
library(rstatix) ##For identify_outliers()
library(car)  ##For levene_test()
library(ggpubr) ##for ggqqplot()

##Load Data
# Direct Google Drive link to "FoRTE/data/soil_respiration/2021"
as_id("https://drive.google.com/drive/folders/18o52piFmoZyrX0nV-jxhqp1sFTx8k24n") %>% 
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "googledrive_data/"
if(!dir.exists(data_dir)) dir.create(data_dir)

#Download date
for(f in seq_len(nrow(gdfiles))) {
  cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
  drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
}

## Import downloaded date from new data directory "googledrive_data"
Rs_2021 <- read.csv("googledrive_data/Rs_2021.csv", na.strings = c("NA", "na"))

Rs_summary <- Rs_2021%>%
  group_by(Plot)%>%
  summarize(ave_Rs = mean(soilCO2Efflux), se_efflux = std.error(soilCO2Efflux), ave_temp = mean(soilTemp), se_efflux_temp = std.error(soilTemp),ave_VWC = mean(VWC), se_efflux_VWC = std.error(VWC))

##Run Stats for mean difference between control/treatment Rs
##Transform variables into factors for model 
Rs_2021$Plot <- as.factor(Rs_2021$Plot)


####Testing Assumptions 
##Test for outliers test: no extreme outliers for Rs, temp and VWC
Rs_2021 %>% 
  group_by(Plot) %>%
  identify_outliers(VWC)

##Equality of variance test: All variances are equal (Rs, temp, VWC)
leveneTest(soilCO2Efflux ~ Plot, data = Rs_2021)
leveneTest(soilTemp ~ Plot, data = Rs_2021)
leveneTest(VWC ~ Plot, data = Rs_2021)

##Normality (Data are normal for Rs and temperature, data are not normal at 0.05 for VWC
# Build the linear model
normality_test  <- lm(soilCO2Efflux ~ Plot,
                      data = Rs_2021)

# Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
# Shapiro test of normality 
shapiro_test(residuals(normality_test))

##T-test for normal data 
t.test(soilCO2Efflux~Plot, data = Rs_2021)
t.test(soilTemp~Plot, data = Rs_2021)
kruskal.test(VWC ~ Plot, data = Rs_2021) 

ggplot(Rs_2021,aes(x = Plot, y = soilCO2Efflux, fill = Plot)) +
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 20), axis.text.y= element_text(size=20), axis.title.x = element_text(size = 25), axis.title.y  = element_text(size=25), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none") +
  labs(x = "Plot", y=expression(paste("Rs (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) 

ggplot(Rs_2021,aes(x = Plot, y = soilTemp, fill = Plot)) +
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 20), axis.text.y= element_text(size=20), axis.title.x = element_text(size = 25), axis.title.y  = element_text(size=25), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none") +
  labs(x = "Plot", y=expression(paste("Soil T (  ",C," )"))) 

ggplot(Rs_2021,aes(x = Plot, y = VWC, fill = Plot)) +
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 20), axis.text.y= element_text(size=20), axis.title.x = element_text(size = 25), axis.title.y  = element_text(size=25), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none") +
  labs(x = "Plot", y=expression(paste("Soil VWC"))) 



