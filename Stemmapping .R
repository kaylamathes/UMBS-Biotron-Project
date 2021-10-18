######################################################
###Biotron Root physiology experiment
##Kayla C. Mathes
###Start Data: July 28th, 2021
#######################################################

####Stem Mapping data

#Load Library
library(ggplot2)
library(dplyr)
library(googledrive)

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
stem_map <- read.csv("googledrive_data/Biotron_stemmap_7_27_21.csv", na.strings = c("NA", "na"))


##Plot Species Distribution (Count): Canopy and subcanopy combined
ggplot(stem_map, aes(Tree_Spc, fill = Plot_ID)) +
  geom_bar(position="dodge", stat="count") +
 theme_classic() +
  xlab("Tree Species") +ylab("Stem Count")

##plot by canopy position 

stem_map <- stem_map%>%
  mutate(Class = case_when (Tree_Dia > 80 ~ "Canopy", 
                            Tree_Dia <= 80 ~ "Subcanopy"))%>%
  mutate(fate = case_when(Class == "Canopy" & Plot_ID == "Treatment" ~ "Kill", TRUE ~ "live"))


##Plot Species Distribution (Count): Canopy and subcanopy faceted 
ggplot(stem_map, aes(Tree_Spc, fill = Plot_ID)) +
  geom_bar(position="dodge", stat="count") +
  theme_classic() +
  xlab("Tree Species") +ylab("Stem Count") +
  facet_wrap(~Class)

##Calculating aboveground biomass 


biomass_set_up <- stem_map%>%
  mutate(DBH_cm = Tree_Dia/10)%>%
  mutate(a = case_when(Tree_Spc == "ACRU" | Tree_Spc == "AMEL" |Tree_Spc == "PRSE" ~ 0.03117, 
                        Tree_Spc == "FAGR" ~  0.1892, 
                        Tree_Spc == "PIRE" ~ 0.0526,
                        Tree_Spc == "PIST" ~ 0.0408, 
                        Tree_Spc == "POGR" ~ 0.1387, 
                        Tree_Spc == "QURU" ~  0.0398 ))%>%
  mutate(b = case_when(Tree_Spc == "ACRU" | Tree_Spc == "AMEL" |Tree_Spc == "PRSE" ~ 2.778, 
                       Tree_Spc == "FAGR" ~  2.3097, 
                       Tree_Spc == "PIRE" ~ 2.5258,
                       Tree_Spc == "PIST" ~ 2.5735, 
                       Tree_Spc == "POGR" ~ 2.3498, 
                       Tree_Spc == "QURU" ~  2.7734))%>%
  mutate(biomass_kg = a*DBH_cm^b)

total_biomass_calculate <- biomass_set_up%>%
  group_by(Plot_ID, Tree_Spc)%>%
  summarize(total_biomass_kg_100m2 = sum(biomass_kg))%>%
  mutate(total_biomass_Mgha = total_biomass_kg_100m2/0.01*0.001)

subcanopy_biomass_calculate <- biomass_set_up%>%
  filter(DBH_cm <= 8)%>%
  group_by(Plot_ID, Tree_Spc)%>%
  summarize(total_biomass_kg_100m2 = sum(biomass_kg))%>%
  mutate(total_biomass_Mgha = total_biomass_kg_100m2/0.01*0.001)

canopy_biomass_calculate <- biomass_set_up%>%
  filter(DBH_cm > 8)%>%
  group_by(Plot_ID, Tree_Spc)%>%
  summarize(total_biomass_kg_100m2 = sum(biomass_kg))%>%
  mutate(total_biomass_Mgha = total_biomass_kg_100m2/0.01*0.001)



###Stem map 

ggplot(stem_map, aes(x = Longitude, y = Latitude)) +
  geom_point(aes(size = Tree_Dia, color = Plot_ID, shape = fate), alpha = 0.8, show.legend = FALSE) +
  theme_classic() +
  scale_colour_manual(values = c("#00CED1", "#D2691E")) +
  scale_shape_manual(values = c(17,16)) +
  scale_size_continuous(range = c(1, 30)) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))

ggsave("stem_map.png",height = 10, width = 10, units = "in")
  

  
  

  
  

