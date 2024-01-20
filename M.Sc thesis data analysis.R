setwd("C:/Users/CP24/Nextcloud2/Thesis/Analysis/Functional traits analysis/Updated data arrangement - Liana")
#Data inspection and cleaning#===============================================
functions_used{
#to inspect data/objects = str(), class(), dim(), summary(), View(), names(), head(), tail()
#to write new excel files = write_xlsx()
}
install.packages('readxl')
install.packages("writexl")
library("readxl")
library("writexl")
library(tidyverse)

1 #Objective 1: Calculating Taxonomic diversity (Shannon-Weiner, Species evenness and Species richness)#=============================================== 
functions_used{
#nrow, ncol, colsums, rowsums, diversity, specnumber, word, mutate, distinct, TPL, filter, select, group_by, summarize, colnames, names
}
install.packages("Taxonstand") #species name correction based on the plant list database
library(vegan)
library(Taxonstand)
  shannon.data <- read_excel('Updated_datasheet_04 Aug.xlsx', sheet = 2)
  nrow(shannon.data)
  ncol(shannon.data)
  mydata <- as.data.frame(shannon.data)
  str(mydata)
  summary(mydata)
  colSums(is.na(mydata) | mydata == "")
  rowSums(is.na(mydata) | mydata == "")
  # data2 <- na.omit(mydata[1:6]) #removing na's assuming there were.
  
 
div_matrix <- as.data.frame(mydata$Plot)
mydata <- tibble::column_to_rownames(mydata, var = "Plot code") # code to move first column 
?diversity
div_matrix$R  <- specnumber(mydata) # species richness
div_matrix$H <- diversity(mydata,index = "shannon") #shannon
div_matrix$J <- div_matrix$H/log(div_matrix$R) # evenness

write_xlsx(div_matrix, "C:/Users/CP24/Nextcloud2/Thesis/Analysis/Functional traits analysis/Updated data arrangement - Liana/Diversity_matrix with Biomass.xlsx")

##Preparing_dataset_for_plotting #====
install.packages("plotrix")
install.packages("psych")
library(plotrix) #for function std.error
library("ggplot2")
library(FSA)
library(psych)
library(coin)
library(dplyr)
data <- read_excel('Functional traits analysis datasheet.xlsx', sheet = 1)

View(data)
class(data)
dim(data)
names(data)
names(data)

str(data)
summary(data)
data  <- as.data.frame(data)
data$Climate<- as.factor(data$Climate)
data$FAO_classification <- as.factor(data$FAO_classification)
data$Site<- as.factor(data$Site)
data$`Land_use_type` <- factor(data$`Land_use_type`,
                               levels = c("Forest", "Fallow", "Cropland"))
data$Plot<- as.factor(data$Plot)
data$Plot_number<- as.factor(data$Plot_number)
data$Species_richness<- round(
  as.numeric(data$Species_richness), 1)
data$Pielou_evenness<- round(
  as.numeric(data$Pielou_evenness), 1)
data$Shannon_diversity_index<- round(
  as.numeric(data$Shannon_diversity_index), 1)

colSums(is.na(data) | data == "") #gives a value for the number of NA's in the row/columns
rowSums(is.na(data) | data == "")

?interaction
data$FAO_classification_LUT <- interaction(data$Land_use_type, data$FAO_classification, sep=":")
str(data)
levels(data$FAO_classification_LUT)
data$FAO_classification_LUT1 <- factor(data$FAO_classification_LUT,
                                       levels = c("Forest:Humid", "Forest:Semi-arid", "Fallow:Humid", 
                                                  "Fallow:Semi-arid", "Cropland:Humid", "Cropland:Semi-arid"))


a = data %>%
  group_by(FAO_classification,Land_use_type) %>%
  summarise(
    mean.shannon = mean(Shannon_diversity_index, na.rm = TRUE),
    mean.SR = mean(Species_richness, na.rm = TRUE),
    mean.PE = mean (Pielou_evenness, na.rm = TRUE))
write_xlsx(a, "C:/Users/CP24/Nextcloud2/Thesis/Analysis/Functional traits analysis/Updated data arrangement - Liana/Mean_Diversity_indices.xlsx")

##mmmm####
par(mfrow=c(2,2))

ggplot(data, aes(x= Land_use_type, y=Species_richness)) +
  geom_boxplot(aes(fill=FAO_classification_LUT)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + #code to rotate the x axis labels
  facet_grid(~FAO_classification) +
  geom_text(data=cld_SR_new, aes(label=group,
                                 y=max(data$Species_richness)*1.1, x=Land_use_type, angle =0), size = 7,
            inherit.aes = F) +
  scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
  ylab("Species richness") + 
  xlab("Land-use intensity")+
  labs(fill = "Land-use:Aridity") +
  theme_bw() +
  #theme_minimal() +  # Use a minimal theme as a starting point
  theme(
    text = element_text(family = "Arial", size = 14),  # Set font family and size
    legend.title = element_text(size = 12),  # Adjust legend title size
    #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # Rotate x-axis text
    #panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    #panel.grid.minor = element_blank(),      # Remove minor grid lines
   # panel.background = element_blank(),  # Remove plot background
    #panel.border = element_blank()    # Remove plot border
   )

ggplot(data, aes(x= Land_use_type, y=Pielou_evenness)) +
  geom_boxplot(aes(fill=FAO_classification_LUT)) +
  facet_grid(~FAO_classification) +
  geom_text(data=cld_PE_new, aes(label=group,
                                 y=max(data$Pielou_evenness)*1.1, x=Land_use_type, angle =0), size = 7,
            inherit.aes = F) +
  scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
  ylab("Pielou's evenness") + 
  xlab("Land-use intensity")+
  theme_bw() +
  labs(fill = "Land-use:Aridity")

ggplot(data, aes(x= Land_use_type, y=Shannon_diversity_index)) +
  geom_boxplot(aes(fill=FAO_classification_LUT)) +
  facet_grid(~FAO_classification) +
  geom_text(data=cld_new, aes(label=group,
                              y=max(data$Shannon_diversity_index)*1.1, x=Land_use_type, angle =0), size = 7,
            inherit.aes = F) +
  scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
  ylab("Shannon's diversity") + 
  xlab("Land-use intensity") +
  theme_bw() +
  labs(fill = "Land-use:Aridity")



1.1##Species richness#====
Preliminary_statistics{
aov4 <- aov(Species_richness ~ Land_use_type, data)
summary(aov4)
TukeyHSD(aov4)

aov5 <- aov(Species_richness ~ FAO_classification, data)
summary(aov5)
TukeyHSD(aov5)

aov6 <- aov(Species_richness ~ Land_use_type*FAO_classification, data)
summary(aov6)
TukeyHSD(aov6)

#PostHOC 

dunnTest(Species_richness ~ Land_use_type, data=data.dry, method="bh")
dunnTest(Species_richness ~ Land_use_type, data=data.wet, method="bh")
wilcox_test (Species_richness ~ FAO_classification, data=cropland, distribution = "exact")
wilcox_test (Species_richness ~ FAO_classification, data=fallow, distribution = "exact")
wilcox_test (Species_richness ~ FAO_classification, data=forest, distribution = "exact")

#Preliminary visualization

boxplot(Species_richness ~ FAO_classification, data)
boxplot(Species_richness ~ Land_use_type, data)
boxplot(Species_richness ~ Land_use_type+FAO_classification, data)

spec_richness.plot <- ggplot(data, aes(x= Land_use_type, y=Species_richness, shape=FAO_classification, colour=FAO_classification)) +
  geom_boxplot(aes(fill=Land_use_type)) +
  geom_point(size=3, aes())+
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  ylab("Species richness") + 
  xlab("Land-use intensity")+
  theme_bw()
spec_richness.plot
}
####Hypothesis 1a####
#using_ANOVA_as_the_statistical_test_and_Tukeys_Post-HOC
#Interaction
  aov6 <- aov(Species_richness ~ Land_use_type*FAO_classification, data)
  summary(aov6) # interaction of land_use & aridity class (FAO_classification) is highly significant
  aov6_emm_new <- emmeans(object = aov6, pairwise ~ Land_use_type*FAO_classification)
  aov6_emm_new

  cld_SR_new <- cld(object = aov6_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
  cld_SR_new$group <- str_trim(cld_SR_new$.group)

#Visualizations - Faceted based on Aridity classes 
  plot.richness1a <- ggplot(data, aes(x= Land_use_type, y=Species_richness)) +
    geom_boxplot(aes(fill=FAO_classification_LUT)) +
    facet_grid(~FAO_classification) +
    geom_text(data=cld_SR_new, aes(label=group,
                                y=max(data$Species_richness)*1.1, x=Land_use_type, angle =0), size = 7,
              inherit.aes = F) +
    scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
    ylab("Species richness") + 
    xlab("Land-use intensity")+
    labs(fill = "Land-use:Aridity") +
    theme_bw() +
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12))
  
#Result
#1a.land-use effect
# In Humid zone: differences existing between; forests and fallows, forests and croplands, and between fallows and croplands
# In Semi-arid zone: differences between forests and croplands, fallow and croplands. no differences in forests and fallows
#write percentages
  
####Hypothesis 1b and 1c####
#using_ANOVA_as_the_statistical_test_and_Tukeys_Post-HOC
    
    cld_SR1b_emm_new <- emmeans(object = aov6, pairwise ~ FAO_classification * Land_use_type)
    cld_SR1b_emm_new

    cld_new_SR1b <- cld(object = cld_SR1b_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
    cld_new_SR1b$group <- str_trim(cld_new_SR1b$.group)
    
#Visualizations - Faceted based on Land-use 
  plot.richness1b.1c <- ggplot(data, aes(x= FAO_classification, y=Species_richness)) +
      geom_boxplot(aes(fill=FAO_classification_LUT1)) +
      facet_grid(~Land_use_type) +
      geom_text(data=cld_new_SR1b, aes(label=group,
                                     y=max(data$Species_richness)*1.1, x=FAO_classification, angle =0), size = 7,
                inherit.aes = F) +
      scale_fill_manual(values = c("darkgreen", "green3", "steelblue4", "steelblue1", "tan4", "sandybrown"))+
      ylab("Species richness") + 
      xlab("Aridity class") +
      labs(fill = "Land-use:Aridity") +
      theme_bw() +
      theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12))
    
#Result
#1b.climate effect
  # In Forests: climate has an effect (differences existing between humid and semi-arid)
  # In fallows: climate has an effect (differences existing between humid and semi-arid)
  # In Croplands: climate has no effect (no differences existing between humid and semi-arid)
  #1c.interactive effect
  #Negative effects of increasing climatic aridity is more pronounced under intensive land-use. cropland in the semi-arid had the lowest species richness and differed significantly from fallow in the semi-arid or fallow in humid
  
####Visualizations for Appendix#### 
1-General_effect_across_Land_use_from_both_Aridity_classes{
#using_ANOVA_as_the_statistical_test_and_Tukey's_Post-HOC
#Land use effect(general)
    aov4 <- aov(Species_richness ~ Land_use_type, data)
    summary(aov4) # effect of land_use is highly significant
    cldLUT_SR_emm_new <- emmeans(object = aov4, pairwise ~ Land_use_type)
    cldLUT_SR_emm_new
        
    cld_new_LUT_SR <- cld(object = cldLUT_SR_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
    cld_new_LUT_SR$group <- str_trim(cld_new_LUT_SR$.group)
        
#Visualizations 
    ggplot(data, aes(x= Land_use_type, y=Species_richness)) +
    geom_boxplot(aes(fill=Land_use_type)) +
    geom_text(data=cld_new_LUT_SR, aes(label=group,
                                          y=max(data$Species_richness)*1.1, x=Land_use_type, angle =0), size = 7,
                    inherit.aes = F) +
      scale_fill_manual(values = c("olivedrab", "dodgerblue", "goldenrod"))+
      ylab("Species richness") + 
      xlab("Land-use intensity")+
      theme_bw() +
      theme(
        text = element_text(family = "Arial", size = 12),  # code to set font family and size
        legend.title = element_text(size = 12)
        )  

#Result
#Land-use has an effect on Species richness (differences existing between forests, fallows and croplands across both humid and semi-arid)
#Increasing land-use intensity negatively influences Species richness
}
2-General_effect_across_Aridity_classes_from_all_Land_use_types{
#using_ANOVA_as_the_statistical_test_and_Tukey's_Post-HOC
#Climate effect(general)
    aov5 <- aov(Species_richness ~ FAO_classification, data)
    summary(aov5) # effect of aridity is highly significant
    cldARID_SR_emm_new <- emmeans(object = aov5, pairwise ~ FAO_classification)
    cldARID_SR_emm_new
        
    cld_new_ARID_SR <- cld(object = cldARID_SR_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
    cld_new_ARID_SR$group <- str_trim(cld_new_ARID_SR$.group)
        
#Visualizations 
    ggplot(data, aes(x= FAO_classification, y=Species_richness)) +
    geom_boxplot(aes(fill=FAO_classification)) +
    geom_text(data=cld_new_ARID_SR, aes(label=group,
              y=max(data$Species_richness)*1.1, x=FAO_classification, angle =0), size = 7,
              inherit.aes = F) +
      scale_fill_manual(values = c("midnightblue", "orangered"))+
      ylab("Species richness") + 
      xlab("Aridity class")+
      labs(fill = "Aridity Class") +
      theme_bw() +
      theme(
        text = element_text(family = "Arial", size = 12),  # code to set font family and size
        legend.title = element_text(size = 12)
      )  

#Result
#Climate has an effect on Species richness (differences existing between Humid and Semi-arid across all Land-use types)
#Increasing arid conditions negatively influences Species richness. In general, if the region becomes or tilts towards more semi-arid conditions, we are likely to lose species richness
}
3-Plotting_Mean_SE_SD{
  data.summary.richness <- data %>%
  group_by(FAO_classification, Land_use_type) %>%
        summarise(
          sd = sd(Species_richness, na.rm = TRUE),
          len = mean(Species_richness, na.rm = TRUE),
          se=std.error(Species_richness, na.rm = TRUE))
  
    f1.richness <- ggplot(data.summary.richness, aes(x=Land_use_type, y=len, ymin=len-se, ymax=len+se, col=FAO_classification, shape= FAO_classification)
      )+facet_grid(~.)
    f1.richness + geom_pointrange(position = dodge) +
    geom_errorbar(aes(ymin = len-se , ymax = len+se), position = dodge, width = 0.25)+
    theme_bw() +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab("Species richness")+
    xlab("Land-use intensity")+
    theme(legend.key.size = unit(1, 'cm'))+
    scale_color_manual(
    values = c( "midnightblue", "orangered"))+
    labs(fill = "Aridity Class") +
    theme(
        text = element_text(family = "Arial", size = 12),  # code to set font family and size
        legend.title = element_text(size = 12)
      )  
}

1.2##Pielou's evenness#====
Preliminary_statistics{
  
aov7 <- aov(Pielou_evenness ~ Land_use_type, data)
summary(aov7)
TukeyHSD(aov7)

aov8 <- aov(Pielou_evenness ~ FAO_classification, data)
summary(aov8)
TukeyHSD(aov8)

aov9 <- aov(Pielou_evenness ~ Land_use_type*FAO_classification, data)
summary(aov9)
TukeyHSD(aov9)

#PostHOC 

kruskal.test(Pielou_evenness ~ Land_use_type, data)
kruskal.test(Pielou_evenness ~ FAO_classification, data)

dunnTest(Pielou_evenness ~ Land_use_type, data=data.dry, method="bh")
dunnTest(Pielou_evenness ~ Land_use_type, data=data.wet.1, method="bh")

wilcox_test (Pielou_evenness ~ FAO_classification, data=cropland, distribution = "exact")
wilcox_test (Pielou_evenness ~ FAO_classification, data=fallow, distribution = "exact")
wilcox_test (Pielou_evenness ~ FAO_classification, data=forest, distribution = "exact")

#Preliminary visualization

boxplot(Pielou_evenness ~ FAO_classification, data)
boxplot(Pielou_evenness ~ Land_use_type, data)
boxplot(Pielou_evenness ~ Land_use_type+FAO_classification, data)

Piel_evenness.plot <- ggplot(data, aes(x= Land_use_type, y=Pielou_evenness, shape=FAO_classification, colour=FAO_classification)) +
  geom_boxplot(aes(fill=Land_use_type)) +
  geom_point(size=3, aes())+
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  ylab("Pielou's evenness") + 
  xlab("Land-use intensity")+
  theme_bw()
Piel_evenness.plot
}
####Hypothesis 1a####
#using_ANOVA_as_the_statistical_test_and_Tukeys_Post-HOC
#Interaction
aov9 <- aov(Pielou_evenness ~ Land_use_type*FAO_classification, data)
summary(aov9) # interaction of land_use & aridity class (FAO_classification) is highly significant
aov9_emm_new <- emmeans(object = aov9, pairwise ~ Land_use_type*FAO_classification)
aov9_emm_new

cld_PE_new <- cld(object = aov9_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
cld_PE_new$group <- str_trim(cld_PE_new$.group)

#Visualizations - Faceted based on Aridity classes 
plot.evenness1a <- ggplot(data, aes(x= Land_use_type, y=Pielou_evenness)) +
  geom_boxplot(aes(fill=FAO_classification_LUT)) +
  facet_grid(~FAO_classification) +
  geom_text(data=cld_PE_new, aes(label=group,
                                 y=max(data$Pielou_evenness)*1.1, x=Land_use_type, angle =0), size = 7,
            inherit.aes = F) +
  scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
  ylab("Pielou's evenness") + 
  xlab("Land-use intensity")+
  labs(fill = "Land-use:Aridity") +
  theme_bw()

#Result
#1a.land-use effect
# In Humid zone: no differences existing between; forests and fallows, forests and croplands, and between fallows and croplands
# In Semi-arid zone: no differences existing between forests and croplands, fallow and croplands. no differences in forests and fallows
#write percentages

####Hypothesis 1b and 1c####
#using_ANOVA_as_the_statistical_test_and_Tukeys_Post-HOC

cld_PE1b_emm_new <- emmeans(object = aov9, pairwise ~ FAO_classification * Land_use_type)
cld_PE1b_emm_new

cld_new_PE1b <- cld(object = cld_PE1b_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
cld_new_PE1b$group <- str_trim(cld_new_PE1b$.group)

#Visualizations - Faceted based on Land-use 
plot.evenness1b.1c <- ggplot(data, aes(x= FAO_classification, y=Pielou_evenness)) +
  geom_boxplot(aes(fill=FAO_classification_LUT1)) +
  facet_grid(~Land_use_type) +
  geom_text(data=cld_new_PE1b, aes(label=group,
                                   y=max(data$Pielou_evenness)*1.1, x=FAO_classification, angle =0), size = 7,
            inherit.aes = F) +
  scale_fill_manual(values = c("darkgreen", "green3", "steelblue4", "steelblue1", "tan4", "sandybrown"))+
  ylab("Pielou's evenness") + 
  xlab("Aridity class")+
  labs(fill = "Land-use:Aridity") +
  theme_bw()


#Result
#1b.climate effect
# In Forests: climate has no effect (no differences existing between humid and semi-arid)
# In fallows: climate has no effect (no differences existing between humid and semi-arid)
# In Croplands: climate has no effect (no differences existing between humid and semi-arid)
#1c.interactive effect
#Negative effects of increasing climatic aridity is not more pronounced under intensive land-use. 

####Visualizations for Appendix#### 
1-General_effect_across_Land_use_from_both_Aridity_classes{
#using_ANOVA_as_the_statistical_test_and_Tukey's_Post-HOC
#Land use effect(general)
  aov7 <- aov(Pielou_evenness ~ Land_use_type, data)
  summary(aov7) # effect of land_use is NOT significant
  cldLUT_PE_emm_new <- emmeans(object = aov7, pairwise ~ Land_use_type)
  cldLUT_PE_emm_new
  
  cld_new_LUT_PE <- cld(object = cldLUT_PE_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
  cld_new_LUT_PE$group <- str_trim(cld_new_LUT_PE$.group)
  
#Visualizations 
  ggplot(data, aes(x= Land_use_type, y=Pielou_evenness)) +
    geom_boxplot(aes(fill=Land_use_type)) +
    geom_text(data=cld_new_LUT_PE, aes(label=group,
                                       y=max(data$Pielou_evenness)*1.1, x=Land_use_type, angle =0), size = 7,
              inherit.aes = F) +
    scale_fill_manual(values = c("olivedrab", "dodgerblue", "goldenrod"))+
    ylab("Pielou's evenness") + 
    xlab("Land-use intensity")+
    theme_bw() +
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12)
    )  
  

#t-test
  ggplot(data, aes(x= Land_use_type, y=Pielou_evenness)) +
    geom_boxplot(aes(fill=Land_use_type)) +
    stat_compare_means(method = "t.test", comparisons = my_comparisons2, label = "p.signif") +
    scale_fill_manual(values = c("olivedrab", "dodgerblue", "goldenrod"))+
    ylab("Pielou's evenness") + 
    xlab("Land-use intensity")+
    theme_bw()
  
#Result
#Land-use has no effect on Species richness (no differences existing between forests, fallows and croplands across both humid and semi-arid)
#Increasing land-use intensity does not negatively influence Species´evenness
}
2-General_effect_across_Aridity_classes_from_all_Land_use_types{
#using_ANOVA_as_the_statistical_test_and_Tukey's_Post-HOC
#Climate effect(general)
  aov8 <- aov(Pielou_evenness ~ FAO_classification, data)
  summary(aov8) # effect of aridity is not significant
  cldARID_PE_emm_new <- emmeans(object = aov8, pairwise ~ FAO_classification)
  cldARID_PE_emm_new
  
  cld_new_ARID_PE <- cld(object = cldARID_PE_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
  cld_new_ARID_PE$group <- str_trim(cld_new_ARID_PE$.group)
  
#Visualizations 
  ggplot(data, aes(x= FAO_classification, y=Pielou_evenness)) +
    geom_boxplot(aes(fill=FAO_classification)) +
    geom_text(data=cld_new_ARID_PE, aes(label=group,
                                        y=max(data$Pielou_evenness)*1.1, x=FAO_classification, angle =0), size = 7,
              inherit.aes = F) +
    scale_fill_manual(values = c("midnightblue", "orangered"))+
    ylab("Pielou's evenness") + 
    xlab("Aridity class")+
  labs(fill = "Aridity Class") +
    theme_bw() +
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12)
    )  
  
#using t-test
  ggplot(data, aes(x= FAO_classification, y=Pielou_evenness)) +
    geom_boxplot(aes(fill=FAO_classification)) +
    stat_compare_means(method = "t.test", comparisons = my_comparisons4, label = "p.signif") +
    scale_fill_manual(values = c("midnightblue", "orangered"))+
    ylab("Pielou's evenness") + 
    xlab("Aridity class")+
    theme_bw()
#Result
#Climate has no effect on Species evenness (no differences existing between Humid and Semi-arid across all Land-use types)
#Increasing arid conditions does not negatively influences Species evenness. 
}
3-Plotting_Mean_SE_SD{
  data.summary.evenness <- data %>%
    group_by(FAO_classification, Land_use_type) %>%
    summarise(
      sd = sd(Pielou_evenness, na.rm = TRUE),
      len = mean(Pielou_evenness, na.rm = TRUE),
      se=std.error(Pielou_evenness, na.rm = TRUE))
  
  f1.evenness <- ggplot(data.summary.evenness, aes(x=Land_use_type, y=len, ymin=len-se, ymax=len+se, col=FAO_classification, shape= FAO_classification)
  )+facet_grid(~.)
  f1.evenness + geom_pointrange(position = dodge) +
    geom_errorbar(aes(ymin = len-se , ymax = len+se), position = dodge, width = 0.25)+
    theme_bw() +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab("Pielou's evenness")+
    xlab("Land-use intensity")+
    theme(legend.key.size = unit(1, 'cm'))+
    scale_color_manual(
      values = c( "midnightblue", "orangered")) +
  labs(fill = "Aridity Class") +
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12)
    )  
}

1.3##Shannon Weiner #====
Preliminary_statistics{
  aov1 <- aov(Shannon_diversity_index ~ Land_use_type, data)
  summary(aov1)
  TukeyHSD(aov1)
  #data %>% ungroup()%>%aov(Shannon_diversity_index ~ Land_use_type,.) %>% TukeyHSD()
  
  aov2 <- aov(Shannon_diversity_index ~ FAO_classification, data)
  summary(aov2)
  TukeyHSD(aov2)
  
  aov3 <- aov(Shannon_diversity_index ~ Land_use_type*FAO_classification, data)
  summary(aov3)
  TukeyHSD(aov3)
  
  #PostHOC 
  kruskal.test(Shannon_diversity_index ~ Land_use_type, data)
  data.wet <- filter(data, FAO_classification == "Humid") #mann-whitney test for two levels
  data.dry <- filter(data, FAO_classification == "Semi-arid") 
  
  dunnTest(Shannon_diversity_index ~ Land_use_type, data=data.dry, method="bh")
  dunnTest(Shannon_diversity_index ~ Land_use_type, data=data.wet, method="bh")
  
  cropland <- filter(data, Land_use_type == "Cropland")
  forest <- filter(data, Land_use_type == "Forest")
  fallow <- filter(data, Land_use_type == "Fallow")
  wilcox_test (Shannon_diversity_index ~ FAO_classification, data=cropland, distribution = "exact")
  wilcox_test (Shannon_diversity_index ~ FAO_classification, data=fallow, distribution = "exact")
  wilcox_test (Shannon_diversity_index ~ FAO_classification, data=forest, distribution = "exact")
  
  #Preliminary visualization
  boxplot(Shannon_diversity_index ~ FAO_classification, data)
  boxplot(Shannon_diversity_index ~ Land_use_type, data)
  boxplot(Shannon_diversity_index ~ Land_use_type + FAO_classification, data)
  #* means an interactive effect of 2 predictor factors while (+) produces separate results for each predictor variable
  
  ggplot(data, aes(x= Land_use_type, y=Shannon_diversity_index, shape=FAO_classification, colour=FAO_classification))+
    geom_boxplot(aes(fill=Land_use_type)) +
    geom_point(size=3, aes())+
    scale_colour_manual(values = c("darkgreen", "lightgreen", "orange"))+
    scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
    ylab("Shannon's diversity") + 
    xlab("Land-use intensity")+
    theme_bw()
  #add line (fill=Land_use_type) if you want the boxplot to be filled
  #add line [scale_fill_manual(values = c("grey1", "grey60", "grey90"), guide = FALSE)+] if you want to create a colour gradient (dark-gray) in the land-use types. disadvantage is that it removes the L.U.T legend
  
}
####Hypothesis 1a####
using_ANOVA_as_the_statistical_test_and_Tukeys_Post-HOC{
  ??str_trim
  install.packages("stringr")
  library(stringr)
  library(emmeans)
  library(multcompView)
  library(multcomp)
  
  #Interaction
  aov3 <- aov(Shannon_diversity_index ~ Land_use_type*FAO_classification, data)
  summary(aov3) # interaction of land_use & aridity class (FAO_classification) is highly significant
  aov3_emm_new <- emmeans(object = aov3, pairwise ~ Land_use_type | FAO_classification)
  # read the above as "doing pairwise comparisons, where Land_use_type are compared to each other, conditioned on aridity classes" or "nested within aridity classes"
  aov3_emm_new
  #Land_use_type are now compared separately, within Humid, and within Semi-arid, but not across (unless you change emmeans to "Land_use_type*FAO_classification")
  aov3_emm_new_1 <- emmeans(object = aov3, pairwise ~ Land_use_type*FAO_classification)
  # read the above as "doing pairwise comparisons, where Land_use_type are compared to each other, even across aridity classes" i.e. they are not "nested within aridity classes"
  aov3_emm_new_1
  #Land_use_type are now compared jointly and across, both Humid, and Semi-arid classification")
  
  cld_new <- cld(object = aov3_emm_new_1, adjust = "Tukey", Letters = letters, alpha = 0.05)
  cld_new$group <- str_trim(cld_new$.group)
  #csd = compact letter display
  
  #Visualizations - Faceted based on Aridity classes 
  plot.shannon1a <- ggplot(data, aes(x= Land_use_type, y=Shannon_diversity_index)) +
    geom_boxplot(aes(fill=FAO_classification_LUT)) +
    facet_grid(~FAO_classification) +
    geom_text(data=cld_new, aes(label=group,
                                y=max(data$Shannon_diversity_index)*1.1, x=Land_use_type, angle =0), size = 7,
              inherit.aes = F) +
    scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
    ylab("Shannon's diversity") + 
    xlab("Land-use intensity")+
    labs(fill = "Land-use:Aridity") +
    theme_bw()
}
using_t.tests_as_the_statistical_test{
  install.packages("ggpubr")
  library(ggpubr)   # for the function 'stat_compare_means()' I want to demonstrate (extends ggplot2)
  ?stat_compare_means # This function extends ggplot2 for adding mean comparison p-values to a ggplot, such as box plots, dot plots, bar plots etc
  #compare_means(Shannon_diversity_index ~ Land_use_type, data = data) did not work
  # function[stat_compare_means]: 
  # to do the comparison between the means of the three groups in 'LUT'. 
  # default test statistic is Wilcoxon test, which tests for significant difference of means between the three groups
  # other test statistics can be chosen: e.g: T-test with line code (method = "t.test")
  # or the format of the p value can be changed to asterixes and 'ns' instead of actual numbers with line code (label = "p.signif")
  # more than 2 groups in the predictor variable LUT so do not use pairwise comparisons(Wilcoxon,t.test), rather compare across several groups
  # more than 2 groups, it chooses a different statistical test (e.g. Kruskal-Wallis or Anova)
  
  # to know between which groups exactly the group means differ significantly?
  # create new vector called comparison which is a list of your comparisons and add extra line [comparisons =] into the stat_compare_means function 
  my_comparisons2 <- list(c("Forest", "Fallow"), c("Fallow", "Cropland"), c("Forest", "Cropland"))
  my_comparisons4 <- list(c("Humid", "Semi-arid"))
  
  # to add asterixes instead of p-values?  add extra line [label = "p.signif"] into the stat_compare_means function
  
  #Visualizations - Faceted based on Aridity classes 
  ggplot(data, aes(x= Land_use_type, y=Shannon_diversity_index)) +
    geom_boxplot(aes(fill=FAO_classification_LUT)) +
    stat_compare_means(method = "t.test", comparisons = my_comparisons2, label = "p.signif") +
    facet_grid(~FAO_classification) +
    scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
    ylab("Shannon's diversity") + 
    xlab("Land-use intensity")+
    theme_bw()
  ggsave("./GG plot Shannon with Significance-brackets.jpg", height = 12, width = 15, units = "cm", dpi = 200)
}
#Result
#1a.land-use effect
# In Humid zone: differences existing between; forests and fallows, forests and croplands. no differences in fallows and croplands
# In Semi-arid zone: differences between forests and croplands, fallow and croplands. no differences in forests and fallows
#write percentages

####Hypothesis 1b and 1c####
using_ANOVA_as_the_statistical_test_and_Tukeys_Post-HOC{
  
  cld1b_emm_new <- emmeans(object = aov3, pairwise ~ FAO_classification | Land_use_type)
  # read the above as "doing pairwise comparisons, where the aridity classes are compared to each other, conditioned on the different land use types" or "nested within land use types"
  cld1b_emm_new
  #Aridity classes are now compared separately, within Forest, Fallow, and cropland, but not across (unless you change emmeans to "FAO_classification*Land_use_type")
  cld1b_emm_new_1 <- emmeans(object = aov3, pairwise ~ FAO_classification*Land_use_type)
  # read the above as "doing pairwise comparisons, where the aridity classes are compared to each other, even across Land use types" i.e. they are not "nested within land-use"
  cld1b_emm_new_1
  #Aridity classes are now compared jointly and across, Land_use_types (Forest, Fallow and Cropland)")
  
  cld_new_1b <- cld(object = cld1b_emm_new_1, adjust = "Tukey", Letters = letters, alpha = 0.05)
  cld_new_1b$group <- str_trim(cld_new_1b$.group)
  
  #Visualizations - Faceted based on Land-use 
  plot.shannon1b.1c <- ggplot(data, aes(x= FAO_classification, y=Shannon_diversity_index)) +
    geom_boxplot(aes(fill=FAO_classification_LUT1)) +
    facet_grid(~Land_use_type) +
    geom_text(data=cld_new_1b, aes(label=group,
                                   y=max(data$Shannon_diversity_index)*1.1, x=FAO_classification, angle =0), size = 7,
              inherit.aes = F) +
    scale_fill_manual(values = c("darkgreen", "green3", "steelblue4", "steelblue1", "tan4", "sandybrown"))+
    ylab("Shannon's diversity") + 
    xlab("Aridity class")+
    labs(fill = "Land-use:Aridity") +
    theme_bw()
}
using_t.tests_as_the_statistical_test{
  #Visualizations - Faceted based on Land-use 
  
  ggplot(data, aes(x= FAO_classification, y=Shannon_diversity_index)) +
    geom_boxplot(aes(fill=FAO_classification_LUT)) +
    stat_compare_means(method = "t.test", comparisons = my_comparisons4, label = "p.signif") +
    facet_grid(~Land_use_type) +
    scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
    ylab("Shannon's diversity") + 
    xlab("Aridity class")+
    theme_bw()
}

#Result
#1b.climate effect
# In Forests: climate has an effect (differences existing between humid and semi-arid)
# In fallows: climate does not have an effect (no differences existing between humid and semi-arid)
# In Croplands: climate has an effect (differences existing between humid and semi-arid)
#1c.interactive effect
#Negative effects of increasing climatic aridity is more pronounced under intensive land-use. cropland in the semi-arid had the lowest diversity and differed significantly from fallow in the semi-arid or humid


####Visualizations for Appendix#### 
1-General_effect_across_Land_use_from_both_Aridity_classes{
  using_ANOVA_as_the_statistical_test_and_Tukeys_Post-HOC{
    #Land use effect(general)
    aov1 <- aov(Shannon_diversity_index ~ Land_use_type, data)
    summary(aov1) # effect of land_use is highly significant
    cldLUT_emm_new <- emmeans(object = aov1, pairwise ~ Land_use_type)
    ?emmeans
    cldLUT_emm_new
    
    cld_new_LUT <- cld(object = cldLUT_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
    cld_new_LUT$group <- str_trim(cld_new_LUT$.group)
    
    #Visualizations 
    ggplot(data, aes(x= Land_use_type, y=Shannon_diversity_index)) +
      geom_boxplot(aes(fill=Land_use_type)) +
      geom_text(data=cld_new_LUT, aes(label=group,
                                      y=max(data$Shannon_diversity_index)*1.1, x=Land_use_type, angle =0), size = 7,
                inherit.aes = F) +
      scale_fill_manual(values = c("olivedrab", "dodgerblue", "goldenrod"))+
      ylab("Shannon Weiner diversity index") + 
      xlab("Land-use intensity")+
      theme_bw() +
      theme(
        text = element_text(family = "Arial", size = 12),  # code to set font family and size
        legend.title = element_text(size = 12)
      )  
  }
  using_t.tests_as_the_statistical_test{
    #Visualizations
    
    ggplot(data, aes(x= Land_use_type, y=Shannon_diversity_index)) +
      geom_boxplot(aes(fill=Land_use_type)) +
      stat_compare_means(method = "t.test", comparisons = my_comparisons2, label = "p.signif") +
      scale_fill_manual(values = c("olivedrab", "dodgerblue", "goldenrod"))+
      ylab("Shannon's diversity") + 
      xlab("Land-use intensity")+
      theme_bw()
  }
  #Result
  #Land-use has an effect on Shannon diversity (differences existing between forests, fallows and croplands across both humid and semi-arid)
  #Increasing land-use intensity negatively influences Shannon diversity
}
2-General_effect_across_Aridity_classes_from_all_Land_use_types{
  using_ANOVA_as_the_statistical_test_and_Tukeys_Post-HOC{
    #Climate effect(general)
    aov2 <- aov(Shannon_diversity_index ~ FAO_classification, data)
    summary(aov2) # effect of aridity is highly significant
    cldARID_emm_new <- emmeans(object = aov2, pairwise ~ FAO_classification)
    cldARID_emm_new
    
    cld_new_ARID <- cld(object = cldARID_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
    cld_new_ARID$group <- str_trim(cld_new_ARID$.group)
    
    #Visualizations 
    ggplot(data, aes(x= FAO_classification, y=Shannon_diversity_index)) +
      geom_boxplot(aes(fill=FAO_classification)) +
      geom_text(data=cld_new_ARID, aes(label=group,
                                       y=max(data$Shannon_diversity_index)*1.1, x=FAO_classification, angle =0), size = 7,
                inherit.aes = F) +
      scale_fill_manual(values = c("midnightblue", "orangered"))+
      ylab("Shannon Weiner diversity index") + 
      xlab("Aridity class")+
    labs(fill = "Aridity Class") +
      theme_bw() +
      theme(
        text = element_text(family = "Arial", size = 12),  # code to set font family and size
        legend.title = element_text(size = 12)
      )  
  }
  using_t.tests_as_the_statistical_test{
    #Visualizations
    ggplot(data, aes(x= FAO_classification, y=Shannon_diversity_index)) +
      geom_boxplot(aes(fill=FAO_classification)) +
      stat_compare_means(method = "t.test", comparisons = my_comparisons4, label = "p.signif") +
      scale_fill_manual(values = c("midnightblue", "orangered"))+
      ylab("Shannon's diversity") + 
      xlab("Aridity class")+
      theme_bw()
  }
  #Result
  #Climate has an effect (differences existing between Humid and Semi-arid across all Land-use types)
  #Increasing arid conditions negatively influences Shannon diversity. In general, if the region becomes or tilts towards more semi-arid conditions, we are likely to lose taxonomic diversity
}
3-Plotting_Mean_SE_SD{
  remove.packages("gtable")
  install.packages("gtable")
  update.packages(checkBuilt = TRUE, ask = FALSE)
  library(gtable)
  data.summary.shannon <- data %>%
    group_by(FAO_classification,Land_use_type) %>%
    summarise(
      sd = sd(Shannon_diversity_index, na.rm = TRUE),
      len = mean(Shannon_diversity_index, na.rm = TRUE),
      se=std.error(Shannon_diversity_index, na.rm = TRUE))
  str(data.summary.shannon)
  
  dodge <- position_dodge(width=0.5)
  f1.shannon <- ggplot(data.summary.shannon, aes(x=Land_use_type, y=len, ymin=len-se, ymax=len+se, col=FAO_classification, shape= FAO_classification)
  )+facet_grid(~.)
  f1.shannon + geom_pointrange(position = dodge) +
    geom_errorbar(aes(ymin = len-se , ymax = len+se), position = dodge, width = 0.25)+
    theme_bw() +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab("Shannon Weiner diversity index")+
    xlab("Land-use intensity")+
    theme(legend.key.size = unit(1, 'cm'))+
    scale_color_manual(
      values = c( "midnightblue", "orangered")) +
    labs(fill = "Aridity Class") +
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12)
    )  
  
  #"red", "darkorchid1
  ggsave("./GG plot Mean, SD and SE for Shannon diversity.pdf", height = 15, width = 15, units = "cm", dpi = 200)
  
  # standard deviation is the upper error bar while standard error is the lower error bar)
  # add line code [+ theme(axis.title = element_text(face = "bold", size=5))] to increase and bolden the axis labels
  # add line code [theme(axis.text.x = element_text(face="bold", color="black", size=30), axis.text.y = element_text(face="bold", color="black", size=30))]
}

####Panels-arranging plots in a grid####
H1a{
library(cowplot)
??plot_grid()
legend <- get_legend(plot.richness1a) # Creating a common legend for both plots with a custom title

combined_plot.1a <- plot_grid(
  plot.richness1a +   scale_x_discrete(guide = guide_axis(angle = 90)) + #code to rotate the x axis labels
    theme(
     text = element_text(family = "Arial", size = 12)) +  # code to set font family and size
        theme(legend.position = "none"),
  plot.evenness1a +   scale_x_discrete(guide = guide_axis(angle = 90)) + #code to rotate the x axis labels
    theme(
      text = element_text(family = "Arial", size = 12)) +  # code to set font family and size
        theme(legend.position = "none"),
    plot.shannon1a +   scale_x_discrete(guide = guide_axis(angle = 90)) + #code to rotate the x axis labels
      theme(
        text = element_text(family = "Arial", size = 12)) +  # code to set font family and size
          theme(legend.position = "none"),
  legend, labels = c('(A)', '(B)', '(C)'), label_size = 12,
  ncol = 2,
  rel_widths = c(2, 2, 2, 0.35))

title <- ggdraw() + 
            draw_label(
              "Land-use effects on Taxonomic diversity",
              fontface = 'bold',
              x = 0,
              hjust = 0) +
            theme(plot.margin = margin(0, 0, 0, 7))
           
plot_grid(title, combined_plot.1a,
          ncol = 1,
          rel_heights = c(0.1, 1),
          align = 'v')
ggsave("H1aa.jpg", height = 9.87, width = 18.67, units = "in", dpi = 300)
}
H1b_and_c{
  legend1b.1c <- get_legend(plot.richness1b.1c) # Creating a common legend for both plots with a custom title
  
  combined_plot.1b.1c <- plot_grid(
    plot.richness1b.1c + 
      #scale_x_discrete(guide = guide_axis(angle = 90)) + #code to rotate the x axis labels
      theme(
        text = element_text(family = "Arial", size = 12)) +  # code to set font family and size
      theme(legend.position = "none"),
    plot.evenness1b.1c +  
      #scale_x_discrete(guide = guide_axis(angle = 90)) + #code to rotate the x axis labels
      theme(
        text = element_text(family = "Arial", size = 12)) +  # code to set font family and size
      theme(legend.position = "none"),
    plot.shannon1b.1c + 
      #scale_x_discrete(guide = guide_axis(angle = 90)) + #code to rotate the x axis labels
      theme(
        text = element_text(family = "Arial", size = 12)) +  # code to set font family and size
      theme(legend.position = "none"),
    legend1b.1c, labels = c('(A)', '(B)','(C)'), label_size = 12,
    ncol = 2,
    rel_widths = c(2, 2, 2, 0.35))
  
  title.1b.1c <- ggdraw() + 
    draw_label(
      "Interactive effects of Land-use and Climate on Taxonomic diversity",
      fontface = 'bold',
      x = 0,
      hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  plot_grid(title.1b.1c, combined_plot.1b.1c,
            ncol = 1,
            rel_heights = c(0.1, 1),
            align = 'v')
  ggsave("H1b&1c.jpg", height = 20, width = 36, units = "cm", dpi = 300)
}
H2a{

  plot.fallow2a <- plot.fallow2a + theme(legend.position = "none")
  plot.cropland2a <- plot.cropland2a + theme(legend.position = "none")
  
  combined_plot.2a <- plot_grid(
    plot.forest2a + theme(legend.position = "none"),
    plot.fallow2a + theme(legend.position = "none"),
    plot.cropland2a + theme(legend.position = "none"),
    legend1b.1c,
    ncol = 2)

  title.2a <- ggdraw() + 
    draw_label(
      "Land-use effects on the health of Tree Populations",
      fontface = 'bold',
      x = 0,
      hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  plot_grid(title.2a, combined_plot.2a,
            ncol = 1,
            rel_heights = c(0.1, 1),
            align = 'v')
  ggsave("H2a.jpg", height = 24, width = 43, units = "cm", dpi = 300)
}
H2b{

  combined_plot.2b <- plot_grid(
    plot.slope2b,
    plot.slope2.b,
    labels = c('(A)', '(B)'), label_size = 12)
  
  ggsave("H2b.jpg", height = 20, width = 36, units = "cm", dpi = 300)
}
##exporting tables####
install.packages("stargazer")
install.packages("broom")
library(stargazer)
library(broom)
library(tibble)
library(flextable)
TukeyHSD(aov6)

a <- tidy(aov6)
a[2:6] <- round(a[2:6], digits = 0)
a$sign[a$p.value < 0.01] <- "*" 
a$sign[a$p.value < 0.001] <- "**" 
a$sign[a$p.value < 0.0001] <- "***"
a$sign[a$p.value > 0.05] <- "ns"

write.table(a, file = "LUC and CC effects on Species richness", sep = ",", quote = FALSE, row.names = F)
#Evenness
summary(aov9)
b <- tidy(aov9)
b$sign[b$p.value < 0.01] <- "*" 
b$sign[b$p.value < 0.001] <- "**" 
b$sign[b$p.value < 0.0001] <- "***"
b$sign[b$p.value > 0.05] <- "ns"
write.table(b, file = "LUC and CC effects on Species Evenness", sep = ",", quote = FALSE, row.names = F)

#Shannon
summary(aov3)
c <- tidy(aov3)
c$sign[c$p.value < 0.01] <- "*" 
c$sign[c$p.value < 0.001] <- "**" 
c$sign[c$p.value < 0.0001] <- "***"
c$sign[c$p.value > 0.05] <- "ns"
write.table(c, file = "LUC and CC effects on Shannon", sep = ",", quote = FALSE, row.names = F)

2 #Objective 2: Size class distribution #====
Preparing_the_dataset{
#loading some libraries
install.packages("ggthemes")
install.packages("ggplot")
install.packages("rcompanion") #package for plotNormalHistogram
install.packages("export") #package for exporting images to powerpoint
library(export)
library(rcompanion)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(corrplot)
SCD <- read_excel("Datasheet for size class distribution.xlsx", sheet = 5) 
head(SCD)
summary(SCD)
str(SCD)
SCD <- as.data.frame(SCD)
SCD$Climate <- as.factor(SCD$Climate)
SCD$FAO_classification <- as.factor(SCD$FAO_classification)
SCD$Site <- as.factor(SCD$Site)
SCD$Land_use_type <- factor(SCD$Land_use_type,
                            levels = c("Forest", "Fallow", "Cropland"))
SCD$Plot_code <- as.factor(SCD$Plot_code)
SCD$Plot_number<- as.factor(SCD$Plot_number)
SCD$Class <- factor(SCD$Class, levels = unique(SCD$Class), ordered=TRUE)
SCD$median<- round(
  as.numeric(SCD$median), 1)
SCD$`Individuals/ha`<- round(
  as.numeric(SCD$`Individuals/ha`), 1)
SCD$FAO_classification_LUT <- interaction(SCD$Land_use_type, SCD$FAO_classification, sep=":")

#transforming the data to remove inconsistencies.e.g we have min values of zero and we also have max values of ~94000#
{
data1 <- SCD[1:10]
data2 <- SCD[11:13]
sum(data2$`Individuals/ha`)
sum(data1$`ln.Individuals/ha+1`)

#Graphical normality distribution before transforming
hist(SCD$`Individuals/ha`)
hist(SCD$`median`)
plotNormalHistogram(SCD$`Individuals/ha`)
plotNormalHistogram(SCD$median)

#data transformation
data1$FAO_classification_LUT <- interaction(data1$Land_use_type, data1$FAO_classification, sep=":")
data1$FAO_classification_LUT <- factor(data1$FAO_classification_LUT,
                                       levels = c("Forest:Humid", "Forest:Semi-arid", "Fallow:Humid", 
                                                  "Fallow:Semi-arid", "Cropland:Humid", "Cropland:Semi-arid"))

data1$`ln.Individuals/ha+1` <- round(log(data2$`Individuals/ha`+ 1),0) #log transformation plus one because of size classes with zero individuals
data1$`ln.median` <- round(log(data2$median),0) # ordinary log transformation
summary(data1)
str(data1)
write_xlsx(data1, "C:/Users/CP24/Nextcloud2/Thesis/Analysis/Functional traits analysis/Updated data arrangement - Liana/density transformed.xlsx")


#Graphical normality distribution after transforming
hist(data1$`ln.Individuals/ha+1`)
hist(data1$`ln.median`)
plotNormalHistogram(data1$`ln.Individuals/ha+1`)
plotNormalHistogram(data1$ln.median)

#statistical tests for normality after transforming
ad.test(data1$`ln.Individuals/ha+1`)
#skewness(), ad.test(), kurtosis(), shapiro.test()
}
}
2.1#Histogram_and_Visualization_for_Size Class Distribution #=========
CORRECT#Histogram using average values. so rather than per plot, it is aggregated per land-use type#
{
  
  avg_data <- aggregate(`ln.Individuals/ha+1` ~ Class + Land_use_type + FAO_classification, data1, mean) #can also use sum instead of mean
  avg_data$FAO_classification_LUT <- interaction(avg_data$Land_use_type, avg_data$FAO_classification, sep=":")
  avg_data$FAO_classification_LUT1 <- factor(avg_data$FAO_classification_LUT,
                              levels = c("Forest:Humid", "Forest:Semi-arid", "Fallow:Humid", 
                                         "Fallow:Semi-arid", "Cropland:Humid", "Cropland:Semi-arid"))
  cropland <- filter(avg_data, Land_use_type == "Cropland")
  forest <- filter(avg_data, Land_use_type == "Forest")
  fallow <- filter(avg_data, Land_use_type == "Fallow")
  summary(avg_data)
  str(avg_data)
  write_xlsx(avg_data, "C:/Users/CP24/Nextcloud2/Thesis/Analysis/Functional traits analysis/Updated data arrangement - Liana/avg denisty transformed.xlsx")
  
}
all_LUT{
  plot.H2a <- ggplot(avg_data, aes(x=Class, y=`ln.Individuals/ha+1`))+
    geom_bar(aes(fill=as.factor(FAO_classification_LUT1)), 
             position = "dodge", stat = "identity") + 
    theme(legend.position = c(0.5, 0.8)) +
    labs(x="Stem diameter size-class (cm)", y = "Individuals per hectare (ln+1)", fill = "Land-use:Aridity Class") + 
    scale_fill_manual(values = c("darkgreen", "green3", "steelblue4", "steelblue1", "tan4", "sandybrown"))+
    facet_grid(~Land_use_type)+
    theme_bw() + #code for removing gray background
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis text
    )
}
forest{
  plot.forest2a <- ggplot(forest, aes(x=Class, y=`ln.Individuals/ha+1`))+
    geom_bar(aes(fill=as.factor(FAO_classification_LUT)), 
             position = "dodge", stat = "identity") + 
    theme(legend.position = c(0.5, 0.8)) +
    labs(x="Stem diameter size-class (cm)", y = "Individuals per hectare (ln+1)", fill = "Land-use:Aridity") + 
    scale_fill_manual(values = c("darkgreen", "green3"))+
    facet_grid(~Land_use_type)+
    theme_bw() + #code for removing gray background
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
}
fallow{
  plot.fallow2a <- ggplot(fallow, aes(x=Class, y=`ln.Individuals/ha+1`))+
    geom_bar(aes(fill=as.factor(FAO_classification_LUT)), 
             position = "dodge", stat = "identity") + 
    theme(legend.position = c(0.5, 0.8)) +
    labs(x="Stem diameter size-class (cm)", y = "Individuals per hectare (ln+1)", fill = "Land-use:Aridity") + 
    scale_fill_manual(values = c("steelblue4", "steelblue1"))+
    facet_grid(~Land_use_type)+
    theme_bw() + #code for removing gray background
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
}
cropland{
  plot.cropland2a <- ggplot(cropland, aes(x=Class, y=`ln.Individuals/ha+1`))+
    geom_bar(aes(fill=as.factor(FAO_classification_LUT)), 
             position = "dodge", stat = "identity") + 
    theme(legend.position = c(0.5, 0.8)) +
    labs(x="Stem diameter size-class (cm)", y = "Individuals per hectare (ln+1)", fill = "Land-use:Aridity") + 
    scale_fill_manual(values = c("tan4", "sandybrown"))+
    facet_grid(~Land_use_type)+
    theme_bw() + #code for removing gray background
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
}

ggsave("./GG plot SCD Histogram.jpg", height = 24, width = 48, units = "cm", dpi = 200)
graph2ppt(file="SCD Histogram.pptx", upscale=TRUE)

NOT_CORRECT!#Histogram using all individual occurrences from individual plots. it stacks up the result#
{
  ggplot(data1) +
    geom_bar(aes(x = Class, y = `ln.Individuals/ha+1`, fill = as.factor(Land_use_type)), 
             position = "dodge", colour = "black", stat = "identity") + 
    theme(legend.position = c(0.5, 0.8)) +
    labs(x="Stem diameter size-class (cm)", y = "Individuals per hectare(ln)", fill = "Land-use type") + 
    scale_fill_manual(values = c("grey10","grey40","grey70"))+
    facet_grid(.~FAO_classification_LUT)
}
2.2#Linear regression models for calculating the SCD slope----
library(FSA)
library(ggpubr)

#best to create individual models for each plot so that we can compare land-use and climatic effects
Plot1_to_9_cropland_Gbele{
  P1 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P1")
  P2 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P2")
  P3 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P3")
  P4 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P5")
  P5 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P6")
  P6 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P7")
  P7 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P8")
  P8 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P9")
  P9 <- data1 %>% filter(Plot_code == "GH_N_Gb_C_P10")
  }
Plot10_to_19_forest_Gbele{
  P10 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P1")
  P11 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P2")
  P12 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P3")
  P13 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P4")
  P14 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P5")
  P15 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P6")
  P16 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P7")
  P17 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P8")
  P18 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P9")
  P19 <- data1 %>% filter(Plot_code == "GH_N_Gb_F_P10")
  }
Plot20_to_29_fallow_Gbele{
  P20 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P1")
  P21 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P2")
  P22 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P3")
  P23 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P4")
  P24 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P5")
  P25 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P6")
  P26 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P7")
  P27 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P8")
  P28 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P9")
  P29 <- data1 %>% filter(Plot_code == "GH_N_Gb_R_P10")
  }
  
Plot30_to_39_cropland_Mole{
    P30 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P1")
    P31 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P2")
    P32 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P3")
    P33 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P4")
    P34 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P5")
    P35 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P6")
    P36 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P7")
    P37 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P8")
    P38 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P9")
    P39 <- data1 %>% filter(Plot_code == "GH_N_Mo_C_P10")
  }
Plot40_to_49_forest_Mole{
    P40 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P1")
    P41 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P2")
    P42 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P3")
    P43 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P4")
    P44 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P5")
    P45 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P6")
    P46 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P7")
    P47 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P8")
    P48 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P9")
    P49 <- data1 %>% filter(Plot_code == "GH_N_Mo_F_P10")
  }
Plot50_to_59_fallow_Mole{
    P50 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P1")
    P51 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P2")
    P52 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P3")
    P53 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P4")
    P54 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P5")
    P55 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P6")
    P56 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P7")
    P57 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P8")
    P58 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P9")
    P59 <- data1 %>% filter(Plot_code == "GH_N_Mo_R_P10")
  }
  
Plot60_to_66_cropland_Bobiri{
    P60 <- data1 %>% filter(Plot_code == "GH_S_Bo_C_P1")
    P61 <- data1 %>% filter(Plot_code == "GH_S_Bo_C_P2")
    P62 <- data1 %>% filter(Plot_code == "GH_S_Bo_C_P4")
    P63 <- data1 %>% filter(Plot_code == "GH_S_Bo_C_P5")
    P64 <- data1 %>% filter(Plot_code == "GH_S_Bo_C_P8")
    P65 <- data1 %>% filter(Plot_code == "GH_S_Bo_C_P9")
    P66 <- data1 %>% filter(Plot_code == "GH_S_Bo_C_P10")
  }
Plot67_to_76_forest_Bobiri{
    P67 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P1")
    P68 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P2")
    P69 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P3")
    P70 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P4")
    P71 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P5")
    P72 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P6")
    P73 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P7")
    P74 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P8")
    P75 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P9")
    P76 <- data1 %>% filter(Plot_code == "GH_S_Bo_F_P10")
  }
Plot77_to_84_fallow_Bobiri{
    P77 <- data1 %>% filter(Plot_code == "GH_S_Bo_R_P1")
    P78 <- data1 %>% filter(Plot_code == "GH_S_Bo_R_P2")
    P79 <- data1 %>% filter(Plot_code == "GH_S_Bo_R_P3")
    P80 <- data1 %>% filter(Plot_code == "GH_S_Bo_R_P4")
    P81 <- data1 %>% filter(Plot_code == "GH_S_Bo_R_P5")
    P82 <- data1 %>% filter(Plot_code == "GH_S_Bo_R_P8")
    P83 <- data1 %>% filter(Plot_code == "GH_S_Bo_R_P9")
    P84 <- data1 %>% filter(Plot_code == "GH_S_Bo_R_P10")
  }
  
Plot85_to_90_cropland_PraAnum{
    P85 <- data1 %>% filter(Plot_code == "GH_S_Pa_C_P1")
    P86 <- data1 %>% filter(Plot_code == "GH_S_Pa_C_P3")
    P87 <- data1 %>% filter(Plot_code == "GH_S_Pa_C_P5")
    P88 <- data1 %>% filter(Plot_code == "GH_S_Pa_C_P6")
    P89 <- data1 %>% filter(Plot_code == "GH_S_Pa_C_P7")
    P90 <- data1 %>% filter(Plot_code == "GH_S_Pa_C_P8")
  }
Plot91_to_100_forest_PraAnum{
    P91 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P1")
    P92 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P2")
    P93 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P3")
    P94 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P4")
    P95 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P5")
    P96 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P6")
    P97 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P7")
    P98 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P8")
    P99 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P9")
    P100 <- data1 %>% filter(Plot_code == "GH_S_Pa_F_P10")
  }
Plot101_to_110_fallow_PraAnum{
    P101 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P1")
    P102 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P2")
    P103 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P3")
    P104 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P4")
    P105 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P5")
    P106 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P6")
    P107 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P7")
    P108 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P8")
    P109 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P9")
    P110 <- data1 %>% filter(Plot_code == "GH_S_Pa_R_P10")
}

summary(lm(`ln.Individuals/ha+1`~ `ln.median`, 
           data = P1_to_P110))

cropland.wet <- filter(data1, Climate == "Wet", Land_use_type == "Cropland")
fallow.wet <- filter(data1, Climate == "Wet", Land_use_type == "Fallow")
forest.wet <- filter(data1, Climate == "Wet", Land_use_type == "Forest")

cropland.dry <- filter(data1, Climate == "Dry", Land_use_type == "Cropland")
fallow.dry <- filter(data1, Climate == "Dry", Land_use_type == "Fallow")
forest.dry <- filter(data1, Climate == "Dry", Land_use_type == "Forest")

summary(lm(`ln.Individuals/ha+1`~ `ln.median`, 
                data = cropland.wet))

Creating_and_plotting_linear_regression_models{
Climate_Dry_LUT_Cropland{
P1.Climate_Dry_LUT_Cropland{
model.P1 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                data = P1)
model.P1$predicted <- (model.P1) # Generate predicted values
summary(model.P1) # Print the model summary
# Extract the coefficients
intercept.P1 <- coef(model.P1)[1]
slope.P1 <- coef(model.P1)[2]
# Interpret the coefficients
cat("intercept:", exp(intercept.P1), "\n")
cat("Slope:", slope.P1, "\n")
# Plotting data points on the graph
ggplot(P1, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", col="red")+
  stat_regline_equation(label.x = 0, label.y = 3.5)
theme_set(theme_classic())  # theme without gray background in the plots
#OR step by step
ggplot(P1, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
  geom_point() + 
  geom_smooth(method="lm", col="red") + #Adding linear regression line 
  stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
  theme_bw() + #Inserting titles
  labs (title = "Number of individuals as a function of size-class midpoint - P1")

#par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
#Plotting the model summary. checks for homoscedacity
plot(model.P1) 
<Return>
}
P2.Climate_Dry_LUT_Cropland{
  model.P2 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                 data = P2)
  model.P2$predicted <- (model.P2) # Generate predicted values
  summary(model.P2) # Print the model summary
  # Extract the coefficients
  intercept.P2 <- coef(model.P2)[1]
  slope.P2 <- coef(model.P2)[2]
  # Interpret the coefficients
  cat("intercept:", exp(intercept.P2), "\n")
  cat("Slope:", slope.P2, "\n")
  # Plotting data points on the graph
  ggplot(P2, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point(alpha=0.2)+
    geom_smooth(method="lm", col="red")+
    stat_regline_equation(label.x = 0, label.y = 3.5)
  theme_set(theme_classic())  # theme without gray background in the plots
  #OR step by step
  ggplot(P2, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point() + 
    geom_smooth(method="lm", col="red") + #Adding linear regression line 
    stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
    theme_bw() + #Inserting titles
    labs (title = "Number of individuals as a function of size-class midpoint - P2")
  
  #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
  #Plotting the model summary. checks for homoscedacity
  plot(model.P2) 
  <Return>
}
P3.Climate_Dry_LUT_Cropland{
  model.P3 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                 data = P3)
  model.P3$predicted <- (model.P3) # Generate predicted values
  summary(model.P3) # Print the model summary
  # Extract the coefficients
  intercept.P3 <- coef(model.P3)[1]
  slope.P3 <- coef(model.P3)[2]
  # Interpret the coefficients
  cat("intercept:", exp(intercept.P3), "\n")
  cat("Slope:", slope.P3, "\n")
  # Plotting data points on the graph
  ggplot(P3, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point(alpha=0.2)+
    geom_smooth(method="lm", col="red")+
    stat_regline_equation(label.x = 0, label.y = 3.5)
  theme_set(theme_classic())  # theme without gray background in the plots
  #OR step by step
  ggplot(P3, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point() + 
    geom_smooth(method="lm", col="red") + #Adding linear regression line 
    stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
    theme_bw() + #Inserting titles
    labs (title = "Number of individuals as a function of size-class midpoint - P3")
  
  #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
  #Plotting the model summary. checks for homoscedacity
  plot(model.P3) 
  <Return>
}
P4.Climate_Dry_LUT_Cropland{
  model.P4 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                 data = P4)
  model.P4$predicted <- (model.P4) # Generate predicted values
  summary(model.P4) # Print the model summary
  # Extract the coefficients
  intercept.P4 <- coef(model.P4)[1]
  slope.P4 <- coef(model.P4)[2]
  # Interpret the coefficients
  cat("intercept:", exp(intercept.P4), "\n")
  cat("Slope:", slope.P4, "\n")
  # Plotting data points on the graph
  ggplot(P4, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point(alpha=0.2)+
    geom_smooth(method="lm", col="red")+
    stat_regline_equation(label.x = 0, label.y = 3.5)
  theme_set(theme_classic())  # theme without gray background in the plots
  #OR step by step
  ggplot(P4, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point() + 
    geom_smooth(method="lm", col="red") + #Adding linear regression line 
    stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
    theme_bw() + #Inserting titles
    labs (title = "Number of individuals as a function of size-class midpoint - P4")
  
  #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
  #Plotting the model summary. checks for homoscedacity
  plot(model.P4) 
  <Return>
}
P5.Climate_Dry_LUT_Cropland{
  model.P5 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                 data = P5)
  model.P5$predicted <- (model.P5) # Generate predicted values
  summary(model.P5) # Print the model summary
  # Extract the coefficients
  intercept.P5 <- coef(model.P5)[1]
  slope.P5 <- coef(model.P5)[2]
  # Interpret the coefficients
  cat("intercept:", exp(intercept.P5), "\n")
  cat("Slope:", slope.P5, "\n")
  # Plotting data points on the graph
  ggplot(P5, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point(alpha=0.2)+
    geom_smooth(method="lm", col="red")+
    stat_regline_equation(label.x = 0, label.y = 3.5)
  theme_set(theme_classic())  # theme without gray background in the plots
  #OR step by step
  ggplot(P5, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point() + 
    geom_smooth(method="lm", col="red") + #Adding linear regression line 
    stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
    theme_bw() + #Inserting titles
    labs (title = "Number of individuals as a function of size-class midpoint - P5")
  
  #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
  #Plotting the model summary. checks for homoscedacity
  plot(model.P5) 
  <Return>
}
P6.Climate_Dry_LUT_Cropland{
  model.P6 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                 data = P6)
  model.P6$predicted <- (model.P6) # Generate predicted values
  summary(model.P6) # Print the model summary
  # Extract the coefficients
  intercept.P6 <- coef(model.P6)[1]
  slope.P6 <- coef(model.P6)[2]
  # Interpret the coefficients
  cat("intercept:", exp(intercept.P6), "\n")
  cat("Slope:", slope.P6, "\n")
  # Plotting data points on the graph
  ggplot(P6, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point(alpha=0.2)+
    geom_smooth(method="lm", col="red")+
    stat_regline_equation(label.x = 0, label.y = 3.5)
  theme_set(theme_classic())  # theme without gray background in the plots
  #OR step by step
  ggplot(P6, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point() + 
    geom_smooth(method="lm", col="red") + #Adding linear regression line 
    stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
    theme_bw() + #Inserting titles
    labs (title = "Number of individuals as a function of size-class midpoint - P6")
  
  #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
  #Plotting the model summary. checks for homoscedacity
  plot(model.P6) 
  <Return>
}
P7.Climate_Dry_LUT_Cropland{
  model.P7 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                 data = P7)
  model.P7$predicted <- (model.P7) # Generate predicted values
  summary(model.P7) # Print the model summary
  # Extract the coefficients
  intercept.P7 <- coef(model.P7)[1]
  slope.P7 <- coef(model.P7)[2]
  # Interpret the coefficients
  cat("intercept:", exp(intercept.P7), "\n")
  cat("Slope:", slope.P7, "\n")
  # Plotting data points on the graph
  ggplot(P7, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point(alpha=0.2)+
    geom_smooth(method="lm", col="red")+
    stat_regline_equation(label.x = 0, label.y = 3.5)
  theme_set(theme_classic())  # theme without gray background in the plots
  #OR step by step
  ggplot(P7, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point() + 
    geom_smooth(method="lm", col="red") + #Adding linear regression line 
    stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
    theme_bw() + #Inserting titles
    labs (title = "Number of individuals as a function of size-class midpoint - P7")
  
  #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
  #Plotting the model summary. checks for homoscedacity
  plot(model.P7) 
  <Return>
}
P8.Climate_Dry_LUT_Cropland{
  model.P8 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                 data = P8)
  model.P8$predicted <- (model.P8) # Generate predicted values
  summary(model.P8) # Print the model summary
  # Extract the coefficients
  intercept.P8 <- coef(model.P8)[1]
  slope.P8 <- coef(model.P8)[2]
  # Interpret the coefficients
  cat("intercept:", exp(intercept.P8), "\n")
  cat("Slope:", slope.P8, "\n")
  # Plotting data points on the graph
  ggplot(P8, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point(alpha=0.2)+
    geom_smooth(method="lm", col="red")+
    stat_regline_equation(label.x = 0, label.y = 3.5)
  theme_set(theme_classic())  # theme without gray background in the plots
  #OR step by step
  ggplot(P8, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point() + 
    geom_smooth(method="lm", col="red") + #Adding linear regression line 
    stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
    theme_bw() + #Inserting titles
    labs (title = "Number of individuals as a function of size-class midpoint - P8")
  
  #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
  #Plotting the model summary. checks for homoscedacity
  plot(model.P8) 
  <Return>
}
P9.Climate_Dry_LUT_Cropland{
  model.P9 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                 data = P9)
  model.P9$predicted <- (model.P9) # Generate predicted values
  summary(model.P9) # Print the model summary
  # Extract the coefficients
  intercept.P9 <- coef(model.P9)[1]
  slope.P9 <- coef(model.P9)[2]
  # Interpret the coefficients
  cat("intercept:", exp(intercept.P9), "\n")
  cat("Slope:", slope.P9, "\n")
  # Plotting data points on the graph
  ggplot(P9, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point(alpha=0.2)+
    geom_smooth(method="lm", col="red")+
    stat_regline_equation(label.x = 0, label.y = 3.5)
  theme_set(theme_classic())  # theme without gray background in the plots
  #OR step by step
  ggplot(P9, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
    geom_point() + 
    geom_smooth(method="lm", col="red") + #Adding linear regression line 
    stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
    theme_bw() + #Inserting titles
    labs (title = "Number of individuals as a function of size-class midpoint - P9")
  
  #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
  #Plotting the model summary. checks for homoscedacity
  plot(model.P9) 
  <Return>
}
}
Climate_Dry_LUT_Forest{
  P10.Climate_Dry_LUT_Forest{
    model.P10 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P10)
    model.P10$predicted <- (model.P10) # Generate predicted values
    summary(model.P10) # Print the model summary
    # Extract the coefficients
    intercept.P10 <- coef(model.P10)[1]
    slope.P10 <- coef(model.P10)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P10), "\n")
    cat("Slope:", slope.P10, "\n")
    # Plotting data points on the graph
    ggplot(P10, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P10, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P10")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P10) 
    <Return>
  }
  P2.Climate_Dry_LUT_Forest{
    model.P2 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P11)
    model.P2$predicted <- (model.P2) # Generate predicted values
    summary(model.P2) # Print the model summary
    # Extract the coefficients
    intercept.P2 <- coef(model.P2)[1]
    slope.P2 <- coef(model.P2)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P2), "\n")
    cat("Slope:", slope.P2, "\n")
    # Plotting data points on the graph
    ggplot(P2, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P2, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P2")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P2) 
    <Return>
  }
  P3.Climate_Dry_LUT_Forest{
    model.P3 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P12)
    model.P3$predicted <- (model.P3) # Generate predicted values
    summary(model.P3) # Print the model summary
    # Extract the coefficients
    intercept.P3 <- coef(model.P3)[1]
    slope.P3 <- coef(model.P3)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P3), "\n")
    cat("Slope:", slope.P3, "\n")
    # Plotting data points on the graph
    ggplot(P3, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P3, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P3")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P3) 
    <Return>
  }
  P4.Climate_Dry_LUT_Forest{
    model.P4 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P13)
    model.P4$predicted <- (model.P4) # Generate predicted values
    summary(model.P4) # Print the model summary
    # Extract the coefficients
    intercept.P4 <- coef(model.P4)[1]
    slope.P4 <- coef(model.P4)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P4), "\n")
    cat("Slope:", slope.P4, "\n")
    # Plotting data points on the graph
    ggplot(P4, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P4, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P4")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P4) 
    <Return>
  }
  P5.Climate_Dry_LUT_Forest{
    model.P5 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P14)
    model.P5$predicted <- (model.P5) # Generate predicted values
    summary(model.P5) # Print the model summary
    # Extract the coefficients
    intercept.P5 <- coef(model.P5)[1]
    slope.P5 <- coef(model.P5)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P5), "\n")
    cat("Slope:", slope.P5, "\n")
    # Plotting data points on the graph
    ggplot(P5, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P5, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P5")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P5) 
    <Return>
  }
  P6.Climate_Dry_LUT_Forest{
    model.P6 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P15)
    model.P6$predicted <- (model.P6) # Generate predicted values
    summary(model.P6) # Print the model summary
    # Extract the coefficients
    intercept.P6 <- coef(model.P6)[1]
    slope.P6 <- coef(model.P6)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P6), "\n")
    cat("Slope:", slope.P6, "\n")
    # Plotting data points on the graph
    ggplot(P6, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P6, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P6")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P6) 
    <Return>
  }
  P7.Climate_Dry_LUT_Forest{
    model.P7 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P16)
    model.P7$predicted <- (model.P7) # Generate predicted values
    summary(model.P7) # Print the model summary
    # Extract the coefficients
    intercept.P7 <- coef(model.P7)[1]
    slope.P7 <- coef(model.P7)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P7), "\n")
    cat("Slope:", slope.P7, "\n")
    # Plotting data points on the graph
    ggplot(P7, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P7, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P7")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P7) 
    <Return>
  }
  P8.Climate_Dry_LUT_Forest{
    model.P8 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P17)
    model.P8$predicted <- (model.P8) # Generate predicted values
    summary(model.P8) # Print the model summary
    # Extract the coefficients
    intercept.P8 <- coef(model.P8)[1]
    slope.P8 <- coef(model.P8)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P8), "\n")
    cat("Slope:", slope.P8, "\n")
    # Plotting data points on the graph
    ggplot(P8, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P8, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P8")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P8) 
    <Return>
  }
  P9.Climate_Dry_LUT_Forest{
    model.P9 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P18)
    model.P9$predicted <- (model.P9) # Generate predicted values
    summary(model.P9) # Print the model summary
    # Extract the coefficients
    intercept.P9 <- coef(model.P9)[1]
    slope.P9 <- coef(model.P9)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P9), "\n")
    cat("Slope:", slope.P9, "\n")
    # Plotting data points on the graph
    ggplot(P9, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P9, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P9")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P9) 
    <Return>
  }
  P20.Climate_Dry_LUT_Forest{
    model.P20 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P19)
    model.P20$predicted <- (model.P20) # Generate predicted values
    summary(model.P10) # Print the model summary
    # Extract the coefficients
    intercept.P20 <- coef(model.P20)[1]
    slope.P20 <- coef(model.P20)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P20), "\n")
    cat("Slope:", slope.P20, "\n")
    # Plotting data points on the graph
    ggplot(P10, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P10, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P9")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P9) 
    <Return>
  }
}
Climate_Dry_LUT_Fallow{
  P10.Climate_Dry_LUT_Forest{
    model.P10 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                    data = P10)
    model.P10$predicted <- (model.P10) # Generate predicted values
    summary(model.P10) # Print the model summary
    # Extract the coefficients
    intercept.P10 <- coef(model.P10)[1]
    slope.P10 <- coef(model.P10)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P10), "\n")
    cat("Slope:", slope.P10, "\n")
    # Plotting data points on the graph
    ggplot(P10, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P10, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P10")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P10) 
    <Return>
  }
  P2.Climate_Dry_LUT_Forest{
    model.P2 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P11)
    model.P2$predicted <- (model.P2) # Generate predicted values
    summary(model.P2) # Print the model summary
    # Extract the coefficients
    intercept.P2 <- coef(model.P2)[1]
    slope.P2 <- coef(model.P2)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P2), "\n")
    cat("Slope:", slope.P2, "\n")
    # Plotting data points on the graph
    ggplot(P2, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P2, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P2")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P2) 
    <Return>
  }
  P3.Climate_Dry_LUT_Forest{
    model.P3 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P12)
    model.P3$predicted <- (model.P3) # Generate predicted values
    summary(model.P3) # Print the model summary
    # Extract the coefficients
    intercept.P3 <- coef(model.P3)[1]
    slope.P3 <- coef(model.P3)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P3), "\n")
    cat("Slope:", slope.P3, "\n")
    # Plotting data points on the graph
    ggplot(P3, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P3, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P3")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P3) 
    <Return>
  }
  P4.Climate_Dry_LUT_Forest{
    model.P4 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P13)
    model.P4$predicted <- (model.P4) # Generate predicted values
    summary(model.P4) # Print the model summary
    # Extract the coefficients
    intercept.P4 <- coef(model.P4)[1]
    slope.P4 <- coef(model.P4)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P4), "\n")
    cat("Slope:", slope.P4, "\n")
    # Plotting data points on the graph
    ggplot(P4, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P4, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P4")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P4) 
    <Return>
  }
  P5.Climate_Dry_LUT_Forest{
    model.P5 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P14)
    model.P5$predicted <- (model.P5) # Generate predicted values
    summary(model.P5) # Print the model summary
    # Extract the coefficients
    intercept.P5 <- coef(model.P5)[1]
    slope.P5 <- coef(model.P5)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P5), "\n")
    cat("Slope:", slope.P5, "\n")
    # Plotting data points on the graph
    ggplot(P5, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P5, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P5")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P5) 
    <Return>
  }
  P6.Climate_Dry_LUT_Forest{
    model.P6 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P15)
    model.P6$predicted <- (model.P6) # Generate predicted values
    summary(model.P6) # Print the model summary
    # Extract the coefficients
    intercept.P6 <- coef(model.P6)[1]
    slope.P6 <- coef(model.P6)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P6), "\n")
    cat("Slope:", slope.P6, "\n")
    # Plotting data points on the graph
    ggplot(P6, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P6, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P6")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P6) 
    <Return>
  }
  P7.Climate_Dry_LUT_Forest{
    model.P7 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P16)
    model.P7$predicted <- (model.P7) # Generate predicted values
    summary(model.P7) # Print the model summary
    # Extract the coefficients
    intercept.P7 <- coef(model.P7)[1]
    slope.P7 <- coef(model.P7)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P7), "\n")
    cat("Slope:", slope.P7, "\n")
    # Plotting data points on the graph
    ggplot(P7, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P7, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P7")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P7) 
    <Return>
  }
  P8.Climate_Dry_LUT_Forest{
    model.P8 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P17)
    model.P8$predicted <- (model.P8) # Generate predicted values
    summary(model.P8) # Print the model summary
    # Extract the coefficients
    intercept.P8 <- coef(model.P8)[1]
    slope.P8 <- coef(model.P8)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P8), "\n")
    cat("Slope:", slope.P8, "\n")
    # Plotting data points on the graph
    ggplot(P8, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P8, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P8")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P8) 
    <Return>
  }
  P9.Climate_Dry_LUT_Forest{
    model.P9 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                   data = P18)
    model.P9$predicted <- (model.P9) # Generate predicted values
    summary(model.P9) # Print the model summary
    # Extract the coefficients
    intercept.P9 <- coef(model.P9)[1]
    slope.P9 <- coef(model.P9)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P9), "\n")
    cat("Slope:", slope.P9, "\n")
    # Plotting data points on the graph
    ggplot(P9, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P9, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P9")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P9) 
    <Return>
  }
  P20.Climate_Dry_LUT_Forest{
    model.P20 <- lm(`ln.Individuals/ha+1`~ `ln.median`, 
                    data = P19)
    model.P20$predicted <- (model.P20) # Generate predicted values
    summary(model.P10) # Print the model summary
    # Extract the coefficients
    intercept.P20 <- coef(model.P20)[1]
    slope.P20 <- coef(model.P20)[2]
    # Interpret the coefficients
    cat("intercept:", exp(intercept.P20), "\n")
    cat("Slope:", slope.P20, "\n")
    # Plotting data points on the graph
    ggplot(P10, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point(alpha=0.2)+
      geom_smooth(method="lm", col="red")+
      stat_regline_equation(label.x = 0, label.y = 3.5)
    theme_set(theme_classic())  # theme without gray background in the plots
    #OR step by step
    ggplot(P10, aes(x=`ln.median`, y=`ln.Individuals/ha+1`))+
      geom_point() + 
      geom_smooth(method="lm", col="red") + #Adding linear regression line 
      stat_regline_equation(label.x = 0, label.y = 3.5) + #Adding equation
      theme_bw() + #Inserting titles
      labs (title = "Number of individuals as a function of size-class midpoint - P9")
    
    #par(mfrow=c(2,2)) # 4 figures arranged in 2 rows and 2 columns
    #Plotting the model summary. checks for homoscedacity
    plot(model.P9) 
    <Return>
  }
}
}
2.3#Visualizing the slopes and effects#====
Preparing_dataset_for_plotting{
library(plotrix)
library("ggplot2")
library(FSA)
library(psych)
library(coin)
library(dplyr)
data <- read_excel('Functional traits analysis datasheet.xlsx', sheet = 1)

View(data)
class(data)
summary(data)
str(data)
data  <- as.data.frame(data)
data$Slope<- round(
  as.numeric(data$Slope), 1)
data$Intercept <- round(
  as.numeric(data$Intercept), 1)

str(data)
b = data %>%
  group_by(FAO_classification,Land_use_type) %>%
  summarise(
    mean.slope = mean(Slope, na.rm = TRUE),
    mean.intercept = mean(Intercept, na.rm = TRUE))
write_xlsx(b, "C:/Users/CP24/Nextcloud2/Thesis/Analysis/Functional traits analysis/Updated data arrangement - Liana/Mean_SCD Slope values.xlsx")
}
Preliminary_statistics_and_visualization{

aov10 <- aov(Slope ~ Land_use_type, data)
summary(aov10)
TukeyHSD(aov10)

aov11 <- aov(Slope ~ FAO_classification, data)
summary(aov11)
TukeyHSD(aov11)

aov12 <- aov(Slope ~ Land_use_type*FAO_classification, data)
summary(aov12)
TukeyHSD(aov12)

#POST HOC
kruskal.test(Slope ~ FAO_classification, data)

#land use effects in same climate zones
data.wet <- filter(data, Climate == "Wet") #mann-whitney test for two levels
data.dry <- filter(data, Climate == "Dry") 
summary(aov(Slope ~ Land_use_type, data = data.dry))
dunnTest(Slope ~ Land_use_type, data=data.dry, method="bh")
summary(aov(Slope ~ Land_use_type, data = data.wet))
dunnTest(Slope ~ Land_use_type, data=data.wet, method="bh")

#Climate effects in same land-use

cropland <- filter(data, Land_use_type == "Cropland")
forest <- filter(data, Land_use_type == "Forest")
fallow <- filter(data, Land_use_type == "Fallow")

summary(aov(Slope ~ `Climate`, data = cropland))
summary(aov(Slope ~ `Climate`, data = fallow))
summary(aov(Slope ~ `Climate`, data = forest))
wilcox_test (Slope ~ `Climate`, data=cropland, distribution = "exact")
wilcox_test (Slope ~ `Climate`, data=fallow, distribution = "exact")
wilcox_test (Slope ~ `Climate`, data=forest, distribution = "exact")

#Preliminary visualization
#* means an interactive effect of 2 predictor factors while (+) produces separate results for each predictor variable
boxplot(Slope ~ Land_use_type, data)
boxplot(Slope ~ `Climate`, data)
boxplot(Slope ~ FAO_classification, data)
boxplot(Slope ~ Land_use_type, data)
boxplot(Slope ~ Land_use_type+ FAO_classification, data)

hist(data$Slope)
hist(data$Intercept)
plotNormalHistogram(data$Slope)
plotNormalHistogram(data$Intercept)
}

#####Hypothesis 2b.A####
#using_ANOVA_as_the_statistical_test_and_Tukey's_Post-HOC
#Interaction
aov12 <- aov(Slope ~ Land_use_type*FAO_classification, data)
summary(aov12) # interaction of land_use & aridity class (FAO_classification) is not significant
aov12_emm_new <- emmeans(object = aov12, pairwise ~ Land_use_type*FAO_classification)
aov12_emm_new

cld_SLOPE_new <- cld(object = aov12_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
cld_SLOPE_new$group <- str_trim(cld_SLOPE_new$.group)

#Visualizations - Faceted based on Aridity classes 
plot.slope2b <- ggplot(data, aes(x= Land_use_type, y=Slope)) +
  geom_boxplot(aes(fill=FAO_classification_LUT)) +
  facet_grid(~FAO_classification) +
  geom_text(data=cld_SLOPE_new, aes(label=group,
                                 y=max(data$Slope)*1.1, x=Land_use_type, angle =0), size = 7,
            inherit.aes = F) +
  scale_fill_manual(values = c("darkgreen", "steelblue4", "tan4", "green3", "steelblue1", "sandybrown"))+
  ylab("SCD slope") + 
  xlab("Land-use intensity")+
  labs(fill = "Land-use:Aridity Class") +
  theme_bw() +
  theme(
  text = element_text(family = "Arial", size = 12),  # code to set font family and size
  legend.title = element_text(size = 12)
)
plot_grid(title.2a, plot.slope2b,
          ncol = 1,
          rel_heights = c(0.1, 1),
          align = 'v')


#Result
#2b.A.land-use effect
# In Humid zone: no differences existing between; forests and fallows; however, there are significant differences in the slopes of forests and croplands, and between fallows and croplands
# In Semi-arid zone: no differences existing between forests and fallows, however, there are significant differences in the slopes of forests and croplands, and between fallows and croplands
#write percentages

####Hypothesis 2b.B####
#using_ANOVA_as_the_statistical_test_and_Tukey's_Post-HOC
aov13 <- aov(Slope ~ FAO_classification*Land_use_type, data)
summary(aov13) # interaction of land_use & aridity class (FAO_classification) is not significant

cld_SLOPE_emm_new <- emmeans(object = aov12, pairwise ~ FAO_classification * Land_use_type)
cld_SLOPE_emm_new

cld_new_SLOPE1b <- cld(object = cld_SLOPE_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
cld_new_SLOPE1b$group <- str_trim(cld_new_SLOPE1b$.group)

#Visualizations - Faceted based on Land-use 
plot.slope2.b <- ggplot(data, aes(x= FAO_classification, y=Slope)) +
  geom_boxplot(aes(fill=FAO_classification_LUT1)) +
  facet_grid(~Land_use_type) +
  geom_text(data=cld_new_SLOPE1b, aes(label=group,
                                   y=max(data$Slope)*1.1, x=FAO_classification, angle =0), size = 7,
            inherit.aes = F) +
  scale_fill_manual(values = c("darkgreen", "green3", "steelblue4", "steelblue1", "tan4", "sandybrown"))+
  ylab("SCD slope") + 
  xlab("Aridity class")+
  labs(fill = "Land-use:Aridity Class") +
  theme_bw() +
  theme(
    text = element_text(family = "Arial", size = 12),  # code to set font family and size
    legend.title = element_text(size = 12))

title.2b <- ggdraw() + 
  draw_label(
    "Interactive effects of Climate and Land-use on the health of Tree Populations",
    fontface = 'bold',
    x = 0,
    hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(title.2b, plot.slope2.b,
          ncol = 1,
          rel_heights = c(0.1, 1),
          align = 'v')


#Result
#2b.B.Climate effect
# In Forests: climate has no effect (no differences existing between humid and semi-arid)
# In fallows: climate has no effect (no differences existing between humid and semi-arid)
# In Croplands: climate has no effect (no differences existing between humid and semi-arid)

####Visualizations for Appendix#### 
1-General_effect_across_Land_use_from_both_Aridity_classes{
#using_ANOVA_as_the_statistical_test_and_Tukey's_Post-HOC
#Land use effect(general)
  aov10 <- aov(Slope ~ Land_use_type, data)
  summary(aov10) # effect of land_use is highly significant
  cldLUT_SLOPE_emm_new <- emmeans(object = aov10, pairwise ~ Land_use_type)
  cldLUT_SLOPE_emm_new
  
  cld_new_LUT_SLOPE <- cld(object = cldLUT_SLOPE_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
  cld_new_LUT_SLOPE$group <- str_trim(cld_new_LUT_SLOPE$.group)
  
#Visualizations 
  ggplot(data, aes(x= Land_use_type, y=Slope)) +
    geom_boxplot(aes(fill=Land_use_type)) +
    geom_text(data=cld_new_LUT_SLOPE, aes(label=group,
                                       y=max(data$Slope)*1.1, x=Land_use_type, angle =0), size = 7,
              inherit.aes = F) +
    scale_fill_manual(values = c("olivedrab", "dodgerblue", "goldenrod"))+
    ylab("SCD Slope") + 
    xlab("Land-use intensity")+
    theme_bw() +
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12)
    )
  
  #Result
  #Land-use has an effect on SCD slope (differences existing between forests and croplands and between fallows and croplands across both humid and semi-arid)
  #Increasing land-use intensity negatively influences SCD Slope
}
2-General_effect_across_Aridity_classes_from_all_Land_use_types{
#using_ANOVA_as_the_statistical_test_and_Tukey's_Post-HOC
#Climate effect(general)
  aov11 <- aov(Slope ~ FAO_classification, data)
  summary(aov11) # effect of aridity is not significant
  cldARID_SLOPE_emm_new <- emmeans(object = aov11, pairwise ~ FAO_classification)
  cldARID_SLOPE_emm_new
  
  cld_new_ARID_SLOPE <- cld(object = cldARID_SLOPE_emm_new, adjust = "Tukey", Letters = letters, alpha = 0.05)
  cld_new_ARID_SLOPE$group <- str_trim(cld_new_ARID_SLOPE$.group)
  
#Visualizations 
  ggplot(data, aes(x= FAO_classification, y=Slope)) +
    geom_boxplot(aes(fill=FAO_classification)) +
    geom_text(data=cld_new_ARID_SLOPE, aes(label=group,
                                        y=max(data$Slope)*1.1, x=FAO_classification, angle =0), size = 7,
              inherit.aes = F) +
    scale_fill_manual(values = c("midnightblue", "orangered"))+
    ylab("SCD Slope") + 
    xlab("Aridity class")+
    labs(fill = "Aridity Class") +
    theme_bw() +
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12)
    )  
  #Result
  #Climate has no effect on SCD Slope (no differences existing between Humid and Semi-arid across all Land-use types)
  #Increasing arid conditions does not negatively influences SCD slope. 
}
3-Plotting_Mean_SE_SD{
  data.summary.slope <- data %>%
    group_by(FAO_classification, Land_use_type) %>%
    summarise(
      sd = sd(Slope, na.rm = TRUE),
      len = mean(Slope, na.rm = TRUE),
      se=std.error(Slope, na.rm = TRUE))
  
  f1.slope <- ggplot(data.summary.slope, aes(x=Land_use_type, y=len, ymin=len-se, ymax=len+se, col=FAO_classification, shape= FAO_classification)
  )+facet_grid(~.)
  f1.slope + geom_pointrange(position = dodge) +
    geom_errorbar(aes(ymin = len-se , ymax = len+se), position = dodge, width = 0.25)+
    theme_bw() +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab("SCD slope")+
    xlab("Land-use intensity")+
    labs(fill = "Aridity Class") +
    theme(legend.key.size = unit(1, 'cm'))+
    scale_color_manual(
      values = c( "midnightblue", "orangered")) +
    theme(
      text = element_text(family = "Arial", size = 12),  # code to set font family and size
      legend.title = element_text(size = 12)
    )  
}
ggsave("./GG plot Mean, SD and SE for Shannon diversity.pdf", height = 15, width = 15, units = "cm", dpi = 200)
  
#not relevant to thesis#----
abline(size_mod, lty = "dashed", col = "blue",
       lwd = 2)
avg_data.wet <- filter(avg_data, Climate == "Wet") #mann-whitney test for two levels
avg_data.dry <- filter(avg_data, Climate == "Dry") 
dunnTest(`ln.Individuals/ha` ~ Class, data=avg_data.dry, method="bh")
dunnTest(`ln.Individuals/ha` ~ Land_use_type, data=avg_data.wet, method="bh")

wilcox_test (`ln.Individuals/ha` ~ `Climate`, data=avg_data.cropland, distribution = "exact")
wilcox_test (`ln.Individuals/ha` ~ `Climate`, data=avg_data.fallow, distribution = "exact")
wilcox_test (`ln.Individuals/ha` ~ `Climate`, data=avg_data.forest, distribution = "exact")


citation("BIOMASS")
??BiodiversityR
library(BIOMASS)
citation("ggpubr")
??"BiodiversityR"
citation()
RStudio.Version()
