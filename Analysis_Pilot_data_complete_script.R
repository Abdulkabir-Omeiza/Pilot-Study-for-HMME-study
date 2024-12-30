#Packages
install.packages("dplyr")
install.packages("gganimate")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("ggpubr")
install.packages("car") 
install.packages("multcompView")
install.packages("readxl")

#Load packages
library(dplyr)
library(ggplot2)
library(gganimate)
library(readxl)
library(car)
library(ggpubr)
library(ggplot2)
library(tidyr)
library(ggsignif)
library(dplyr)
library(multcompView)
        
####Import data
df <- read.csv("C:/Users/AAO/Desktop/PhD/Abdul_Arabidopsis_pilot_data/Arabidopsis_growth_Soil_Abdulkabir_20241111_PlantEye_Edited.csv")
View(df)

###Data Cleaning
##select columns of interest
df1 <- df %>% select("Day", "treatment" , "Projected.Leaf.Area.mm.","NDVI.Average", "Digital.Biomass.mm." ) 
View(df1)

##sort columns by the days
df1 <- df1[order(df1$Day),]
df1 <- rename(df$treatment, BP1 = "BP1")

##Rename replicate treatments with single treatment name
##\\.. matches the dot and .* matches character after the dot and \\d+ matches one or more digits

patterns <- c("^BP1\\..*", "^BP2\\..*", "^BP3\\..*", "^KS1\\..*", "^KS2\\..*", "^KS3\\..*", 
              "^KSC\\d+", "^BPC\\d+", "^BC\\d+")
replacements <- c("BP1", "BP2", "BP3", "KS1", "KS2", "KS3", "KSC", "BPC", "BC")

for (i in seq_along(patterns)) {
  df1$treatment <- gsub(patterns[i], replacements[i], df1$treatment)
}

##Replace na with 0
df2 <- replace(df1, is.na(df1), 0)
View(df2)

##Remove rows with "none"
df3 <- df2[-which(df2$treatment == "none"),] 


###Data Analysis
##Calculate mean of replicate treatment for the different days

df4 <- df3 %>% group_by(Day, treatment) %>%
  summarise(mean_projected_leaf_Area = mean(Projected.Leaf.Area.mm.),
            mean_NDVI = mean(NDVI.Average), mean_digital_biomass = mean(Digital.Biomass.mm.),
            .groups = 'drop') %>%
  as.data.frame()
df4

View(df4)

##Analyse Projected Leaf Area data
##Plot
df_plot1 <- ggplot (df4, aes(x = Day, y = mean_projected_leaf_Area))+
  geom_line(aes(color = treatment), linewidth = 1) +
  geom_point()+
  theme_minimal()

df_plot1 + scale_x_continuous(breaks = c(19,21,23,26)) + labs(title = "Projected Leaf Area per treatment")+ 
  xlab ("Day") + ylab ("Projected Leaf Area") + theme(plot.title = element_text(hjust = 0.5))



##Analyse NDVI.Average
##Plot
df_plot2 <- ggplot (df4, aes(x = Day, y = mean_NDVI))+
  geom_line(aes(color = treatment), linewidth = 1) +
  geom_point()+
  theme_minimal()

df_plot2 + scale_x_continuous(breaks = c(19,21,23,26)) + labs(title = "NDVI per treatment")+ 
  xlab ("Day") + ylab ("NDVI") + theme(plot.title = element_text(hjust = 0.5))





##Analyse digital_biomass
##Plot
df_plot3 <- ggplot (df4, aes(x = Day, y = mean_digital_biomass))+
  geom_line(aes(color = treatment), linewidth = 1) +
  geom_point()+
  theme_minimal()

df_plot3 + scale_x_continuous(breaks = c(19,21,23,26)) + labs(title = "digital_biomass per treatment")+ 
  xlab ("Day") + ylab ("digital_biomass") + theme(plot.title = element_text(hjust = 0.5))



##Bar plot
df_plot4 <- ggplot(df4, aes(x = treatment, y = mean_digital_biomass))+
  geom_bar(stat = "identity")

df_plot4

###Data Analysis by days
######################Subset for day 19

df5 <- df3[df3$Day == 19, ]
df5

##test for normality
shapiro_test_results <- df5 %>%
  group_by(treatment)%>%
  summarise(Shapiro_p_value = shapiro.test(Projected.Leaf.Area.mm.)$p.value)
print(shapiro_test_results)


#perform Anova to compare means across different treatment groups (normal distribution is assumed)
res.aov <- aov(Projected.Leaf.Area.mm. ~ treatment, data = df5)
summary(res.aov)


#Perform Tukey HSD test to ssee which of the means are statistically different
TUKEY = TukeyHSD(res.aov)

##Plot the confidence level
plot(TUKEY, las=1, col = 'brown')

#compact letter display
cld <- multcompLetters4(res.aov, TUKEY)
print(cld)

#Table with factors and 3rd quantile
Tk <- group_by(df5, treatment) %>%
  summarise(mean=mean(Projected.Leaf.Area.mm.), quant = quantile(Projected.Leaf.Area.mm., probs =0.75)) %>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the TK table
cld <- as.data.frame.list(cld$treatment)
Tk$cld <- cld$Letters
print(Tk)

#Construct box plot of measured projected leaf area for each treatment
data_boxplot <- ggplot(df5, aes(x = treatment, y = Projected.Leaf.Area.mm.))+
  geom_boxplot(aes(fill = treatment)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +  
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(data = Tk, aes(label = cld, x = treatment, y = quant), vjust = -1, hjust = -1, size = 3) +
  labs(title = "Projected Leaf Area at Day 19", x = "Treatments", y = " Projected Leaf Area") 
data_boxplot


######################Subset for day 21

df6 <- df3[df3$Day == 21, ]
View(df6)

##test for normality
shapiro_test_results2 <- df6 %>%
  group_by(treatment)%>%
  summarise(Shapiro_p_value = shapiro.test(Projected.Leaf.Area.mm.)$p.value)
print(shapiro_test_results2)


#perform Anova to compare means across different treatment groups (normal distribution is assumed)
res.aov2 <- aov(Projected.Leaf.Area.mm. ~ treatment, data = df6)
summary(res.aov2)


#Perform Tukey HSD test to ssee which of the means are statistically different
TUKEY2 = TukeyHSD(res.aov2)

##Plot the confidence level
plot(TUKEY2, las=1, col = 'brown')

#compact letter display
cld2 <- multcompLetters4(res.aov2, TUKEY2)
print(cld2)

#Table with factors and 3rd quantile
Tk2 <- group_by(df6, treatment) %>%
  summarise(mean=mean(Projected.Leaf.Area.mm.), quant = quantile(Projected.Leaf.Area.mm., probs =0.75)) %>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the TK table
cld2 <- as.data.frame.list(cld2$treatment)
Tk2$cld2 <- cld2$Letters
print(Tk2)

#Construct box plot of measured projected leaf area for each treatment
data_boxplot2 <- ggplot(df6, aes(x = treatment, y = Projected.Leaf.Area.mm.))+
  geom_boxplot(aes(fill = treatment)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +  
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(data = Tk2, aes(label = cld2, x = treatment, y = quant), vjust = -1, hjust = -1, size = 3) +
  labs(title = "Projected Leaf Area at Day 21", x = "Treatments", y = " Projected Leaf Area") 
data_boxplot2


######################Subset for day 23

df7 <- df3[df3$Day == 23, ]
df7

##test for normality
shapiro_test_results3 <- df7 %>%
  group_by(treatment)%>%
  summarise(Shapiro_p_value = shapiro.test(Projected.Leaf.Area.mm.)$p.value)
print(shapiro_test_results3)


#perform Anova to compare means across different treatment groups (normal distribution is assumed)
res.aov3 <- aov(Projected.Leaf.Area.mm. ~ treatment, data = df7)
summary(res.aov3)


#Perform Tukey HSD test to ssee which of the means are statistically different
TUKEY3 = TukeyHSD(res.aov3)

##Plot the confidence level
plot(TUKEY3, las=1, col = 'brown')

#compact letter display
cld3 <- multcompLetters4(res.aov3, TUKEY3)
print(cld3)

#Table with factors and 3rd quantile
Tk3 <- group_by(df7, treatment) %>%
  summarise(mean=mean(Projected.Leaf.Area.mm.), quant = quantile(Projected.Leaf.Area.mm., probs =0.75)) %>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the TK table
cld3 <- as.data.frame.list(cld3$treatment)
Tk3$cld3 <- cld3$Letters
print(Tk3)

#Construct box plot of measured projected leaf area for each treatment
data_boxplot3 <- ggplot(df7, aes(x = treatment, y = Projected.Leaf.Area.mm.))+
  geom_boxplot(aes(fill = treatment)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +  
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(data = Tk3, aes(label = cld3, x = treatment, y = quant), vjust = -1, hjust = -1, size = 3) +
  labs(title = "Projected Leaf Area at Day 23", x = "Treatments", y = " Projected Leaf Area") 
data_boxplot3


######################Subset for day 26

df8 <- df3[df3$Day == 26, ]
df8

##test for normality
shapiro_test_results4 <- df8 %>%
  group_by(treatment)%>%
  summarise(Shapiro_p_value = shapiro.test(Projected.Leaf.Area.mm.)$p.value)
print(shapiro_test_results4)


#perform Anova to compare means across different treatment groups (normal distribution is assumed)
res.aov4 <- aov(Projected.Leaf.Area.mm. ~ treatment, data = df8)
summary(res.aov4)


#Perform Tukey HSD test to ssee which of the means are statistically different
TUKEY4 = TukeyHSD(res.aov4)

##Plot the confidence level
plot(TUKEY4, las=1, col = 'brown')

#compact letter display
cld4 <- multcompLetters4(res.aov4, TUKEY4)
print(cld4)

#Table with factors and 3rd quantile
Tk4 <- group_by(df8, treatment) %>%
  summarise(mean=mean(Projected.Leaf.Area.mm.), quant = quantile(Projected.Leaf.Area.mm., probs =0.75)) %>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the TK table
cld4 <- as.data.frame.list(cld4$treatment)
Tk4$cld4 <- cld4$Letters
print(Tk4)

#Construct box plot of measured projected leaf area for each treatment
data_boxplot4 <- ggplot(df8, aes(x = treatment, y = Projected.Leaf.Area.mm.))+
  geom_boxplot(aes(fill = treatment)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +  
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(data = Tk4, aes(label = cld4, x = treatment, y = quant), vjust = -1, hjust = -1, size = 3) +
  labs(title = "Projected Leaf Area at Day 26", x = "Treatments", y = " Projected Leaf Area") 
data_boxplot4

View(df8)



### Data Visualisation
##Bar Plot the for projected leaf area by treatment and day using ggplot2
df4$Day <- as.factor(df4$Day)
ggplot(df4, aes(x = Day, y = mean_projected_leaf_Area, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Projected Leaf Area by Treatment and Day",
    x = "Day",
    y = "Projected Leaf Area",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

##Bar Plot the for NDVI by treatment and day using ggplot2
ggplot(df4, aes(x = Day, y = mean_NDVI, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Normalized Difference Vegetation Index (NDVI) by Treatment and Day",
    x = "Day",
    y = "NDVI",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


##Bar Plot the for Digital Biomass by treatment and day using ggplot2
ggplot(df4, aes(x = Day, y = mean_digital_biomass, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Digital Biomass by Treatment and Day",
    x = "Day",
    y = "Digital Biomass",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


write.csv(df4, "C:/Users/AAO/Desktop/PhD/Abdul_Arabidopsis_pilot_data/Analysis_Pilot_Project/summary_data.csv")
