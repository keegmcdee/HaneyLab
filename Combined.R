#KJM May/30/2024
#Haney Lab
#Growth Curves for experiments - Combined 
#trying to get single strain output from all combined expts. 

library(gcplyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

#set working directory
setwd("~/R_HaneyLab")

#Read in files -> All Denoised!!
A <- read.csv(file = "FinalData/final_datA.csv")#removed invalid data NO CTRL
B <- read.csv(file = "FinalData/final_datB.csv")
C <- read.csv(file = "FinalData/final_datC.csv")
D <- read.csv(file = "FinalData/final_datD.csv")#Data is kinda crazy lookin
E <- read.csv(file = "FinalData/final_datE.csv")
F <- read.csv(file = "FinalData/final_datF.csv")#some strains ungrown
G <- read.csv(file = "FinalData/final_datG.csv")
H <- read.csv(file = "FinalData/final_datH.csv")
#I <- read.csv(file = "FinalData/final_datI.csv")#All LB no Ctrl
J <- read.csv(file = "FinalData/final_datJ.csv")#Batch 1

#Merge all csvs into one list

# Plotting
#ggplot(J, 
#       aes(x = Time, y = Measurements, colour = Growth_Media))+
#  geom_point()+
#  facet_wrap(~Growth_Media)

all_dat <- rbind(A, B) %>%
  rbind(C) %>%
  rbind(D) %>%
  rbind(E) %>%
  rbind(F) %>%
  rbind(G) %>%
  rbind(H) %>%
  rbind(J)


#Removing N/As (times that go past 24hrs messes with lubridate fn)
final_dat_nona <- na.omit(all_dat)

#For loop that standardizes all PA01 into the same Strain Name
for (i in 1:nrow(final_dat_nona)) {
  if (final_dat_nona$Strain_Names[i] == "PAO1" || final_dat_nona$Strain_Names[i] == "PA01") {
    final_dat_nona$Strain_Names[i] <- "PAO1"
  }
}

for (i in 1:nrow(final_dat_nona)) {
  if (final_dat_nona$Strain_Names[i] == "PhzA" || final_dat_nona$Strain_Names[i] == "phzA") {
    final_dat_nona$Strain_Names[i] <- "PhzA2"
  }
}

for (i in 1:nrow(final_dat_nona)) {
  if (final_dat_nona$Strain_Names[i] == "PhzB" || final_dat_nona$Strain_Names[i] == "phzB") {
    final_dat_nona$Strain_Names[i] <- "PhzB2"
  }
}
for (i in 1:nrow(final_dat_nona)) {
  if (final_dat_nona$Strain_Names[i] == "GacA" || final_dat_nona$Strain_Names[i] == "gacA") {
    final_dat_nona$Strain_Names[i] <- "GacA"
  }
}
for (i in 1:nrow(final_dat_nona)) {
  if (final_dat_nona$Strain_Names[i] == "PAO1" || final_dat_nona$Strain_Names[i] == "H103") {
    final_dat_nona$Strain_Names[i] <- "PAO1"
  }
}


colnames(final_dat_nona)
strains <- unique(final_dat_nona[6])

#Combined batches (PhzB2, mexB, hcnB just from batch 2)
#Filter to create Batch1 and Batch2
batch1_dat <- filter(final_dat_nona,
                     Growth_Media!="LB",
                     !Strain_Names %in% c("PhzB2", "hcnB", "mexB"),
                     !file %in% c("GC_J/J1", "GC_J/J2")
                     )
batch2_dat <- filter(final_dat_nona,
                     Growth_Media!="LB",
                     file %in% c("GC_J/J1", "GC_J/J2")
                     )

comb_dat <- rbind(batch1_dat, batch2_dat)

all_strains <- c("PAO1","tssM", "tse5", "pvdF", "PhzA2", "PhzB2", 
                 "mexB", "GacA", "fetA","MPAO1","H103", 
                 "PhzA1","PhzB1", "hcnB","PA3836","PA5506","PA5170")
strains1 <- c("PAO1","tssM", "tse5", "pvdF",  
              "mexB", "hcnB", "GacA", "fetA")
strains2 <- c("MPAO1","PAO1","PhzA1","PhzB1",
              "PhzB2", "PhzA2")
strainsM <- c("MPAO1","PAO1","PA3836","PA5506","PA5170")

#Strain Filtering
comb_filtered_final <- filter(batch2_dat, 
                              Strain_Names%in%c("PAO1", "PhzA1","PhzB1",
                                                "PhzB2", "hcnB","mexB")
                              )

#c("MPAO1","H103","PAO1")
#c("MPAO1","H103","PA3836","PA5506","PA5170")

#Batch 1 Strains
#c("PAO1","tssM", "tse5", "pvdF", "PhzA2", "PhzB2", "mexB", "hcnB", "GacA", "fetA")
#Batch 2 Strains
#c("MPAO1","H103", "PhzA1","PhzB1","PhzB2", "hcnB","mexB","PA3836","PA5506","PA5170")

##Colours
# Generate a base color palette for all strains
plot_strains <- unique(comb_filtered_final$Strain_Names)
base_palette <- setNames(brewer.pal(length(plot_strains), "Set1"), plot_strains)

# Selectively override specific strain colors
custom_colors <- c(
  "PAO1" = "skyblue"
)

# Merge the custom colors with the base palette (custom overrides base)
final_colors <- c(base_palette, custom_colors)

# Ensure that custom colors override the base palette
final_colors <- final_colors[!duplicated(names(final_colors), fromLast = TRUE)]

#Summarzing Data
#This also combines by timepoint!
comb_sum_final <- comb_filtered_final %>%
  group_by(Time, Growth_Media, Strain_Names) %>%
  summarise( 
    n=n(),
    mean=mean(Measurements),
    sd=sd(Measurements)
  )
comb_sum_final
# Plotting

ggplot(comb_sum_final, 
       aes(x = Time, y = mean, colour = Strain_Names)) +
  geom_point() +
  geom_line() +
  geom_errorbar(data=comb_sum_final, 
                aes(x=Time,ymin=mean-sd, ymax=mean+sd),
                width=0.4, alpha=0.4, linewidth=0.5) +
  scale_color_manual(values = final_colors) +
  facet_wrap(~Growth_Media)+
#  ggtitle("Growth Curves of",paste(comb_sum_final[1,3]))+
  labs(y = "OD600", x = "Time(Hrs)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),  # Invert background color
  panel.grid = element_line(colour = "grey")
  )


###Tasks
#remove outliers, average Data -> ppt
#STANDARDIZE


####Code Dump####
#Takes in all measurements and gives averages as combined by timepoint
#mean_data <- aggregate(my_sum_final$mean, 
#          list(my_sum_final$Time), FUN = mean)

  