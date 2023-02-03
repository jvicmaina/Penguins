
library(readxl)
library(ggplot2)
# Load library
library(tidyverse)
#settting working directory

setwd("C:/Users/jvicm/Desktop/New folder/2023") 

data<- read_excel("penguins_2022_S3.xlsx")
data
summary(data)
str(data)
head(data)




# access specific worksheet by name
penguins<- readxl::read_excel("C:/Users/jvicm/Desktop/New folder/2023/penguins_2022_S3.xlsx", sheet = "penguins")
summary(penguins)
head(penguins)

ggplot(penguins, aes(x=Sex, y=`Bill length` , fill=Sex)) +
  geom_boxplot() +
  labs(title="Bill Length by Sex in Pygoscelis penguins", 
       x="Sex", y="Bill Length (mm)") +
  theme_classic()


# Calculate numerical summaries for female bill lengths
female_summaries <- summary(penguins$`Bill length`[penguins$Sex == "female"])
female_summaries_rounded <- round(female_summaries, 2)
head(female_summaries_rounded)

# Calculate numerical summaries for male bill lengths
male_summaries <- summary(penguins$`Bill length`[penguins$Sex == "male"])
male_summaries_rounded <- round(male_summaries, 2)


# Create a table with the values rounded appropriately
summary_table <- data.frame(Sex = c("female", "male"),
                            Minimum = c(female_summaries_rounded[1], male_summaries_rounded[1]),
                            Lower_Quartile = c(female_summaries_rounded[2], male_summaries_rounded[2]),
                            Median = c(female_summaries_rounded[3], male_summaries_rounded[3]),
                            Mean = c(female_summaries_rounded[4], male_summaries_rounded[4]),
                            Upper_Quartile = c(female_summaries_rounded[5], male_summaries_rounded[5]),
                            Maximum = c(female_summaries_rounded[6], male_summaries_rounded[6]),
                            Sample_size = c(length(penguins$`Bill length`[penguins$Sex == "female"]), 
                                            length(penguins$`Bill length`[penguins$Sex == "male"])))
summary_table



# Load library
library(tidyverse)

# Load data
bill_lengths <- data.frame(sex = c(rep("female", 170), rep("male", 172)),
                           length = c(penguins$`Bill length`[penguins$Sex == "female"], 
                                      penguins$`Bill length`[penguins$Sex == "male"]))

# Perform two-sample t-test
t.test(length ~ sex, data = bill_lengths)


#Shapiro test to check  for normality 

# Q-Q Plot for female bill lengths
ggplot(data = bill_lengths[bill_lengths$sex == "female", ], 
       aes(sample = length)) + 
  geom_qq() + 
  ggtitle("Q-Q Plot of Female Bill Lengths")

# Q-Q Plot for male bill lengths
ggplot(data = bill_lengths[bill_lengths$sex == "male", ], 
       aes(sample = length)) + 
  geom_qq() + 
  ggtitle("Q-Q Plot of Male Bill Lengths")


# Shapiro-Wilk test for female bill lengths
shapiro.test(bill_lengths[bill_lengths$sex == "female", ]$length)

# Shapiro-Wilk test for male bill lengths
shapiro.test(bill_lengths[bill_lengths$sex == "male", ]$length)

#Table Part 2

# Load library
library(tidyverse)



# Create a table of counts for species in the sample for each season
species_counts_by_season <- penguins %>% 
  count(Species, Season) %>% 
  spread(Season, n, fill = 0)

species_counts_by_season


# create table of counts
counts <- table(penguins$Species, penguins$Season)

# calculate proportions of species in each season
proportions <- prop.table(counts, margin = 1)
proportions



# Create a table of counts for species in the sample for each season
species_counts_by_season <- penguins %>% 
  count(Species, Season) %>% 
  spread(Season, n, fill = 0)




# Load the ggplot2 library
library(ggplot2)
library(reshape2)
library(tidyr)

# Create a data frame with the species count by season data
species_count_by_season <- data.frame(season = c("season1", "season2", "season3"), 
                                      species_A = c(100, 150, 200), 
                                      species_B = c(50, 75, 100), 
                                      species_C = c(25, 40, 60))

# Melt the data frame to make it suitable for plotting with ggplot2
melted_species_count_by_season <- melt(species_count_by_season, id.vars = "season")

# Plot the stacked bar plot
ggplot(data = melted_species_count_by_season, 
       aes(x = season, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Season", y = "Proportion of Penguins", fill = "Species") + 
  theme(legend.position = "bottom")


# Create a contingency table
contingency_table <- table(penguins$Sex, penguins$`Bill length`)

# Perform the Chi-Squared test
chisq.test(contingency_table)

chisq.test(counts)

residuals <- chisq.test(counts)$residuals

print(residuals)
