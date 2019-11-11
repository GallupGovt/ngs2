# Created by Pablo Diego Rosell, PhD, for Gallup inc. in November 2019

rm(list = ls(all = TRUE))
set.seed(12345)

# Set working directory for latest data
setwd ("C:/Users/C_pablo_diego-rosell/Desktop/Projects/DARPA/Cycle 2/Analytics")

# Load all required libraries

if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(ggplot2, caret)

# Load data
data <- read.csv("game_empanelment.csv")
## Drop cases with NA values for 'innovation'
data <- data[!is.na(data$innovation),]
## Drop cases with NA values for 'Leader'
data <- data[!is.na(data$Leader),]
## Drop unusable variables (PII, variables with lots of "NA" values, no variation)
data <- subset(data, 
               select=-c(h2.2, h2.3, h2.4, h2.5, h2.6, leaderChoice, consumableKey, settingsNum, nid, 
                         EndDate, IPAddress, LocationAccura, Q100, Q101_1, Q101_2, Q101_3, Q101_4, Q102, 
                         Q102_TEXT, Q108, Q109_5_TEXT, Q109_6_TEXT, Q18_TEXT, Q30, Q31, Q43, Q45, Q54, 
                         RecipientEmail, RecipientFirstName, RecipientLastName, ResponseSet,Status, 
                         Q109_2_TEXT, Leader, match_round_id, matchid, Q1,Q10,Q2,Q3,Q40,Q44,Q46,Q5,Q99))

#Define variables as numeric, integer or factor

df <- data.frame(lapply(data, factor))
df$wp16rec <-as.numeric(data$wp16rec)
df$weights <-as.numeric(data$weights)
df$children.total <-as.numeric(data$children.total)
df$adults.total <-as.numeric(data$adults.total)
df$age <-as.numeric(data$age)
df$agesq <-as.numeric(data$agesq)
df$h3rec <-as.numeric(data$h3rec)
df$h5arec <-as.numeric(data$h5arec)
df$ndrinks <-as.numeric(data$ndrinks)
df$d3rec <-as.numeric(data$d3rec)
df$log.popdens <-as.numeric(data$log.popdens)
df$INT_DATE <-as.numeric(data$INT_DATE)
df$HEIGHT <-as.numeric(data$HEIGHT)
df$BMI <-as.numeric(data$BMI)
data<-df
rm(df)

# Score remaining measures (DO THIS!!!)
## Ten-Item Personality Inventory (TIPI)
## Social Dominance Orientation (SDO)
## Social Cognition
## Leadership Style
## Overconfidence
## Rational/Intuitive Decision Style

# Do remaining data transformations
# Split data into training and validation sets (stratify by 'innovation' and experimental conditions)

inBuild <- createDataPartition(y= c(data$innovation, 
                                    data$h1.1,
                                    data$h2.1, 
                                    data$h3.1, 
                                    data$h3.2, 
                                    data$h3.3, 
                                    data$h3.4), 
                                    p=0.80, list=FALSE)
training <- data [inBuild,]
validation <- data [-inBuild,]

# Run descriptive statistics for all vars. Check for approximate balance (training vs validation). 
prop.table(table(training$innovation))
prop.table(table(validation$innovation))

# Select features from 'training' data
### DO THIS!!!

# Run several machine learning vs. traditional approaches
### DO THIS!!!

# Relaimpo calculations
# Evaluate predictive model with 'validation' data
### DO THIS!!!









