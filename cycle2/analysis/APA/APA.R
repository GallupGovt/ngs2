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

#Define variables as numeric, integer or factor

#df <- data.frame(lapply(data, factor))
#df$wp16rec <-as.numeric(data$wp16rec)

# Score remaining measures (DO THIS!!!)
## Life Evaluations 
data$life_today <- data$Q106_1
data$life_5year <- data$Q106_2

## Ten-Item Personality Inventory (TIPI): https://gosling.psy.utexas.edu/wp-content/uploads/2014/09/tipi.pdf
data$tipi_extra <- data$Q6_1 + (abs(data$Q6_6 - 7)+1) # extraversion
data$tipi_agree <- data$Q6_7 + (abs(data$Q6_2 - 7)+1) # agreeableness
data$tipi_consc <- data$Q6_3 + (abs(data$Q6_8 - 7)+1) # conscientiousness
data$tipi_emost <- data$Q6_9 + (abs(data$Q6_4 - 7)+1) # emotional stability
data$tipi_opexp <- data$Q6_5 + (abs(data$Q6_10 - 7)+1) # openness to experience

## Leadership Style (GTL)
data$gtl <- data$Q107_1+data$Q107_2+data$Q107_3+data$Q107_4+data$Q107_5+data$Q107_6+data$Q107_8

## Overconfidence: http://www.econ.uzh.ch/static/wp/econwp106.pdf
# Miscalibration score: percentage of answers lying inside the confidence interval (True Answers in parantheses)
#1. Martin Luther King's age at death (39).
#table(data$Q55.1_1_2_TEXT)
data$Q55.1_1_2_TEXT <- gsub("40's", "45", data$Q55.1_2_1_TEXT)
data$Q55.1_1_2_INT <- as.numeric(data$Q55.1_1_2_TEXT)
#
mlkdeath <- 39
data$mlk [data$Q55.1_1_1_TEXT <= mlkdeath & data$Q55.1_1_2_INT >= mlkdeath] <- 1
data$mlk [data$Q55.1_1_1_TEXT > mlkdeath | data$Q55.1_1_2_INT < mlkdeath] <- 0

#2. Length (in miles) of the Nile River (4145).
#First clean data
#table(data$Q55.1_2_1_TEXT)
data$Q55.1_2_1_TEXT <- gsub(",", "", data$Q55.1_2_1_TEXT)
data$Q55.1_2_1_TEXT <- gsub(" miles", "", data$Q55.1_2_1_TEXT)
data$Q55.1_2_1_INT <- as.numeric(data$Q55.1_2_1_TEXT)
data$Q55.1_2_2_TEXT <- gsub(",", "", data$Q55.1_2_2_TEXT)
data$Q55.1_2_2_TEXT <- gsub(" miles", "", data$Q55.1_2_2_TEXT)
data$Q55.1_2_2_INT <- as.numeric(data$Q55.1_2_2_TEXT)
#
nilemiles <- 4145
data$nile [data$Q55.1_2_1_INT <= nilemiles & data$Q55.1_2_2_INT >= nilemiles] <- 1
data$nile [data$Q55.1_2_1_INT> nilemiles | data$Q55.1_2_2_INT < nilemiles] <- 0

#3. Number of countries that are members of the United Nations (193).
#table(data$Q55.1_3_2_TEXT)
data$Q55.1_3_2_TEXT <- gsub("140-150", "145", data$Q55.1_3_2_TEXT)
data$Q55.1_3_2_INT <- as.numeric(data$Q55.1_3_2_TEXT)

uncountries <- 193
data$un [data$Q55.1_3_1_TEXT <= uncountries & data$Q55.1_3_2_INT >= uncountries] <- 1
data$un [data$Q55.1_3_1_TEXT > uncountries | data$Q55.1_3_2_INT < uncountries] <- 0

#4. Number of books in the Old Testament (5).
#table(data$Q55.1_4_1_TEXT)
books <- 5
data$books [data$Q55.1_4_1_TEXT <= books & data$Q55.1_4_2_TEXT >= books] <- 1
data$books [data$Q55.1_4_1_TEXT > books | data$Q55.1_4_2_TEXT < books] <- 0

#5. Weight (in pounds) of an empty Boeing 747 (398794).
data$Q55.1_5_1_TEXT <- gsub(",", "", data$Q55.1_5_1_TEXT)
data$Q55.1_5_1_TEXT <- gsub("\\.", "", data$Q55.1_5_1_TEXT)
data$Q55.1_5_1_TEXT <- gsub("1.5 ton", "3000", data$Q55.1_5_1_TEXT)
data$Q55.1_5_1_TEXT <- gsub("6 tons", "12000", data$Q55.1_5_1_TEXT)
data$Q55.1_5_1_TEXT <- gsub("10 tons", "20000", data$Q55.1_5_1_TEXT)
data$Q55.1_5_1_TEXT <- gsub("15 ton", "30000", data$Q55.1_5_1_TEXT)
data$Q55.1_5_1_TEXT <- gsub("150 tons", "300000", data$Q55.1_5_1_TEXT)
data$Q55.1_5_1_TEXT <- gsub("What\\?", "", data$Q55.1_5_1_TEXT)
data$Q55.1_5_1_INT <- as.numeric(data$Q55.1_5_1_TEXT)

data$Q55.1_5_2_TEXT <- gsub(",", "", data$Q55.1_5_2_TEXT)
data$Q55.1_5_2_TEXT <- gsub("\\.", "", data$Q55.1_5_2_TEXT)
data$Q55.1_5_2_TEXT <- gsub("Weird questions", "", data$Q55.1_5_2_TEXT)
data$Q55.1_5_2_TEXT <- gsub("10 tons", "20000", data$Q55.1_5_2_TEXT)
data$Q55.1_5_2_TEXT <- gsub("2 tons", "4000", data$Q55.1_5_2_TEXT)
data$Q55.1_5_2_TEXT <- gsub("25 tons", "50000", data$Q55.1_5_2_TEXT)
data$Q55.1_5_2_TEXT <- gsub("25 ton", "50000", data$Q55.1_5_2_TEXT)
data$Q55.1_5_2_TEXT <- gsub("250 tons", "500000", data$Q55.1_5_2_TEXT)
data$Q55.1_5_2_INT <- as.numeric(data$Q55.1_5_2_TEXT)

boeing <- 398794
data$boeing [data$Q55.1_5_1_INT <= boeing & data$Q55.1_5_2_INT >= boeing] <- 1
data$boeing [data$Q55.1_5_1_INT > boeing | data$Q55.1_5_2_INT < boeing] <- 0

#6. Year in which J.S. Bach was born (1685).

data$Q55.1_6_1_TEXT <- gsub("3/31 1685", "1685", data$Q55.1_6_1_TEXT)
data$Q55.1_6_1_INT <- as.numeric(data$Q55.1_6_1_TEXT)

data$Q55.1_6_2_TEXT <- gsub("3/31/1685", "1685", data$Q55.1_6_2_TEXT)
data$Q55.1_6_2_TEXT <- gsub("3-31-1685", "1685", data$Q55.1_6_2_TEXT)
data$Q55.1_6_2_TEXT <- gsub("16 -17 century", "1550", data$Q55.1_6_2_TEXT)
data$Q55.1_6_2_INT <- as.numeric(data$Q55.1_6_2_TEXT)

bach <- 1685
data$bach [data$Q55.1_6_1_INT <= bach & data$Q55.1_6_2_INT >= bach] <- 1
data$bach [data$Q55.1_6_1_INT > bach | data$Q55.1_6_2_INT < bach] <- 0

#7. Gestation period (in days) of an Asian elephant (645).

data$Q55.1_7_1_TEXT <- gsub("1.5 years", "548", data$Q55.1_7_1_TEXT)
data$Q55.1_7_1_TEXT <- gsub("2 Years", "730", data$Q55.1_7_1_TEXT)
data$Q55.1_7_1_TEXT <- gsub("2years", "730", data$Q55.1_7_1_TEXT)
data$Q55.1_7_1_TEXT <- gsub("1yr No idea", "365", data$Q55.1_7_1_TEXT)
data$Q55.1_7_1_TEXT <- gsub("9 mnths", "274", data$Q55.1_7_1_TEXT)
data$Q55.1_7_1_INT <- as.numeric(data$Q55.1_7_1_TEXT)

data$Q55.1_7_2_TEXT <- gsub(",", "", data$Q55.1_7_2_TEXT)
data$Q55.1_7_2_TEXT <- gsub("2years", "730", data$Q55.1_7_2_TEXT)
data$Q55.1_7_2_TEXT <- gsub("2 Years", "730", data$Q55.1_7_2_TEXT)
data$Q55.1_7_2_TEXT <- gsub("2 years", "730", data$Q55.1_7_2_TEXT)
data$Q55.1_7_2_TEXT <- gsub("2.5 years", "913", data$Q55.1_7_2_TEXT)
data$Q55.1_7_2_TEXT <- gsub("16 mnths", "487", data$Q55.1_7_2_TEXT)
data$Q55.1_7_2_TEXT <- gsub("3yrs couldnâ???Tt care less", "1095", data$Q55.1_7_2_TEXT)
data$Q55.1_7_2_TEXT <- gsub("4years", "1460", data$Q55.1_7_2_TEXT)
data$Q55.1_7_2_INT <- as.numeric(data$Q55.1_7_2_TEXT)

elefant <- 645
data$elefant [data$Q55.1_7_1_INT <= elefant & data$Q55.1_7_2_INT >= elefant] <- 1
data$elefant [data$Q55.1_7_1_INT > elefant | data$Q55.1_7_2_INT < elefant] <- 0

#8. Diameter (in miles) of the moon (2159).

data$Q55.1_8_1_TEXT <- gsub(",", "", data$Q55.1_8_1_TEXT)
data$Q55.1_8_1_TEXT <- gsub("2.5 million", "2500000", data$Q55.1_8_1_TEXT)
data$Q55.1_8_1_TEXT <- gsub("2.5lion", "2500000", data$Q55.1_8_1_TEXT)
data$Q55.1_8_1_TEXT <- gsub(" miles", "", data$Q55.1_8_1_TEXT)
data$Q55.1_8_1_TEXT <- gsub(" mil", "", data$Q55.1_8_1_TEXT)
data$Q55.1_8_1_TEXT <- gsub("\\?", "", data$Q55.1_8_1_TEXT)
data$Q55.1_8_1_INT <- as.numeric(data$Q55.1_8_1_TEXT)

data$Q55.1_8_2_TEXT <- gsub(",", "", data$Q55.1_8_2_TEXT)
data$Q55.1_8_2_TEXT <- gsub(" miles", "", data$Q55.1_8_2_TEXT)
data$Q55.1_8_2_TEXT <- gsub("6.5 million", "6500000", data$Q55.1_8_2_TEXT)
data$Q55.1_8_2_TEXT <- gsub(" mil", "", data$Q55.1_8_2_TEXT)
data$Q55.1_8_2_TEXT <- gsub("\\?", "", data$Q55.1_8_2_TEXT)
data$Q55.1_8_2_INT <- as.numeric(data$Q55.1_8_2_TEXT)

moon <- 2159
data$moon [data$Q55.1_8_1_INT <= moon & data$Q55.1_8_2_INT >= moon] <- 1
data$moon [data$Q55.1_8_1_INT > moon | data$Q55.1_8_2_INT < moon] <- 0

#9. Air distance (in kilometers) from London to Tokyo (9550).

data$Q55.1_9_1_TEXT <- gsub(",", "", data$Q55.1_9_1_TEXT)
data$Q55.1_9_1_TEXT <- gsub("I don't", "", data$Q55.1_9_1_TEXT)
data$Q55.1_9_1_TEXT <- gsub("\\?", "", data$Q55.1_9_1_TEXT)
data$Q55.1_9_1_TEXT <- gsub("2500 miles", "4023", data$Q55.1_9_1_TEXT)
data$Q55.1_9_1_INT <- as.numeric(data$Q55.1_9_1_TEXT)

data$Q55.1_9_2_TEXT <- gsub(",", "", data$Q55.1_9_2_TEXT)
data$Q55.1_9_2_TEXT <- gsub("\\?", "", data$Q55.1_9_2_TEXT)
data$Q55.1_9_2_TEXT <- gsub("know how big a kilometer is tbh", "", data$Q55.1_9_2_TEXT)
data$Q55.1_9_2_TEXT <- gsub("3500 miles", "5634", data$Q55.1_9_2_TEXT)
data$Q55.1_9_2_TEXT <- gsub("4 -5000", "", data$Q55.1_9_2_TEXT)
data$Q55.1_9_2_INT <- as.numeric(data$Q55.1_9_2_TEXT)

tokyo <- 9550
data$tokyo [data$Q55.1_9_1_INT <= tokyo & data$Q55.1_9_2_INT >= tokyo] <- 1
data$tokyo [data$Q55.1_9_1_INT > tokyo | data$Q55.1_9_2_INT < tokyo] <- 0

#10. Deepest known point (in feet) in the oceans (36201).

data$Q55.1_10_1_TEXT <- gsub(",", "", data$Q55.1_10_1_TEXT)
data$Q55.1_10_1_TEXT <- gsub("No idea", "", data$Q55.1_10_1_TEXT)
data$Q55.1_10_1_TEXT <- gsub("1 mile", "5280", data$Q55.1_10_1_TEXT)
data$Q55.1_10_1_TEXT <- gsub("2miles", "10560", data$Q55.1_10_1_TEXT)
data$Q55.1_10_1_TEXT <- gsub("2000 miles", "10560000", data$Q55.1_10_1_TEXT)
data$Q55.1_10_1_TEXT <- gsub("1000 -1500", "1250", data$Q55.1_10_1_TEXT)
data$Q55.1_10_1_INT <- as.numeric(data$Q55.1_10_1_TEXT)

data$Q55.1_10_2_TEXT <- gsub(",", "", data$Q55.1_10_2_TEXT)
data$Q55.1_10_2_TEXT <- gsub("No idea", "", data$Q55.1_10_2_TEXT)
data$Q55.1_10_2_TEXT <- gsub("1.5 miles", "7920", data$Q55.1_10_2_TEXT)
data$Q55.1_10_2_TEXT <- gsub("3miles", "15840", data$Q55.1_10_2_TEXT)
data$Q55.1_10_2_TEXT <- gsub("3000 miles", "15840000", data$Q55.1_10_2_TEXT)
data$Q55.1_10_2_TEXT <- gsub("1500p", "1500", data$Q55.1_10_2_TEXT)
data$Q55.1_10_2_INT <- as.numeric(data$Q55.1_10_2_TEXT)

ocean <- 36201
data$ocean [data$Q55.1_10_1_INT <= ocean & data$Q55.1_10_2_INT >= ocean] <- 1
data$ocean [data$Q55.1_10_1_INT > ocean | data$Q55.1_10_2_INT < ocean] <- 0

data$overconfidence <- rowMeans(
  data[c("mlk","un","nile","books","ocean","tokyo","elefant","bach","boeing","moon")])

## Social Cognition: Reading the mind in the eyes
#Correct responses
data$Q90_cat [data$Q90==1] <- 1
data$Q96_cat [data$Q96==2] <- 1
data$Q86_cat [data$Q86==3] <- 1
data$Q87_cat [data$Q87==2] <- 1
data$Q82_cat [data$Q82==3] <- 1
data$Q59_cat [data$Q59==2] <- 1
data$Q63_cat [data$Q63==3] <- 1
data$Q72_cat [data$Q72==2] <- 1
data$Q62_cat [data$Q62==4] <- 1
data$Q67_cat [data$Q67==1] <- 1
data$Q95_cat [data$Q95==3] <- 1
data$Q79_cat [data$Q79==3] <- 1
data$Q71_cat [data$Q71==2] <- 1
data$Q81_cat [data$Q81==4] <- 1
data$Q68_cat [data$Q68==1] <- 1
data$Q83_cat [data$Q83==2] <- 1
data$Q74_cat [data$Q74==1] <- 1
data$Q70_cat [data$Q70==1] <- 1
#Incorrect responses
data$Q90_cat [data$Q90!=1] <- 0
data$Q96_cat [data$Q96!=2] <- 0
data$Q86_cat [data$Q86!=3] <- 0
data$Q87_cat [data$Q87!=2] <- 0
data$Q82_cat [data$Q82!=3] <- 0
data$Q59_cat [data$Q59!=2] <- 0
data$Q63_cat [data$Q63!=3] <- 0
data$Q72_cat [data$Q72!=2] <- 0
data$Q62_cat [data$Q62!=4] <- 0
data$Q67_cat [data$Q67!=1] <- 0
data$Q95_cat [data$Q95!=3] <- 0
data$Q79_cat [data$Q79!=3] <- 0
data$Q71_cat [data$Q71!=2] <- 0
data$Q81_cat [data$Q81!=4] <- 0
data$Q68_cat [data$Q68!=1] <- 0
data$Q83_cat [data$Q83!=2] <- 0
data$Q74_cat [data$Q74!=1] <- 0
data$Q70_cat [data$Q70!=1] <- 0

data$eyes <- rowMeans(
  data[c("Q90_cat","Q96_cat","Q86_cat","Q87_cat","Q82_cat","Q59_cat","Q63_cat","Q72_cat","Q62_cat",
         "Q67_cat","Q95_cat","Q79_cat","Q71_cat","Q81_cat","Q68_cat","Q83_cat","Q74_cat","Q70_cat")])

## Rational/Intuitive Decision Style: http://www.sjdm.org/dmidi/files/Decision_Styles_Scale_Items.pdf
data$rational <- rowMeans(data[c("Q56_1","Q56_2","Q56_3","Q56_4","Q56_5")])
data$intuitive <- rowMeans(data[c("Q56_6","Q56_7","Q56_8","Q56_9","Q56_10")])

# Internet use
data$smartphone <- abs(data$Q32_1-9)
data$computer <- abs(data$Q32_2-9)
data$tablet <- abs(data$Q32_3-9)

# Set categorical vs. numeric

df <- data.frame(lapply(data, factor))
df$CSE <-as.numeric(data$CSE)
df$nConnected <-as.numeric(data$nConnected)
df$LocationLatitude <-as.numeric(data$LocationLatitude)
df$LocationLongitude <-as.numeric(data$LocationLongitude)
df$Q11 <-as.numeric(data$Q11)
df$Q15 <-as.numeric(data$Q15)
df$Q25 <-as.numeric(data$Q25)
df$Q26 <-as.numeric(data$Q26)
df$Q28 <-as.numeric(data$Q28)
df$Q34 <-as.numeric(data$Q34)
df$Q36 <-as.numeric(data$Q36)
df$Q37 <-as.numeric(data$Q37)
df$tol_ambiguity <-as.numeric(data$tol_ambiguity)
df$trans_leadership_sum <-as.numeric(data$trans_leadership_sum)
df$trans_leadership_mean <-as.numeric(data$trans_leadership_mean)
df$trans_leadership_mean <-as.numeric(data$trans_leadership_mean)
df$life_today <-as.numeric(data$life_today)
df$life_5year <-as.numeric(data$life_5year)
df$tipi_extra <-as.numeric(data$tipi_extra)
df$tipi_agree <-as.numeric(data$tipi_agree)
df$tipi_consc <-as.numeric(data$tipi_consc)
df$tipi_emost <-as.numeric(data$tipi_emost)
df$tipi_opexp <-as.numeric(data$tipi_opexp)
df$gtl <-as.numeric(data$gtl)
df$overconfidence <-as.numeric(data$overconfidence)
df$rational <-as.numeric(data$rational)
df$intuitive <-as.numeric(data$intuitive)
df$smartphone <-as.numeric(data$smartphone)
df$computer <-as.numeric(data$computer)
df$tablet <-as.numeric(data$tablet)
df$eyes <-as.numeric(data$eyes)
df$date.time <- as.POSIXct(data$date.time,format="%Y-%m-%d %H:%M:%S")
data<-df

# Do remaining data transformations and pre-processing (near-zero variance, standardization, splines, PCs)

## Drop unused variables (PII, variables with lots of "NA" values, no variation, used in scales)
data <- subset(data, 
               select=-c(h2.2, h2.3, h2.4, h2.5, h2.6, leaderChoice, consumableKey, settingsNum, nid, 
                         EndDate, IPAddress, LocationAccura, Q100, Q101_1, Q101_2, Q101_3, Q101_4, Q102, 
                         Q102_TEXT, Q108, Q109_5_TEXT, Q109_6_TEXT, Q18_TEXT, Q30, Q31, Q43, Q45, Q54, 
                         RecipientEmail, RecipientFirstName, RecipientLastName, ResponseSet,Status, 
                         Q109_2_TEXT, Leader, match_round_id, matchid, Q1,Q10,Q2,Q3,Q40,Q44,Q46,Q5,Q99, 
                         Q106_1, Q106_2, Q6_1, Q6_2, Q6_3, Q6_4, Q6_5, Q6_6, Q6_7, Q6_8, Q6_9, Q6_10, 
                         Q107_1, Q107_2, Q107_3, Q107_4, Q107_5, Q107_6, Q107_8,
                         mlk,un,nile,books,ocean,tokyo,elefant,bach,boeing,moon,
                         Q56_1,Q56_2,Q56_3,Q56_4,Q56_5, Q56_6,Q56_7,Q56_8,Q56_9,Q56_10, 
                         Q57_1,Q57_10,Q57_11,Q57_12,Q57_2,Q57_3,Q57_4,Q57_5,Q57_6,Q57_7,Q57_8,Q57_9,Q59,
                         Q60,Q62,Q63,Q67,Q68,Q70,Q71,Q72,Q74,Q79,Q81,Q82,Q83,Q84,Q86,Q87,Q55.1_1_2_INT,
                         Q55.1_2_1_INT,Q55.1_2_2_INT,Q55.1_3_2_INT,Q55.1_5_1_INT,Q55.1_5_2_INT,Q55.1_6_1_INT,
                         Q55.1_6_2_INT,Q55.1_7_1_INT,Q55.1_7_2_INT,Q55.1_8_1_INT,Q55.1_8_2_INT,Q55.1_9_1_INT,
                         Q55.1_9_2_INT,Q55.1_10_1_INT,Q55.1_10_2_INT,Finished,Q109_7_TEXT,Q32_1,Q32_2,Q32_3, 
                         Q55.1_10_1_TEXT,Q55.1_10_2_TEXT,Q55.1_1_1_TEXT,Q55.1_1_2_TEXT,Q55.1_2_1_TEXT,
                         Q55.1_2_2_TEXT,Q55.1_3_1_TEXT,Q55.1_3_2_TEXT,Q55.1_4_1_TEXT,Q55.1_4_2_TEXT,
                         Q55.1_5_1_TEXT,Q55.1_5_2_TEXT,Q55.1_6_1_TEXT,Q55.1_6_2_TEXT,Q55.1_7_1_TEXT,
                         Q55.1_7_2_TEXT,Q55.1_8_1_TEXT,Q55.1_8_2_TEXT,Q55.1_9_1_TEXT,Q55.1_9_2_TEXT,
                         StartDate,Q90,Q95,Q96,Q70,Q74,Q83,Q68,Q81,Q71,Q79,Q67,Q62,Q72,Q63,Q59,Q82,Q87,
                         Q86,Q90_cat,Q96_cat,Q86_cat,Q87_cat,Q82_cat,Q59_cat,Q63_cat,Q72_cat,Q62_cat,Q67_cat,Q95_cat,
                         Q79_cat,Q71_cat,Q81_cat,Q68_cat,Q83_cat,Q74_cat,Q70_cat))
write.csv(data, file = "data.csv")
summary (data)

# Impute variables with plenty of missing data

overconfidence
Q37
Q105
CSE


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









