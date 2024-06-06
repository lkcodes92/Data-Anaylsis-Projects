gss <- read.csv("C:\\Users\\Laotia\\Downloads\\GSSData.csv")

head(gss)
attach(gss)
tvhours

#Mean
mean(tvhours)
mean(age)
mean(chldidel)

#Median Quantitative
median(tvhours)
median(age)
median(chldidel)

#Min
min(tvhours)
min(age)
min(chldidel)

#Max
max(tvhours)
max(age)
max(chldidel)

#Range
range(tvhours)
range(age)
range(chldidel)

#Quartiles
quantile(tvhours)
quantile(age)
quantile(chldidel)

#Percentiles
quantile(tvhours, seq(0,1,0.1))
quantile(age, seq(0,1,0.1))
quantile(chldidel, seq(0,1,0.1))

#IQR Quantitative
IQR(tvhours)
IQR(age)
IQR(chldidel)

#Variance
var(tvhours)
var(age)
var(chldidel)

#Standard Deviation
sd(tvhours)
sd(age)
sd(chldidel)

#Summary Statistics
summary(tvhours)
summary(age)
summary(chldidel)
summary(gss)

#Summary
subset_gss <- gss[, c(7, 10, 11)]
summary(subset_gss)

#Table of Quanitiative Statistics
table1::label(tvhours) <- "TV Hours"
table1::label(age) <- "Age"
table1::label(chldidel)<- "Ideal # of Children"

table1::table1(~ tvhours + age + chldidel, data = gss)

#Histograms for quantitative variables
hist(gss$age, main="Figure 1: Histogram of Age (n=397)" ,
     xlab = "Age", ylab ="Number of Residents")

hist(gss$tvhours, main="Figure 2: Histogram of Hours of TV Watched (n=397)" ,
     xlab = "Hours of TV Watched", ylab ="Number of Residents")

#GGplot through tidyverse used for histograms and boxplots
install.packages("tidyverse")
library(ggplot2)

chldidel_hist <- ggplot(gss, aes(x=chldidel)) + geom_histogram(fill="gray", color="black", binwidth = 1)

print(chldidel_hist + labs( title= "Figure 3: Histogram of Ideal Number of Children (n=397)", y="Number of Residents",
                            x = "Ideal Number of Children"))

tvhours_hist <- ggplot(gss, aes(x=tvhours)) + geom_histogram(fill="gray", color="black", binwidth = 1)

print(tvhours_hist + labs( title= "Figure 2: Histogram of Hours Spent Watching TV (n=397)", y="Number of Residents",
                            x = "Hours of TV Watched"))

age_hist <- ggplot(gss, aes(x=age)) + geom_histogram(fill="gray", color="black", binwidth = 1)

print(age_hist + labs( title= "Figure 1: Histogram of Years of Age (n=397)", y="Number of Residents",
                           x = "Age"))


age_boxplot <- ggplot(gss, aes(x=age)) + geom_boxplot(outlier.colour = "red", outlier.shape =8,
                                                                  outlier.size =4)

print(age_boxplot + labs( title= "Figure 1a: Boxplot of Age (n=397)",
                            x = "Residents Age"))

tvhours_boxplot <- ggplot(gss, aes(x=tvhours)) + geom_boxplot(outlier.colour = "red", outlier.shape =8,
                                                                           outlier.size =4)

print(tvhours_boxplot + labs( title= "Figure 2a: Boxplot of Hours Spent Watching TV (n=397)",
                          x = "Number of Hours Watching TV"))

chldidel_boxplot <- ggplot(gss, aes(x=chldidel)) + geom_boxplot(outlier.colour = "red", outlier.shape =8,
                                                              outlier.size =4)

print(chldidel_boxplot + labs( title= "Figure 3a: Boxplot of Number of Ideal Children (n=397)",
                              x = "Ideal Number of Children"))

#####################################################################################################
################################ Univariate Categorical Statistics ###################################

#Frequency Counts
degree_table <- table(degree)
degree_table
relig_table <- table(relig)
polparty_table <- table(polparty)

#addtotals
addmargins(degree_table)


#Relative Frequency Counts
prop.table(degree_table)

#Percentages
round(100*prop.table(degree_table),digits=1)

#adding % Sign
apply(100*prop.table(degree_table),1, function(u) sprintf("%.1f%%", u))


####################################################################################
########################### Tables of Statistics #####################################

#Formatted Table
table1::label(gss$degree) <- "Degree"
table1::label(gss$relig) <- "Religion"
table1::label(gss$polparty) <- "Political Party"

table1::table1(~ degree + relig + polparty, data = gss)

#######################Graphs Displays for Univariate Categorical Variables ####################

#Pie Chart for Univariate Categorical Statistics

degree_counts <- table(degree)
degree_labels <- round((degree_counts / sum(degree_counts)) * 100, 1)
degree_labels <- paste(degree_labels, "%")
colors <- c("steelblue", "steelblue3", "steelblue2", "steelblue1", "skyblue1")

pie(degree_counts,
    main = "Figure 1: Pie Chart of Highest Level of Education (n=397)",
    col = colors,
    init.angle = 90,
    clockwise = TRUE,
    labels = degree_labels)

########## Religion ########
relig_counts <- table(relig)
relig_labels <- round((relig_counts / sum(relig_counts)) * 100, 1)
relig_labels <- paste(relig_labels, "%")
colors <- c("steelblue", "steelblue3", "steelblue2", "steelblue1")

pie(relig_counts,
    main = "Figure 2: Pie Chart of Religious Affliation (n=397)",
    col = colors,
    init.angle = 90,
    clockwise = TRUE,
    labels = relig_labels)

# Add a legend
legend("topright", c("Catholic", "Jewish", "Other", "Protestant"),
       fill = colors)

######## Political Party ###########

polparty_counts <- table(polparty)
polparty_labels <- round((polparty_counts / sum(polparty_counts)) * 100, 1)
polparty_labels <- paste(polparty_labels, "%")
colors <- c("steelblue", "steelblue3", "steelblue2", "steelblue1")

pie(polparty_counts,
    main = "Figure 3: Pie Chart of Political Party (n=397)",
    col = colors,
    init.angle = 90,
    clockwise = TRUE,
    labels = polparty_labels)

# Add a legend
legend("topright", c("Democratic", "Independent", "Other", "Republican"),
       fill = colors)

##### Barplot ######

# Create a bar chart with custom colors
degree_bar <- ggplot(gss, aes(x = degree, fill = degree)) +
  geom_bar(color = "black") +
  labs(
    title = "Figure 1a: Bar Chart of Highest Degree Level (n=397)",
    y = "Number of Residents",
    x = "Highest Degree Level Obtained"
  ) +
  scale_fill_manual(values = c("steelblue", "steelblue3", "steelblue2", "steelblue1", "skyblue1"))

print(degree_bar)

######Religion######
relig_bar <- ggplot(gss, aes(x = relig, fill = relig)) +
  geom_bar(color = "black") +
  labs(
    title = "Figure 2a: Bar Chart of Religous Affliation (n=397)",
    y = "Number of Residents",
    x = "Religious Affiliation"
  ) +
  scale_fill_manual(values = c("steelblue", "steelblue3", "steelblue2", "steelblue1"))

print(relig_bar)

######### Political Party Bar Chart ########

polparty_bar <- ggplot(gss, aes(x = polparty, fill = polparty)) +
  geom_bar(color = "black") +
  labs(
    title = "Figure 3a: Bar Chart of Political Party (n=397)",
    y = "Number of Residents",
    x = "Political Party"
  ) +
  scale_fill_manual(values = c("steelblue", "steelblue3", "steelblue2", "steelblue1"))

print(polparty_bar)

### Bivariate Stratified Table of Statistics ###
### Two categorical ###

#Basic Table
polparty_table <- ggplot(gss, aes(x = polparty, ))

polparty_table <- table(gunlaw, polparty)
polparty_table

## add totals
addmargins(polparty_table)

#relative Frequency Counts
prop.table(polparty_table)

#percentages
round(100*prop.table(polparty_table),digits=1)

#add percent sign
apply(100*prop.table(polparty_table), 1, function(u) sprintf( "%.1f%%", u))

##Formatted Tables ##
table1::table1(~ polparty | gunlaw , data = gss)

##Bivariate Graphical Display
## Two Quant ##

##Create Stacked Bar Plot with Colors and Legend###
counts <- table(gunlaw, polparty)
barplot(counts, main="Stacked Bar Chart for Political Party and Gun Law",
        xlab="Politcal Party", ylab="Gun Law Favor", ylim = c(0, 200),
        col = c("steelblue", "red"),
        legend = rownames(counts))

## 100% Stacked Bar Plot with Colors and Legend ##

###Contingency Table###

pol_gun_table <- table(gunlaw, polparty)
##Transform into % ###
percentage <- apply(pol_gun_table, 2, function(x){x*100/sum(x,na.rm=T)})

#Create the Graph#
barplot(percentage, main="100% Stacked Bar Chart for Political Party and Gun Law",
        xlab="Politcal Party", ylab="Gun Law Favor", ylim = c(0, 110),
        col = c("steelblue", "red"),
        legend = rownames(percentage))


#### Bivariate Analysis 1 Cat 1 Quant ####

table1::label(gss$tvhours) <- "Tv Hours"
table1::label(gss$age) <- "Age"
table1::label(gss$chldidel) <- "Ideal # of Children"

table1::table1(~ tvhours + age + chldidel | sex, data = gss)

### Side by Side Boxplot ###

tvhours_sex_box <- ggplot(gss, aes(x=tvhours, color=sex)) +
  geom_boxplot(outlier.colour = "red" ,
               outlier.shape = 8, outlier.size = 4)
print(tvhours_sex_box + labs( title = "Boxplot of Tv Hours Watched by Sex", x = "Tv Hours Watched"))

