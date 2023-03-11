### GDP LCU Analysis ###


library ("tidyverse")
library ("googlesheets4")
library ("rmarkdown")

setwd("C:/Users/uriar/R Projects/gdplcu")
#This was the name of the directory I used, you'll need to set your own

library ("readxl")
read_excel("LCSS_Data.xlsx", sheet=2)
#read_excel is a function of the package readxl that comes with tidyverse.
#However, it does not necessarily seem to load just by loading tidyverse
#the function generates a tibble
#To see all the packages installed and loaded use library()

GDPLCU <-read_excel("LCSS_Data.xlsx", sheet=2)
#The above command creates an object called "GDPLCU" with the table you want
#to work with. This allows then running commands on the object
#Note that this data is not per capita but total GDP for each country,
#so trajectories include the influence of population growth
#I further below turn to GDP per capita

head (GDPLCU)


##GDP LCU Data Prep##


data2 <-GDPLCU %>% select("country_name", "year", "value")
data2
#creates a new object (data2) with only 3 of the columns from GDPLCU

data3 <- data2 %>% filter(year >= 1961, year <= 2020)
#filters rows for 1961 to 2020 and creates new object (data3) just with these rows

countries_to_use<-data3 %>% count(country_name) %>% filter (n == 60)
#counts the number of countries for which there is data for all 60 years selected
#generates a tibble with 90 country names listed in the first column
#assigns a name and creates an object for this tibble

data_clean <- data3 %>% filter(country_name %in% countries_to_use$country_name)
#selects from data3 only those countries with 60 years of data and stores the
#values in data_clean. This should be the final data set to use.

data_clean_wide<-spread(data_clean, country_name, value)
#transforms the long form data frame to a wide form one

years<-data_clean_wide [,1]
#isolates the year column from the data frame into a separate object

m<-as.matrix(data_clean_wide[,-1])
#isolates the remaining columns of the data frame into a matrix to facilitate manipulation

m1<-t(100*t(m)/m[1,])
#multiplies all rows of the matrix by 100 and divides by the first row to create
#an index with 1961=100. Need to use t() to transpose because otherwise operations
#are applied to columns

data_clean_wide_index<-as.data.frame(cbind(years,m1))
view(data_clean_wide_index)
#combines again the years column with the others and transforms back to a data frame


## GDP LCU Data Analysis##


install.packages("SimilarityMeasures")
library(SimilarityMeasures)

list_vectors<-as.list(as.data.frame(m1))
#separates each column (country) of the m1 matrix into a separate vector and creates
#a list of country vectors

list_matrices<-lapply(list_vectors, cbind, years)
#attaches a column with years to each country vector in the list created above.
#Creates a new list of data frames, one for each country, each data frame with two columns,
#one for the year, and another for the GDP LCU index for that country.

list_matrices<-lapply(list_matrices, as.matrix)
#transforms each data frame in the list created above into a matrix


#"list-matrices" is now ready to be used with the function LCSS, which will compare two-by-two,
#country growth trajectories and assign a measure of distance between them
#The measure corresponds to the number of points considered equivalent among
#two trajectories, based on a few parameters. So, in this case, the maximum
#value for LCSS is 60 (sixty years considered identical) and the minimum
#value is 1 (because the index made the value for all countries equal to 100
#in 1961)

#Just for the record, for each individual country, something like this
#would have also worked: Algeria_m<-as.matrix(cbind(years,m1[,1]))

#LCSS has three parameters that need calibrating. Used Argentina and Uruguay
#two countries whose trajectories are apparently (based on a line chart) somewhat
#closely fluctuating to calibrate those parameters as shown below

LCSS(list_matrices$Argentina, list_matrices$Uruguay,1,3,0.5)
LCSS(list_matrices$Argentina, list_matrices$Uruguay,2,3,0.5)
LCSS(list_matrices$Argentina, list_matrices$Uruguay,3,3,0.5)
LCSS(list_matrices$Argentina, list_matrices$Uruguay,5,3,0.5)
LCSS(list_matrices$Argentina, list_matrices$Uruguay,2,5,0.5)
LCSS(list_matrices$Argentina, list_matrices$Uruguay,2,7,0.5)
LCSS(list_matrices$Argentina, list_matrices$Uruguay,2,5,0.2)
LCSS(list_matrices$Argentina, list_matrices$Uruguay,2,5,0.8)

#Based on the above and understanding of parameters, chose to stick to
#2, 5 and 0.5 to apply to call countries

#Given enough time, the script below should generate a matrix with a measure of
#the distance between the growth trajectories of all pairs of countries, among
#the 90 countries in the sample. This should be a 90 x 90 matrix (8100 results), with
#diagonal =1 (each countries trajectory compared to itself, leaving 8100-90 = 8010
#results that compare different countries). Because the matrix will compare
#Argentina to Uruguay and then Uruguay to Argentina, the number of unique results 
#comparing two different countries will actually be 8010/2 = 4005


LCSS_Matrix<-sapply(list_matrices, function (x) sapply(list_matrices, function (y) LCSS(x,y,2,5,0.5)))
#This took over 11 hours to run for me

write.csv(LCSS_Matrix, file = "LCSS Matrix 1")
#Exported results to a csv file

LCSS_dataframe<-as.data.frame(LCSS_Matrix)
#turned results matrix into a data frame

LCSS_dataframe2<-data.frame(country1 = row.names(LCSS_dataframe),LCSS_dataframe)
#created a second dataframe that includes the names of the first as a column

LCSS_long<-gather(LCSS_dataframe2, country2, Score, Algeria:Zimbabwe)
#converted the second data frame to long form

write.csv(LCSS_long, file = "LCSS Long 1")
#Exported results to a csv file

#Sorting LCSS Long 1 by scores [I did this and analysed the results by opening
#the CSV results file in Excel], we see that all country pairs with scores 40 or
#above are rich countries. The rich countries that make up those pairs with scores
#40 or above are 7 countries.


ggplot(data_clean_wide_index, aes(x = year)) +
  geom_line(aes(y = France, color = "France")) +
  geom_line(aes(y = `United Kingdom`, color = "United Kingdom")) +
  geom_line(aes(y = `United States`, color = "United States")) +
  geom_line(aes(y = Austria, color = "Austria")) +
  geom_line(aes(y = Belgium, color = "Belgium")) +
  geom_line(aes(y = Norway, color = "Norway")) +
  geom_line(aes(y = Finland, color = "Finland")) +
  geom_line(aes(y = Sweden, color = "Sweden")) +
  labs(title = "Countries Whith Pairing Scoring Over 40", y = "Index")
#Plots the 10 countries that pair up with scores above 40

ggplot(data_clean_wide_index, aes(x = year)) +
  geom_line(aes(y = France, color = "France")) +
  geom_line(aes(y = Austria, color = "Austria")) +
  labs(title = "Closest Growth Trajectories", y = "Index")
#Plots the 2 countries that pair up with the highest score (53)

ggplot(data_clean_wide_index, aes(x = year)) +
  geom_line(aes(y = Uruguay, color = "Uruguay")) +
  geom_line(aes(y = Argentina, color = "Argentina")) +
  labs(title = "Growth Trajectories: Argentina and Uruguay", y = "Index")
#For comparison, two countries that pair up for a score of 33: Argentina and Uruguay

ggplot(data_clean_wide_index, aes(x = year)) +
  geom_line(aes(y = France, color = "France")) +
  geom_line(aes(y = Austria, color = "Austria")) +
  geom_line(aes(y = Uruguay, color = "Uruguay")) +
  geom_line(aes(y = Argentina, color = "Argentina")) +
  labs(title = "Growth Trajectories: Two Pairs of Countries", y = "Index")
#Putting the four countries in the same chart

ggplot(data_clean_wide_index, aes(x = year)) +
  geom_line(aes(y = France, color = "France")) +
  geom_line(aes(y = Austria, color = "Austria")) +
  geom_line(aes(y = Uruguay, color = "Uruguay")) +
  geom_line(aes(y = Argentina, color = "Argentina")) +
  geom_line(aes(y = `Costa Rica`, color = "Costa Rica")) +
  geom_line(aes(y = Belize, color = "Belize")) +
  labs(title = "Growth Trajectory Similarities: Austria and France = 53, 
       Argentina and Uruguay = 33, Belize and Costa Rica = 18", y = "Index")
#Adding two other countries

#The The problem I encountered with this first attempt was that, because my data
#were in an index that set all countries equal to 100 in 1961, and because the
#LCSS function compares years based on the distance between their values, there
#are many more years considered the same by the function in the early period of
#the trajectories (say, 1960s) than in the later period (say, 2010s).
#This is not what we would like. We would like all period of the trajectories
#to be valued the same when accessing similarity between two trajectories.

#So my next attempt was to focus on growth rates, rather than on an index.
#Doing so would mean that, when one countries growth trajectory goes up,
#say 3 percentage points, and another one does too, the two trajectories
#would be considered equivalent in that year. To transform my data into growth
#rates:

m2<-t(100*((t(m1)/(t(lag(m1))))-1))
#As before, I use t() to transpose because otherwise operations
#are applied to columns. I also use lag() to divide each row by a previous one

data_clean_wide_growth<-as.data.frame(cbind(years,m2))
data_clean_wide_growth<-data_clean_wide_growth[-1,]
view(data_clean_wide_growth)

m2a<-m2[-1,]
yearsm2a<-years[-1,]

list_vectors_growth<-as.list(as.data.frame(m2a))
list_matrices_growth<-lapply(list_vectors_growth, cbind, yearsm2a)
list_matrices_growth<-lapply(list_matrices_growth, as.matrix)

#Because I changed my unit of analysis (from an index to percentages), I had to
#also adjust the pointDistance parameter. In calibrating this parameter I again
#used Argentina and Uruguay and compared their growth rates. This time, I also
#used France and Austria, since these countries's trajectories seemed close
#in the previous exercise

LCSS(list_matrices_growth$Argentina, list_matrices_growth$Uruguay,2,1,0.5)
LCSS(list_matrices_growth$Argentina, list_matrices_growth$Uruguay,2,2,0.5)
LCSS(list_matrices_growth$Argentina, list_matrices_growth$Uruguay,2,3,0.5)
LCSS(list_matrices_growth$Argentina, list_matrices_growth$Uruguay,2,5,0.5)
LCSS(list_matrices_growth$Argentina, list_matrices_growth$Uruguay,2,7,0.5)
LCSS(list_matrices_growth$France, list_matrices_growth$Austria,2,1,0.5)
LCSS(list_matrices_growth$France, list_matrices_growth$Austria,2,2,0.5)


#I ended up choosing pointDistance=2, which generated a high score for
#France x Austria (57) and a medium score for Argentina vs Uruguay (33)

LCSS_Matrix_growth<-sapply(list_matrices_growth, function (x) sapply(list_matrices_growth, function (y) LCSS(x,y,2,2,0.5)))
#This took over 11 hours to run for me

LCSS_dataframe_growth<-as.data.frame(LCSS_Matrix_growth)
#turned results matrix into a data frame

LCSS_dataframe_growth2<-data.frame(country1 = row.names(LCSS_dataframe_growth),LCSS_dataframe_growth)
#created a second dataframe that includes the names of the first as a column

LCSS_growth_long<-gather(LCSS_dataframe_growth2, country2, Score, Algeria:Zimbabwe)
#converted the second data frame to long form

write.csv(LCSS_growth_long, file = "LCSS Growth Long")
#Exported results to a csv file


## GDP Per Capita LCU Data Analysis##

#I now turn to look at GDP per capita as opposed to total GDP


GDPCAPLCU_clean_wide_index <-read_excel("LCSS_Data.xlsx", sheet=4)
#GDP Per Capita data index I had already cleaned in Excel and have it in wide form

GDPCAPLCU_clean_wide_growth <-read_excel("LCSS_Data.xlsx", sheet=5)
#GDP Per Capita data growth rates (%) that I had already cleaned in Excel and
#have it in wide form

m3<-as.matrix(GDPCAPLCU_clean_wide_growth[,-1])
#creates a matrix with all columns of the data frame except the years column

list_vectors2<-as.list(as.data.frame(m3))
#separates each column (country) of the m3 matrix into a separate vector and creates
#a list of country vectors

list_matrices2<-lapply(list_vectors2, cbind, yearsm2a)
#attaches a column with years to each country vector in the list created above.
#Creates a new list of data frames, one for each country, each data frame with two columns,
#one for the year, and another for the GDP LCU index for that country.

list_matrices2<-lapply(list_matrices2, as.matrix)
#transforms each data frame in the list created above into a matrix


#"list-matrices2" is now ready to be used with the function LCSS, which will compare two-by-two,
#country growth trajectories and assign a measure of distance between them


#LCSS has three parameters that need calibrating. I again used Argentina and Uruguay
#to check for to see if previous calibration for GDP trajectoris is also ok for
#GDP per capita

LCSS(list_matrices2$Argentina, list_matrices2$Uruguay,2,1,0.5)
LCSS(list_matrices2$Argentina, list_matrices2$Uruguay,2,2,0.5)
LCSS(list_matrices2$Argentina, list_matrices2$Uruguay,2,3,0.5)
LCSS(list_matrices2$Argentina, list_matrices2$Uruguay,2,5,0.5)
LCSS(list_matrices2$Argentina, list_matrices2$Uruguay,2,7,0.5)
LCSS(list_matrices2$France, list_matrices2$Austria,2,1,0.5)
LCSS(list_matrices2$France, list_matrices2$Austria,2,2,0.5)


#I again chose pointDistance=2, which generated a high score for
#France x Austria (57) and a medium score for Argentina vs Uruguay (34)

#Given enough time, the script below should generate a matrix with a measure of
#the distance between the growth trajectories of all pairs of countries, among
#the 90 countries in the sample.


LCSS_Matrix3<-sapply(list_matrices2, function (x) sapply(list_matrices2, function (y) LCSS(x,y,2,2,0.5)))
#This took over 11 hours to run for me

write.csv(LCSS_Matrix3, file = "LCSS Matrix 3")
#Exported results to a csv file

LCSS_dataframe4<-as.data.frame(LCSS_Matrix3)
#turned results matrix into a data frame

LCSS_dataframe5<-data.frame(country1 = row.names(LCSS_dataframe4),LCSS_dataframe4)
#created a second dataframe that includes the names of the first as a column

LCSS_long3<-gather(LCSS_dataframe5, country2, Score, Algeria:Zimbabwe)
#converted the second data frame to long form

write.csv(LCSS_long3, file = "LCSS Long 3")
#Exported results to a csv file

#Sorting LCSS Long 3 by scores[I did this and analysed the results by opening
#the CSV results file in Excel], we see that all country pairs with scores 50 or
#above are rich countries, with the exception of the pair Guatemala-Bolivia, 
#whose growth trajectories match on 51 of the 59 years. The 5 countries
#with the closest growth trajectories are France, Belgium, Netherlands, Italy
#and Austria. The next closest pairing is United States with the United Kingdom.

ggplot(GDPCAPLCU_clean_wide_index, aes(x = Years)) +
  geom_line(aes(y = France, color = "France")) + 
  geom_line(aes(y = Belgium, color = "Belgium")) + 
  geom_line(aes(y = Netherlands, color = "Netherlands")) + 
  geom_line(aes(y = Italy, color = "Italy")) + 
  geom_line(aes(y = Austria, color = "Austria")) + 
  labs(title = "F1. Closest Growth Trajectories, GDP Per Capita, 1961=100",
       y = "Index", color = "Country", caption = "Data Source: World Bank, World Development Indicators (WDI)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(.35, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) +
  scale_color_manual(values = c("black", "blue", "darkgreen", "darkred", "darkorange"))

ggplot(GDPCAPLCU_clean_wide_index, aes(x = Years)) +
  geom_line(aes(y = France, color = "France")) + 
  geom_line(aes(y = Belgium, color = "Belgium")) + 
  labs(title = "F2. France and Belgium, GDP Per Capita, 1961=100", y = "Index",
       color = "Country", caption = "Data Source: World Bank, World Development Indicators (WDI)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(.35, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) +
  scale_color_manual(values = c("black", "blue"))

ggplot(GDPCAPLCU_clean_wide_growth, aes(x = Years)) +
  geom_line(aes(y = France, color = "France")) + 
  geom_line(aes(y = Belgium, color = "Belgium")) + 
  labs(title = "F3. France and Belgium, GDP Per Capita Growth Rates",
       y = "Growth Rate (%)", color = "Country",
       caption = "Data Source: World Bank, World Development Indicators (WDI)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(.35, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) +
  scale_color_manual(values = c("black", "blue"))

ggplot(GDPCAPLCU_clean_wide_index, aes(x = Years)) +
  geom_line(aes(y = Argentina, color = "Argentina")) +
  geom_line(aes(y = Brazil, color = "Brazil")) +
  geom_line(aes(y = Uruguay, color = "Uruguay")) +
  geom_line(aes(y = Chile, color = "Chile")) +
  geom_line(aes(y = Paraguay, color = "Paraguay")) +
  labs(title = "F4. Southern Cone, GDP PerCapita, 1961=100", y = "Index",
       color = "Country", caption = "Data Source: World Bank, World Development Indicators (WDI)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(.35, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) +
  scale_color_manual(values = c("black", "blue", "darkgreen", "darkred", "darkorange"))

ggplot(GDPCAPLCU_clean_wide_index, aes(x = Years)) +
  geom_line(aes(y = France, color = "France")) +
  geom_line(aes(y = Belgium, color = "Belgium")) +
  geom_line(aes(y = `United States`, color = "United States")) +
  geom_line(aes(y = `United Kingdom`, color = "United Kingdom")) +
  geom_line(aes(y = Argentina, color = "Argentina")) +
  geom_line(aes(y = Uruguay, color = "Uruguay")) +
  labs(title = "Sected Countries, GDP Per Capita, 1960=100", y = "Index",
       color = "Country", caption = "Data Source: World Bank, World Development Indicators (WDI)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(.35, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) +
  scale_color_manual(values = c("black", "blue", "darkgreen", "darkred", "darkorange", "purple"))

ggplot(GDPCAPLCU_clean_wide_index, aes(x = Years)) +
  geom_line(aes(y = `Korea Republic`, color = "South Korea")) +
  geom_line(aes(y = `China (P.R.C.)`, color = "China")) +
  labs(title = "F5. China and South Korea, GDP Per Capita, 1961=100",
       y = "Index", color = "Country",
       caption = "Data Source: World Bank, World Development Indicators (WDI)") + theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(.35, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) + scale_color_manual(values = c("black", "blue"))

ggplot(GDPCAPLCU_clean_wide_growth, aes(x = Years)) +
  geom_line(aes(y = `Korea Republic`, color = "South Korea")) +
  geom_line(aes(y = `China (P.R.C.)`, color = "China")) +
  labs(title = "F6. China and South Korea, GDP Per Capita Growth Rates",
       y = "Growth (%)", color = "Country",
       caption = "Data Source: World Bank, World Development Indicators (WDI)") + theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) + scale_color_manual(values = c("black", "blue"))

ggplot(GDPCAPLCU_clean_wide_index, aes(x = Years)) +
  geom_line(aes(y = `United States`, color = "United States")) +
  geom_line(aes(y = `United Kingdom`, color = "United Kingdom")) +
  labs(title = "F7. US and UK, GDP Per Capita, 1961=100",
       y = "Index", color = "Country",
       caption = "Data Source: World Bank, World Development Indicators (WDI)") + theme(
         plot.title = element_text(hjust = 0.5),
         legend.position = c(.35, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6)) + scale_color_manual(values = c("black", "blue"))

ggplot(GDPCAPLCU_clean_wide_index, aes(x = Years)) +
  geom_line(aes(y = Guatemala, color = "Guatemala")) +
  geom_line(aes(y = Bolivia, color = "Boivia")) +
  labs(title = "F8. Guatemala and Bolivia, GDP Per Capita, 1961=100",
       y = "Index", color = "Country",
       caption = "Data Source: World Bank, World Development Indicators (WDI)") + theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(.35, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)) + scale_color_manual(values = c("black", "blue"))
