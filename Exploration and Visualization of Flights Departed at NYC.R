
#importing the file and installing the packages

nyflight <- read.csv("C:/Users/bharathi/Desktop/ISM 645/Assignment/NYC_Flights.csv", header = TRUE, stringsAsFactors = TRUE)
str(nyflight)
summary(nyflight)
head(nyflight)
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("tidyverse")
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)

#===================================================================

#Add a new variable, total_delay, which is the sum of dep_delay and arr_delay.

nyflight $ total_delay <- nyflight$dep_delay + nyflight$arr_delay

#Remove the columns "year" and "cancelled" from your data frame because it is all 2014 and there were no cancelled flights.

nyflight_totaldelay <- data.frame(nyflight[c(2,3,4,5,6,7,9,10,11,12,13)])
head(nyflight_totaldelay)

# Make a table of the number of flights by carrier, and then remove the carriers with less than 1000 flights in 2014.


carrierwith_morethan_1000flights <- table(nyflight_totaldelay$carrier)
carrierwith_morethan_1000flights_df <- as.data.frame(carrierwith_morethan_1000flights)
table_carrier_with_morethan_1000flights <- subset(carrierwith_morethan_1000flights_df, Freq >1000)
table_carrier_with_morethan_1000flights


df <- subset(nyflight_totaldelay, nyflight_totaldelay$carrier!= "AS" & nyflight_totaldelay$carrier!= "F9" & nyflight_totaldelay$carrier!= "HA" & nyflight_totaldelay$carrier != "OO")
View(df)

#Sort flights (data frame from Question 1) by column "total_delay" in descending order. 


df_ordered_by_totaldelay<- df[order(df$total_delay, decreasing = TRUE),]
df_ordered_by_totaldelay

# Then, calculate the average of flight distance among the top 10 flights with the longest total delay.

longest10_delay_flights <- head(df_ordered_by_totaldelay, 10)
mean(longest10_delay_flights$distance)


#DPLYR style:-

longest10_delay_flights_dplyr <- df %>% arrange(desc(total_delay))%>% top_n(n =10, total_delay)
mean(longest10_delay_flights_dplyr$distance)
     
# Draw box plots of total delay separately by carriers.

ggplot(df, aes(carrier, total_delay)) +geom_boxplot()+labs(title = "Total delay by carriers", x = "carrier", y = "Total Delay", size = 3)

# Extract flights with a major delay (defined as more than 60 minutes delay in either departure or arrival).

major_delay_flights <- subset(df, subset = df$dep_delay >60 | df$arr_delay >60)
head(major_delay_flights)

# Then, draw a pie chart to examine the most frequently delayed carrier in NYC

pie_table <- table(major_delay_flights $ carrier)
pie(pie_table, main = "Most Delayed Carrier in NYC")

# Among the flights with a major delay, defined above,
# Visualize the number of flights by destination, in decreasing order, that departed from JFK airport. (Hint: sort function can be applied to various data types, such as data frame, vector, table)


jfk_depart <-  major_delay_flights%>% filter(origin == "JFK")
head(jfk_depart)
table_1 <- table(df_ordered_by_destination $dest)
sorted <- sort(table_1, decreasing = TRUE)
sorted

par(las = 2)

barplot(sorted, xlab = "Destination", ylab = "Number of flights", cex.axis =0.8, cex.names = 0.5)

#Visualize the histogram of total delay of flights that departed from JFK airport.

hist(jfk_depart$total_delay, breaks = 10, xlab = "Total Delay", ylab = "Number of flights")
      
# Among the flights with a major delay, defined above,
# Visualize the relationship between total delay time and flight distance.


plot(major_delay_flights $distance, major_delay_flights $total_delay, xlab = "Flight Distance", ylab = "Total Delay")

#Visualize the histogram of total delay of flights that departed from JFK airport.

cor(major_delay_flights $distance, major_delay_flights $total_delay)

#Correlation value between total delay time and flight distance is 0.00360, We know that if correlation value is above 0 then the two variables have positive relationship. This proves that longer the flight distance , longer is the delay time. 


# Among the flights that departed from NYC, which carriers and destinations are the worst in terms of frequency of delay for each airport?

install.packages("ggplot2")
install.packages("ggrepel")
install.packages("tidyverse") # As tidyverse is a collection of package including dpylr
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)
library(tidyverse)
library(dplyr)

no_of_flights <- major_delay_flights %>% group_by(origin, dest, carrier) %>% summarise(count = n())


ggplot(no_of_flights, aes(carrier, count)) + geom_point(aes(color = carrier), size = 3)+ 
  labs(x= "carrier for each origin port", y= "number of major delays")+
  geom_text_repel(aes(label=dest), data= subset(no_of_flights, count >300), size = 2.5)+
  facet_wrap(~origin)

  
#From the plot, we can clearly see, From airport EWR ,UA carrier , destination to SFO seem to have major number of delays.
#Similarly from JFK airport, carrier B6 desitination to FLL seem to have more number of delays and 
#From the LGA airport, carrier AA destined to ORD seem to have frequent delays
#===================================================================
