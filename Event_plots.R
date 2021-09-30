############################################################################
#
# Replication of Metcalf and Stock (2020) The Macroeconomic Impact of      
# Europe's Carbon Taxes
# Event Plots
# Jeffrey Giddens
# Submitted September 30 2021
#
############################################################################


#### Setup ####

# Set working directory
#setwd("~/Documents/Thesis/final_code")

#Install any packages 
#install.packages("ggplot2")

library(ggplot2)
library(stargazer)

data = read.csv("data.csv")           #this dataset extends to 2020 dataset includes all of the Metcalf dataset


############################################################################


#### CO2 800k years plot - Figure 1.1 ####

#Read the data in #separate csv from the main dataset
icecore = read.csv("icecore.csv", sep = ",")

iceplot = ggplot(data=icecore, aes(x=years_before_present, y=co2_ppm)) + 
  geom_line(color="navy") + scale_x_reverse(labels = scales::comma) +
  theme_bw() + #+ggtitle("Co2 concentrations in the atmosphere over the last 800,000 years") 
  ylab("CO2 ppm") + xlab("Years before present") 

iceplot

#adjust width while saving -  800 wide in the document


#### Table B.1 Carbon taxes of each country####

table1_data = data %>% group_by(country) %>% slice(which(year==2020))
table1 = cbind(table1_data$country, table1_data$ctaxyear, round(table1_data$ctax_21_real,2), 100*table1_data$share21)
colnames(table1) = c("Country", "Year of Enactment", "2020 Tax Rate (2018 USD)", "Share of GHG Emissions Covered by Tax")
#library(stargazer)
stargazer(table1, type="latex",title="EU+ Carbon Taxes")

table1


#### Carbon tax rates, all - Figure B.1 ####

carbon_tax_plot = ggplot(data=data[data$ctaxever==1,], aes(x=year, y=ctax_21_real, color=country)) + geom_line(size=.7) +
  scale_color_discrete(type = getOption("ggplot2.discrete.colour")) + 
  #scale_color_brewer(palette = c("Set1","Paired")) +
  xlab("Year") +
  ylab("") +
  ggtitle("Real carbon tax rates") +
  theme_bw() +
  theme(legend.position = "bottom")

carbon_tax_plot


#### Event studies - Figures B.2-B.5 ####
# switch y between dlrgdp21, dlemptot21, dlemission_trans, dlemission_total
#remove the subtraction to align by year instead of by starting year of carbon tax

event_plot = ggplot(data=data, aes(x= data$year - data$ctaxyear, y=dlemission_total, color=country)) + geom_line() +
  scale_color_hue() +
  theme_bw() +
  xlab("Year from first imposition of carbon tax") + ylab("Percentage points") +
  theme(legend.position = "none") 
  #xlim(-5,5) #to narrow in on 5 years before and after. add a + to the line above

event_plot





############################################################################


#Sources:
#Icecore:
#https://www.ncdc.noaa.gov/paleo-search/?dataTypeId=7
#https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/antarctica2015co2composite.txt

# Metcalf code:
# https://www.openicpsr.org/openicpsr/project/120535/version/V1/view;jsessionid=50F86AA74FE95A5057594DA28B417345?path=/openicpsr/120535/fcr:versions/V1/readme_AERPP.txt&type=file



  
 