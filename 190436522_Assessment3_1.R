library(palmerpenguins)
library(tidyverse)
library(patchwork)
library(ggplot2)

Death_cases <- read.table("wmr_cases_deaths_modelling.txt",sep="\t",header=T) #file is imported using the read.table function and labled as Death_cases

DCA <- Death_cases %>% # A filtered version of the Death_cases is labled as DCA
  group_by(region) %>% # table is grouped by regions 
  filter(region == "Africa") # Data filtered to Africa so that DCA represents data from africa 

highburden <- DCA$country[DCA$year==2020 & DCA$deaths>2e4] # separate the high burden cases from the data 
lowburden <- DCA$country[DCA$year==2020 & DCA$deaths<2e4] # separate the low burden cases from the data


Plottittle_A <- plot_annotation(title ="Total malaria burden in Africa 2000-21", 
                                subtitle = 'Despite falling mortality, transmission remains dominated by 8 high burden countries',
                                caption = 'Source: World malaria Report 2022') # Name the title, subtitle and caption and group them as data 

# Section_A

Section_A <- ggplot(DCA,aes(year,deaths/1000,group=country))+ # lable the data as Section_A and specify the data for the plot as DCA using ggplot and set the axies
  geom_area(aes(fill = "lowburden")) + # create a stacked area plot using geom_area for groups of countries that are "low burden" 
  geom_area(aes(fill = "highburden"))+ # create a stacked area plot using geom_area for groups of countries that are "high burden" 
  geom_area(fill="lightgrey") + # fills the low burden data in the colour light grey in the graph
  geom_area(fill="darkred", # fills the high burden data in the colour dark red in the graph
            data=DCA %>% filter(country %in% highburden))+ # selects only high burden cases from the data 

scale_y_continuous(breaks= c(0, 200, 400, 600, 800)) + # changes the Y axis to only show the metrics in 0,200,400,800
  labs(y = "death (x1000)") + # label the y axies 
scale_fill_discrete(name = "",
                    labels = c("low burden countries", "high burden countries"), # lable the figures in the legend
                    type = c("lightgrey", "darkred")) + # colour the figures in the appropriate colours in the legend 
       theme(axis.text.x = element_blank(), # theme is used to modify details in the plot  
             axis.title.x = element_blank()) # remove name from x axis 



# Section_B

Mortality <- DCA %>% # lable a group in the selected data as mortality 
  group_by(year, country) %>% # group the data by year and country 
  summarize(Mortality = sum(deaths/population)*100000) # find the average mortality rate in each country by dividing death by number of population and multiplying it by 1000000

Section_B <- Mortality %>% filter(country %in% highburden) %>% # lable the group as Section_B and filter the data to only high burden 
ggplot(aes(x = year, y = country, fill = Mortality)) + # set the X and Y axies on the plot 
  geom_raster() + # adds a raster layer to the plot to fill the rectangles based on values in a representative tone of colour 
  labs(y = "mortality (deaths/100k)") + # change the name of the y axis 
  scale_y_discrete(limits = c("Uganda", "Mali", "Burkina Faso", "Mozambique", "Niger", "United Republic of Tanzania", "Democratic Republic of the Congo", "Nigeria")) # limits the name of the data represented on the y axis by only the high burden cases





# Section_C

PopM <- DCA %>%  # name a group for population mean
  group_by(country) %>% # group the data by country 
  summarise(PopM = mean(population)) # filter the data by its population means 


PopM %>% filter(country %in% highburden)  # filter the data to only hiburden countrys 
HighBurden <- data.frame(PopM %>% filter(country %in% highburden)) # lable the filtered data 




Section_C <- HighBurden %>% ggplot(aes(x = PopM/10000, y = country)) + # name the variable as Section_C as well as selecting the X and Y axis 
  geom_col(data = HighBurden) + # Specify the type of plot as a column chart 
  scale_y_discrete(limits = c("Uganda", "Mali", "Burkina Faso", "Mozambique", "Niger", "United Republic of Tanzania", "Democratic Republic of the Congo", "Nigeria")) + # limits the name of the data represented on the y axis by only the high burden cases
labs(x = "population (m)") + # rename y axis "population (m)"
  theme(axis.text.y = element_blank(), # theme is used to modify details in the plot  
        axis.title.y = element_blank()) # remove name from x axis 




P1 <- collecting_datasets <- (Section_A/ Section_B - plot_spacer() / Section_C +
                                Plottittle_A + 
                                plot_layout(guides = "collect")) # combine all plot set and titles into one window and name it P1 

ggsave ("190436522_assessmen3_1.png", plot = P1, width = 300, height = 150, units = "mm") # save plot as an image file 
