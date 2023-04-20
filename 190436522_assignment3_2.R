
Commoditys <- read.delim("wmr_commodity_distribution.txt") #file is imported using the read.delim function and labled as Commoditys


COMS1 <- Commoditys %>% filter(region=="EastMediterranean") %>%  group_by(country) %>% # filter the data to information about the East Mediterranean
  summarise(ITNs = sum(No_of_LLINs_delivered)) 


COMS1$country <- as.factor(COMS1$country) # order data by country 
NO_NET <- COMS1 %>% ggplot(aes(x = country, y = ITNs/1000000))+ # plot the x and y axies as well as naming the filtered plot 
  geom_col()+# Specify the type of plot as a column chart 
  labs(y = "Number of nets (M)")+ # rename the y axis 
  coord_flip() # flip the X and Y axes of a plot



NOLL <- Commoditys %>% ggplot(aes(No_of_LLINs_delivered, Modelled_percentage_of_population_with_access_to_an_ITN))+ # set the x and y axis
  geom_point()+ #create a scatter plot of data points
  labs(x = "Number of LLINs delivered",
       y = " % of population with access to ITN") # name the x and y axis



CDM <- read.delim("wmr_pop_sizes_cases.txt") # import data and name new variable 


POP <- CDM %>% filter(region =="EastMediterranean") %>% group_by(country) %>%  # filter the data ande create a new variable 
  summarise(total_pop = mean(population)/1000000) %>% # calculate the mean of a variable and transform it into a new variable.
  top_n(8) %>% # filter to the 8 countrys 
  ggplot(aes(x = country, y = total_pop)) + #create a new ggplot object with x mapped to the country variable and y mapped to the total_pop variable.
  geom_col() +# Specify the type of plot as a column chart 
  labs(y = "population (m)")+ # rename the y axis 
  coord_flip() + # flip the X and Y axes of a plot
theme(axis.text.y = element_blank(), # theme is used to modify details in the plot  
      axis.title.y = element_blank() # remove name from x axis 
      ) 


Plots <- NO_NET +  POP + NOLL + # group the plots and lable it as a variable 
 plot_layout(ncol = 2, guides = "collect")+ # structure the lay out of the plot 
  plot_annotation(
    title = "Distribution of nets in East Mediterranean", #create a title 
    subtitle = "Large quantity of the population in East Mediterranean do not use nets, despite having access", # create a subtitle 
    caption = "Source: World malaria report 2022" # create a caption 
  )














