library(ggplot2)
library(tidyverse)
library(RSocrata)
library(sf)
library(jsonlite)
library(httr)
library(geojsonio)
library(ggthemes)
library(tidycensus)
library(tidyverse)

### Import population data from US Census

# need census key #

list = list()
for (i in c(2010:2020)){
  pop2 <- get_acs(geography = "county subdivision", 
                state = "NY",
                 variables = "B01001_001E", 
                 year = i)

  pop2 = pop2[pop2$NAME %in% c("Brooklyn borough, Kings County, New York","Manhattan borough, New York County, New York",
                             "Bronx borough, Bronx County, New York","Staten Island borough, Richmond County, New York",
                             "Queens borough, Queens County, New York"),][c("NAME","estimate")]

  pop2$NAME = word(pop2$NAME, 1, sep = fixed(' '))

  pop2 = pop2 %>% rename("borough" = NAME, "population" = estimate)
  
  pop2$year = i
  
  list[[i]] = pop2
}

pop = do.call(rbind, list)

pop$year = as.character(pop$year)

#ggplot(data=pop, aes(x = year, y = population)) +
 # geom_col(aes(fill = borough),width = 0.7)+
 # scale_fill_discrete(type = "viridis", name = "") 

### Import NYC housing Geojson and merge with census data

df = read_sf("https://data.cityofnewyork.us/resource/kyz5-72x5.geojson")

comp = df[c('boro','comp2010','comp2011','comp2012','comp2013','comp2014',
            'comp2015','comp2016','comp2017','comp2018','comp2019','comp2020')]

comp = comp %>% rename("borough" = boro)

comp = comp %>% pivot_longer(cols = c('comp2010','comp2011','comp2012','comp2013','comp2014',
                               'comp2015','comp2016','comp2017','comp2018','comp2019','comp2020'),
                      names_to = "year", 
                      values_to = 'comps')

comp = comp %>% 
  group_by(borough, year) %>%
  summarise(total_comp = sum(as.numeric(comps), na.rm = TRUE))

comp$year =substring(comp$year, 5)

merge = merge(comp, pop, by=c('borough','year'))

merge$comp_person = as.numeric(merge$total_comp) / (as.numeric(merge$population)/1000)

# Number of completed projects per 1000 resident

ggplot(data=merge) +
  geom_bar(aes(x = year, y = comp_person, fill = borough),
           stat = 'identity')

### Create some maps ###

df$net_change = as.numeric(df$total) - as.numeric(df$cenunits10)

b = df %>%
  group_by(boro) %>%
  summarise(net_change = sum(net_change, na.rm = TRUE),
            approved = sum(as.numeric(approved), na.rm = TRUE), 
            filed = sum(as.numeric(filed), na.rm = TRUE))

b$comp_ratio = b$approved/b$filed

# Net change in housing units since 2010

ggplot(data = b,aes(fill = net_change, geometry = geometry,label = boro)) + 
  geom_sf() + 
  scale_fill_continuous(type = "viridis", 
                        guide = guide_colorbar(barheight = unit(4,"cm"),
                                               barwidth = unit(1,"cm")),
                        labels = c('0','20,000','40,000','60,000','80,000'),
                        name = "")+
  geom_sf_label(fill = "white", fun.geometry = sf::st_centroid)

# Completion ratio of housing projects

ggplot(data = b,aes(fill = comp_ratio, geometry = geometry,label = boro)) + 
  geom_sf() + 
  scale_fill_continuous(type = "viridis", 
                        guide = guide_colorbar(barheight = unit(4,"cm"),
                                               barwidth = unit(1,"cm")),
                        name = "")+
  geom_sf_label(fill = "white", fun.geometry = sf::st_centroid)

df2 <- read_sf('https://data.cityofnewyork.us/resource/7t3b-ywvw.geojson')

