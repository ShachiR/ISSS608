packages = c('ggiraph', 'plotly', 
             'DT', 'patchwork',
             'gganimate', 'tidyverse',
             'readxl', 'gifski', 'gapminder',
             'treemap', 'treemapify',
             'rPackedBar')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

GDP <- read_csv("data/GDP.csv")
WorldCountry <- read_csv("data/WorldCountry.csv")

GDP_selected <- GDP %>%
  mutate(Values = as.numeric(`2020`)) %>%
  select(1:3, Values) %>%
  pivot_wider(names_from = `Series Name`,
              values_from = `Values`) %>%
  left_join(y=WorldCountry, by = c("Country Code" = "ISO-alpha3 Code"))

treemap(GDP_selected,
        index=c("Continent", "Country Name"),
        vSize="GDP (current US$)",
        vColor="GDP (current US$)",
        title="GDP (current US$) , 2020",
        title.legend = "GDP per capita (current US$)"
)

treemap(GDP_selected,
        index=c("Continent", "Country Name"),
        vSize="GDP (current US$)",
        vColor="GDP per capita (current US$)",
        type = "value",
        title="GDP (current US$) , 2020",
        title.legend = "GDP per capita (current US$)"
)

tm<-treemap(GDP_selected,
        index=c("Continent", "Country Name"),
        vSize="GDP (current US$)",
        vColor="GDP per capita (current US$)",
        #type = "value",
        algorithm = "squarified",
        title="GDP (current US$) , 2020",
        title.legend = "GDP per capita (current US$)"
)

library(devtools)
install_github("timelyportfolio/d3treeR")
library(d3treeR)
treemap()
d3tree(tm, rootname = "World" )
