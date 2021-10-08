# Class 15 Oct. 08, 2021

library(readxl)
library(magrittr)
library(wpp2019)
library(tidyverse)

options(digits=3, scipen=999)

## read World Bank poverty dat
MPM2021 <- read_xlsx(path="Sahel_data/MPM-Data-SM2021.xlsx")

cnames1 <- unlist(MPM2021 %>% slice(1)) ## table headers fixup
cnames2 <- unlist(MPM2021 %>% slice(2))
cnames <- c(cnames2[-11],cnames1[11])


MPM2021 %<>% slice(3:126) ## update MPM2021 to include only data rows

colnames(MPM2021) <- cnames ## update column names


## vector with Sahel country names

countries_Sahel <- c("Burkina Faso","Cameroon","Chad","Gambia","Guinea","Mauritania","Mali","Niger","Nigeria","Senegal")


## data from wpp2019 package filtered down to Sahel countries

data("UNlocations")

countries <- UNlocations %>% filter(name %in% countries_Sahel)

countries %<>% select(name, country_code)


data(pop)

cpop <- pop %>% filter(country_code %in% countries$country_code)

cpop %<>% select(name, country_code, `2015`, `2020`)


pov <- MPM2021 %>% filter(Economy %in% cpop$name)









## alter the data to change the map

## focus on the Sahel countries

## find the index numbers of the Sahel countries in the world data frame

n <- length(countries$name)
x=1:n

## replace the country names not in the Sahel with blanks
label_keep <- sapply(x, cnumb <- function(x){
  a = which(world$name == countries$name[x])
  return(a)
}, simplify = TRUE )

## put the Sahel country names back

nname <- length(world$name)
world$name <- rep("", nname)

for(i in 1:n){
  world$name[label_keep[i]] = countries$name[i]
}

## Do the same thing with population

world$pop_est <- rep(0, nname)

for(i in 1:n){
  world$pop_est[label_keep[i]]= cpop$`2020`[i]
}

## load a package for the labels

## devtools::install_github("yutannihilation/ggsflabel")

library(ggsflabel)

## make the map
map_a1 <- world %>% ggplot() + geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "turbo") +
  geom_sf_label(aes(label = name)) +
  coord_sf(xlim = c(-20.0, 20.2), ylim = c(0,23.5), expand = TRUE)
