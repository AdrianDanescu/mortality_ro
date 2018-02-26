
# Libraries ---------------------------------------------------------------
library(ggplot2)
library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(directlabels)


# Read Data ---------------------------------------------------------------

setwd("C:/Users/AdrianD/Documents/Diverse proiecte/Social causes/Mortality diseases/")

# absolute mortality counts
mort <- rbindlist(list(
  read.csv2("exportPivot_POP206C_1.csv", sep = ","),
  read.csv2("exportPivot_POP206C_2.csv", sep = ",")))

read.csv2("exportPivot_POP206C_1.csv", sep = ",") %>% View
fread("exportPivot_POP206C_1.csv", sep = ",") %>% View

path <- "C:/Users/AdrianD/Documents/Diverse proiecte/Social causes/Population/"
pop <- read.csv2(paste0(path, "exportPivot_POP107A_totals_county.csv"), 
                 sep = ",") %>% as.data.table
  



# Clean Data --------------------------------------------------------------

# mort
{
  # remove uninformative variables
  mort[, UM..Numar.persoane := NULL]
  
  # edit variable names
  names(mort)
  setnames(mort, 
           names(mort), 
           c("disease", "county", "year", "n_people"))
  
  # filter county data
  levels_to_remove <- str_subset(mort$county, 
                                 "Regiunea|MACROREGIUNEA|TOTAL|Mun. Bucuresti -incl. SAI")
  mort <- mort[! county %in% levels_to_remove]
  
  
  # remove total disease numbers per county
  mort <- mort[! disease == "Total"]
  
  
  # clean year variable
  mort[, year := str_split_fixed(year, " ", 3)[, 3] %>% as.numeric]
  
  
  # trim county strings
  mort[, county := str_trim(county)]
  
  # trim disese strings
  mort[, disease := str_trim(disease)]
  # str(mort)  
}


# pop
{
  # remove uninformative variables
  pop[, c("Varste.si.grupe.de.varsta", "Sexe", "Medii.de.rezidenta", 
           "UM..Numar.persoane") := NULL]
  
  # edit variable names
  setnames(pop, 
           names(pop), 
           c("county", "year", "pop"))
  
  # filter county data
  levels_to_remove <- str_subset(pop$county, 
                                 "Regiunea|MACROREGIUNEA|TOTAL|Mun. Bucuresti -incl. SAI")
  pop <- pop[! county %in% levels_to_remove]
  
  # clean year variable
  pop[, year := str_split_fixed(year, " ", 3)[, 3] %>% as.numeric]
  
  # trim county strings
  pop[, county := str_trim(county)]
}


# merge pop and mort
mort <- merge(mort, pop, by = c("county", "year"), all.x = T)

# remove years with no population data
mort <- mort[! is.na(pop)]

# calculate incidence per 10000 people
mort[, incidence := n_people * 10000 / pop]



# Explore data ------------------------------------------------------------

mort[, unique(disease)] # 21 levels
subset_diseases <- c("Tumori", 
                     "Malformatii congenitale  deformatii si anomalii  cromozomiale",
                     "Boli ale aparatului digestiv",
                     "Boli ale aparatului respirator",
                     "Boli ale aparatului circulator", 
                     "Boli endocrine  de nutritie si metabolism",
                     "Boli infectioase si parazitare",
                     "Tulburari mentale si de comportament")


p <- 
  mort %>% 
  filter(disease %in% subset_diseases) %>%
  ggplot(aes(year, incidence, col = county)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~disease, scales = "free") +
  scale_x_continuous(limits = c(1990, 2020)) +
  guides(color = "none")

direct.label(p, "last.points")

p <- 
  mort %>% 
  filter(disease %in% subset_diseases[1]) %>%
  ggplot(aes(year, incidence, col = county)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~disease, scales = "free") +
  scale_x_continuous(limits = c(1990, 2020)) +
  guides(color = "none")

direct.label(p, "last.points")

p <- 
  mort %>% 
  filter(disease %in% subset_diseases[2]) %>%
  ggplot(aes(year, incidence, col = county)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~disease, scales = "free") +
  scale_x_continuous(limits = c(1990, 2020)) +
  guides(color = "none")

direct.label(p, "last.points")

p <- 
  mort %>% 
  filter(disease %in% subset_diseases[4]) %>%
  ggplot(aes(year, incidence, col = county)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~disease, scales = "free") +
  scale_x_continuous(limits = c(1990, 2020)) +
  guides(color = "none")

direct.label(p, "last.points")


p <- 
  mort %>% 
  filter(disease %in% subset_diseases[5]) %>%
  ggplot(aes(year, incidence, col = county)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~disease, scales = "free") +
  scale_x_continuous(limits = c(1990, 2020)) +
  guides(color = "none")

direct.label(p, "last.points")

# Build map ---------------------------------------------------------------


