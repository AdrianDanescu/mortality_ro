
# Load packages -----------------------------------------------------------

library(data.table)
library(dplyr)

library(stringr)

library(broom)
library(purrr)
library(tidyr)

library(ggplot2)
library(gganimate) # needs ImageMagick, https://imagemagick.org/script/download.php
library(directlabels)
library(shiny)

library(sf)
library(tmap)
library(classInt) # for univariate class intervals - important for legend categ
library(animation)


# Read Data ---------------------------------------------------------------

# absolute mortality counts
mort <- rbindlist(list(
  read.csv2("data/exportPivot_POP206C_1.csv", sep = ","),
  read.csv2("data/exportPivot_POP206C_2.csv", sep = ",")))

pop <- read.csv2("data/exportPivot_POP107A_totals_county.csv", 
                 sep = ",") %>% as.data.table

# manually add admin_reg (Regiuni administrative)
# SOURCE: https://ro.wikipedia.org/wiki/Organizarea_administrativ-teritorial%C4%83_a_Rom%C3%A2niei
admin_regions <- 
  rbindlist(list(
    data.frame(admin_reg = "Nord-Est", 
               county = c("Iasi", "Botosani", "Neamt", "Suceava", "Bacau", 
                          "Vaslui")),
    data.frame(admin_reg = "Vest", 
               county = c("Arad", "Caras-Severin", "Hunedoara", "Timis")),
    data.frame(admin_reg = "Nord-Vest", 
               county = c("Bihor", "Bistrita-Nasaud", "Cluj", "Maramures", 
                          "Satu Mare", "Salaj")),
    data.frame(admin_reg = "Centru", 
               county = c("Alba", "Sibiu", "Mures", "Harghita", "Covasna", 
                          "Brasov")),
    data.frame(admin_reg = "Sud-Est", 
               county = c("Tulcea", "Vrancea", "Galati", "Braila", "Buzau", 
                          "Constanta")),
    data.frame(admin_reg = "Sud-Muntenia", 
               county = c("Arges", "Dambovita", "Prahova", "Ialomita", 
                          "Calarasi", "Giurgiu", "Teleorman")),
    data.frame(admin_reg = "Bucuresti - Ilfov", 
               county = c("Municipiul Bucuresti", "Ilfov")),
    data.frame(admin_reg = "Sud-Vest Oltenia", 
               county = c("Mehedinti", "Gorj", "Valcea", "Olt", "Dolj"))
  ))



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


# merge admin_regions and mort
mort <- merge(mort, admin_regions, by = c("county"), all.x = T)


subset_diseases <- c("Tumori", 
                     "Malformatii congenitale  deformatii si anomalii  cromozomiale",
                     "Boli ale aparatului digestiv",
                     "Boli ale aparatului respirator",
                     "Boli ale aparatului circulator", 
                     "Boli endocrine  de nutritie si metabolism",
                     "Boli infectioase si parazitare",
                     "Tulburari mentale si de comportament")


# Explore data ------------------------------------------------------------

# mort[, unique(disease)] # 21 levels



for (i in seq_along(subset_diseases[1:7])) {
  p <- mort %>% 
    filter(disease %in% subset_diseases[i]) %>%
    ggplot(aes(year, incidence, col = county)) +
    geom_line(alpha = 1/3) +
    facet_wrap(~ admin_reg, scales = "free_x") +
    scale_x_continuous(limits = c(1990, 2020)) +
    guides(color = "none") +
    ggtitle(subset_diseases[i])
  
  print(direct.label(p, "last.qp"))
}

# ERROR: the last disease set, labels probably to close to each other
# p <- mort %>% 
#   filter(disease %in% subset_diseases[8]) %>%
#   ggplot(aes(year, incidence, col = county)) +
#   geom_line(alpha = 1/3) +
#   facet_wrap(~ admin_reg, scales = "free_x") +
#   scale_x_continuous(limits = c(1990, 2020)) +
#   guides(color = "none") +
#   ggtitle(subset_diseases[8])
# 
# direct.label(p, "last.qp")


# Calculate linear regression coefficients
{
  mort %>% 
    filter(disease %in% subset_diseases) %>% 
    nest(-c(county, disease)) %>% 
    mutate(models = map(data, ~lm(incidence ~ year, data = .)),
           tidied = map(models, ~tidy(.))) %>% 
    unnest(tidied) %>% 
    filter(term == "year") %>% 
    mutate(p.adjusted = p.adjust(p.value)) %>% 
    filter(p.adjusted < 0.05) %>% 
    arrange(desc = -estimate) %>% 
    View
}

# Tumor data: View individual counties with regression lines
{
  # simple regression line for each region
  mort %>% 
    filter(disease %in% subset_diseases[1]) %>%
    ggplot(aes(year, incidence, col = county)) +
    geom_line(alpha = 1/3) +
    facet_wrap(~ admin_reg, scales = "free_x") +
    scale_x_continuous(limits = c(1990, 2020)) +
    guides(color = "none") +
    ggtitle(subset_diseases[1]) +
    geom_smooth(aes(group = 1), method = lm, se = F) 
  
  
  # make each county visible with all others in the background
  counties <- mort[, unique(county)]
  for (i in seq_along(counties)) {
    p <-  mort %>% 
      filter(disease %in% subset_diseases[1]) %>%
      ggplot(aes(year, incidence, group = county)) +
      geom_line(aes(group = county), alpha = 1/3, col = "gray70") +
      geom_smooth(aes(group = 1), method = lm, se = F, 
                  col = "gray70", lwd = 2) +
      geom_line(data = mort[county == counties[i] &
                              disease %in% subset_diseases[1], ], 
                aes(col = county), lwd = 1) +
      scale_x_continuous(limits = c(1990, 2020)) +
      guides(color = "none") +
      ggtitle(subset_diseases[1]) 
    print(direct.label(p, "last.qp"))
  }
  
  # make each county visible with all others in the background (gganimate)
  mort_tum <- mort %>% 
    filter(disease %in% subset_diseases[1])
  p <-  
    ggplot(mort_tum, aes(year, incidence)) +
    geom_line(aes(group = county), alpha = 1/3, col = "gray70") +
    geom_smooth(aes(group = 1), method = lm, se = F, 
                col = "gray40", lwd = 2) +
    geom_smooth(aes(frame = county), method = lm, se = F, 
                col = "gray70", alpha = 0.1, lwd = 1.25) +
    geom_line(aes(frame = county), col = "red", lwd = 1) +
    scale_x_continuous(limits = c(1990, 2018),
                       breaks = seq(1990, 2020, by = 5)) +
    scale_y_continuous(limits = c(5, 30), 
                       name = "Incidence per 10,000 persons") +
    guides(color = "none") +
    ggtitle(label = "County:", 
            subtitle = "Time-series of tumor-related mortality") +
    theme_minimal()
  
  # gganimate(p, interval = .9)
  gganimate(p, interval = .9, "output/tumors_ts.gif", 
            ani.width  = 800, 
            ani.height = 800)
  gganimate(p, interval = .9, "output/tumors_ts.mp4", 
            ani.width  = 800, 
            ani.height = 800)
  # animation::ani.options()
}


# Shiny app ---------------------------------------------------------------


counties <- sort(unique(mort$county))


# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      HTML("In Romania there is a strong positive trend in tumor-related mortality"),
      
      # break for visual separation
      br(), br(),
      
      # Select variable for y-axis
      selectInput(inputId = "counties",
                  label = "Select your county:",
                  choices = counties,
                  selected = "Municipiul Bucuresti")
      
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "lineplot")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot
  output$lineplot <- renderPlot({
    req(input$counties)
    mort_tum <- mort %>% 
      filter(disease %in% subset_diseases[1]) 
    
    mort_tum %>% 
      filter(county %in% input$counties) %>% 
      ggplot(aes(year, incidence)) +
      geom_line(data = mort_tum, aes(group = county), alpha = 1/3, col = "gray70") +
      geom_smooth(data = mort_tum, aes(group = 1), method = lm, se = F, 
                  col = "gray40", lwd = 2) +
      geom_smooth(aes(group = county), method = lm, se = F, 
                  col = "gray70", alpha = 0.1, lwd = 1.25) +
      geom_line(aes(group = county), col = "red", lwd = 1) +
      scale_x_continuous(limits = c(1990, 2018),
                         breaks = seq(1990, 2020, by = 5)) +
      scale_y_continuous(limits = c(8, 30), 
                         name = "Incidence per 10,000 persons",
                         breaks = seq(10, 35, by = 5)) +
      guides(color = "none") +
      ggtitle(label = paste0("County: ", unique(input$counties)), 
              subtitle = "Time-series of tumor-related mortality") +
      theme_minimal()
  })
  
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)


# Animated map - tumors ---------------------------------------------------

# Read shapefile prepared with `prepare_nuts_ro.r` script
counties_poly <- sf::st_read(dsn = "data/shp_ro/nuts_ro_county/nuts_ro_county.shp",
                             stringsAsFactors = FALSE)

# Check county naming differences
setdiff(unique(mort$county),
        unique(counties_poly$NAME_ASCI))

setdiff(unique(counties_poly$NAME_ASCI),
        unique(mort$county))

mort[county == "Municipiul Bucuresti", county := "Bucuresti"]

# Add incidence data to county polygons.
# Issue: does not return NA-s for non-matching cases 
# (e.g. Bucuresti in 1995 should be NA).
# So cannot control for NA color in tmap 
# (by default they are white/transparent).
counties_poly <- merge(counties_poly, 
                       mort_tum, 
                       by.x = "NAME_ASCI",
                       by.y = "county",
                       all.x = TRUE)


# Check stats of incidence to get an idea about legend breaks.
# NOTE: Choosing the type of breaks has huge impact on map!
hist(mort_tum$incidence)
summary(mort_tum$incidence)

# Choose univariate class intervals. 
# Check ?classIntervals, Details for various styles
cls <- classIntervals(var = mort_tum$incidence,
                      n = 4,
                      style = "jenks") # change style if needed
cls
# View classes along "Empirical distribution function"
plot(cls, pal = c("wheat1", "red3"))
# Round breaks to use with tmap functions
my_breaks <- 
  c( 
    floor(cls$brks[1]),                 # round down the min value
    cls$brks[-c(1, length(cls$brks))],  # breaks without the min and max values
    ceiling(cls$brks[length(cls$brks)]) # round up the max value
  ) 
my_breaks <- round(my_breaks)
rbind(cls$brks, my_breaks) # print to check rounding


# Animate map – incidence across time
saveGIF(
  expr =
  {
    for (yr in sort(unique(mort_tum$year)))
      print(
        counties_poly %>%
          filter(year == yr) %>%
          tm_shape() +
          tm_polygons(col = "incidence",
                      # Set breaks manually to keep same legend/categ. across time
                      style  = "fixed",
                      # breaks = c(10, 15, 20, 25, 30),
                      breaks = my_breaks,
                      # When set manual breaks, set auto.palette.mapping to FALSE
                      auto.palette.mapping = FALSE,
                      colorNA = "gray70", # in case of NA-s
                      textNA = "Missing",
                      title = paste0(yr, ": tumors incidence"))
      )
  },
  movie.name = "output/tumors_map.gif",
  ani.width  = 1200,
  ani.height = 800,
  interval   = .5
)
