# Code to run all the models with CDS data 

rm(list=ls())
start.time <- Sys.time()
print(start.time)

# Create data path 

data_path = "data"
set.seed(14091998)
# Select number of iterations and burn-in phases 

n.iter = 1e7
n.burn = 10000

# Load required libraries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, BMS, lubridate,zoo,ggplot2,stringr)

# Read CDS data file 

cds.file = "data/cds_data.xlsx"

country.names = sort(excel_sheets(path = cds.file))

# Load the necessary functions 

func.files <- list.files(path = "funcs", pattern = "\\.R$", full.names = TRUE)

for (file in func.files) {
  
  source(file)
  
}

for (country in country.names) {
  
  nam <- paste(country, "cds", sep = ".")
  assign(nam, get.cds(country))
  
}

cds.data = rbind(austria.cds, belgium.cds, finland.cds, france.cds,
                 germany.cds, greece.cds, ireland.cds, italy.cds, latvia.cds,
                 lithuania.cds, netherlands.cds, portugal.cds, 
                 slovakia.cds, slovenia.cds,spain.cds)

# To do some plots, import the dataset with the synch data 

plot.data = read_excel(file.path(data_path, "synch_levels.xlsx")) %>% 
  select(date, country,synch)

plot.data = inner_join(plot.data, cds.data %>% 
                         select(date, country, cdsgr))

# Make some plots 

plot.data %>% 
  mutate(country = str_to_title(country)) %>% 
  ggplot(aes(y = synch, x = cdsgr)) +
  geom_point(size=.5, color = "red") + 
  geom_smooth(method = "lm", se = T, color = "black") +
  facet_wrap(~country, scales = "free") + 
  theme_bw() + 
  ylab("Yield synchronization rates") + 
  xlab("CDS USD 5y growth rate (log-diff, in %)")
  
ggsave("plots/cds_synch.eps", width = 8, height = 6, units = "in", device = cairo_ps)

# Start preparing the data for the BMA models 

# Delete contemporaneous CDS 

cds.data = cds.data %>% 
  select(-cdsgr)

# Import the full dataset 

fulldata = read_excel(file.path(data_path, "synch_levels.xlsx"))


# Add Draghi dummy + interactions

fulldata = fulldata %>% 
  mutate(draghi = ifelse(date >= '2012-07-01', 1, 0), 
         draghisynch_lag1 = draghi * synch_lag1, 
         draghisynch_lag2 = draghi * synch_lag2, 
         draghisynch_lag3 = draghi * synch_lag3, 
         draghisynch_lag4 = draghi * synch_lag4, 
         draghiuncert_lag1 = draghi * uncert_lag1,
         draghiuncert_lag2 = draghi * uncert_lag2,
         draghiuncert_lag3 = draghi * uncert_lag3,
         draghiuncert_lag4 = draghi * uncert_lag4, 
         draghigdp_lag1 = draghi * gdp_lag1, 
         draghigdp_lag2 = draghi * gdp_lag2,
         draghigdp_lag3 = draghi * gdp_lag3,
         draghigdp_lag4 = draghi * gdp_lag4, 
         draghibop_lag1 = draghi * bop_lag1, 
         draghibop_lag2 = draghi * bop_lag2,
         draghibop_lag3 = draghi * bop_lag3,
         draghibop_lag4 = draghi * bop_lag4, 
         draghidebttogdp_lag1 = draghi * debttogdp_lag1, 
         draghidebttogdp_lag2 = draghi * debttogdp_lag2, 
         draghidebttogdp_lag3 = draghi * debttogdp_lag3, 
         draghidebttogdp_lag4 = draghi * debttogdp_lag4, 
         draghiinflation_lag1 = draghi * inflation_lag1, 
         draghiinflation_lag2 = draghi * inflation_lag2,
         draghiinflation_lag3 = draghi * inflation_lag3,
         draghiinflation_lag4 = draghi * inflation_lag4) 

# Merge the two datasets and generate interaction terms with CDS variable

fulldata.cds = inner_join(fulldata,cds.data)

# Generate the interaction terms with the CDS variable

fulldata.cds = fulldata.cds %>% 
  mutate(pigscds_lag1 = pigs * cds_lag1, 
         pigscds_lag2 = pigs * cds_lag2, 
         pigscds_lag3 = pigs * cds_lag3, 
         pigscds_lag4 = pigs * cds_lag4, 
         reccds_lag1 = rec * cds_lag1, 
         reccds_lag2 = rec * cds_lag2, 
         reccds_lag3 = rec * cds_lag3, 
         reccds_lag4 = rec * cds_lag4,
         zlbcds_lag1 = zlb * cds_lag1, 
         zlbcds_lag2 = zlb * cds_lag2, 
         zlbcds_lag3 = zlb * cds_lag3, 
         zlbcds_lag4 = zlb * cds_lag4,
         draghicds_lag1 = draghi * cds_lag1, 
         draghicds_lag2 = draghi * cds_lag2, 
         draghicds_lag3 = draghi * cds_lag3, 
         draghicds_lag4 = draghi * cds_lag4)

# Delete variables at time t for clarity

fulldata.cds = fulldata.cds %>% 
  select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation)


# Note: number of countries does not change, but number of years does. 

country_dummy_names = c("d_belgium", "d_finland", "d_france", "d_germany", "d_greece", "d_ireland", 
                        "d_italy", "d_latvia", "d_lithuania", "d_netherlands", "d_portugal", "d_slovakia", 
                        "d_slovenia", "d_spain")

year_dummy_names = c("d_2005",
                     "d_2006",
                     "d_2007",
                     "d_2008",
                     "d_2009",
                     "d_2010",
                     "d_2011",
                     "d_2012",
                     "d_2013",
                     "d_2014",
                     "d_2015",
                     "d_2016",
                     "d_2017",
                     "d_2018",
                     "d_2019",
                     "d_2020", 
                     "d_2021")

fe_data = fulldata.cds %>% 
  mutate(d_belgium = ifelse(country == "belgium", 1 , 0),
         d_finland = ifelse(country == "finland", 1 , 0),
         d_france = ifelse(country == "france", 1 , 0),
         d_germany = ifelse(country == "germany", 1 , 0),
         d_greece = ifelse(country == "greece", 1 , 0),
         d_ireland = ifelse(country == "ireland", 1 , 0),
         d_italy = ifelse(country == "italy", 1 , 0),
         d_latvia = ifelse(country == "latvia", 1 , 0),
         d_lithuania = ifelse(country == "lithuania", 1, 0),
         d_netherlands = ifelse(country == "netherlands", 1 , 0),
         d_portugal = ifelse(country == "portugal", 1 , 0),
         d_slovakia = ifelse(country == "slovakia", 1 , 0),
         d_slovenia = ifelse(country == "slovenia", 1 , 0),
         d_spain = ifelse(country == "spain", 1 , 0)) %>%
  
  mutate(d_2005 = ifelse(year == 2005, 1 , 0),
         d_2006 = ifelse(year == 2006, 1 , 0),
         d_2007 = ifelse(year == 2007, 1 , 0),
         d_2008 = ifelse(year == 2008, 1 , 0),
         d_2009 = ifelse(year == 2009, 1 , 0),
         d_2010 = ifelse(year == 2010, 1 , 0),
         d_2011 = ifelse(year == 2011, 1 , 0),
         d_2012 = ifelse(year == 2012, 1 , 0),
         d_2013 = ifelse(year == 2013, 1 , 0),
         d_2014 = ifelse(year == 2014, 1 , 0),
         d_2015 = ifelse(year == 2015, 1 , 0),
         d_2016 = ifelse(year == 2016, 1 , 0),
         d_2017 = ifelse(year == 2017, 1 , 0),
         d_2018 = ifelse(year == 2018, 1 , 0),
         d_2019 = ifelse(year == 2019, 1 , 0),
         d_2020 = ifelse(year == 2020, 1 , 0),
         d_2021 = ifelse(year == 2021, 1 , 0)) %>% 
  select(-year,-country)

model.fe <-  bms(fe_data %>% 
                   select(-pigs), burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                 nmodel = 10000, mcmc = "bd", user.int = F, 
                 fixed.reg = c(year_dummy_names, country_dummy_names), randomizeTimer = F)


# No FE model (remember, here we estimate PIIGS dummy because we are not adding country fixed effects)

nofe_data = fe_data %>% 
  select(!country_dummy_names) 


model.nofe <-  bms(nofe_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                 nmodel = 10000, mcmc = "bd", user.int = F, 
                 fixed.reg = year_dummy_names, randomizeTimer = F)

# Heredity model (remember, here we are again estimating a PIIGS dummy because we are not adding country fixed effects)
# The model cannot be estimated if we do not include 

heredity_data = nofe_data %>% 
  rename("rec#synch_lag1" = recsynch_lag1,
         "rec#synch_lag2" = recsynch_lag2,
         "rec#synch_lag3" = recsynch_lag3,
         "rec#synch_lag4" = recsynch_lag4,
         "rec#uncert_lag1" = recuncert_lag1,
         "rec#uncert_lag2" = recuncert_lag2,
         "rec#uncert_lag3" = recuncert_lag3,
         "rec#uncert_lag4" = recuncert_lag4,
         "rec#bop_lag1" = recbop_lag1,
         "rec#bop_lag2" = recbop_lag2,
         "rec#bop_lag3" = recbop_lag3,
         "rec#bop_lag4" = recbop_lag4, 
         "rec#debttogdp_lag1" = recdebttogdp_lag1,
         "rec#debttogdp_lag2" = recdebttogdp_lag2,
         "rec#debttogdp_lag3" = recdebttogdp_lag3,
         "rec#debttogdp_lag4" = recdebttogdp_lag4,
         "rec#gdp_lag1" = recgdp_lag1,
         "rec#gdp_lag2" = recgdp_lag2,
         "rec#gdp_lag3" = recgdp_lag3,
         "rec#gdp_lag4" = recgdp_lag4,
         "rec#inflation_lag1" = recinflation_lag1,
         "rec#inflation_lag2" = recinflation_lag2,
         "rec#inflation_lag3" = recinflation_lag3,
         "rec#inflation_lag4" = recinflation_lag4,
         "pigs#synch_lag1" = pigssynch_lag1,
         "pigs#synch_lag2" = pigssynch_lag2,
         "pigs#synch_lag3" = pigssynch_lag3,
         "pigs#synch_lag4" = pigssynch_lag4,
         "pigs#uncert_lag1" = pigsuncert_lag1,
         "pigs#uncert_lag2" = pigsuncert_lag2,
         "pigs#uncert_lag3" = pigsuncert_lag3,
         "pigs#uncert_lag4" = pigsuncert_lag4,
         "pigs#bop_lag1" = pigsbop_lag1,
         "pigs#bop_lag2" = pigsbop_lag2,
         "pigs#bop_lag3" = pigsbop_lag3,
         "pigs#bop_lag4" = pigsbop_lag4,
         "pigs#debttogdp_lag1" = pigsdebttogdp_lag1,
         "pigs#debttogdp_lag2" = pigsdebttogdp_lag2,
         "pigs#debttogdp_lag3" = pigsdebttogdp_lag3,
         "pigs#debttogdp_lag4" = pigsdebttogdp_lag4,
         "pigs#gdp_lag1" = pigsgdp_lag1,
         "pigs#gdp_lag2" = pigsgdp_lag2,
         "pigs#gdp_lag3" = pigsgdp_lag3,
         "pigs#gdp_lag4" = pigsgdp_lag4,
         "pigs#inflation_lag1" = pigsinflation_lag1,
         "pigs#inflation_lag2" = pigsinflation_lag2,
         "pigs#inflation_lag3" = pigsinflation_lag3,
         "pigs#inflation_lag4" = pigsinflation_lag4,
         "zlb#synch_lag1" = zlbsynch_lag1,
         "zlb#synch_lag2" = zlbsynch_lag2,
         "zlb#synch_lag3" = zlbsynch_lag3,
         "zlb#synch_lag4" = zlbsynch_lag4,
         "zlb#uncert_lag1" = zlbuncert_lag1,
         "zlb#uncert_lag2" = zlbuncert_lag2,
         "zlb#uncert_lag3" = zlbuncert_lag3,
         "zlb#uncert_lag4" = zlbuncert_lag4,
         "zlb#bop_lag1" = zlbbop_lag1,
         "zlb#bop_lag2" = zlbbop_lag2,
         "zlb#bop_lag3" = zlbbop_lag3,
         "zlb#bop_lag4" = zlbbop_lag4,
         "zlb#debttogdp_lag1" = zlbdebttogdp_lag1,
         "zlb#debttogdp_lag2" = zlbdebttogdp_lag2,
         "zlb#debttogdp_lag3" = zlbdebttogdp_lag3,
         "zlb#debttogdp_lag4" = zlbdebttogdp_lag4,
         "zlb#gdp_lag1" = zlbgdp_lag1,
         "zlb#gdp_lag2" = zlbgdp_lag2,
         "zlb#gdp_lag3" = zlbgdp_lag3,
         "zlb#gdp_lag4" = zlbgdp_lag4,
         "zlb#inflation_lag1" = zlbinflation_lag1,
         "zlb#inflation_lag2" = zlbinflation_lag2,
         "zlb#inflation_lag3" = zlbinflation_lag3,
         "zlb#inflation_lag4" = zlbinflation_lag4,
         
         "draghi#synch_lag1" = draghisynch_lag1,
         "draghi#synch_lag2" = draghisynch_lag2,
         "draghi#synch_lag3" = draghisynch_lag3,
         "draghi#synch_lag4" = draghisynch_lag4,
         "draghi#uncert_lag1" = draghiuncert_lag1,
         "draghi#uncert_lag2" = draghiuncert_lag2,
         "draghi#uncert_lag3" = draghiuncert_lag3,
         "draghi#uncert_lag4" = draghiuncert_lag4,
         "draghi#bop_lag1" = draghibop_lag1,
         "draghi#bop_lag2" = draghibop_lag2,
         "draghi#bop_lag3" = draghibop_lag3,
         "draghi#bop_lag4" = draghibop_lag4,
         "draghi#debttogdp_lag1" = draghidebttogdp_lag1,
         "draghi#debttogdp_lag2" = draghidebttogdp_lag2,
         "draghi#debttogdp_lag3" = draghidebttogdp_lag3,
         "draghi#debttogdp_lag4" = draghidebttogdp_lag4,
         "draghi#gdp_lag1" = draghigdp_lag1,
         "draghi#gdp_lag2" = draghigdp_lag2,
         "draghi#gdp_lag3" = draghigdp_lag3,
         "draghi#gdp_lag4" = draghigdp_lag4,
         "draghi#inflation_lag1" = draghiinflation_lag1,
         "draghi#inflation_lag2" = draghiinflation_lag2,
         "draghi#inflation_lag3" = draghiinflation_lag3,
         "draghi#inflation_lag4" = draghiinflation_lag4, 
         
         "zlb#cds_lag1" = zlbcds_lag1, 
         "zlb#cds_lag2" = zlbcds_lag2,
         "zlb#cds_lag3" = zlbcds_lag3,
         "zlb#cds_lag4" = zlbcds_lag4, 
         
         "draghi#cds_lag1" = draghicds_lag1, 
         "draghi#cds_lag2" = draghicds_lag2,
         "draghi#cds_lag3" = draghicds_lag3,
         "draghi#cds_lag4" = draghicds_lag4, 
         
         "rec#cds_lag1" = reccds_lag1, 
         "rec#cds_lag2" = reccds_lag2,
         "rec#cds_lag3" = reccds_lag3,
         "rec#cds_lag4" = reccds_lag4, 
         
         "pigs#cds_lag1" = pigscds_lag1, 
         "pigs#cds_lag2" = pigscds_lag2,
         "pigs#cds_lag3" = pigscds_lag3,
         "pigs#cds_lag4" = pigscds_lag4)


model.heredity =  bms(heredity_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                      nmodel = 10000, mcmc = "bd.int", user.int = F, 
                      fixed.reg = year_dummy_names, randomizeTimer = F)


coefs.fe <- coef(model.fe,  std.coefs = T, order.by.pip = F)
coefs.nofe <- coef(model.nofe,  std.coefs = T, order.by.pip = F)
coefs.heredity <- coef(model.heredity,  std.coefs = T, order.by.pip = F)

# Do not select the Fixed Effects results (year & country) #

coefs.fe = coefs.fe[!row.names(coefs.fe) %in% c(country_dummy_names, year_dummy_names),1:3]
coefs.nofe = coefs.nofe[!row.names(coefs.nofe) %in% c(year_dummy_names),1:3]
coefs.heredity = coefs.heredity[!row.names(coefs.heredity) %in% c(year_dummy_names),1:3]


# Stop the clock

current.time = Sys.time()
difftime(current.time, start.time, units = 'mins')
