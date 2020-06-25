# Load Libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggrepel)
library(VCA)

# Theme for plotting
theme_set(ggthemes::theme_few() + theme(panel.background = element_rect(fill = "white"),
                                        legend.background = element_blank(),
                                        legend.key = element_blank()))

# Read metadata files
runs <- read.csv("data/simulation_factors.csv")
parm_sets <- read.csv("data/SALUS_parameter_sets.csv")
sites <- read.csv("data/site_description.csv")

# Global variables
harvestDoy <- 288 # to compute annual productivity
initDOY <- 125 # to compute annual changes for SOC
harvestFraction <- 0.65 # i.e., 35% of the Aboveground biomass is non-harvestable litter and other residues 
biomassCfraction <- 0.44
distRange <- 0.9 # For computing ranges 
