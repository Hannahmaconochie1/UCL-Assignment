
### R Health Data Science assignment project code -----------------------------------------------

# Produce an intelligence report to inform the funding of a mobile heart health screening programme in your local area (Cornwall UTLA)

## Task 1: Analysis to help the ICB determine how much funding Cornwall UTLA should be allocated for mobile heart health screening programme --------

# Descriptive analysis of 2 indicators (e.g. stroke) related to CVD burden (incidence, prevalence or mortality) split by sex, age, time and/or geography

# 1 indicator should be benchmarked against other UTLAs, region and/or England overall

# Don't need to calculate the funding amount. Don't need to further justify your chosen indicator, but you cannot use the example indicator (diabetes).

# Load packages
library(here)
library(dplyr)
library(tidyverse)
library(sp)
library(viridis)
library(plotly)
library(sf)
library(ggplot2)
library(fingertipsR)

#Set wd

working_area = here()

setwd(working_area)

# Load data

cardiovascular_mortality <- fingertips_data(IndicatorID = 93956, AreaTypeID = 501) # IndicatorID = Cardiovascular mortality, AreaTypeID = Counties & UAs

# Filter for area and single years

cardiovascular_mortality_area <- filter(cardiovascular_mortality, AreaName %in% c('Cornwall', 'South West region (statistical)', 'England') & nchar(Timeperiod) == 4)

# Plot graph

cardiovascular_mortality_plot <- ggplot(data = cardiovascular_mortality_area, aes(x=as.numeric(Timeperiod), y=Value, colour=Sex))+
  geom_line(linewidth = 0.8)+
  scale_colour_brewer(palette="Paired")+
  labs(title = "Cardiovascular Disease Mortality Rate by Area")+
  ylab("Mortality Rate (per 100,000") +
  xlab("Time Period (Year)")+
  theme_light() +
  theme(axis.text.x = element_text(angle = -90,hjust=1))+
  facet_wrap(~factor(AreaName, c('Cornwall', 'South West region (statistical)', 'England')))

cardiovascular_mortality_plot

## Task 2: Analysis of 3 indicators of CVD by GP-level ------------------------------------------------------------------

# The indicators must not be diabetes and must be different from task 1. 

# 1 indicator should be a risk factor with a justification of why that risk factor is important for informing policy & service provision.

# All indicators should be split by sex, age, time and/or geography

# Use mapping file to determine your LTLA GP practice/codes


## Task 2b: recommend 4 different locations for mobile heart health --------

# Recommendations should be guided by GP-level analysis in task 2. They should take into account accessibility considerations e.g. transport.
