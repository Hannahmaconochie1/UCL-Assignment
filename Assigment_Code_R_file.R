
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
library(readODS)
library(readxl)

#Set wd

working_area = here()

setwd(working_area)

# Load Fingertips API lookups

API_area_lookup <- read_ods(here("Inputs","api_annex.ods"), sheet = 5)

API_indicator_lookup <- read_ods(here("Inputs","api_annex.ods"), sheet = 4)

# Load GEOjson file

lad_boundaries_2024 <- read_sf(here("Inputs","Local_Authority_Districts_December_2024_Boundaries_UK_BFC_-8514277369542505193.geojson"))

# Load cardiovascular mortality data

cardiovascular_mortality <- fingertips_data(IndicatorID = 93956, AreaTypeID = 501) # IndicatorID = Cardiovascular mortality, AreaTypeID = Districts & UAs (from Apr 2023)

# Filter for area and single years

cardiovascular_mortality_area <- filter(cardiovascular_mortality, AreaName %in% c('Cornwall', 'South West region (statistical)', 'England') & nchar(Timeperiod) == 4)

# Plot cardiovascular mortality graph

cardiovascular_mortality_plot <- ggplot(data = cardiovascular_mortality_area, aes(x=as.numeric(Timeperiod), y=Value, colour=Sex))+
  geom_line(linewidth = 0.8)+
  geom_ribbon(aes(ymin = LowerCI95.0limit, ymax = UpperCI95.0limit, fill = Sex), alpha = 0.2, colour = NA) + 
  scale_colour_brewer(name = "Area",palette="Paired")+
  scale_fill_brewer(name = "Area", palette = "Paired")+
  labs(title = "Cardiovascular Disease Mortality Rate by Area")+
  ylab("Mortality Rate (per 100,000)")+
  xlab("Time Period (Year)")+
  theme_light()+
  theme(axis.text.x = element_text(angle = -90,hjust=1))+
  facet_wrap(~factor(AreaName, c('Cornwall', 'South West region (statistical)', 'England')), labeller = as_labeller(c('Cornwall' = 'Cornwall', 'South West region (statistical)' = 'South West region', 'England' = 'England')))

cardiovascular_mortality_plot

# Plot cardiovascular mortality chloropleth map 

combined_cardiovascular_data <- lad_boundaries_2024 %>% 
  left_join(cardiovascular_mortality, 
            by = c("LAD24CD" = "AreaCode")) %>% 
  filter(Sex=='Persons'&Timeperiod %in% c('2019','2023'))

cardio_mortality_choro_map <- combined_cardiovascular_data %>% 
  ggplot(aes(fill = Value))+ 
  geom_sf(colour = NA) + # Adding 'colour = NA' removes boundaries around LA
  scale_fill_viridis("Value") + 
  labs(title = "Cardiovascular Disease Mortality Rate by Area")+
  theme_void()+
  facet_wrap(~Timeperiod)

cardio_mortality_choro_map

# Load stroke mortality data
ckd_prevalence <- fingertips_data(IndicatorID = 258, AreaTypeID = 502) # IndicatorID = Cardiovascular mortality, AreaTypeID = Counties & UAs

# Filter for area and single years

ckd_prevalence_area <- filter(ckd_prevalence, AreaName %in% c('Cornwall', 'England')) 

ckd_prevalence_area$Timeperiodgraph <- substr(ckd_prevalence_area$TimeperiodSortable, start=1, stop=4) 

# Plot CKD prevalence graph
ckd_prevalence_area_plot <- ggplot(data = ckd_prevalence_area, aes(x=as.numeric(Timeperiodgraph), y=Value, colour=AreaName))+
  geom_line(linewidth = 0.8)+
  geom_ribbon(aes(ymin = LowerCI95.0limit, ymax = UpperCI95.0limit, fill = AreaName), alpha = 0.2, colour = NA) + 
  scale_colour_brewer(name = "Area", palette = "Paired")+
  scale_fill_brewer(name = "Area", palette = "Paired")+
  labs(title = "Chronic Kidney Disease Prevalence (%)")+
  ylab("Chronic Kidney Disease Prevalence")+
  xlab("Time Period (Financial Year)")+
  theme_light()+
  theme(axis.text.x = element_text(angle = -90,hjust=1))

ckd_prevalence_area_plot

## Task 2: Analysis of 3 indicators of CVD by GP-level ------------------------------------------------------------------

# The indicators must not be diabetes and must be different from task 1. 

# 1 indicator should be a risk factor with a justification of why that risk factor is important for informing policy & service provision.

# All indicators should be split by sex, age, time and/or geography

# Use mapping file to determine your LTLA GP practice/codes

# Load mapping file data

GP_lookup_data <- read_excel(here("Inputs","x-boundary-mapping-2023-24-v1.xlsx"), sheet=2)

names(GP_lookup_data) <- GP_lookup_data[2,]

GP_lookup_data_area <- filter(GP_lookup_data, UTLA21name=='Cornwall')

GP_Cornwall_codes <- GP_lookup_data_area$Practice

# Load heart failure prevalence data

QOF_HF_AFib_Data <- fingertips_data(IndicatorID=262, AreaTypeID=7) # IndicatorID =  Heart Failure: QOF prevalence, AreaTypeID = GPs

QOF_HF_AFib_Data_area <- filter(QOF_HF_AFib_Data, AreaCode %in% c(GP_Cornwall_codes))

# Plot heart failure bar chart 
QOF_HF_AFib_Data_area_bar_increase <- QOF_HF_AFib_Data_area %>% 
  filter(Timeperiod=='2023/24'& `ComparedtoPCNs(v.25/10/24)valueorpercentiles`=='Higher 99.8'&RecentTrend=='Increasing') %>% 
  arrange(desc(Value))

QOF_HF_AFib_Data_area_bar_chart_increase <- ggplot(QOF_HF_AFib_Data_area_bar_increase, aes(x = AreaName, y = Value))+
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1")+
  geom_errorbar(aes(x=AreaName, ymin=LowerCI95.0limit, ymax=UpperCI95.0limit), width=0.2, colour="#333333", alpha=0.5, size=0.5)+
  geom_hline(yintercept = 0, size = 1, colour="#333333")+
  labs(title="Heart Failure Prevalence by GP")+
  ylab("Heart Failure Prevalence (%)") +
  xlab("")+
  theme(axis.text.x = element_text(angle = -90))  

QOF_HF_AFib_Data_area_bar_chart_increase

QOF_HF_AFib_Data_area_bar_value <- QOF_HF_AFib_Data_area %>% 
  filter(Timeperiod=='2023/24') %>% 
  arrange(desc(Value)) %>% 
  head(6)

QOF_HF_AFib_Data_area_bar_chart_value <- ggplot(QOF_HF_AFib_Data_area_bar_value, aes(x = AreaName, y = Value))+
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1")+
  geom_errorbar( aes(x=AreaName, ymin=LowerCI95.0limit, ymax=UpperCI95.0limit), width=0.2, colour="#333333", alpha=0.5, size=0.5)+
  geom_hline(yintercept = 0, size = 1, colour="#333333")+
  labs(title="Heart Failure Prevalence by GP")+
  ylab("Heart Failure Prevalence (%)") +
  xlab("")+
  theme(axis.text.x = element_text(angle = -90))  

QOF_HF_AFib_Data_area_bar_chart_value

# Load smoking prevalence data

QOF_Smoking_Data <- fingertips_data(IndicatorID=91280, AreaTypeID=7) # IndicatorID =  Smoking: QOF prevalence (% pts over 15 who are recorded as smokers), AreaTypeID = GPs

# Filter smoking prevalence data

QOF_Smoking_Data_area <- filter(QOF_Smoking_Data, AreaCode %in% c(GP_Cornwall_codes))

# Plot all smoking prevalence data for 23/24

QOF_Smoking_Data_area_all_bar <- QOF_Smoking_Data_area %>% 
  filter(Timeperiod %in% c('2023/24')) 

QOF_Smoking_Data_area_all_bar_chart <- ggplot(QOF_Smoking_Data_area_all_bar, aes(x = AreaName, y = Value))+
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1")+
  geom_errorbar( aes(x=AreaName, ymin=LowerCI95.0limit, ymax=UpperCI95.0limit), width=0.2, colour="#333333", alpha=0.5, size=0.5)+
  geom_hline(yintercept = 0, size = 1, colour="#333333")+
  labs(title="Smoking Prevalence by GP")+
  ylab("Smoking Prevalence (15+ years old, %)")+
  xlab("")+
  theme(axis.text.x = element_text(angle = -90))

QOF_Smoking_Data_area_all_bar_chart

# Plot highest smoking prevalence data for 2013/14, 2018/19, 2023/24

QOF_Smoking_Data_area_high_bar <- QOF_Smoking_Data_area %>% 
  filter(Timeperiod %in% c('2013/14', '2018/19', '2023/24')) %>% 
  group_by(Timeperiod) %>% 
  slice_max(order_by = Value, n = 6)

QOF_Smoking_Data_area_high_bar_chart <- ggplot(QOF_Smoking_Data_area_bar, aes(x = AreaName, y = Value))+
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1")+
  geom_errorbar( aes(x=AreaName, ymin=LowerCI95.0limit, ymax=UpperCI95.0limit), width=0.2, colour="#333333", alpha=0.5, size=0.5)+
  geom_hline(yintercept = 0, size = 1, colour="#333333")+
  labs(title="Smoking Prevalence by GP")+
  ylab("Smoking Prevalence (15+ years old, %)")+
  xlab("")+
  theme(axis.text.x = element_text(angle = -90))+
  facet_wrap(~Timeperiod)

QOF_Smoking_Data_area_high_bar_chart

# Load Hypertension prevalence data

QOF_Hypertension_Data <- fingertips_data(IndicatorID=219, AreaTypeID=7) # IndicatorID =  Hypertension: QOF prevalence, AreaTypeID = GPs

# Filter data
QOF_Hypertension_Data_area <- filter(QOF_Hypertension_Data, AreaCode %in% c(GP_Cornwall_codes))

QOF_Hypertension_Data_area_all <- QOF_Hypertension_Data_area %>% 
  filter(Timeperiod %in% c('2023/24'))

# Plot all Hypertension prevalence bar chart

QOF_Hypertension_Data_area_bar <- QOF_Hypertension_Data_area %>% 
  filter(Timeperiod %in% c('2013/14', '2018/19', '2023/24')) %>% 
  group_by(Timeperiod) %>% 
  slice_max(order_by = Value, n = 6)

QOF_Hypertension_Data_area_bar_chart <- ggplot(QOF_Hypertension_Data_area_bar, aes(x = AreaName, y = Value))+
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1")+
  geom_errorbar( aes(x=AreaName, ymin=LowerCI95.0limit, ymax=UpperCI95.0limit), width=0.2, colour="#333333", alpha=0.5, size=0.5)+
  geom_hline(yintercept = 0, size = 1, colour="#333333")+
  labs(title="Hypertension Prevalence by GP")+
  ylab("Hypertension Prevalence")+
  xlab("")+
  theme(axis.text.x = element_text(angle = -90))+
  facet_wrap(~Timeperiod)

QOF_Hypertension_Data_area_bar_chart

## Task 2b: recommend 4 different locations for mobile heart health --------

# Recommendations should be guided by GP-level analysis in task 2. They should take into account accessibility considerations e.g. transport.

# Load health check uptake data	

#https://www.google.com/maps/d/edit?mid=10FLq8_gJE6_sYli1EGtWEM6qUiA_zYA&usp=sharing