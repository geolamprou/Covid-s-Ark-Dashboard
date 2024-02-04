# Create Rshiny Dashboard for Covid's Ark Database

#Libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(RPostgreSQL)
library(RPostgres)
library(DBI)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rnaturalearth)
library(sf)
library(glue)
library(tidyverse)
library(patchwork)
library(plotly)
library(maps)
library(mapdata)
library(stringr)
library(DT)
library(readxl)
library(ggwordcloud)

#Connect to PostgreSQL

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'covids_ark',
  host = 'localhost',
  port = '5432',
  user = 'postgres',
  password = 'lamiara75'
)

postgres_tables <-as.data.frame(dbListTables(conn))


#Epidemiological Data

cases_deaths_who <- dbGetQuery(conn, "SELECT  * FROM who_latest_reported_daily_covid19_cases_and_deaths")
comments_who_region <- c("AFRO - Africa", "AMRO - Americas", "EMRO - Eastern Mediterranean", "EURO - Europe", "Other - Other Regions", "SEARO - South-East Asia", "WPRO - Western Pacific")

timeline_cases_who <- ggplot(cases_deaths_who, aes(x=date_reported, y = new_cases, col = who_region))+
  geom_line(stat = 'identity') + 
  labs(caption = "AFRO - Africa, AMRO - Americas, EMRO - Eastern Mediterranean, EURO - Europe, Other - Other Regions, SEARO - South-East Asia, WPRO - Western Pacific")+
  theme_hc() +
  ggtitle("New Covid-19 Cases by WHO Region Code") +
  scale_color_brewer(palette = "Paired")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 10),
    axis.text.x = element_text(face = "italic"),
    axis.title = element_text(size=15)
  )

timeline_cases_who_interactive <- ggplotly(timeline_cases_who)

# Add a footnote-like annotation
timeline_cases_who_interactive <- timeline_cases_who_interactive %>% layout(
  annotations = list(
    x = 0.5,  # x-coordinate position
    y = 1.02, # y-coordinate position (negative values to move it below the plot)
    xref = "paper",
    yref = "paper",
    text = "<i>AFRO - Africa, AMRO - Americas, EMRO - Eastern Mediterranean, EURO - Europe, Other - Other Regions, SEARO - South-East Asia, WPRO - Western Pacific</i>", # Footnote text (use HTML tags for formatting)
    showarrow = FALSE,
    font = list(size = 15, color = "grey") # Adjust font size and color as needed
  )
)

timeline_deaths_who <- ggplot(cases_deaths_who, aes(x=date_reported, y = new_deaths, col = who_region))+
  geom_line(stat = 'identity') + 
  labs(caption = "AFRO - Africa, AMRO - Americas, EMRO - Eastern Mediterranean, EURO - Europe, Other - Other Regions, SEARO - South-East Asia, WPRO - Western Pacific")+
  theme_hc() +
  ggtitle("New Covid-19 Cases by WHO Region Code") +
  scale_color_brewer(palette = "Paired")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 10),
    axis.text.x = element_text(face = "italic"),
    axis.title = element_text(size=15)
  )

timeline_deaths_who_interactive <- ggplotly(timeline_deaths_who)

# Add a footnote-like annotation
timeline_deaths_who_interactive <- timeline_deaths_who_interactive %>% layout(
  annotations = list(
    x = 0.5,  # x-coordinate position
    y = 1.02, # y-coordinate position (negative values to move it below the plot)
    xref = "paper",
    yref = "paper",
    text = "<i>AFRO - Africa, AMRO - Americas, EMRO - Eastern Mediterranean, EURO - Europe, Other - Other Regions, SEARO - South-East Asia, WPRO - Western Pacific</i>", # Footnote text (use HTML tags for formatting)
    showarrow = FALSE,
    font = list(size = 15, color = "grey") # Adjust font size and color as needed
  )
)

# Cases and deaths in special living conditions

covid_behind_bars_latest_counts <- dbGetQuery(conn, "SELECT * FROM covid_behind_bars_usa_latest_state_counts")
covid_behind_bars_latest_facility_counts <- dbGetQuery(conn, "SELECT * FROM covid_behind_bars_usa_latest_facility_counts")


covid_behind_bars_latest_facility_counts_values <- covid_behind_bars_latest_facility_counts %>%
  filter(residents_deaths !='0')


Prison <-paste(covid_behind_bars_latest_facility_counts_values$state, covid_behind_bars_latest_facility_counts_values$name)

covid_behind_bars_resident_deaths <- ggplot(covid_behind_bars_latest_facility_counts_values, aes(x=date, y=residents_deaths, col=Prison))+
                                                geom_point()+
                                                theme_hc() +
                                                ggtitle("Prisoners Covid-19 Deaths in United States's Jails") +
                                                theme(
                                                  plot.title = element_text(hjust = 0.5, size = 13),
                                                  panel.grid = element_blank(),
                                                  axis.text = element_text(color = "blue", size = 10),
                                                  axis.text.x = element_text(face = "italic"),
                                                  axis.title = element_text(size=15)
                                                )

covid_behind_bars_resident_confirmed <- ggplot(covid_behind_bars_latest_facility_counts_values, aes(x=date, y=residents_confirmed, col=Prison))+
                                                   geom_point()+
                                                   theme_hc() +
                                                   ggtitle("Prisoners Covid-19 Confirmed Cases in United States's Jails") +
                                                   theme(
                                                     plot.title = element_text(hjust = 0.5, size = 13),
                                                     panel.grid = element_blank(),
                                                     axis.text = element_text(color = "blue", size = 10),
                                                     axis.text.x = element_text(face = "italic"),
                                                     axis.title = element_text(size=15)
                                                   )

prison_cases <- dbGetQuery(conn, "SELECT * FROM a_state_by_state_look_at_covid_in_prisons_prisons_cases")
prison_cases$as_of_date <- as.Date(prison_cases$as_of_date, tryFormats = "%m/%d/%y")

prison_cases_without_Nas <- prison_cases %>%
  filter(!is.na(total_prisoner_cases))

prisoner_staff_deaths<-ggplot(prison_cases, aes(x=total_prisoner_deaths, y = total_staff_deaths)) +
  geom_point(col='darkgreen') +
  geom_smooth(se = FALSE, col='red')+
  theme_hc() +
  ggtitle('Correlation between Prisoners and Staff Deaths')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic"))

usa_states <-map_data('state')
usa_states$region <- str_to_title(usa_states$region)

colnames(prison_cases_without_Nas)[1] <- "region"
merged_prison_cases_world <- merge(usa_states, prison_cases_without_Nas, sort=FALSE, by = "region")

merged_prison_cases_world <- merged_prison_cases_world[order(merged_prison_cases_world$order), ]

prison_cases_map <- ggplot(merged_prison_cases_world, aes(long, lat))+
  geom_polygon(aes(group = group, fill=total_prisoner_cases, color=region)) +
  coord_map() +
  theme_hc() +
  ggtitle("Prisoner Covid-19 Cases in United States's Jails") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 5),
    axis.text.x = element_text(face = "italic")
  )

#Epidemiological Forecasting

delphi_projections <- dbGetQuery(conn, "SELECT * FROM delphi_covid_analytics_cases_projections")
delphi_clinical_data <- dbGetQuery(conn, "SELECT * FROM delphi_covid_analytics_clinical_data")

delphi_projections <- delphi_projections %>%
  filter(country !='None')

timeline_delphi_projections <- ggplot(delphi_projections, aes(x=active, y = active_hospitalized, col = country))+
  geom_point(stat = 'identity') + 
  geom_smooth(se = FALSE, color='black', size = 0.2) +
  theme_hc() +
  ggtitle("Active Hospitalized Patients by Delphi Covid19 Dataset") +
  #scale_color_brewer(palette = "Paired")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 10),
    axis.text.x = element_text(face = "italic"),
    axis.title = element_text(size=15)
  )

timeline_delphi_projections_interactive <- ggplotly(timeline_delphi_projections)

# Vaccination against Covid-19 Data - 1. Vaccine Development

milken_vaccine_tracker <- dbGetQuery(conn, "SELECT  * FROM milken_institute_vaccine_tracker_covid19")
milken__treatments_vaccine_tracker <- dbGetQuery(conn, "SELECT  * FROM milken_institute_treatments_vaccine_tracker_covid19")

milken_vaccine_development <- list(milken__treatments_vaccine_tracker, milken_vaccine_tracker)
milken_vaccine_development_join <- milken_vaccine_development %>%
  reduce(full_join, by='developer_researcher')

who_vaccine_tracker_data <- dbGetQuery(conn, "SELECT  * FROM who_covid19_vaccine_tracker_and_landscape_clinical_data")
who_vaccine_tracker_pre_data <- dbGetQuery(conn, "SELECT  * FROM who_covid19_vaccine_tracker_and_landscape_pre_clinical_data_")

colnames(who_vaccine_tracker_data)[8] <- 'developers'
who_vaccine_development <- list(who_vaccine_tracker_data, who_vaccine_tracker_pre_data)
who_vaccine_development_join <- who_vaccine_development %>%
  reduce(full_join, by = 'developers')

# Joining of Vaccine Development

colnames(milken_vaccine_development_join)[1] <- "developers"

vaccine_development <- list(who_vaccine_development_join, milken_vaccine_development_join)
vaccine_development_join <- vaccine_development %>%
  reduce(full_join, by = 'developers')

vaccine_development_join$number_of_doses <- as.numeric(vaccine_development_join$number_of_doses)

vaccine_development_join_without_NAs <- vaccine_development_join %>%
  filter(!is.na(developers), !is.na(number_of_doses)) %>%
  filter(!duplicated(developers)) %>%
  group_by(number_of_doses)

vaccines_with_1_dose <- vaccine_development_join_without_NAs %>%
  filter(number_of_doses ==1)

vaccines_with_2_dose <- vaccine_development_join_without_NAs %>%
  filter(number_of_doses ==2)

vaccines_with_3_dose <- vaccine_development_join_without_NAs %>%
  filter(number_of_doses ==3)


# Vaccination against Covid-19 Data - 2. Vaccination Rollout

our_world_in_data <- dbGetQuery(conn, "SELECT * FROM data_on_covid_our_worldindata")
our_world_in_data_vaccinations <- our_world_in_data[ ,c(1,2,3,4, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47)]

# Line plots of Total Vaccinations by Continent provided by Our World in Data

our_world_in_data_vaccinations_Europe <- our_world_in_data_vaccinations %>%
  filter(continent =='Europe') %>%
  filter(!is.na(total_vaccinations))

our_world_in_data_vaccinations_Asia <- our_world_in_data_vaccinations %>%
  filter(continent =='Asia') %>%
  filter(!is.na(total_vaccinations))

our_world_in_data_vaccinations_South_America <- our_world_in_data_vaccinations %>%
  filter(continent =='South America') %>%
  filter(!is.na(total_vaccinations))

our_world_in_data_vaccinations_Oceania <- our_world_in_data_vaccinations %>%
  filter(continent =='Oceania') %>%
  filter(!is.na(total_vaccinations))

our_world_in_data_vaccinations_North_America <- our_world_in_data_vaccinations %>%
  filter(continent =='North America') %>%
  filter(!is.na(total_vaccinations))

our_world_in_data_vaccinations_Africa <- our_world_in_data_vaccinations %>%
  filter(continent =='Africa') %>%
  filter(!is.na(total_vaccinations))

#Plots for Vaccination Data - 1. Vaccine Development

vaccines_with_1_dose_graph <- ggplot(vaccines_with_1_dose, aes(x=vaccine_platform_description.x, fill=vaccine_platform_description.x)) + 
  geom_bar(stat = 'count', show.legend = TRUE, alpha = 0.7) +
  theme_hc() +
  ggtitle('Vaccine Platform Description of Vaccines with 1 Dose') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 10),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") +
  geom_text(
    aes(label = stat(count)),
    stat = 'count',
    vjust = 0.5,
    size = 5,
    position = position_stack(vjust = 0.5)
  )

vaccines_with_2_dose_graph <- ggplot(vaccines_with_2_dose, aes(x=vaccine_platform_description.x, fill = vaccine_platform_description.x)) + 
  geom_bar(stat = 'count', show.legend = TRUE, alpha = 0.8) +
  theme_hc() +
  ggtitle('Vaccine Platform Description of Vaccines with 2 Doses') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 10),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") +
  geom_text(
    aes(label = stat(count)),
    stat = 'count',
    vjust = 0.5,
    size = 5,
    position = position_stack(vjust = 0.5)
  )

vaccines_with_3_dose_graph <- ggplot(vaccines_with_3_dose, aes(x=vaccine_platform_description.x, fill = vaccine_platform_description.x )) + 
  geom_bar(stat = 'count', show.legend = TRUE, alpha = 0.7) +
  theme_hc() +
  ggtitle('Vaccine Platform Description of Vaccines with 3 Doses') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 10),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") +
  geom_text(
    aes(label = stat(count)),
    stat = 'count',
    vjust = 0.5,
    size = 5,
    position = position_stack(vjust = 0.5)
  )


description_of_vaccines_type <- vaccines_with_1_dose_graph / vaccines_with_2_dose_graph / vaccines_with_3_dose_graph

#Plots for Vaccination Data - 2. Vaccination Roll out

europe_vacc<-ggplot(data = our_world_in_data_vaccinations_Europe, aes(x=date_of_observation, y = total_vaccinations, col = location)) + 
  geom_line(stat = 'identity',show.legend = TRUE, alpha = 0.7) +
  theme_hc() +
  ggtitle('Total Vaccinations in Europe by Country provided by Our World in Data') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 6),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") 

africa_vacc<-ggplot(data = our_world_in_data_vaccinations_Africa, aes(x=date_of_observation, y = total_vaccinations, col = location)) + 
  geom_line(stat = 'identity',show.legend = TRUE, alpha = 0.7) +
  theme_hc() +
  ggtitle('Total Vaccinations in Africa by Country provided by Our World in Data') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 6),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") 

south_america_vacc<-ggplot(data = our_world_in_data_vaccinations_South_America, aes(x=date_of_observation, y = total_vaccinations, col = location)) + 
  geom_line(stat = 'identity',show.legend = TRUE, alpha = 0.7) +
  theme_hc() +
  ggtitle('Total Vaccinations in South America by Country provided by Our World in Data') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 6),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") 

north_america_vacc<-ggplot(data = our_world_in_data_vaccinations_North_America, aes(x=date_of_observation, y = total_vaccinations, col = location)) + 
  geom_line(stat = 'identity',show.legend = TRUE, alpha = 0.7) +
  theme_hc() +
  ggtitle('Total Vaccinations in North America by Country provided by Our World in Data') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 6),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") 

oceania_vacc<-ggplot(data = our_world_in_data_vaccinations_Oceania, aes(x=date_of_observation, y = total_vaccinations, col = location)) + 
  geom_line(stat = 'identity',show.legend = TRUE, alpha = 0.7) +
  theme_hc() +
  ggtitle('Total Vaccinations in Oceania by Country provided by Our World in Data') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 6),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") 

asia_vacc<-ggplot(data = our_world_in_data_vaccinations_Asia, aes(x=date_of_observation, y = total_vaccinations, col = location)) + 
  geom_line(stat = 'identity',show.legend = TRUE, alpha = 0.7) +
  theme_hc() +
  ggtitle('Total Vaccinations in Asia by Country provided by Our World in Data') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 6),
    axis.text.x = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = "RdGy") 


#Vaccination Distribution

vaccination_distribution_usa <- dbGetQuery(conn, "SELECT * FROM covid_19_vaccinations_in_the_usa")

vaccination_distribution_usa <- vaccination_distribution_usa %>%
  filter(jurisdiction_state_territory_or_federal_entity !='United States')


total_doses_distributed <- ggplot(vaccination_distribution_usa, aes(x=total_doses_distributed, y=jurisdiction_state_territory_or_federal_entity, fill='darkred'))+
  geom_bar(stat='identity')+
  theme_hc() +
  ggtitle('Total Covid-19 Vaccine Doses Distributed in USA by State') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    panel.grid = element_blank(),
    axis.text = element_text(color = "blue", size = 10),
    axis.text.x = element_text(face = "italic")
  )



distribution_janssen_pfizer <- ggplot(vaccination_distribution_usa, aes(x=total_number_of_janssen_doses_distributed, y = total_number_of_original_pfizer_doses_distributed, col=jurisdiction_state_territory_or_federal_entity))+
   geom_jitter(stat = 'identity') + 
   theme_hc() +
   ggtitle("Active Hospitalized Patients by Delphi Covid19 Dataset") +
   #scale_color_brewer(palette = "Paired")+
   theme(
     plot.title = element_text(hjust = 0.5, size = 13),
     panel.grid = element_blank(),
     axis.text = element_text(color = "blue", size = 10),
     axis.text.x = element_text(face = "italic"),
     axis.title = element_text(size=15)
   )




# Mobility Data

mobility_data <- dbGetQuery(conn, "SELECT * FROM google_covid19_open_data_cloud_mobility")


#Public Measures against Covid-19 Data

kaizer_health_systems_measures <- dbGetQuery(conn, "SELECT  * FROM kaizer_foundation_covid19_policy_health_systems_measures")
kaizer_economic_measures <- dbGetQuery(conn, "SELECT  * FROM kaizer_foundation_covid19_policy_economic_measures")
kaizer_social_measures <- dbGetQuery(conn, "SELECT  * FROM kaizer_foundation_covid19_social_distancing_closure_measures")
#phsm <- dbGetQuery(conn, "SELECT  * FROM public_measures_phsm")


#Plots for Public Measures against Covid-19 Data

# Join the Kaizer Database
kaizer_measures_against_covid19 <- list(kaizer_health_systems_measures, kaizer_economic_measures, kaizer_social_measures)
kaizer_measures_against_covid19 <- kaizer_measures_against_covid19 %>%
  reduce(full_join, by='country')

# Making a Combined Map
world <- ne_countries(returnclass = "sf")
merged_data_kaizer_health <- merge(world, kaizer_health_systems_measures, by.x = "name", by.y = "country")
merged_data_kaizer_economic <- merge(world, kaizer_economic_measures, by.x = "name", by.y = "country")
merged_data_kaizer_social <- merge(world, kaizer_social_measures, by.x = "name", by.y = "country")



base_map_kaizer_health <- leaflet(data = merged_data_kaizer_health) %>%
  addTiles()
base_map_kaizer_economic <- leaflet(data = merged_data_kaizer_economic) %>%
  addTiles()
base_map_kaizer_social <- leaflet(data = merged_data_kaizer_social) %>%
  addTiles()


# Combine 2 categories in one spot
merged_data_kaizer_health_comments <- merged_data_kaizer_health %>%
  mutate(popup_info=paste(name, "<br/>","Vaccination Eligibility:", vaccination_eligibility, "<br/>", "Facial Covering:", facial_coverings))

merged_data_kaizer_economic_comments <- merged_data_kaizer_economic %>%
  mutate(popup_info=paste(name, "<br/>","Income Support:", income_support, "<br/>", "Debt Contract Reilief:", debt_contract_relief))

merged_data_kaizer_social_comments <- merged_data_kaizer_social %>%
  mutate(popup_info=paste(name, "<br/>","Cancel Public Events:",cancel_public_events, "<br/>", "Stay at Home Requirements:", stay_at_home_requirements,
                          "<br/>", "Workplace Closing:", workplace_closing,"<br/>", "School Closing:", school_closing,"<br/>", "Restrictions on Gatherings:", restrictions_on_gatherings,
                          "<br/>", "Restrictions on Internal Movement:", restrictions_on_internal_movement,"<br/>", "International Travel Controls:", international_travel_controls))

map_kaizer_health_measures <- leaflet(data = merged_data_kaizer_health_comments) %>%
  addTiles() %>%
  addPolygons(fillOpacity = 0.2, color = "lightgreen", weight = 1) %>%
  addCircleMarkers(lng = ~label_x, lat = ~label_y, radius = ~3,opacity = 3, 
                   popup = ~popup_info,
                   color = 'darkred') %>%
  addControl(html = 'Public Health Measures by Kaizer Foundation', position = 'bottomleft')


map_kaizer_economic_measures <- leaflet(data = merged_data_kaizer_economic_comments) %>%
  addTiles() %>%
  addPolygons(fillOpacity = 0.2, color = "lightgreen", weight = 1) %>%
  addCircleMarkers(lng = ~label_x, lat = ~label_y, radius = ~3,opacity = 3, 
                   popup = ~popup_info,
                   color = 'darkred') %>%
  addControl(html = 'Economic Measures by Kaizer Foundation', position = 'bottomleft')

map_kaizer_social_measures <- leaflet(data = merged_data_kaizer_social_comments) %>%
  addTiles() %>%
  addPolygons(fillOpacity = 0.2, color = "lightgreen", weight = 1) %>%
  addCircleMarkers(lng = ~label_x, lat = ~label_y, radius = ~3,opacity = 3, 
                   popup = ~popup_info,
                   color = 'darkred') %>%
  addControl(html = 'Social and Closing Measures by Kaizer Foundation', position = 'bottomleft')



# Patient Clinical Status

# Clinical Characteristics of Covid-19 

papers_for_symptoms_covid19 <- dbGetQuery(conn, 'SELECT * FROM symptoms_and_occurrence_of_the_symptoms_self_reported_patients')

# Clinical Characteristics of Long Covid-19

estimated_number_of_longcovid_symptoms_in_uk_until_march2023 <- dbGetQuery(conn, "SELECT * FROM prevalence_longcovid_symptoms_uk_set2_table_2")
long_covid_symptoms <- dbGetQuery(conn, "SELECT  * FROM long_covid_symptoms")

# Plots for Long Covid Symptoms

long_covid_symptoms_8 <- top_n(long_covid_symptoms,8, relative_frequency)
long_covid_symptoms_8

estimated_number_of_longcovid_symptoms_in_uk_until_march2023_at_least_12 <- estimated_number_of_longcovid_symptoms_in_uk_until_march2023 %>%
  filter(duration == 'At least 12 weeks') %>%
  mutate(estimate = replace(estimate, estimate == 1.243, 1243))

estimated_number_of_longcovid_symptoms_in_uk_until_march2023_at_least_12$estimate <- as.numeric(estimated_number_of_longcovid_symptoms_in_uk_until_march2023_at_least_12$estimate)

long_covid_with_the_highest_freq <- ggplot(long_covid_symptoms_8, mapping = aes(x= symptom, y=relative_frequency, fill = symptom)) +
  geom_bar(stat = 'identity', col = 'black',show.legend = TRUE, alpha = 0.7)+
  theme_hc() +
  ggtitle('Long Covid Symptoms with The Highest Relative Frequency (Percent)')+
  theme(plot.title = element_text(hjust = 0.5, size = 15),panel.grid 
        = element_blank()) +
  geom_text(aes(label = round(relative_frequency, 2)), position = position_stack(vjust = 1.05,), color = "black", size = 5) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic"))+
  scale_fill_brewer(palette="RdGy")

long_covid_estimated_number_at_least_12 <- ggplot(estimated_number_of_longcovid_symptoms_in_uk_until_march2023_at_least_12, mapping = aes(x= estimate, y=symptom)) +
  geom_bar(stat = 'identity', col = 'black',fill='darkred',show.legend = TRUE, alpha=0.6)+
  theme_hc() +
  ggtitle('Estimated Number (in thousands) of at least 12 weeks Long Covid Symptoms in UK until March 2023')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  geom_text(aes(label = round(estimate, 2)), position = position_stack(vjust = 1.16), color = "black", size = 5) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic")) +
  scale_fill_brewer(palette="RdGy")



# Medical Imaging

xrayscovid19 <- dbGetQuery(conn, "SELECT * FROM xrays_covid19_metadata_join")

xrayscovid19 <- xrayscovid19 %>%
  filter(age >0, !is.na(sex))


xrays_findings_by_sex <- ggplot(xrayscovid19, aes(y=finding, fill=sex)) +
                          geom_bar(stat = 'count')+
                          theme_hc() +
                          ggtitle("Xray's Finding by Patient Age")+
                          theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
                                = element_blank()) +
                          theme(axis.text = element_text(color = "blue", size = 12),
                                axis.text.x = element_text(face = "italic")) +
                          scale_fill_manual(values = c('maroon', 'lightgreen'))


xrays_findings_by_survival <- ggplot(xrayscovid19, aes(y=finding, fill=survival)) +
  geom_bar(stat = 'count')+
  theme_hc() +
  ggtitle("Xray's Finding by Survival Status")+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic"))

# Sound Data

coughvid <- dbGetQuery(conn, "SELECT * FROM coughvid_crowdsorucing_metadata_dataset")
coswara <- dbGetQuery(conn, "SELECT * FROM coswara_all_days_covid_data")




coughvid_status <- coughvid %>%
  filter(!is.na(status))


coughvid_status_graph <- ggplot(coughvid_status, aes(x=status, fill=gender)) +
  geom_bar(stat = 'count') +
  theme_hc() +
  ggtitle("Patient Status based on Cough Sound Detection Method by Gender by Coughvid")+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic"))+
  scale_fill_manual(values = c('maroon', 'lightgreen', 'black'))




# Epidemiological Data at Animal Kingdom

animals_cases_list <- read_excel("Cases List.xlsx")
animals_variant_table <- read_excel("Variant Table.xlsx")



#animals_covid_detection_methods <- animals_cases_list %>%
  #count(animals_cases_list$`Method of Initial Diagnosis**`)

animals_tests <- ggplot(animals_cases_list, aes(y = `Location Type - Animal*`, fill = `Method of Initial Diagnosis**`)) +
                  geom_bar(stat = 'count') +
                  theme_hc() +
                  ggtitle('Tests Species for Covid-19 Detection of Cases in Animals by USDA: US. Department of Agriculture') +
                  theme(plot.title = element_text(hjust = 0.5, size = 15), panel.grid = element_blank()) +
                  theme(axis.text = element_text(color = "blue", size = 12),
                        axis.text.x = element_text(face = "italic"))


animals_cases <- ggplot(animals_cases_list, aes(y=`Location Type - Animal*`, fill =State)) +
                  geom_bar(stat='count')+
                  theme_hc() +
                  ggtitle('Covid-19 Cases in Animals by USA State by USDA: US. Department of Agriculture') +
                  theme(plot.title = element_text(hjust = 0.5, size = 15), panel.grid = element_blank()) +
                  theme(axis.text = element_text(color = "blue", size = 12),
                        axis.text.x = element_text(face = "italic"))




animals_cases_list_animals_count <- animals_cases_list %>%
  count(`Location Type - Animal*`)


animals_cases_list_animals_count <- as.data.frame(animals_cases_list_animals_count)

# Data
set.seed(1)
animals_freq <- ggwordcloud(words = animals_cases_list_animals_count$`Location Type - Animal*`, freq = animals_cases_list_animals_count$n)



animals_covid_virus_1 <- ggplot(animals_variant_table, aes(x=`Animal Type`, y=Alpha, fill=Source)) +
  geom_bar(stat='identity') +
  theme_hc() +
  ggtitle('Alpha Mutation in Animals by Source')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic")) +
  scale_fill_brewer(palette="RdGy")
animals_covid_virus_2 <-ggplot(animals_variant_table, aes(x=`Animal Type`, y=Delta, fill=Source)) +
  geom_bar(stat='identity')+
  theme_hc() +
  ggtitle('Delta Mutation in Animals by Source')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic")) +
  scale_fill_brewer(palette="RdGy")
animals_covid_virus_3 <-ggplot(animals_variant_table, aes(x=`Animal Type`, y=Epsilon, fill=Source)) +
  geom_bar(stat='identity')+
  theme_hc() +
  ggtitle('Epsilon Mutation in Animals by Source')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic")) +
  scale_fill_brewer(palette="RdGy")
animals_covid_virus_4 <-ggplot(animals_variant_table, aes(x=`Animal Type`, y=Omicron, fill=Source)) +
  geom_bar(stat='identity')+
  theme_hc() +
  ggtitle('Omicron Mutation in Animals by Source')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic")) +
  scale_fill_brewer(palette="RdGy")
animals_covid_virus_5 <-ggplot(animals_variant_table, aes(x=`Animal Type`, y=Gamma, fill=Source)) +
  geom_bar(stat='identity')+
  theme_hc() +
  ggtitle('Alpha Mutation in Animals by Source')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic")) +
  scale_fill_brewer(palette="RdGy")
animals_covid_virus_6 <-ggplot(animals_variant_table, aes(x=`Animal Type`, y=Iota, fill=Source)) +
  geom_bar(stat='identity')+
  theme_hc() +
  ggtitle('Iota Mutation in Animals by Source')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic")) +
  scale_fill_brewer(palette="RdGy")
animals_covid_virus_7 <-ggplot(animals_variant_table, aes(x=`Animal Type`, y=Mu, fill=Source)) +
  geom_bar(stat='identity')+
  theme_hc() +
  ggtitle('Mu Mutation in Animals by Source')+
  theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
        = element_blank()) +
  theme(axis.text = element_text(color = "blue", size = 12),
        axis.text.x = element_text(face = "italic")) +
  scale_fill_brewer(palette="RdGy")

animals_alpha_delta_epsilon <- (animals_covid_virus_1) / (animals_covid_virus_2) / (animals_covid_virus_3)  
animals_omicron_gamma <- (animals_covid_virus_4) / (animals_covid_virus_5) 
animals_iota_mu <- animals_covid_virus_6 / animals_covid_virus_7


# Meteorological Data

meteo_data <- dbGetQuery(conn, "SELECT * FROM meteostat_weather_stations_full_global_stations")
delve_weather <- dbGetQuery(conn, "SELECT * FROM delve_global_covid_19_dataset")
delve_weather <- delve_weather[ ,c(1,2,3,62,63,64,65,66,67,68)]


# Biological Data

genome_data <- dbGetQuery(conn, "SELECT * FROM gencode_reference_annotation_for_the_human_genome_history")
pharmacy_data <- dbGetQuery(conn, "SELECT * FROM gtopdb_ligands_coronavirus")


# Covid-19 Studies

papers_by_nih <- dbGetQuery(conn, "SELECT * FROM covid19_portfolio_by_nih")

papers_country <- ggplot(papers_by_nih, aes(y=journal_country, fill=source)) +
                    geom_bar(stat = 'count') +
                    theme_hc() +
                    ggtitle("Jounal's Country by Source by NIH")+
                    theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
                          = element_blank()) +
                    theme(axis.text = element_text(color = "blue", size = 8),
                          axis.text.x = element_text(face = "italic"))

papers_journal <- papers_by_nih %>%
  count(journal_name_full) %>%
  arrange(desc(n)) %>%
  filter(!is.na(journal_name_full))

papers_journal_top_40 <- head(papers_journal, 40)

top_40_journals <- ggplot(papers_journal_top_40, aes(y=journal_name_full, x=n)) +
                    geom_bar(stat = 'identity')+
                    theme_hc() +
                    ggtitle("Top 40 Journals during Covid-19") +
                    theme(plot.title = element_text(hjust = 0.5, size=15),panel.grid 
                          = element_blank()) +
                    geom_text(aes(label = n, color = "black"), position = position_stack(vjust = 1.35), size = 4) +
                    theme(axis.text = element_text(color = "blue", size = 8),
                          axis.text.x = element_text(face = "italic"))


journal_top_40_freq <- ggwordcloud(words = papers_journal_top_40$journal_name_full, freq = papers_journal_top_40$n)


papers_condition <- papers_by_nih %>%
  count(condition) %>%
  filter(!is.na(condition), n >50)

set.seed(1)
papers_condition_with_freq_bigger_than_50 <- ggwordcloud(words = papers_condition$condition, freq = papers_condition$n)




# Set the server

ui_dashboard <- dashboardPage(skin='green',
  dashboardHeader(title = "Covid's Ark Database Dashboard"),
  dashboardSidebar(width = 270,
    sidebarMenu(
      menuItem("Epidemiological Data",
               menuSubItem("Covid-19 Cases and Deaths", tabName = "cases_deaths"),
               menuSubItem("Covid-19 Cases and Deaths in Prisons", tabName = "cases_deaths_special"),
               menuSubItem("Epidemiological Forecasting", tabName = "epidemiological_forecasting")),
      menuItem("Vaccination against Covid-19 Data",  
               menuSubItem("Vaccines' Development", tabName = "vaccine_develop"),
               menuSubItem("Vaccination Rollout", tabName = "vaccine_rollout"),
               menuSubItem("Vaccination Distribution", tabName = "vaccine_distribution")),
      menuItem("Mobility Data", tabName = "mobility"),
      menuItem("Public Measures against Covid-19", tabName = "public_measures"),
      menuItem("Patient Clinical Status",
               menuSubItem("Clinical Characteristics of Covid-19", tabName = "covid19_symptoms"),
               menuSubItem("Clinical Characteristics of Long Covid-19", tabName = "longcovid"),
               menuSubItem("Medical Imaging", tabName = "covid_images"),
               menuSubItem("Covid-19 Sound Data Info", tabName = "covid_sound")),
      menuItem("Epidemiological Data in Animal Kingdom", tabName = "animals_data"),
      menuItem("Meteorological Data", tabName = "meteo"),
      menuItem("Biological Data", tabName = 'biological_data'),
      menuItem("Covid-19 Studies", tabName = 'covid_studies')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("cases_deaths",
              tags$h2("Covid-19 Cases and Deaths by Continent", align="center", style = "color: darkred"),
              box(
                width = 13,
                plotlyOutput("timeline_cases_who_interactive", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("timeline_deaths_who_interactive", height = '700px')
              ),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_12 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_12", "Dataset Download")
      ),
      tabItem("cases_deaths_special",
              tags$h2("Covid-19 Cases and Deaths in Special Living Conditions (Prisons)", align ="center", style = "color: darkred"),
              box(
                width = 13,
                plotlyOutput("covid_behind_bars_resident_deaths", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("covid_behind_bars_resident_confirmed", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("prisoner_staff_deaths", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotOutput("prison_cases_map", height = '700px')
              ),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_13 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_13", "Dataset Download"),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                              #downloadData_14 {
                                text-align: center;
                                display: block;
                                margin: 0 auto;
                              }
                            ")
              ),
              downloadButton("downloadData_14", "Dataset Download")
    ),
    tabItem("epidemiological_forecasting",
            tags$h2("Covid-19 Epidemiological Forecasting - Projections by Delphi Dataset", align="center", style = "color: darkred"),
            box(
              width = 13,
              plotlyOutput("timeline_delphi_projections", height = '700px'),
              tags$div(style = "height: 20px;"),
              tags$h2("Covid-19 Epidemiological Forecasting - Clinical Data by Delphi Dataset", align="center", style = "color: darkred"),
              dataTableOutput("clinicaldata")
            ),
            tags$div(style = "height: 10px;"),
            tags$style(
              HTML("
                      #downloadData_15 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
            ),
            downloadButton("downloadData_15", "Dataset Download"),
            tags$div(style = "height: 10px;"),
            tags$style(
              HTML("
                      #downloadData_16 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
            ),
            downloadButton("downloadData_16", "Dataset Download")
  ),
      tabItem("vaccine_develop",
              tags$h2("Development of Covid-19 Vaccines Data", align="center", style = "color: darkred"),
              box(
                width = 13,
                plotOutput("description_of_vaccines_type", height = '700px')
              ),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_4 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_4", "Dataset Download")
      ),
      tabItem("vaccine_rollout",
              tags$h2("Vaccination Rollout Data", align="center", style = "color: darkred"),
              box(
                width = 13,
                plotlyOutput("europe_vacc", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("asia_vacc", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("oceania_vacc", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("africa_vacc", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("north_america_vacc", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("south_america_vacc", height = '700px'),
                tags$div(style = "height: 20px;"),
              ),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_5 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_5", "Europe Vaccination Data by Our World in Data - Download"),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_6 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_6", "Asia Vaccination Data by Our World in Data - Download"),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_7 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_7", "Oceania Vaccination Data by Our World in Data - Download"),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_8 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_8", "Africa Vaccination Data by Our World in Data - Download"),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_9 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_9", "North America Vaccination Data by Our World in Data - Download"),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_10 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_10", "South America Vaccination Data by Our World in Data - Download")
      ),
      tabItem("vaccine_distribution",
              tags$h2("Distribution of Covid-19 Vaccines", align="center", style = "color: darkred"),
              box(
                width = 13,
                plotlyOutput("total_doses_distributed", height = '700px'),
                tags$div(style = "height: 20px;"),
                plotlyOutput("distribution_janssen_pfizer", height = '700px')
              ),
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData_11 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
              ),
              downloadButton("downloadData_11", "Dataset Download")
      ),
  tabItem("mobility",
          tags$h2("Mobility Data", align="center", style = "color: darkred"),
          dataTableOutput("mobilitytable"),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_17 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_17", "Dataset Download")
  ),
      tabItem("public_measures",
              tags$h2('Public Health Measures by Kaizer Foundation', align="center", style = "color: darkred"),
              map_kaizer_health_measures,
              tags$h2('Economic Measures by Kaizer Foundation', align="center", style = "color: darkred"),
              map_kaizer_economic_measures,
              tags$h2('Social and Closing Measures by Kaizer Foundation', align="center", style = "color: darkred"),
              map_kaizer_social_measures,
              tags$div(style = "height: 10px;"),
              tags$style(
                HTML("
                      #downloadData {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
                    ),
              downloadButton("downloadData", "Dataset Download")
              ),
  tabItem("covid19_symptoms",
          tags$h2('Scientific Articles about Covid-19 Symptoms', align="center", style = "color: darkred"),
          dataTableOutput("symptomscovid19table"),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_25 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_25", "Dataset Download")
  ),
  tabItem("longcovid",
          tags$h2("Long Covid Symptoms Frequency", align="center", style = "color: darkred"),
          dataTableOutput("longcovidtable"),
          box(
            width = 13,
            plotOutput("long_covid_with_the_highest_freq", height = '600px'),
            tags$div(style = "height: 20px;"),
            plotOutput('long_covid_estimated_number_at_least_12',height = '600px')
          ),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_2 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_2", "Dataset Download (Graph_1)"),
          tags$style(
            HTML("
                      #downloadData_3 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_3", "Dataset Download (Graph_2)")
  ),
  tabItem("covid_images",
          tags$h2('Xrays Images Metadata Table by Cohen et al', align="center", style = "color: darkred"),
          dataTableOutput("xrayscovid19"),
          tags$div(style = "height: 10px;"),
          tags$h2("Xray's Finding by Patient Age ", align="center", style = "color: darkred"),
          plotlyOutput("xrays_findings_by_sex", height = '700px'),
          tags$h2("Xray's Finding by Survival Status ", align="center", style = "color: darkred"),
          plotlyOutput("xrays_findings_by_survival", height = '700px'),
          tags$style(
            HTML("
                      #downloadData_26 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_26", "Dataset Download")
  ),
  tabItem("covid_sound",
          tags$h2('Covid-19 Sounds Metadata Table by Coughvid Dataset', align="center", style = "color: darkred"),
          dataTableOutput("coughvidtable"),
          tags$h2('Covid-19 Sounds Metadata Table by Coswara Dataset', align="center", style = "color: darkred"),
          dataTableOutput("coswaratable"),
          tags$h2("Patient Status based on Cough Sound Detection Method by Gender by Coughvid", align="center", style = "color: darkred"),
          plotlyOutput("coughvid_status_graph", height = '700px'),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_27{
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_27", "Dataset Download (Coughvid)"),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_28{
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_28", "Dataset Download (Coswara)")
  ),
  tabItem("animals_data",
          tags$h2('Animal Covid-19 Cases List by USDA: US. Department of Agriculture', align="center", style = "color: darkred"),
          dataTableOutput("animalstable"),
          tags$h2('Tests Species for Covid-19 Detection of Cases in Animals', align="center", style = "color: darkred"),
          plotlyOutput("animals_tests", height = '700px'),
          tags$h2('Covid-19 Cases in Animals by USA State', align="center", style = "color: darkred"),
          plotlyOutput("animals_cases", height = '700px'),
          tags$h2('Frequency of Occurrence of Cases in Animal Kingdom', align="center", style = "color: darkred"),
          plotOutput("animals_freq", height = '700px'),
          tags$h2('Covid-19 Mutations in Animal Kingdom', align="center", style = "color: darkred"),
          plotOutput("animals_alpha_delta_epsilon", height = '700px'),
          tags$div(style = "height: 10px;"),
          plotOutput("animals_omicron_gamma", height = '700px'),
          tags$div(style = "height: 10px;"),
          plotOutput("animals_iota_mu", height = '700px'),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_18 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_18", "Dataset_1 Download"),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_19 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_19", "Dataset_1 Download")
  ),
  tabItem("meteo",
          tags$h2("Meteorological Data during Covid-19 period by Delve Dataset", align="center", style = "color: darkred"),
          dataTableOutput("meteodelvetable"),
          tags$div(style = "height: 10px;"),
          tags$h2("Meteorological Data during Covid-19 period based on Weather Stations by Meteostat", align="center", style = "color: darkred"),
          dataTableOutput("meteostattable"),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_20 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_20", "Dataset Meteo Data by Delve Dataset"),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_21 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_21", "Dataset Meteo Data based on Weather Stations by Meteostat"),
  ),
  tabItem("biological_data",
          tags$h2("Gencode Annotations for Human by Encyclopaedia of DNA Elements", align="center", style = "color: darkred"),
          dataTableOutput("gencodetable"),
          tags$div(style = "height: 10px;"),
          tags$h2("Pharmacology Data by GtoPdb Coronavirus Information Portal", align="center", style = "color: darkred"),
          dataTableOutput("pharmacytable"),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_22 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_22", "Dataset for Gencode"),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_23 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_23", "Dataset for Pharmacology Data"),
  ),
  tabItem("covid_studies",
          tags$h2('Covid-19 Studies Info Table based on NIH Covid-19 Portfolio', align="center", style = "color: darkred"),
          dataTableOutput("papersnihtable"),
          tags$h2("Journal's  Country Barplot by Source ", align="center", style = "color: darkred"),
          plotlyOutput("papers_country", height = '700px'),
          tags$h2('Top 40 Journals with top published papers during Covid-19 period', align="center", style = "color: darkred"),
          plotlyOutput("top_40_journals", height = '700px'),
          tags$h2('WordCloud of top 40 Journals with top published papers during Covid-19 period ', align="center", style = "color: darkred"),
          plotOutput("journal_top_40_freq", height = '700px'),
          tags$div(style = "height: 10px;"),
          tags$style(
            HTML("
                      #downloadData_24 {
                        text-align: center;
                        display: block;
                        margin: 0 auto;
                      }
                    ")
          ),
          downloadButton("downloadData_24", "Dataset Download"),
  )
      
      )
)

)

server <- function(input, output) {
  data <- reactive({
    conn <- dbConnect(
      RPostgres::Postgres(),
      dbname = 'covids_ark',
      host = 'localhost',
      port = '5432',
      user = 'postgres',
      password = 'lamiara75'
    )
    dbDisconnect(conn)
  })
  output$timeline_cases_who_interactive <-renderPlotly({
    timeline_cases_who_interactive
  })
  output$timeline_deaths_who_interactive <-renderPlotly({
    timeline_deaths_who_interactive
  })
  output$covid_behind_bars_resident_deaths <-renderPlotly({
    covid_behind_bars_resident_deaths
  })
  output$covid_behind_bars_resident_confirmed <-renderPlotly({
    covid_behind_bars_resident_confirmed
  })
  output$prisoner_staff_deaths <-renderPlotly({
    prisoner_staff_deaths
  })
  output$prison_cases_map <-renderPlot({
    prison_cases_map
  })
  output$timeline_delphi_projections <-renderPlotly({
    timeline_delphi_projections
  })
  output$clinicaldata <- renderDataTable({
      datatable(delphi_clinical_data, 
                options = list(
                  pageLength = 25,  # Number of rows per page
                  scrollY = '700px',  # Vertical scroll height
                  scrollX = TRUE,  # Horizontal scroll
                  paging = TRUE,  # Enable paging
                  searching = TRUE,  # Disable search bar
                  lengthChange = FALSE  # Hide the entries per page dropdown
                )
      )
    })
  output$total_doses_distributed <-renderPlotly({
    total_doses_distributed
  })
  output$distribution_janssen_pfizer <-renderPlotly({
    distribution_janssen_pfizer
  })
  output$mobilitytable <- renderDataTable({
    datatable(mobility_data, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '700px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$long_covid_with_the_highest_freq <-renderPlot({
    long_covid_with_the_highest_freq
  })
  output$long_covid_estimated_number_at_least_12 <-renderPlot({
    long_covid_estimated_number_at_least_12
  })
  output$longcovidtable <-renderDataTable(long_covid_symptoms)
  output$description_of_vaccines_type <-renderPlot({
    description_of_vaccines_type
  })
  output$europe_vacc <-renderPlotly({
    europe_vacc
  })
  output$asia_vacc <-renderPlotly({
    asia_vacc
  })
  output$oceania_vacc <-renderPlotly({
    oceania_vacc
  })
  output$africa_vacc <-renderPlotly({
    africa_vacc
  })
  output$north_america_vacc <-renderPlotly({
    north_america_vacc
  })
  output$south_america_vacc <-renderPlotly({
    south_america_vacc
  })
  output$animals_tests <-renderPlotly({
    animals_tests
  })
  output$animals_cases <-renderPlotly({
    animals_cases
  })
  output$animals_freq <-renderPlot({
    animals_freq
  })
  output$animals_alpha_delta_epsilon <-renderPlot({
    animals_alpha_delta_epsilon
  })
  output$animals_omicron_gamma <-renderPlot({
    animals_omicron_gamma
  })
  output$animals_iota_mu <-renderPlot({
    animals_iota_mu
  })
  output$animalstable <- renderDataTable({
    datatable(animals_cases_list, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '700px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$meteodelvetable <- renderDataTable({
    datatable(delve_weather, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '700px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$meteostattable <- renderDataTable({
    datatable(meteo_data, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '700px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$gencodetable <- renderDataTable({
    datatable(genome_data, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '700px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$pharmacytable <- renderDataTable({
    datatable(pharmacy_data, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '700px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$papersnihtable <- renderDataTable({
    datatable(papers_by_nih, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '500px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$papers_country <-renderPlotly({
    papers_country
  })
  output$top_40_journals <-renderPlotly({
    top_40_journals
  })
  output$journal_top_40_freq <-renderPlot({
    journal_top_40_freq
  })
  output$symptomscovid19table <- renderDataTable({
    datatable(papers_for_symptoms_covid19, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '500px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$coughvidtable <- renderDataTable({
    datatable(coughvid, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '500px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$coswaratable <- renderDataTable({
    datatable(coswara, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '500px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$xrayscovid19 <- renderDataTable({
    datatable(xrayscovid19, 
              options = list(
                pageLength = 25,  # Number of rows per page
                scrollY = '500px',  # Vertical scroll height
                scrollX = TRUE,  # Horizontal scroll
                paging = TRUE,  # Enable paging
                searching = TRUE,  # Disable search bar
                lengthChange = FALSE  # Hide the entries per page dropdown
              )
    )
  })
  output$xrays_findings_by_sex <-renderPlotly({
    xrays_findings_by_sex
  })
  output$xrays_findings_by_survival <-renderPlotly({
    xrays_findings_by_survival
  })
  output$papers_country <-renderPlotly({
    papers_country
  })
  output$coughvid_status_graph <-renderPlotly({
    coughvid_status_graph
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("kaizer_health_systems_measures-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(kaizer_measures_against_covid19, file)
    }
  )
  output$downloadData_2 <- downloadHandler(
    filename = function() {
      paste("long_covid_symptoms_by_Patient_Led_Research_Collaborative-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(long_covid_symptoms, file)
    }
  )
  output$downloadData_3 <- downloadHandler(
    filename = function() {
      paste("estimated_number_of_longcovid_symptoms_in_uk_until_march2023_by_UK_Office_for_National_Statistics-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(estimated_number_of_longcovid_symptoms_in_uk_until_march2023, file)
    }
  )
  output$downloadData_4 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Vaccine_Development_by_WHO_and_Milken_Institute-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vaccine_development_join, file)
    }
  )
  output$downloadData_5 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Europe_Vaccination_Rollout_by_Our_World_in_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vaccine_development_join, file)
    }
  )
  output$downloadData_6 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Asia_Vaccination_Rollout_by_Our_World_in_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vaccine_development_join, file)
    }
  )
  output$downloadData_7 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Oceania_Vaccination_Rollout_by_Our_World_in_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vaccine_development_join, file)
    }
  )
  output$downloadData_8 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Africa_Vaccination_Rollout_by_Our_World_in_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vaccine_development_join, file)
    }
  )
  output$downloadData_9 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_North_America_Vaccination_Rollout_by_Our_World_in_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vaccine_development_join, file)
    }
  )
  output$downloadData_10 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_South_America_Vaccination_Rollout_by_Our_World_in_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vaccine_development_join, file)
    }
  )
  output$downloadData_11 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Vaccine_Distribution_in_USA_by_US_CDC-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vaccination_distribution_usa, file)
    }
  )
  output$downloadData_12 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Cases_and_Deaths_by_WHO-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(cases_deaths_who, file)
    }
  )
  output$downloadData_13 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Covid_Behind_Bars_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(covid_behind_bars_latest_facility_counts_values, file)
    }
  )
  output$downloadData_14 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_A_State_by_State_Look_at_Covid_in_prisons-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(prison_cases, file)
    }
  )
  output$downloadData_15 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Covid-19_Epidemiological_Forecasting_Projections_by_Delphi_Dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(delphi_projections, file)
    }
  )
  output$downloadData_16 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Covid-19_Epidemiological_Forecasting_Clinical_Data_by_Delphi_Dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(delphi_clinical_data, file)
    }
  )
  output$downloadData_17 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Mobility_Data_by_Google_Open_Data_Cloud-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(mobility_data, file)
    }
  )
  output$downloadData_18 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Animals_Covid19_Cases_List_by_USDA_US_Department_of_Agriculture-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(animals_cases_list, file)
    }
  )
  output$downloadData_19 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Animals_Covid19_Variant_Table_by_USDA_US_Department_of_Agriculture-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(animals_variant_table, file)
    }
  )
  output$downloadData_20 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Meteorological_Data_by_Delve_Dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(delve_weather, file)
    }
  )
  output$downloadData_21 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Meteorological_Data_based_on_Weather_Staions_by_Meteostat-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(meteo_data, file)
    }
  )
  output$downloadData_22 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Gencode_Annotations_for_Human_by_Encyclopaedia_of_DNA_Elements-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(genome_data, file)
    }
  )
  output$downloadData_23 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Pharmacology_Data_by_GtoPdb_Coronavirus_Information_Portal-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(pharmacy_data, file)
    }
  )
  output$downloadData_24 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Covid-19_Studies_info_by_NIH_Covid19_Portfolio-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(papers_by_nih, file)
    }
  )
  output$downloadData_25 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Articles_about_Covid19_Symptoms-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(papers_for_symptoms_covid19, file)
    }
  )
  output$downloadData_26 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_XraysCovid19_Metadata_by_Cohen's_et_al_Dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(xrayscovid19, file)
    }
  )
  output$downloadData_27 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Covid19_Sounds_Metadata_by_Coughvid_Dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(coughvid, file)
    }
  )
  output$downloadData_28 <- downloadHandler(
    filename = function() {
      paste("Covids_Ark_Covid19_Sounds_Metadata_by_Coswara_Dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(coswara, file)
    }
  )
}



shinyApp(ui_dashboard, server)
