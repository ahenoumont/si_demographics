#Analysis of Education in Staten Island by citizens 25 and over
#Get and manipulate data
#install.packages("lwgeom")
#install.packages("RColorBrewer")
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(RColorBrewer)
api_key <- 'e598c3c5061e24f346054a29807b641a355f013b'
census_api_key(api_key, install = TRUE, overwrite = TRUE)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
getOption("tigris_class")
#Staten island education by tract from the 2016 5yr American Community Survey

#Load list of variables
v16 = load_variables(year = 2016, dataset = "acs5", cache = T)

#Look for variables with this
#View(v16)
#Using Educational atainment

eduVar <- v16[str_detect(v16$name, "B15003"),]

#View(eduVar) 

#Using key words and regex to recode variables
##Table B15003: Educational Attainment for individuals 25 and over
si_Education <- get_acs("tract", table = "B15003", cache_table = TRUE,
               geometry = TRUE, state = "NY", county = "richmond",
               year = 2016, output = "tidy", key = api_key)
  

si_Education

si_Education <- si_Education %>%
  mutate(
    id = str_extract(variable, "[0-9]{3}$") %>% as.integer
  ) %>%
  # variable 1 is the "total", which is just the sum of the others
  filter(id > 1) %>%
  mutate(education =case_when(
    id %>% between(2, 16) ~ "No HS diploma",
    id %>% between(17, 21) ~ "HS, no Bachelors",
    id == 22 ~ "Bachelors",
    id > 22 ~ "Post-Bachelors"
  )) %>% 
  group_by(GEOID, education) %>% 
  summarise(estimate = sum(estimate))

si_Education

# Generate dots, create a group column, and group by group column
si_split <- si_Education %>%
  filter(estimate > 0) %>% 
  split(.$education)

generate_samples <- function(data) 
  suppressMessages(st_sample(data, size = round(data$estimate / 100)))

si_dots <- map(si_split, generate_samples)
si_dots <- imap(si_dots, 
               ~st_sf(data_frame(education = rep(.y, length(.x))),
                      geometry = .x))
si_dots <- do.call(rbind, si_dots)

si_dots <- si_dots %>% group_by(education) %>% summarise()
si_dots <- si_dots %>%
  mutate(education = factor(
    education,
    levels = c("No HS diploma", "HS, no Bachelors",
               "Bachelors", "Post-Bachelors")))
# view how many dots are in each layer
si_dots %>% mutate(n_dots = map_int(geometry, nrow))

#Plot the dots
theme_set(theme_minimal() +
            theme(panel.grid.major = element_line(size = 0),
                  plot.background = element_rect(fill = "#fdfdfd",
                                                 colour = NA),
                  axis.title = element_blank(),
                  text = element_text(family = "serif"),
                  axis.text = element_blank(),
                  legend.position = "bottom"))


ggplot() +
  geom_sf(data = si_dots, aes(color = education, fill = education), size = 0.1)+
  scale_color_brewer(type = "div", palette = 4) + 
  scale_fill_brewer(type = "div", palette = 4)


#Get Staten Island Features
# Filter the DC roads object for major roads only
si_roads <- roads("NY", "richmond") %>%
  filter(RTTYP %in% c("I", "S", "U"))

# Get an area water dataset for DC
si_water <- area_water("NY", "richmond")

# Get the boundary of DC
si_boundary <- county_subdivisions("NY", "richmond", cb = TRUE)

#Plot the graph
theme_set(theme_minimal() +
            theme(panel.grid.major = element_line(size = 0),
                  plot.background = element_rect(fill = "#fdfdfd",
                                                 colour = NA),
                  axis.title = element_blank(),
                  text = element_text(family = "serif"),
                  axis.text = element_blank(),
                  legend.position = "bottom"))




ggplot() +
  geom_sf(data = si_boundary, color = NA, fill = "white")+
  geom_sf(data = si_dots, aes(color = education, fill = education), size = 0.3) +
  geom_sf(data = si_water, color = "lightblue", fill = "lightblue") +
  geom_sf(data = si_roads, color = "grey") +
  coord_sf(crs = 26918, datum = NA) +
  scale_color_brewer(palette = rev("Set1"), guide = FALSE) +
  scale_fill_brewer(palette = rev("Set1"))+
  labs(title = "Education Attainment of Staten Island, NY",
       subtitle = "2012-2016 American Community Survey",
       fill = "",
       caption = "1 dot = approximately 100 people.\nData acquired with the R tidycensus and tigris packages.")


