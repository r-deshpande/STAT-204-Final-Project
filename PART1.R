# Import libraries
library(leaflet)
library(sf)        
library(dplyr)     
library(ggplot2)   
library(tigris)

# Load data frame
df<- read.csv("Crash_Locations_Last_Five_Years.csv")

# different data types for reference
names(df)[sapply(df, is.numeric)]
names(df)[sapply(df, is.character)]
names(df)[sapply(df, is.integer)]
names(df)[sapply(df, is.logical)]


#check number of rows and columns
dim(df)

# Find the number of missing values for each variable and their percentage
missing_percent <- data.frame(
  number_na = colSums(is.na(df)),
  na_percent = round(colSums(is.na(df)) / nrow(df) * 100, 2), 
  above_thresh = colSums(is.na(df)) / nrow(df) * 100 > 0.4
)
 missing_percent

# Change the missing values of Intoxicated to 0 as a False in the dataset
df <- df %>% mutate(VEHICLEDRIVERINTOXICATED = ifelse(is.na(VEHICLEDRIVERINTOXICATED), 0, VEHICLEDRIVERINTOXICATED))


### PART 1 WORK

# Sources:
# Source - https://stackoverflow.com/a/53131001
# https://r-spatial.github.io/sf/reference/st_join.html
# https://stackoverflow.com/questions/9981929/how-to-display-all-x-labels-in-r-barplot
# Posted by kwalkertcu
# Retrieved 2026-11-08, License - CC BY-SA 4.0
# https://stackoverflow.com/questions/55227713/creating-a-choropleth-map-with-point-data-using-voronoi-created-surface-polygons
# https://stackoverflow.com/questions/62884687/adding-circle-markers-on-leaflet-r-on-to-a-map-already-plotted
# https://rstudio.github.io/leaflet/articles/markers.html


# return the spatial data as a simple features object
options(tigris_class = "sf", tigris_use_cache = TRUE)
set.seed(1234)

# retreive the geographic boundaries of san jose
sj <- places("CA", cb = TRUE) %>%
  filter(NAME == "San Jose") %>%
  st_transform(26910) #projected coordinate system

# overlay a 10 x 10 grid for the data to be plotted within (Adjust as needed)
g <- sj %>%
  st_make_grid(n = c(10,10)) %>%
  st_intersection(sj) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())

# Convert crash coordinates to spatial points
df_spatial <- st_as_sf(
  df,
  coords = c("LONGITUDE", "LATITUDE"),
  crs = 4326
) %>%
  st_transform(26910)

# Filter exactly to the accidents that have happened within SJ
df_spatial <- st_filter(df_spatial, sj)

# Plot san jose and the overlayed grid
plot(g$geometry)
# Plot the points onto the original boundaries G
plot(st_geometry(df_spatial), add = TRUE, col = "red", pch = 16, cex = 0.2)

# Count NUMBER OF PEOPLE DYING OR SEVERELY INJURED
deaths_grid <- g %>%
  st_join(df_spatial) %>%
  group_by(id) %>%
  summarize(num_obs = sum(KSIINJURIES >= 1, na.rm = TRUE))

# count number of people crashing in each
crashes_grid <- g %>%
  st_join(df_spatial) %>%
  group_by(id) %>%
  summarize(num_obs = n())


# Plot crashes per square
plot(crashes_grid["num_obs"])
#plot the number of deaths per square
plot(deaths_grid["num_obs"])

# Create a filtered subset, where we are looking at all squares of areas most dense with crashes 
danger_square <- filter(crashes_grid, num_obs > 1500) # adjust the number of observations as needed

# Create a filtered subset, where we are looking at all squares of areas most dense with killed or severly injured instances
death_square <- filter(deaths_grid, num_obs > 50)

# Plot these subsets
plot(danger_square)
plot(death_square)


# View the different subsets as data sets
h <- st_filter(df_spatial, danger_square)
v <- st_filter(df_spatial, death_square)
view(h)
view(v)


# Check out the proportion of collision types
S <- prop.table(table(h$COLLISIONTYPE))
S
barplot(S,
  ylim = c(0, 0.4),
  main = "Collision Types in High-Crash Areas",
  ylab = "Proportion of Crashes",
  las = 2,
  cex.names = 0.6
)

# Compare the proportion accidents that happen at a 1 way stop in comparison to a 
A <- prop.table(table(h$INTTRAFFICCONTROLTYPE))
S
barplot(A,
  ylim = c(0, 0.7),
  main = "Collision Types in High-Crash Areas",
  ylab = "Proportion of Crashes",
  las = 2,
  cex.names = 0.6
)


#Compare these two ideas
SA <- as.data.frame(prop.table(
  table(h$INTTRAFFICCONTROLTYPE, h$COLLISIONTYPE),
  margin = 1
))
view(SA)


# Compare the proportions of accident types occurring at different traffic controls
ggplot(h, aes( x= INTTRAFFICCONTROLTYPE, fill = COLLISIONTYPE))+
  geom_bar(position="fill") + labs(
    title = "Collision Types by Traffic Control",
    x = "Traffic Control Type",
    y = "Proportion of Crashes",
    fill = "Collision Type"
  ) + theme_minimal() + theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Huge amount of sideswipes, which could make sense; but we need to see where these are happening. Empirical analysis is best here



## Empirically address where sideswipes are happening, to understand for reporting purposes

# filter to only do sideswipes
h_sideswipe <- h %>%
  filter(COLLISIONTYPE == "Sideswipe")

#Create a continuous color gradient for the cells
colors <- colorNumeric(
  palette = c("green", "yellow", "red", "steelblue"),
  domain = danger_square$num_obs,
  reverse = TRUE
)

# create a color pallette for the different traffic control types
pal <- colorFactor(
  "Set1",
  domain = h$COLLISIONTYPE
)

# Create the interractive map

#
leaflet(st_transform(danger_square, 4326)) %>% # Initialize the map
  addTiles() %>% # add the background map
  addPolygons( # draw the grid cells 
    fillColor = ~colors(num_obs), # color the cells based on the obs count
    fillOpacity = 0.3, # opacity of the cells
    color = "black", # border color
    weight = 1, #set the border thickness
    popup = ~paste( # create lil popups for when you touch the map
      "<strong>Number of Crashes:</strong>", num_obs,
      "<br><strong>Grid ID:</strong>", id
    )
  ) %>%
  addCircleMarkers(
    data = st_transform(h_sideswipe, 4326), # What data specifically i want my circles to represent
    radius = 2, # set the size of the circles (observational points)
    color = ~pal(COLLISIONTYPE), # make the color pallette of the points based on the traffic control type they are associated with
    stroke = FALSE, # no outline of the circle
    fillOpacity = 0.5 # opacity of the points
  ) %>%
  addLegend(
    position = "bottomright", # position the ledgend in teh bottom right
    pal = pal, # make the pallette based on the pallette
    values = h_sideswipe$COLLISIONTYPE, # make the values for the pallette based on the type of collision
    title = "Traffic Control Type",
    opacity = 1 # opacity
  )




