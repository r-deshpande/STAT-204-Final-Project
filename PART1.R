
# LOADING OF LIBRARIES AND DATA

# Import libraries
library(leaflet)
library(sf)        
library(dplyr)     
library(ggplot2)   
library(tigris)
library(tidyr)

# Load data
df<- read.csv("Crash_Locations_Last_Five_Years.csv")







# EDA PART 1
dim(df)

# Explore what variables correspond to which data type, for reference
names(df)[sapply(df, is.numeric)]
names(df)[sapply(df, is.character)]
names(df)[sapply(df, is.integer)]
names(df)[sapply(df, is.logical)]

# Check the number of rows and columns
dim(df)

# Find the number of missing values for each variable and their percentage (use for general reference)
missing_percent <- data.frame(
  variable = names(df), 
  number_na = colSums(is.na(df)), 
  na_percent = round(colSums(is.na(df)) / nrow(df) * 100, 2))
 missing_percent

# Retrieve the names of the variables that posess missing values
missing_vars <- names(df)[colSums(is.na(df)) > 0]
missing_vars

# For each variable missing data, compare KSIINJURED stats for "complete cases" vs "incomplete cases"
# Noting the mean, number of missing values, the percent of crashes that resulted in KSIINJURED >= 1, and the total crashes
# Comment out the results, draw a conclusion at the end of this EDA

# 1 (DISTANCE)

df %>%
  group_by(is_missing = is.na(DISTANCE))%>%
  summarise(Mean = mean(KSIINJURIES, na.rm = TRUE),
    KSI_pct = mean(KSIINJURIES >= 1, na.rm = TRUE) * 100,
    totalcrashes = n())

# Result:
# Mean is reasonably lower, though the sample size of observations missing DISTANCE is significantly lower which may be attributed to this difference
# The amount of observations in which DISTANCE is missing makes up around 6% of the data. Will leave, though it could be argued this decision might change the results 

# 2 (INTERSECTIONDIMID)
df %>%
  group_by(is_missing = is.na(INTERSECTIONDIMID))%>%
  summarise(Mean = mean(KSIINJURIES, na.rm = TRUE), 
    KSI_pct = mean(KSIINJURIES >= 1, na.rm = TRUE) * 100,
    totalcrashes = n())

# Result : 
# Difference is very small, with a similar sampling issue as before

# 3 (DATEDIMID)

df %>%
  group_by(is_missing = is.na(DATEDIMID))%>%
  summarise(Mean = mean(KSIINJURIES, na.rm = TRUE), 
    KSI_pct = mean(KSIINJURIES >= 1, na.rm = TRUE) * 100,
    totalcrashes = n())

# Result: 
# Exact same results as #2 

# 4 (NOTES)

df %>%
  group_by(is_missing = is.na(NOTES))%>%
  summarise(Mean = mean(KSIINJURIES, na.rm = TRUE),
    KSI_pct = mean(KSIINJURIES >= 1, na.rm = TRUE) * 100, 
    totalcrashes = n())

# Result: 
# It would seem that all of the data is missing notes, so we can ignore

# 5 (VEHICLECOUNT)

df %>%
  group_by(is_missing = is.na(VEHICLECOUNT))%>%
  summarise(Mean = mean(KSIINJURIES, na.rm = TRUE),
    KSI_pct = mean(KSIINJURIES >= 1, na.rm = TRUE) * 100,
    totalcrashes = n())

# Result: 
# Super small portion of the data is missing these, and while they do not contain any observations in which 
# KSIINJURED >= 1, It is a doubtful conclusion that this would drastically change the direction of the data and its results







### CREATING THE MAP OF SAN JOSE

# Sources:
# https://stackoverflow.com/a/53131001
# https://r-spatial.github.io/sf/reference/st_join.html
# https://stackoverflow.com/questions/9981929/how-to-display-all-x-labels-in-r-barplot
# https://stackoverflow.com/questions/55227713/creating-a-choropleth-map-with-point-data-using-voronoi-created-surface-polygons
# https://stackoverflow.com/questions/62884687/adding-circle-markers-on-leaflet-r-on-to-a-map-already-plotted
# https://rstudio.github.io/leaflet/articles/markers.html


# Set options for how the tigris package will handle the data

options(tigris_class = "sf", # (1) return the geographic data as a simple features object
  tigris_use_cache = TRUE) # (2) cache downloaded geographic files locally

# (HEAVY AMOUNT OF COMMENTS INCOMING, done for our own sake of understanding of unfamiliar code)

# Create San Jose so to speak
sj <- places("CA", cb = TRUE, year = 2024) %>% # retrieve simplified boundaries of SJ from U.S. Census Bureau's cartographic boundary files (as of 2024)
  filter(NAME == "San Jose") %>% # filter the shape to only represent SJ
  st_transform(26910) # Convert the projected coordinate system into meters, does not affect the following results (included in sources)

# Divide San Jose into a clipped 10 x 10 grid to examine the geographic distribution of crashes across smaller  # regions. This grid composition is chosen such that we can identify general yet still specific patterns

g <- sj %>%
  st_make_grid(n = c(15,15)) %>% # Create a Rectangular Grid over the entire map, squares are identified as objects
  st_intersection(sj) %>% # clip the grid cells to the city's shape
  st_sf() %>% # convert these objects into a simple features data frame (can then use tidyverse to perform analysis)
  mutate(id = row_number()) # assign a unique identifier to each object in SJ

# Convert the original data to goegraphic data
df_spatial <- st_as_sf( # convert df to a sf data frame
  df,
  coords = c("LONGITUDE", "LATITUDE"), # what columns contain geographic coordinates
  crs = 4326 # tells R to read these columns as longitude and latitude
) %>%
  st_transform(26910) # convert to units in meters (maintain consistency)

# Create our spatial data frame 
df_spatial <- st_filter(df_spatial, sj)
nrow(df_spatial)

# Check out the work

# Plot san jose and the overlayed grid
plot(g$geometry)

# Plot the points onto the original boundaries G
plot(st_geometry(df_spatial), add = TRUE, col = "red", pch = 16, cex = 0.2)





# SEGREGATING SAN JOSE

# Count the total crashes and KSI crashes in each grid cell
grid_summary <- g %>%
  st_join(df_spatial %>% mutate(crash_id = row_number())) %>%
  group_by(id) %>%
  summarize(
    crashes = sum(!is.na(crash_id)),
    KSI_crashes = sum(KSIINJURIES >= 1, na.rm = TRUE)
  )
grid_summary


# Plot the crashes per square
plot(grid_summary["crashes"])
# Plot the KSIINJURED per square
plot(grid_summary["KSI_crashes"])

# Select grid cells with crash counts at or above the 90th percentile (highest crash areas of san jose) 
danger_square <- filter(grid_summary, crashes >= quantile(grid_summary$crashes, 0.90))
nrow(danger_square)

# Select grid cells with KSI crash counts at or above the 90th percentile (highest frequency of severe crashes)
KSI_square <- filter(grid_summary, KSI_crashes >= quantile(grid_summary$KSI_crashes, 0.9))
nrow(KSI_square)










# EDA part 2

# Plot the two observational regions
plot(danger_square)
plot(KSI_square)

# Grab and make separate references for the regions data
h <- st_filter(df_spatial, danger_square)
nrow(h)
v <- st_filter(df_spatial, KSI_square)
nrow(v)
# Note: both regions contain the same grid cells, despite being identifed using different criteria 
# Total crash frequency : KSI crash frequency
# Consequently, they contain the same observations, but represent different measures
# We will retain KSI crashes in the danger_square because our goal is to identify areas with the highest 
# overall crash frequency, regardless of severity. 
# Since we are not directly comparing these regions as independent samples, 
# separating KSI crashes form the general crash observations is not necessary


# view proportions of observations by the COLLISIONTYPE (in the general crash region)
S <- prop.table(table(h$COLLISIONTYPE))
S
barplot(S,
  ylim = c(0, 0.4),
  main = "Collision Types in High-Crash Areas",
  ylab = "Proportion of Crashes",
  las = 2,
  cex.names = 0.6
)

# Viewing the proportions of accidents by traffic control type (in the general crash region)
A <- prop.table(table(h$INTTRAFFICCONTROLTYPE))
A
barplot(A,
  ylim = c(0, 0.7),
  main = "Collision Types in High-Crash Areas",
  ylab = "Proportion of Crashes",
  las = 2,
  cex.names = 0.6
)

# Check out the proportions as one
SA <- as.data.frame(prop.table(
  table(h$INTTRAFFICCONTROLTYPE, h$COLLISIONTYPE),
  margin = 1
))
SA

# Visualize the proportion of each collision type within each traffic control category in high - crash areas
# identify patterns in how collision types are distributed across different traffic controls
ggplot(h, aes( x= INTTRAFFICCONTROLTYPE, fill = COLLISIONTYPE))+
  geom_bar(position="fill") + labs(
    title = "Collision Types by Traffic Control",
    x = "Traffic Control Type",
    y = "Proportion of Crashes",
    fill = "Collision Type"
  ) + theme_minimal() + theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Run this also for KSI_square (double check consistency) 
ggplot(v, aes(x = INTTRAFFICCONTROLTYPE, fill = COLLISIONTYPE)) +
  geom_bar(position = "fill") +
  labs(
    title = "Collision Types by Traffic Control in High-Severity Areas",
    x = "Traffic Control Type",
    y = "Proportion of Crashes",
    fill = "Collision Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Same results, check







# STATISTICAL ANALYSIS

# Create a final data set to work with in predicting the odds of a KSI collision based on its type, and the traffic control it occurred at, and the lighting

model_df <- h %>%
  st_drop_geometry() %>% # convert the data back to a regular dataframe
  mutate(KSI_binary = as.integer(KSIINJURIES >= 1)) %>%
  select(KSI_binary, COLLISIONTYPE, LIGHTING, INTTRAFFICCONTROLTYPE) %>%
  filter(INTTRAFFICCONTROLTYPE != "")
nrow(model_df)
model_df


# Run a logistic regression

model_1 <- glm(KSI_binary ~ COLLISIONTYPE + LIGHTING + INTTRAFFICCONTROLTYPE, data = model_df, family = binomial())
nobs(model_1)
summary(model_1)

# show the coefficients, the significances, and the probabilities for each effect
cbind(round(exp(coef(model_1)), 3), signif(coef(model_1)),round(plogis(coef(model_1)[1] + c(0, coef(model_1)[-1])), 3))

# Since Distance was a slightly problematic variable, let us check to see if the findings differ by any significant amount

model_df2 <- h %>%
  st_drop_geometry() %>% # convert the data back to a regular dataframe
  mutate(KSI_binary = as.integer(KSIINJURIES >= 1), DISTANCE_missing = as.integer(is.na(DISTANCE))) %>%
  select(KSI_binary, COLLISIONTYPE, LIGHTING, INTTRAFFICCONTROLTYPE, DISTANCE_missing) %>%
  filter(INTTRAFFICCONTROLTYPE != "")
nrow(model_df2)
  

model_2 <- glm(KSI_binary ~ COLLISIONTYPE + LIGHTING + INTTRAFFICCONTROLTYPE + DISTANCE_missing, data = model_df2,family = binomial())
nobs(model_2)
model_2

cbind(round(exp(coef(model_2)), 3), signif(coef(model_2)),round(plogis(coef(model_2)[1] + c(0, coef(model_2)[-1])), 3))


















