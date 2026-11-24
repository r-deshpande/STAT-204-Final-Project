# PART 3: ANALYZING TIME OF DAY / DAY OF WEEK / MONTH / WEATHER

# (iii): Is there an observable pattern of higher incident rates
# during varying times of day, month, or weather events?

# Looking at these the whole way:
#   (1) How many crashes happen (frequency and rate)
#   (2) How severe these crashes are (severe is crashes with KSIINJURIES >= 1)
# these two metrics can disagree


# Sources used
# why lubridate: https://www.r-bloggers.com/2024/09/mastering-date-and-time-data-in-r-with-lubridate/


# LOADING OF LIBRARIES AND DATA

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)   

# Loading in the dataset
df <- read.csv("Crash_Locations_Last_Five_Years.csv")

# Ensuring these variables exist in the data
part3_vars <- c("CRASHDATETIME", "WEATHER", "LIGHTING", "ROADWAYSURFACE", "KSIINJURIES")
part3_vars[!part3_vars %in% names(df)]

# Looking at the timestamps before parsing them
head(df$CRASHDATETIME, 10)


# DATA CLEANING

# Timestamps stored in UTC (ending in "+00" or "Z").
# There is a 7-8 hour difference if so, so I am converting these to PST.
raw_dt <- df$CRASHDATETIME
is_utc <- any(grepl("(\\+00(:?00)?|Z)$", raw_dt))
is_utc

raw_clean <- gsub("T", " ", sub("(\\+00(:?00)?|Z)$", "", raw_dt))
parsed <- parse_date_time(raw_clean,
  orders = c("Ymd HMS", "Ymd HM", "mdY HMS", "mdY IMS p", "mdY HM"),
  tz = "UTC", quiet = TRUE)

crash_dt <- if (is_utc) with_tz(parsed, "America/Los_Angeles") else force_tz(parsed, "America/Los_Angeles")
sum(is.na(crash_dt))   # number of timestamps that failed to parse

df3 <- df %>%
  mutate(
    crash_dt = crash_dt,
    KSI_binary = as.integer(KSIINJURIES >= 1),
    hour = hour(crash_dt),
    month = month(crash_dt),
    month_lab = factor(month.abb[month], levels = month.abb),
    dow = wday(crash_dt, week_start = 1),   # 1 = Monday, 2 = Tuesday, etc
    dow_lab = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[dow],
                     levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    weekend = ifelse(dow >= 6, "Weekend", "Weekday"),
    year = year(crash_dt),
    time_block = cut(hour,
      breaks = c(-1, 5, 9, 14, 18, 23),
      labels = c("Overnight (12-5am)", "Morning (6-9am)", "Midday (10am-2pm)",
                 "Evening commute (3-6pm)", "Night (7-11pm)")),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% 3:5 ~ "Spring",
      month %in% 6:8 ~ "Summer",
      TRUE ~ "Fall"),
    # blank strings -> NA
    across(c(WEATHER, LIGHTING, ROADWAYSURFACE), ~ na_if(trimws(.x), ""))
  ) %>%
  filter(!is.na(crash_dt), !is.na(KSI_binary))
nrow(df3)

# CHECK: If the crash peak is around 10pm-1am, the timestamps were UTC and did not get converted.
ggplot(df3, aes(x = hour)) +
  geom_bar() +
  labs(title = "Crashes by Hour of Day (time zone check)", x = "Hour", y = "Crashes") +
  theme_minimal()

# Check for "fake" midnight times (unknown times are recorded as 00:00:00)
# If this is a big chunk of hour 0 crashes, those rows should be left out.
sum(hour(df3$crash_dt) == 0 & minute(df3$crash_dt) == 0 & second(df3$crash_dt) == 0)
sum(df3$hour == 0)

# Date range covered
range(df3$crash_dt)

# Missing values in the weather-type variables
colSums(is.na(df3[, c("WEATHER", "LIGHTING", "ROADWAYSURFACE")]))



# EDA

# Count how many calendar days of each month and weekday are in the date range,
all_days <- data.frame(date = seq(as.Date(min(df3$crash_dt)), as.Date(max(df3$crash_dt)), by = "day")) %>%
  mutate(month = month(date), dow = wday(date, week_start = 1))
month_days <- count(all_days, month, name = "days")
dow_days <- count(all_days, dow, name = "days")


# 1 (HOUR OF DAY)

# total crashes, KSI crashes, and KSI % for each hour
hour_summary <- df3 %>%
  group_by(hour) %>%
  summarise(
    crashes = n(),
    KSI_crashes = sum(KSI_binary),
    KSI_pct = round(mean(KSI_binary) * 100, 2),
    .groups = "drop")
hour_summary

ggplot(hour_summary, aes(x = hour, y = KSI_pct)) +
  geom_line() + geom_point() +
  labs(title = "Share of Crashes Resulting in KSI by Hour", x = "Hour", y = "KSI crashes (%)") +
  theme_minimal()

time_block_summary <- df3 %>%
  group_by(time_block) %>%
  summarise(
    crashes = n(),
    KSI_crashes = sum(KSI_binary),
    KSI_pct = round(mean(KSI_binary) * 100, 2),
    .groups = "drop")
time_block_summary


# 2 (DAY OF WEEK)

dow_summary <- df3 %>%
  group_by(dow) %>%
  summarise(
    crashes = n(),
    KSI_crashes = sum(KSI_binary),
    KSI_pct = round(mean(KSI_binary) * 100, 2),
    .groups = "drop") %>%
  left_join(dow_days, by = "dow") %>%
  mutate(crashes_per_day = round(crashes / days, 2),
         dow_lab = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[dow],
                          levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
dow_summary

# Hour x day of week heat map (crash counts)
df3 %>%
  count(dow_lab, hour) %>%
  ggplot(aes(x = hour, y = dow_lab, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Crashes by Hour and Day of Week", x = "Hour", y = NULL, fill = "Crashes") +
  theme_minimal()

# KSI % by time block, weekday vs weekend
df3 %>%
  group_by(time_block, weekend) %>%
  summarise(KSI_pct = mean(KSI_binary) * 100, crashes = n(), .groups = "drop") %>%
  ggplot(aes(x = time_block, y = KSI_pct, fill = weekend)) +
  geom_col(position = "dodge") +
  labs(title = "KSI Share by Time of Day, Weekday vs Weekend",
       x = NULL, y = "KSI crashes (%)", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3 (MONTH)

month_summary <- df3 %>%
  group_by(month) %>%
  summarise(
    crashes = n(),
    KSI_crashes = sum(KSI_binary),
    KSI_pct = round(mean(KSI_binary) * 100, 2),
    .groups = "drop") %>%
  left_join(month_days, by = "month") %>%
  mutate(crashes_per_day = round(crashes / days, 2),
         month_lab = factor(month.abb[month], levels = month.abb)) %>%
  arrange(month)
month_summary

ggplot(month_summary, aes(x = month_lab, y = crashes_per_day)) +
  geom_col() +
  labs(title = "Average Crashes per Day by Month", x = NULL, y = "Crashes per day") +
  theme_minimal()

ggplot(month_summary, aes(x = month_lab, y = KSI_pct)) +
  geom_col() +
  labs(title = "Share of Crashes Resulting in KSI by Month", x = NULL, y = "KSI crashes (%)") +
  theme_minimal()

# Checking the crash totals by year. If one year is unusally high/low, it can 
# skew month averages since all years are pooled together. 
df3 %>%
  group_by(year) %>%
  summarise(
    crashes = n(),
    KSI_crashes = sum(KSI_binary),
    KSI_pct = round(mean(KSI_binary) * 100, 2),
    .groups = "drop")


# 4 (WEATHER / ROAD SURFACE / LIGHTING)

weather_summary <- df3 %>%
  group_by(WEATHER) %>%
  summarise(
    crashes = n(),
    KSI_crashes = sum(KSI_binary),
    KSI_pct = round(mean(KSI_binary) * 100, 2),
    .groups = "drop") %>%
  arrange(desc(crashes))
weather_summary

surface_summary <- df3 %>%
  group_by(ROADWAYSURFACE) %>%
  summarise(
    crashes = n(),
    KSI_crashes = sum(KSI_binary),
    KSI_pct = round(mean(KSI_binary) * 100, 2),
    .groups = "drop") %>%
  arrange(desc(crashes))
surface_summary

lighting_summary <- df3 %>%
  group_by(LIGHTING) %>%
  summarise(
    crashes = n(),
    KSI_crashes = sum(KSI_binary),
    KSI_pct = round(mean(KSI_binary) * 100, 2),
    .groups = "drop") %>%
  arrange(desc(crashes))
lighting_summary

# the dataset has no info on how many hours it was actually raining/foggy in San Jose, which makes it 
# so we can't determine causality due to rain counts alone. 
# dataset is mostly clear weather though
# we ARE able to compare severity though: given a crash happened, is it more likely to be KSI >= 1  in bad weather?

# Weather and road surface overlap, checking before putting both in a model
table(df3$WEATHER, df3$ROADWAYSURFACE)
# Same for lighting and time of day (night -> dark)
table(df3$time_block, df3$LIGHTING)


# STATISTICAL TESTS

#Do crash COUNTS differ across hours / weekdays / months more than chance would allow?
# using chi-square goodness of fit. Expected counts are proportional to how much time each category covers.
# with around 24k crashes (high volume) almost any difference will be somewhat significant, so size of 
# depth table differences are more important(crashes per day tables above)

chisq.test(hour_summary$crashes)   # every hour is equally available, so uniform is the right null
chisq.test(dow_summary$crashes, p = dow_summary$days / sum(dow_summary$days))
chisq.test(month_summary$crashes, p = month_summary$days / sum(month_summary$days))

# (b) Does the KSI SHARE differ across groups? Chi-square test of independence.
# simulate.p.value is used where some cells are small (rare weather types)
chisq.test(table(df3$time_block, df3$KSI_binary))
chisq.test(table(df3$weekend, df3$KSI_binary))
chisq.test(table(df3$month, df3$KSI_binary))
chisq.test(table(df3$WEATHER, df3$KSI_binary), simulate.p.value = TRUE, B = 5000)
chisq.test(table(df3$ROADWAYSURFACE, df3$KSI_binary), simulate.p.value = TRUE, B = 5000)



# LOGISTIC REGRESSION
# Odds of a crash being KSI given when it happened and the conditions, controlling for each other

model_df3 <- df3 %>%
  select(KSI_binary, time_block, weekend, season, year, WEATHER, ROADWAYSURFACE, LIGHTING) %>%
  drop_na() %>%   # same rows in every model so they can be compared
  # putting the rare categories (fewer than 50 crashes) together so the model doesn't overfit on levels with a handful of crashes
  mutate(
    WEATHER = ifelse(WEATHER %in% names(which(table(WEATHER) < 50)), "Other", WEATHER),
    ROADWAYSURFACE = ifelse(ROADWAYSURFACE %in% names(which(table(ROADWAYSURFACE) < 50)), "Other", ROADWAYSURFACE),
    LIGHTING = ifelse(LIGHTING %in% names(which(table(LIGHTING) < 50)), "Other", LIGHTING)) %>%
  # Using the most common level as the reference (clear, dry, daylight)
  mutate(
    WEATHER = relevel(factor(WEATHER), ref = names(which.max(table(WEATHER)))),
    ROADWAYSURFACE = relevel(factor(ROADWAYSURFACE), ref = names(which.max(table(ROADWAYSURFACE)))),
    LIGHTING = relevel(factor(LIGHTING), ref = names(which.max(table(LIGHTING)))),
    time_block = relevel(time_block, ref = "Midday (10am-2pm)"),
    season = relevel(factor(season), ref = "Spring"),
    year = factor(year))
nrow(model_df3)
table(model_df3$KSI_binary)

# Model A: time + weather (no lighting, since lighting is mostly a stand in for time of day)
model_3a <- glm(KSI_binary ~ time_block + weekend + season + WEATHER + ROADWAYSURFACE + year,
                data = model_df3, family = binomial())
nobs(model_3a)
summary(model_3a)

# Model B: adding lighting. does it explain anything beyond time of day?
model_3b <- update(model_3a, . ~ . + LIGHTING)
summary(model_3b)
anova(model_3a, model_3b, test = "LRT")

# One p-value per variable 
# does time of day matter overall?
# does weather matter overall?
drop1(model_3a, test = "LRT")

# Odds ratios, 95% CIs, and actual p-values
coefs_3a <- summary(model_3a)$coefficients
ci_3a <- confint.default(model_3a)
or_3a <- data.frame(
  odds_ratio = round(exp(coefs_3a[, 1]), 3),
  ci_low = round(exp(ci_3a[, 1]), 3),
  ci_high = round(exp(ci_3a[, 2]), 3),
  p_value = signif(coefs_3a[, 4], 3))
or_3a

# Odds ratio plot (excluding the intercept and year controls)
or_3a %>%
  mutate(term = rownames(or_3a)) %>%
  filter(term != "(Intercept)", !grepl("^year", term)) %>%
  ggplot(aes(x = odds_ratio, y = reorder(term, odds_ratio))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  geom_point() +
  scale_x_log10() +
  labs(title = "Odds of a KSI Crash (vs. reference levels)",
       x = "Odds ratio (log scale)", y = NULL) +
  theme_minimal()

# Baseline KSI probability (midday, weekday, spring, most common weather/surface, first year)
plogis(coef(model_3a)[1])