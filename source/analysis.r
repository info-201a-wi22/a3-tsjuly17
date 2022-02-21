incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

# Load packages
library(tidyverse)
library(fBasics)
library(plotly)
library(sf)
install.packages("devtools")
devtools::install_github("UrbanInstitute/urbnmapr")

library(urbnmapr)

# Read data
inc <- read.csv("incarceration_trends.csv", stringsAsFactors = FALSE)

# Select variables of interest
my_dat <- inc %>%
  select(
    state, year, total_jail_pop, land_area, black_jail_pop_rate,
    white_jail_pop_rate, latinx_jail_pop_rate, native_jail_pop_rate,
    urbanicity
  )

## Summary statistics
summ <- basicStats(my_dat[, -c(1, 2, 9)])[c(3, 4, 7, 8, 13:16), ]
t_summ <- as.data.frame(t(round(summ, 3)))
names(t_summ) <- c("Min", "Max", "Mean", "Med", "Var", "St.dev", "Skew", "Kurt")

t_summ

# Generate dataframe for average population by year. Select top 10 yrs
dat1 <- my_dat %>%
  group_by(year) %>%
  summarise(Avg.Pop = mean(total_jail_pop, na.rm = T)) %>%
  arrange(desc(Avg.Pop)) %>%
  head(10)
dat1

# Generate dataframe for average population by state. Select top 10 states
dat2 <- my_dat %>%
  group_by(state) %>%
  summarise(Avg.Pop = mean(total_jail_pop, na.rm = T)) %>%
  arrange(desc(Avg.Pop)) %>%
  head(10)
dat2

# Generate dataframe for average population by year
df1 <- my_dat %>%
  group_by(year) %>%
  summarise(Avg.Pop = mean(total_jail_pop, na.rm = T))

# Make plot
ggplot(df1, aes(x = year, y = Avg.Pop)) +
  geom_line() +
  labs(
    x = "Year", y = "Average population",
    title = "Evolution of average incacerations over the years"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Scatter plot
ggplot(my_dat, aes(
  x = log(land_area), y = log(total_jail_pop),
  color = urbanicity
)) +
  geom_point(size = 1) +
  labs(
    x = "Log Land area (Sq.Km)", y = "Log Total population",
    title = "Relationsip between land area and total jail population"
  )


# Geographic representation
df2 <- my_dat %>%
  group_by(state) %>%
  summarise(Avg.Pop = mean(total_jail_pop, na.rm = T)) %>%
  dplyr::rename(state_abbv = state)


spatial_data <- left_join(df2,
  get_urbn_map(map = "states", sf = TRUE),
  by = "state_abbv"
) %>%
  st_as_sf()


ggplot() +
  geom_sf(spatial_data,
    mapping = aes(fill = log(Avg.Pop))
  ) +
  scale_fill_distiller("Average total jail population", palette = "Spectral") +
  labs(
    fill = "Log Average Population",
    title = "Average population by State"
  )
