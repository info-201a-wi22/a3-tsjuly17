# Load packages
library(tidyverse)
library(fBasics)
library(plotly)
library(sf)
library(urbnmapr)
library(reshape2)
# Read data
inc <- read_csv("incarceration_trends.csv")

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
df1 <- inc %>%
  dplyr::select(
    white_jail_pop, black_jail_pop, native_jail_pop, latinx_jail_pop,
    other_race_jail_pop, year
  ) %>%
  dplyr::rename(
    White = "white_jail_pop", Black = "black_jail_pop",
    Native = "native_jail_pop", Latin = "latinx_jail_pop",
    Other = "other_race_jail_pop"
  ) %>%
  reshape2::melt(id = "year") %>%
  dplyr::group_by(year, variable) %>%
  summarise(Avg.Pop = mean(value, na.rm = T))



# Make plot
ggplot(df1, aes(x = year, y = Avg.Pop, color = variable)) +
  geom_line() +
  labs(
    x = "Year", y = "Average population", color = "Race",
    title = "Evolution of average incacerations over the years"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Box plot
ggplot(my_dat, aes(
  x = urbanicity, y = log(total_jail_pop),
  fill = urbanicity
)) +
  geom_boxplot() +
  labs(
    x = "Region", y = "Log Total population",
    title = "Relationsip between region and total jail population"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

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


m <- ggplot() +
  geom_sf(spatial_data, mapping = aes(fill = log(Avg.Pop))) +
  geom_sf_text(
    data = get_urbn_labels(map = "states", sf = TRUE),
    aes(label = state_abbv), size = 2
  ) +
  scale_fill_distiller("Log Average Population", palette = "Spectral") +
  labs(title = "Average population by State", x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(m)

helllo
