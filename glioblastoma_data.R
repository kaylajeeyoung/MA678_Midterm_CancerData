library(tidyverse)
library(magrittr)
library(maps)

region_code <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")

clean_text <- function(file){
  file <- file[,-1] #remove the "notes" column, which is NA values 
  #some rows are completely empty, filter them out 
  file <- file[which(complete.cases(file)),]
  file <- within(file, rm("Year Code"))
  return(file)
}


brain_death <- clean_text(read_delim("brain_death.txt",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE, show_col_types = FALSE))
brain_incident <- clean_text(read_delim("brain_incident.txt",
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE, show_col_types = FALSE))
brain_rate <- clean_text(read_delim("brain_rate.txt",
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE, show_col_types = FALSE))

region_incident <- clean_text(read_delim("region_inc.txt",
                                         delim = "\t", escape_double = FALSE, 
                                         trim_ws = TRUE, show_col_types = FALSE))
region_death <- clean_text(read_delim("region_mort.txt",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE, show_col_types = FALSE))

#clean up brain incidence
age_group <- strsplit(brain_death$`Age Group Code`, "-")
age_group <- sapply(age_group, "[[", 1)
age_group <- as.numeric(substr(age_group, 1, 2))
brain_death <- cbind(brain_death, age_group)

age_group <- strsplit(brain_incident$`Age Groups Code`, "-")
age_group <- sapply(age_group, "[[", 1)
age_group <- as.numeric(substr(age_group, 1, 2))
brain_incident <- cbind(brain_incident, age_group)

brain_incident <- brain_incident %>% add_column(Region = NA)
for(i in 1:nrow(brain_incident)){
  state <- which(region_code$State == brain_incident$States[i])
  brain_incident$Region[i] <- region_code$Region[state]
}
brain_death <- brain_death %>% add_column(Region = NA)
for(i in 1:nrow(brain_death)){
  state <- which(region_code$State == brain_death$State[i])
  brain_death$Region[i] <- region_code$Region[state]
}

age_group <- strsplit(region_incident$`Age Groups Code`, "-")
age_group <- sapply(age_group, "[[", 1)
age_group <- as.numeric(substr(age_group, 1, 2))
region_incident <- cbind(region_incident, age_group)

age_group <- strsplit(region_death$`Age Group Code`, "-")
age_group <- sapply(age_group, "[[", 1)
age_group <- as.numeric(substr(age_group, 1, 2))
region_death <- cbind(region_death, age_group)

#visualize the brain incidence rate by age group 
ggplot(brain_incident, aes(x = age_group, y = Count)) + theme_bw() + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) + geom_jitter() + 
  stat_summary(geom = "line", fun = "mean", col  = "blue", lwd = 2)
#visualize with year 
plot_inc <- ggplot(brain_incident, aes(x = age_group, y = `Crude Rate`, color = Year)) + 
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  theme(legend.position ="none") + 
#        legend.justification = c("left", "top"), legend.box.just = "left") +
  geom_jitter(aes(alpha = 0.5)) + scale_color_gradient(low = "blue", high = "red") + 
  stat_summary(geom = "line", fun = "mean", col  = "black", lwd = 1) + 
  xlab("Age (Grouped)") + ylab("Crude Rate (10^-5)") + ggtitle(" Incidence Rate") + 
  facet_wrap(~Year)

#visualize the brain incidence rate by age group 
ggplot(brain_death, aes(x = age_group, y = `Crude Rate`, color = Year)) + theme_bw() + 
  geom_jitter(aes(alpha = 0.5))+ 
  stat_summary(geom = "line", fun = "mean", col  = "blue", lwd = 2) + 
  facet_wrap(~Year)
#visualize with year 
plot_death <- ggplot(brain_death, aes(x = age_group, y = `Crude Rate`, group = Year, color = Year)) + 
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  theme(legend.position ="none") + 
  geom_jitter(aes(alpha = 0.5)) + scale_color_gradient(low = "blue", high = "red") + 
  stat_summary(geom = "line", fun = "mean", col  = "black", lwd = 1) + 
  xlab("Age (Grouped)") + ylab("Crude Rate (10^-5)") + ggtitle(" Death Rate of Glioblastoma") + 
  facet_wrap(~Year)
  

#Region
ggplot(region_incident, aes(x = Year, y = `Crude Rate`, group = Region)) + 
  geom_point(aes(color = Region), size = 1) + theme_bw() + 
  ylab("Crude Rate (10^-5") + ggtitle("Crude Incidence Rate of Glioblastoma")

mean_region <- region_incident %>% group_by(Region, Year) %>% 
  dplyr::summarize(mean = mean(`Crude Rate`), sd = sd(`Crude Rate`))

mean_region_plot <- ggplot(mean_region, aes(x = Year, y = mean, group = Region)) + 
  geom_point(aes(color = Region), size = 1) + theme_bw() +
  theme(legend.position = "none") +
  ylab("Average Crude Rate (10^-5)") + ggtitle("Crude Incidence Rate")

region_death_plot <- ggplot(brain_death, aes(x = age_group, y = `Crude Rate`, color = Region)) + 
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  #  theme(legend.position = c(.05, .95),
  #        legend.justification = c("left", "top"), legend.box.just = "left") +
  geom_smooth(size = 0.5, se = FALSE, method = "loess", formula = y~x) + 
  #stat_summary(geom = "line", fun = "mean", col  = "black", lwd = 1) + 
  xlab("Age (Grouped)") + ylab("Crude Rate (10^-5)") + ggtitle(" Death Rate") + 
  facet_wrap(~Year)
region_incident_plot <- ggplot(brain_incident, aes(x = age_group, y = `Crude Rate`, color = Region)) + 
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  #  theme(legend.position = c(.05, .95),
  #        legend.justification = c("left", "top"), legend.box.just = "left") +
  geom_smooth(size = 0.5, se = FALSE, method = "loess", formula = y~x) + 
  #stat_summary(geom = "line", fun = "mean", col  = "black", lwd = 1) + 
  xlab("Age (Grouped)") + ylab("Crude Rate (10^-5)") + ggtitle(" Incidence Rate") + 
  facet_wrap(~Year)



mean_death <- region_death %>% group_by(Region, Year) %>% 
  dplyr::summarize(mean = mean(`Crude Rate`), sd = sd(`Crude Rate`))

mean_death_plot <- ggplot(mean_death, aes(x = Year, y = mean, group = Region)) + 
  geom_point(aes(color = Region), size = 1) + theme_bw() +
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"), legend.box.just = "right") +
  ylab("Average Crude Rate (10^-5)") + ggtitle("Crude Death Rate of Glioblastoma")

#Map
df <- brain_incident
year <- df %>% distinct(Year)
year <- year$Year
MainStates <- map_data("state")
MainStates$region <- str_to_title(MainStates$region)
colnames(MainStates) <- c("long", "lat", "group", "order", "States", "subregion")

df_mean <- df %>% subset(Year == 2017) %>% group_by(States)

plot_data <- inner_join(df_mean, MainStates, by = "States") %>% 
  dplyr::select("States", "long", "lat", "Crude Rate", "Year", "group")

plot_data$rate <- plot_data$`Crude Rate`

visual_map <- ggplot() + 
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),color="black", fill="seashell1", size = .3) + 
  geom_polygon(data = plot_data, aes(x = long, y = lat, group = States, fill = rate), 
               color = "grey", size = .3) + 
  scale_color_gradient(name="Crude Rate (10^-5)", 
                       low = "blue2", 
                       high = "brown3", 
                       na.value = "grey50") + labs(title="Glioblastoma incidence rates")

