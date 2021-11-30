library(tidyverse)

clean_text <- function(file){
  file <- file[,-1] #remove the "notes" column, which is NA values 
  #some rows are completely empty, filter them out 
  file <- file[which(complete.cases(file)),]
  file <- within(file, rm("Year Code"))
  return(file)
}

brain_death <- clean_text(read_delim("brain_death.txt",
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE))
brain_incident <- clean_text(read_delim("brain_incident.txt",
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE))
brain_rate <- clean_text(read_delim("brain_rate.txt",
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE))

#clean up brain incidence
age_group <- strsplit(brain_incident$`Age Groups Code`, "-")
age_group <- sapply(age_group, "[[", 1)
age_group <- as.numeric(substr(age_group, 1, 2))
brain_incident <- cbind(brain_incident, age_group)

#visualize the brain incidence rate by age group 
ggplot(brain_incident, aes(x = age_group, y = Count)) + theme_bw() + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) + geom_jitter()

#clean up brain deaths
age_group <- strsplit(brain_death$`Age Group Code`, "-")
age_group <- sapply(age_group, "[[", 1)
age_group <- as.numeric(substr(age_group, 1, 2))
brain_death <- cbind(brain_death, age_group)

#visualize the brain incidence rate by age group 
ggplot(brain_death, aes(x = age_group, y = Deaths)) + theme_bw() + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) + geom_jitter()


#clean up brain rates
brain_rate$`Mortality-Incidence Age-Adjusted Rate Ratio`
