library(tidyverse)

#clean text file 
clean_text <- function(file){
  file <- file[,-1] #remove the "notes" column, which is NA values 
  #some rows are completely empty, filter them out 
  file <- file[which(complete.cases(file)),]
  file <- within(file, rm("Year Code"))
  return(file)
}

#read in the text files 
nl_cancer <- read_delim("nervous_lung_cancer.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)

child_cancer <- read_delim("child_cancer.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

leading_cancer <- read_delim("leading_cancer.txt", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)

nl_death <- read_delim("nl_death.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

child_death <- read_delim("leading_child_death.txt", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

child_death2 <- read_delim("leading_child_death2.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

cdeath_region <- read_delim("child_death_ethregion.txt",
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)


leading_death <- read_delim("leading_death.txt", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

options(warn=-1)
#clean data frames
nl_cancer <- clean_text(nl_cancer)
child_cancer <- clean_text(child_cancer)
leading_cancer <- clean_text(leading_cancer)
nl_death <- clean_text(nl_death)
child_death <- clean_text(child_death)
child_death2 <- clean_text(child_death2)
cdeath_region <- clean_text(cdeath_region)
leading_death <- clean_text(leading_death)


options(warn=0)

cbrain_region <- cdeath_region[grepl((cdeath_region$`Cancer Sites`), pattern = "Brain"),] %>% filter(Race == "White")
#cbrain_region <- cbrain_region %>% group_by(across(c("Region", "Year"))) %>% summarise(sum(Deaths))
#cbrain_region <- cbrain_region[!grepl((cbrain_region$Population), pattern = "Not Applicable"),]
cbrain_region$Population <- as.numeric(cbrain_region$Population)
cbrain_region$`Death Rate` <- cbrain_region$Deaths/cbrain_region$Population
log(cbrain_region$`Death Rate`)
#cbrain_region <- cbrain_region %>% group_by(across(c("Region", "Year"))) %>% 
#  summarise("Total Deaths" = sum(Deaths), "Total Population" = sum(Population), "Death Rate" = sum(Deaths)/sum(Population))
ggplot(cbrain_region, aes(x = Year, y = `Age-Adjusted Rate`, group = Region)) + geom_line(aes(color = Region), size = 1)
ggplot(cbrain_region, aes(x = Year, y = log(`Death Rate`), group = Region)) + geom_line(aes(color = Region), size = 1)
ggplot(cbrain_region[cbrain_region$Region == "Midwest",], aes(x = Population, group = Year)) + geom_histogram(aes(fill = Year))


#obtain only CNS cancers
df <- child_cancer[grepl("III(b) Astrocytomas", child_cancer$`Childhood Cancers`,fixed = TRUE),]
child_brain_cancer  <- child_cancer[grepl((child_cancer$`Childhood Cancers`), pattern = "III"),]
child_brain_cancer <- child_brain_cancer %>% group_by(across(c(States, Year))) %>% summarise(Count = sum(Count), 
                                                                                             Population = unique(Population),
                                                                                             Rate = sum(Count)/unique(Population) * 10000) 

lm(log(`Death Rate`) ~ Year + Region, data = cbrain_region)

