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
leading_death <- clean_text(leading_death)

options(warn=0)

#obtain only CNS cancers
df <- child_cancer[grepl("III(b) Astrocytomas", child_cancer$`Childhood Cancers`,fixed = TRUE),]

