library(readr)
library(tidyverse)

nl_cancer <- read_delim("nervous_lung_cancer.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)

child_cancer <- read_delim("child_cancer.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)
leading_cancer <- read_delim("leading_cancer.txt", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)

leading_death <- read_delim("leading_death.txt", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

child_death <- read_delim("leading_child_death.txt", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

nl_death <- read_delim("nl_death.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)
