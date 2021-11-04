library(readr)

us_cancer <- read_delim("us_cancer.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)

child_cancer <- read_delim("child_cancer.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)
leading_cancer <- read_delim("leading_cancer.txt", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)

