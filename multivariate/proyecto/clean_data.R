# Clean Data
# 25/10/2024
# Christian Badillo

#- --------------------------------------------------------------------------------------------
library(tidyverse) # metapackage of all tidyverse packages

#- --------------------------------------------------------------------------------------------
# Change the value of the players from xxx.xxx.xxx to number, erasing the last zeros.
to_money <- function(x){
    out.vector <- c()
    count = 1
    for(i in x){
        if(nchar(i) > 7){
            spl <- unlist(strsplit(i, "[.]"))
            newnumber <- paste(spl[1], spl[2], spl[3], sep="")
            newnumber <- as.numeric(newnumber)
            out.vector[count] <- newnumber
            count <- count + 1
        }
        if(nchar(i) <= 7){
            spl <- unlist(strsplit(i, "[.]"))
            newnumber <- paste(spl[1], spl[2], sep="")
            newnumber <- as.numeric(newnumber)
            out.vector[count] <- newnumber
            count <- count + 1
        }
    }
    return(out.vector)
}

#- -------------------------------------------------------------------------------------------------
# Data
data <- read.csv("player_stats.csv")

# Head
data %>%
    head()

index <- 1
for (i in data$value){
    spl <- unlist(strsplit(i, "[$]"))
    data$value[index] <- spl[2]
    index <- index + 1
}

# Head
data %>%
    head()

data$value <- to_money(data$value) / 1000000

# Head
data %>%
    head()

data %>%
    apply(2, is.nan) %>%
    apply(2, sum)

# Store Data

write_csv(data, "fifa_data_clean.csv")
