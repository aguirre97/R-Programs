# Alexandra Aguirre
# 2-27-18
# COP 4020 - Spring 2018
# R Assignment 1

# This assignment uses the "hw1_data.csv" file 
# located in the same folder

# read in the CSV file
data <- read.csv("hw1_data.csv")


# Assignment 1: For all even day numbers (days 2, 4, 6, 8, etc) in June, 
# with a Wind value greater than 9, find the mean of the Wind column
RAssignment1Problem1 <- function() {

    # get the even days in July w/ Wind greater than 9.0
    julyItems <- subset(data, Day%%2 == 0 & Month == 6 & Wind > 9)

    # mean of the Wind Column
    mean <- (sum(julyItems$Wind)/nrow(julyItems))

    header <- "Assignment 1 Problem 1: "
    print(header)
    return (print(mean))
}

# Assignment 2: For all records with no NA columns, find the minimum and
# maximum values in the Ozone, Solar.R, Wind, and Temp columns
RAssignment1Problem2 <- function(){

    # subset of values without "NA"
    retval <- subset(data, !is.na(data[,1]))
    retval2 <- subset(data, !is.na(data[,2]))
    # print(retval)

    # max for Ozone in columns with no NA values
    ozoneMax <- max(retval$Ozone)
    # min for Ozone in columns with no NA values
    ozoneMin <- min(retval$Ozone)

    # max for Solar.R in columns with no NA values
    solarMax <- max(retval2$Solar.R)
    # min for Solar.R in columns with no NA values
    solarMin <- min(retval2$Solar.R)

    # max for Wind in columns with no NA values
    windMax <- max(retval$Wind)
    # min for Wind in columns with no NA values
    windMin <- min(retval$Wind)

    # max for Temp in columns with no NA values
    tempMax <- max(retval$Temp)
    #min for Temp in columns with no NA values
    tempMin <- min(retval$Temp)

    # creating the new vectors for the data frame
    Ozone <- c(ozoneMin, ozoneMax)
    Solar.R <- c(solarMin, solarMax)
    Wind <- c(windMin, windMax)
    Temp <- c(tempMin, tempMax)

    # new data frame of mins and maxs
    df <- data.frame(Ozone, Solar.R, Wind, Temp)
    #print(df)
    header <- "Assignemnt 1 Problem 2: "
    print(header)
    return (print(df))
}


RAssignment1Problem1 <- RAssignment1Problem1()
RAssignment1Problem2 <- RAssignment1Problem2()
