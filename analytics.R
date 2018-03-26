# Alexandra Aguirre
# 3-20-18
# COP 4020 - Spring 2018
# R Assignment 2 - Analytics

# In this assignment, you are required to write a R script to analyze real product review data from Amazon.  
# You are given the following json file. review.data Contains 10000 reviews
# ASSIGNMENT: Find all the reviews with more than 100 words and group the result by “overall”, 
# and save the result to file.

# needed to use str_count
install.packages('stringr')
library(stringr)

# needed to read json file
install.packages('jsonlite')
library(jsonlite)

# set the working directory, this may be different for everyone
setwd('~/Desktop')
json = stream_in(file('review.data'))

# Calculate how many words in a string
# taken from COP 4020 annoucnements page
nWords <- function(string, pseudo = F){
    ifelse( pseudo, 
    pattern <- "\\S+", 
    pattern <- "[[:alpha:]]+" 
    )
    str_count(string, pattern)
}

# analytics function
RAnalytics <- function()
{
    # get the total word count using the nWords function
    json$totalWords <- sapply(json$reviewText, function(x) nWords(x))

    # see if it is more than 100
    reviews <- subset(json, totalWords > 100)

    # group by overall 
    overall <- reviews[order(reviews$overall),]

    # count the total amount of reviews with more than 100 words
    result <- subset(overall, select = -c(totalWords))
    
    # create the dataframe
    df = data.frame(result)

    # create the output as a JSON file
    write_json(df, file("orderedReviews.json"), pretty=TRUE)

    # print the numbers of rows
    print(nrow(result))

    # create as a text file instead of JSON
   # write.table(result, "Analytics.txt", sep="\t", row.names = FALSE)
}

RAnalytics <- RAnalytics()