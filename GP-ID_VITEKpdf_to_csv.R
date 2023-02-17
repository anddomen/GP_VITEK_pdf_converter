## Name of script: AST-GP_VITEK_pdf_to_csv.R
##
## Purpose:
## Starting with the PDF files from the bioMerieux Vitek 2 Compact,
## we iterate through each PDF file, extract the necessary information
## for each isolate, and create a Master CSV file containing the
## data. 
##
## This is specifically for the AST-GP card type.
##
## Authors: Andrea Domen and James Molyneux
##

## Requires user info: filepath where pdf's are located
## the desired master file name
## the desired filepath for exported file 

## Load necessary R libraries:
## tidyverse for general data manipulations
library(tidyverse)
## tabulizer for ripping tables from PDFs
library(tabulizer)

# USER INPUTTED DATA

# Where are your pdf's? Make sure they're in one file
# with the isolate names as the title of the file
# Also make sure you're only saving the first page of
# the vitek output as a pdf
PDF_DIRECTORY <- ""

# What do you want your file to be named? Put what you
# want it called in the quotations under FILE_NAME
# make sure it ends in .csv
FILE_NAME <- ".csv"

## Where do you want the exported file to go?
EXPORT_FILE_PATH <- ""

#########################################################
## Create a function which formats the data extracted
## from the PDF files
## Note: Might need to "clean" this up slightly for the
## sake of "longevity".
##
## The idea is to scrape each PDF table and then send it
## through this function to scrub it into a nice "clean"
## data frame to work with and manipulate later on.

format_cells <- function(x) {
  # Initial goal: Coerce matrix from PDF to data.frame
  # Make the extracted pdf table into a data.frame for manipulation
  y <- data.frame(x)
  # Skip the first row because it just says "Biochemical details"
  y <- slice_tail(y, n = 8)
  # Break up the first column because it decided to combine the number,
  # the test name, and result
  y <- separate(y, X1, into = c("X001", "X01", "X1"), sep = " ")
  # Remove the test number order because it's useless
  y <- select(y, -c("X001", "X2", "X5", "X8", "X11", "X14"))
  # Extract all the paired data and assign their own dataframes for
  # combining later. Eg, column 1 and 2, column 3 and 4
  # for df2-6, they all have a blank row at the end, so those need
  # to be removed
  df1 <- y[1:2];names(df1) <- c("Test", "Result")
  df2 <- slice_head(y[3:4], n = 7);names(df2) <- c("Test", "Result")
  df3 <- slice_head(y[5:6], n = 7);names(df3) <- c("Test", "Result")
  df4 <- slice_head(y[7:8], n = 7);names(df4) <- c("Test", "Result")
  df5 <- slice_head(y[9:10], n = 7);names(df5) <- c("Test", "Result")
  df6 <- slice_head(y[11:12], n = 7);names(df6) <- c("Test", "Result")
  # combine the above data frames into one
  bind_rows(df1, df2, df3, df4, df5, df6)
}


# Make a function to grab the organism that the VITEK
# identified it as, if any
add_organism <- function(x) {
  organism_name <- x[4, 3]
  return(organism_name)
}

#########################################################
## This is where the bulk of the work happens. We want 
## to find where the PDF files are 
## located and then extract all of the names of the files
## so we can (1) scrape the data from the pdf and (2) 
## pull some of the info from the PDF file name.

## Point to the base directory containing the PDF file
## Need this info from USER above
base_dir <- PDF_DIRECTORY

## Extract the file names
pdf_files <- list.files(base_dir)

## Find the isolate names using the file names. Isolate
## name is the title of the PDF.
## Uses wildcard (*) to ID the isolate numbers and trim
## off the .pdf
isolate_numbers <- gsub("(*).pdf$", "\\1", pdf_files)

#########################################################
## Now, we want to loop through each isolate and 
## format the data 

## Use i = 1 so I could look at intermediate outputs
## while prototyping REMEMBER TO COMMENT OUT
# i = 1

## Define the number of files in the folder for use 
## in the for loop
files <- dir(path = base_dir, pattern = "*.pdf")

for (i in 1:length(files)) {
  
  ## Define the "card type", which we take to always be the
  ## same value.
  card_type <- "GP-ID"
  
  #########################################################  
  ## Our next goal is to scrape the PDF tables using tabulizer
  ## We use extract_tables(...)[[3]] to get the "3rd" table 
  ## in the pdf doc, which has the identification data. 
  ##And each doc is found using some file path trickery 
  ##inside paste0
  bac_pdf <- extract_tables(paste0(base_dir,"/", pdf_files[i]))
  
  ## Use the format_cells() function we made to format the 
  ## scraped data
  bac_df <- format_cells(bac_pdf[[3]])
  bac_df <- bac_df |> 
     mutate(Organism = add_organism(bac_pdf[[2]]))
  
  ## Create a temporary data frame which we can "add onto" as
  ## we iterate through the isolates. Create this table by
  ## taking our combined data frames post cleaning and merging.
  ## Makes the data "wide" so the tests are columns with the
  ## results (+/-) as rows. Each pdf and by extension, each
  ## isolate will have one row
  tmp_df <- bac_df %>%
    # Makes the dataframe wider with tests as column names, pipes
    # to mutate
    pivot_wider(names_from = Test, values_from = Result) %>%

    ## Include info about the isolate and card type
    ## Remember isolate_numbers are from the title of the file
    mutate(Isolate = isolate_numbers[i],
           Card = card_type) %>%
    ## Here, we altered one variable name to match the
    ## original master data.
    dplyr::select(Isolate, Card, everything())
  
  ## For the first iteration only, we setup our first cleaned
  ## pdf and call it the "master" data. We then move to the 
  ## next iteration to avoid the "joining" process shown
  ## below.
  if (i == 1) {
    master_df <- tmp_df
    # master_df <- bac_df
    next 
  }
  

  ## Create our master_df by joining the new data (tmp_df)
  ## onto the original master_df
  master_df <- full_join(master_df, tmp_df) #%>%
  # master_df <- full_join(master_df, bac_df) #%>%

}

#########################################################
## Once we've iterated and created our updated master
## data file, we output that data file using the line
## of code below.

## glues date onto user inputted file name
output_file_name <- paste0(gsub("-", "", Sys.Date()), FILE_NAME)

## glues desired exported path with the file name
output_file_path <- paste0(EXPORT_FILE_PATH, output_file_name)

## Export it!
## Sit back and marvel
    # Or something happened and it broke
write_csv(master_df, file = output_file_path)


