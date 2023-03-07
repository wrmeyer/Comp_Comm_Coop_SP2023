packages <- c("striprtf", # To read .rtf files
              "stringr", # To parse character strings
              "readr", # To parse numbers
              "lubridate", # To process date strings
              "here") 
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], repos = 'http://cran.us.r-project.org')
}
invisible(lapply(packages, library, character.only = TRUE))

setwd(here()) 

nexis <- read_rtf("Files (100).rtf")
factiva <- read_rtf("factiva.rtf")

Narticles <- data.frame(matrix(ncol = 7, nrow = 100))
variables <- c("Title", "Date_Media", "Section", "Length", "Byline", "Highlight", "Body")
colnames(Narticles) <- variables

# We start by putting the 1st article's info in the 1st row.
articleNo <- 1 

# Making sure that the title cells are empty strings instead of NAs so that we can append strings at the end of it. You don't have to do this if you are directly assigning values to a cell.
Narticles$Title <- "" 

# The title switch is on because this will be the first piece of useful information we encounter.
title <- TRUE 

# The date and body switches are off because the first piece of information is not an article's date or body.
date <- FALSE
body <- FALSE

for (line in nexis) {
  # Turning off title and turning on date when we see "The New York Times" because this line lies between title and date.
  # We use paste to concatenate strings instead of directly assigning a line to the Title cell because a title can take multiple lines and we hope to make sure we get the complete title.
  if (title == TRUE & nchar(line)>2){
    Narticles$Title[articleNo] <- paste(Narticles$Title[articleNo], line)
    title <- FALSE
    date <- TRUE
  } 
  # Assigning the date line to the Date cell of the corresponding article.
  else if (date == TRUE) {
    Narticles$Date_Media[articleNo] <- line
    if (nchar(line)<2){
      date <- FALSE
    }
  } 
  # Assigning the section line to the Section cell of the corresponding article.
  else if (startsWith(line, "Section:")) {
    Narticles$Section[articleNo] <- line
  } 
  # Assigning the length line to the Length cell of the corresponding article.
  # parse_number function allows us to keep only the numeric part of the line.
  else if (startsWith(line, "Length:")) {
    Narticles$Length[articleNo] <- parse_number(line)
  } 
  # Assigning the byline line to the Byline cell of the corresponding article.
  else if (startsWith(line, "Byline")) {
    Narticles$Byline[articleNo] <- line
  } 
  # Assigning the highlight line to the Highlight cell of the corresponding article.
  else if (startsWith(line, "Highlight")) {
    Narticles$Highlight[articleNo] <- line
  } 
  # When we see a line with the single word "Body", we know the upcoming lines all belong to the current article's body, so we switch on the body switch, and put an empty string in the Body cell of this row, getting ready to append later parts of the article's body to this cell as we loop through them.
  else if (line == "Body") {
    body <- TRUE
    Narticles$Body[articleNo] <- ""
  } 
  # If an article contains a photo, a description of the photo will appear immediately after the article's body. As we see the photo description, we are sure that the article is finished, so we can turn off the body switch to stop recording later lines as part of the article's body.
  # We could omit this "else if" claim at the "PHOTO" line and the next "else if" claim at the "Load-Date" line. I include this two to switch off the body recording as early as possible, so that information about photo and load date will not be included in the body of the article.
  else if (startsWith(line,"PHOTO")) {
    body <- FALSE
  } 
  # We are not sure if all articles contain a photo, so we turn off the body switch again when we see a line that starts with "Load-Date", just to make sure.
  else if (startsWith(line,"Load-Date")){
    body <- FALSE
  } 
  # Here, we are (1) turning off the body switch again just to make sure; (2) turning on the title switch because this is definitely the end of one article. Later lines will either be the next article or spaces between the two articles; and (3) adding our article count by 1 so that later information will be recorded onto the next row.
  else if (startsWith(line,"End of Document")){
    body <- FALSE
    title <- TRUE
    articleNo <- articleNo + 1
  }
  # If the body switch is on, we'll concatenate the line at the end of the body cell of the current article. This line is put at the end because, as mentioned above in the bold text - you should always put the "turn off the switch" statement before the "if the switch is on, do xxx" statement.
  else if (body == TRUE) {
    Narticles$Body[articleNo] <- paste(Narticles$Body[articleNo], line)
  } 
}

