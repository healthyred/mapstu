
#Modifying the text file to get the number of students coming into Williams per year per country/state into a readable csv file
#When creating the txt files, I removed page numbers and the US term, and International Term
#the text file must have the format (Country Value) and not separated by a new line
readDatayears <- function(dataset){
  input <- readLines(dataset, warn = FALSE)
  #targets the entire string that we want w/ 3 cases where countries have up to 5 words in their name
  #regone= "[a-zA-Z]+ [0-9]+"
  #regtwo= "[a-zA-Z]* [a-zA-Z]+ [0-9]+"
  #regthree= "[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+"
  m <- regexpr("[a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]*[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+" , input)
  input <- regmatches(input, m)
  #take out all blank spaces
  input <- gsub(" ","",input)
  #put a comma in between every letter and number
  input <- gsub("([a-z])([0-9])", "\\1, \\2", input)
  #puts a space between each lower case and capital letter
  #not yet sure how to fix cases of Districtof and Trinidadand
  input <- gsub("([a-z])([A-Z])", "\\1 \\2", input)
}
input
#Read years is a funtion that is a more general adaption of readDatayears and will not use all of the regex that was 
#used to sort through most of the dataset
readYears <- function(dataset, name){
  input <- readLines(dataset, warn = FALSE)
  #takes out all of the excess periods in every dataset
  input <- gsub("[.]","",input)
  #takes out all of the spaces in each dataset
  input <- gsub(" ","",input)
  #Inserts a comma between the country and its value
  input <- gsub("([a-z])([0-9])", "\\1, \\2", input)
  #Puts spaces between every start of a new word
  input <- gsub("([a-z])([A-Z])", "\\1 \\2", input)
  #Puts spaces in between the values in the words
  input <- gsub("([0-9])([A-Z])", "\\1 \\2", input)
  write(input, file = name)
}

#test code
#input <- readLines("~/HutchinHill/2000-2001.rtf")
#m <- regexpr("[a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]*[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+" , input)
#input <- regmatches(input, m)
