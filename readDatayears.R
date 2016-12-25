
#Modifying the text file to get the number of students coming into Williams per year per country/state into a readable csv file
#the text file must have the format (Country Value) and not separated by a new line
readDatayears <- function(dataset){
  input <- readLines(dataset, warn = FALSE)
  #targets the entire string that we want w/ 3 cases where countries have 1, 2, or 3 names
  #regone= "[a-zA-Z]+ [0-9]+"
  #regtwo= "[a-zA-Z]* [a-zA-Z]+ [0-9]+"
  #regthree= "[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+"
  m <- regexpr("[a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+" , input)
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
#reset code
input <- readLines("~/HutchinHill/2000-2001.rtf")
m <- regexpr("[a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+" , input)
input <- regmatches(input, m)
