
#Modifying the text file to get the number of students coming into Williams per year per country/state into a readable csv file

readDatayears <- function(dataset){
  input <- readLines(dataset, warn = FALSE)
  #targets the entire string that we want w/ 3 cases where countries have 1, 2, or 3 names
  regone= "[a-zA-Z]+ [0-9]+"
  regtwo= "[a-zA-Z]* [a-zA-Z]+ [0-9]+"
  regthree= "[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+"
  m <- regexpr("[a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+" , input)
  input <- regmatches(input, m)
  #puts in a comma between each blank space
  for(i in input){
    if (gsub(regone,"", i == ""){
      print(input[i])
    }
  }
        input[i] <- gsub(" ", ",", input)
    }
  }
    else if
  }
}
input
#reset code
input <- readLines("~/HutchinHill/2000-2001.rtf")
m <- regexpr("[a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]+ [0-9]+|[a-zA-Z]* [a-zA-Z]* [a-zA-Z]+ [0-9]+" , input)
input <- regmatches(input, m)
