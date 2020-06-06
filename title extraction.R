#Utility function for extracting title
extracttitle=function(Name) {
  if(length(grep("Miss.", Name))>0) {
    return("Miss.")
  } else if(length(grep("Master.", Name))>0) {
    return("Master.")
  } else if(length(grep("Mrs.", Name))>0) {
    return("Mrs.")
  } else if(length(grep("Mr.", Name))>0) {
    return("Mr.")
  } else {
    return("Others")
  }
}


Title=NULL
for(i in 1:nrow(Titan)) {
  Title=c(Title, extracttitle(Titan[i, "Name"]))
}
Titan$Title=as.factor(Title)