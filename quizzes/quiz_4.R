#install.packages(c("dplyr","textdata","tm","RColorBrewer","wordcloud","wordcloud2","tidyverse","methods"))
library(methods)
#new method to load multiple packages
lapply(c("dplyr","textdata","tm","RColorBrewer","wordcloud","wordcloud2","tidyverse"),require, character.only=TRUE)


#define new class CHOL amd omotoa;ize CHOL object
setClass("CHOL",slots=list(Name="character",HDL="numeric"))
patientdata=new("CHOL",Name="Norm Niner",HDL=c(5.4,5.2,6.2))

#create method for object of class CHOL that prints avd value of HDL measurements
setGeneric("print.average",def=function(x){print(c("The average HDL mmol/L is",mean(x@HDL)))})
print.average(patientdata)

#demonstrate that generic function still works for another class containing HDL
setClass("CHOL2",slots=list(Name="character",HDL="numeric"))
patientdata=new("CHOL2",Name="Norm Niner",HDL=c(5.4,5.2,6.2))
print.average(patientdata)

#produce word cloud
data = dataset_ag_news()
text =  dplyr::filter(data,grepl('bird flu',description))
Docs = Corpus(VectorSource(text))
Docs = Docs %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace)
Docs = tm_map(Docs, content_transformer(tolower))
Docs = tm_map(Docs, removeWords,  stopwords("english"))
TDM = as.matrix(TermDocumentMatrix(Docs))
Tokens = sort(rowSums(TDM),decreasing=TRUE)
df = data.frame(token=names(Tokens),frequency=Tokens)
df = df[2:dim(df)[1],]
df = df[2:dim(df)[1],]
set.seed(3)
wordcloud(words = df$token,freq=df$frequency, min.freq=25,max.words=20,colors=brewer.pal(6,"Set1"))

#construct list called health.measurements and return the second diastolic bp measurement
LDL=c(4.2,4.7,5.3)
BP=data.frame(Systolic=c(170,180),Diastolic=c(100,110))
health.measurements=list(LDL,BP)
print(health.measurements[[2]][2,2])