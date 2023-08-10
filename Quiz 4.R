#Q1
install.packages("methods")
library(methods)
setClass("CHOL",slots=list(Name="character",HDL="numeric"))
patientdata=new("CHOL",Name="Norm Niner",HDL=c(5.4,5.2,6.2))

#Q2
setGeneric("print.average",def=function(x){print(c("The average HDL mmol/L is",mean(x@HDL)))})
print.average(patientdata)

#Q3
setClass("CHOL2",slots=list(Name="character",HDL="numeric"))
patientdata=new("CHOL2",Name="Norm Niner",HDL=c(5.4,5.2,6.2))
print.average(patientdata)

#Q4
FALSE

#Q5
FALSE

#Q6 - debug sqrt of abs value
a="7"
b = tryCatch(sqrt(abs(a)),error=function(e){"Not a number"})

#Q7 - mutate, can you create a new variable?
yes

#Q8
#install packages
install.packages(c("dplyr","textdata","tm","RColorBrewer","wordcloud","wordcloud2","tidyverse"))
lapply(c("dplyr","textdata","tm","RColorBrewer","wordcloud","wordcloud2","tidyverse"),require, character.only=TRUE)
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

#Q9 - return the second diastolic bp measurement
LDL=c(4.2,4.7,5.3)
BP=data.frame(Systolic=c(170,180),Diastolic=c(100,110))
health.measurements=list(LDL,BP)
print(health.measurements[[2]][2,2])

#Q10
FALSE