
#Summary about lexical style



#Text Analysis
library(quanteda)
library(quanteda.textplots)
library(readtext)

#Data Management
library(stringr)

#Visualization
library(ggplot2)
library(ggthemes)
library(ggrepel)



#Summary about lexical style

#What is "lexical style"? Lexical elements of style often occur at the word level. We can think of stylistic variations 
#that result from things like the addition, deletion, or substitution of words. 
#These variations can give rise to text that is characteristically different in terms of tone, formality, excitement etc.

#Tasks 

#Task 1: Load in Data and Turn into a corpus Object using quanteda
#Task 2: Data Management -- Pull out some data from our corpus that we can use for plotting and put into a dataset and put into a dataframe
#Task 3: Using the plot function to look at TTR over time
#Task 4: Examining the text for top words and lexical features that may distinguish speakers from one another apply a populist dictionary  
#Task 5: Using the plot function to look at populist terms over time

#Task 1: Load in Data and Turn into a corpus Object using quanteda


speeches <- readtext("*.txt")  #astrix mean take everything
speeches

speeches_corpus <- corpus(speeches) # Turn into a corpus, or collection of documents
speeches_corpus

as.character(speeches_corpus[4]) # Hillary Clinton

typeof(speeches_corpus)

summary(speeches_corpus) # overview information Unique works, tokens words or words, number of sentences

# Now its your turn 

## Let's extract some information we might be interested in how unique of vocab 

TTR <- ntype(speeches_corpus)/ntoken(speeches_corpus) # Type to token ratio 


#This measures the ratio of the number of different words (types) against the total number of words (tokens). The ratio is the number of types divided by the number of tokens.
#Traditionally, this measure has been considered important in evaluating the difficulty of a text. it is important to note, however, that 
#it is sensitive to different lengths of text. To deal with this we are going to subset the dataset and include speeches after 1950


#Now its your turn.

#Task 2: Data Management - Pull out some data from our corpus that we can use for plotting and put into a dataset and put into a dataframe

nms <- names(speeches_corpus) # names
nms

Dates <- as.numeric(gsub('[^[:digit:]]','', nms)) #gsub regular expressions
Dates

Party <- str_sub(nms, start= -5, -5) # fifth from end
Party

PNames  <- str_sub(nms, end = -12)
PNames

#Okay, so now we have extracted all of this information out of the filename, how about we organize it

info_summary <- data.frame(Dates = Dates, 
                           Party = Party, 
                           PNames = PNames, 
                           TTR = TTR) # Type to Token Ratio
head(info_summary)

#Now we want to subset our data by Date. We can use the following

info_summary <- info_summary[ which(info_summary$Dates >= 1950), ]
head(info_summary)
dim(info_summary)

# Now its your turn.

#Task 3: Using the plot function to look at TTR over time

sp <- ggplot(data=info_summary, aes(x=Dates, y= TTR, color=Party)) +
  geom_point() +
  theme_minimal()
sp

#But we see that the Dems are red and the Rep are blue, which is the opposite of what we would think so we want to change this

sp <- sp + scale_color_manual(values=c("blue", "red"))  
sp 

# maybe we want to add the names of the People so we can see who is who

sp <- sp + geom_label_repel(
  label=info_summary$PNames,
  position = position_dodge(width = 1.0))
sp

sp <- sp + theme(legend.position = "none")

sp <- sp + labs(x = "Year", y = "Type to Token Ratio")
sp

# So we can say something about share of unique words going down in our corpus

#Task 4: Examining the text for top words and lexical features that may distinguish speakers from one another 

#Let's see about populist language used by the speakers 

speeches_corpus_postwwar <- corpus_subset(speeches_corpus, Dates >= 1950) # Starting in 1995 
summary(speeches_corpus_postwwar)


tokens_postwar <- tokens(tolower(speeches_corpus_postwwar)) # tokens are characters of individual word and punctuation split into parts
# We are also going to make everything lowercase. This makes lower and upper case words equivalent 
tokens_postwar
postwar_dfm <- dfm(tokens_postwar) # Now we turn into a dfm or document feature matrix. Here our speeches are the rows, tokens are the columns, and the cell is the word frequency 
postwar_dfm

# Rather than keep all the words, we are interested in lexical style, so we are going to look at "populist speech" 

populism_dic <- dictionary(list(`(the) people` = c("people", "the people", "the peoples"),
                               `(the) public` = c( "public", "the public"), 
                               `(the) citizen(s)` = c("citizen","citizens", "the citizens" , "the citizen"),
                               `(the) voter(s)` = c("voter", "voters" ,"the voter", "the voters"),
                               `(the) resident(s)` = c("resident", "residents", "the resident", "the residents"),
                               `(the) population` = c("population", "populations", "the population", "the populations"),
                               `Anti-elitism` = c("undemocratic*", "caste", "consensus*","corrupt*", "dishonest", "elit*", "establish*", "deceit*", "lie*", "propagand*", "betray*", "shame*", "truth*"),
                               `People-centrism` = c("citizen*", "consumer*", "taxpayer*", "voter*", "people*"),
                                sum =c("people", "the people", "the peoples",
                                      "public", "the public",
                                      "citizen","citizens", "the citizens" , "the citizen",
                                      "voter", "voters" ,"the voter", "the voters",
                                      "resident", "residents", "the resident", "the residents",
                                      "undemocratic*", "caste", "consensus*","corrupt*", "dishonest", "elit*", "establish*", "deceit*", "lie*", "propagand*", "betray*", "shame*", "truth*",
                                      "population", "populations", "the population", "the populations")))

populism_dic
postwar_dfm_pop <- dfm_lookup(postwar_dfm, populism_dic, valuetype = "glob") 
postwar_dfm_pop
postwar_dfm_pop  <- as.data.frame(postwar_dfm_pop)
postwar_dfm_pop 



nms <- postwar_dfm_pop$doc_id # names
nms

postwar_dfm_pop$Dates <- as.numeric(gsub('[^[:digit:]]','', nms)) #gsub regular expressions
postwar_dfm_pop$Dates

postwar_dfm_pop$Party <- str_sub(nms, start= -5, -5) # fifth from end
postwar_dfm_pop$Party

postwar_dfm_pop$PNames  <- str_sub(nms, end = -12)
postwar_dfm_pop$PNames

head(postwar_dfm_pop)

#Task 5: Using the plot function to look at populist terms over time

sp <- ggplot(data = postwar_dfm_pop, aes(x = Dates, y = sum, color=Party)) + 
      geom_point() +
      theme_minimal()
sp


sp <- sp + scale_color_manual(values=c("blue", "red"))  #But we see that the Dems are red and the Rep are blue, which is the opposite of what we would think
sp

# maybe we want to add the names of the People so we can see who is who

sp <- sp + geom_label_repel(
  label=info_summary$PNames,
  position = position_dodge(width = 1.0))
sp

sp <- sp + theme(legend.position = "none")

sp <- sp + labs(x = "Year", y = "Number of Populist Terms")
sp

# According to this, we see that Dukakis is the most populist and that there is a slight increase in populist language for the Democracts and no 
#consistent pattern for the Republicans 


