# Write Up

## Overview  

NLP applications are everywhere, sometimes we interact directly with them, for example when using an online-translators or for text-completion, other times they work in the background, such as spam detection. As a field of study, Natural Language Processing (NLP) lies at the intersection between linguistics and data science. Linguistics provide the foundational base of language, the grammar rules and patterns that dictate the use of a language, while data science provides the tools and techniques needed to explore large corpuses in order to extract insights from them. These insights can help enrich our understanding of language, its use, and how it reflects on the human condition, what is known as Culturomic analysis. For example, the 2013 paper by Acerbi et al. [1] explored the expression of emotions throughout the 20<sup>th</sup> century using sentiment analysis. Another application of NLP is to make predictions, for example, in 2011 researchers from Indiana University and the University of Manchester were able to make predictions regarding the DJIA closing values using public mood states extracted from Twitter text [2]. Other areas of NLP research, such as text generation and composition, have also seen major advances, and today we are seeing AI systems that are able to generate song lyrics [3] as well as compose short stories [4].   

In this report, I will use NLP to analyze four novels by the Russian author Fyodor Dostoevsky that span the entirety of his career. Specifically, I will apply topic modelling in order to extract the main themes and motifs in each of these works, and see if there are any recurrent ideas. I will also use sentiment analysis to capture the stories' progression, in order to create visual plots that describe the overall arc of the story. My goal here is to gain better understanding of these works by exploring the tropes the author used and the themes he explored. The results show that the author was concerned with topics relating to religion, philosophy, and human psychology. The results also shed some light on his development, showing how his own personal experiences might have impacted him as a writer. Such analysis can provide valuable insights that can be applied for predictive tasks as well (although that is out of the scope of this particular study). For example, the identified topics and sentiment progression can be used as features for recommender systems, something explored by Bhowmick et al [5].  

## Data and Process  

### Data Source  

I used data from the Gutenberg project [6], a large online repository for e-books. The text data of these books is accessible through the gutenbergr R package [7], developed and maintained by rOpenSci. The package provides several functions to navigate the data, and the ```gutenberg_download()``` function to download the text data (you will need the text ID in order to download any specific book).

Below, I provide a small plot summary for the four novels I will be analyzing:

**Poor Folk, 1845 [8]**: This was Dostoevsky's first published work. It tells the story of two relatives, Varvara Dobroselova and Makar Devushkin, through the letters they exchanged over the span of a 5 months period. The letters describe their living conditions, which are extremely tough as a result of their poverty. They also talk about their lives and interests, and they discuss different books which they exchange with one another. Makar starts developing feelings for Varvara, and acts on these feelings by sending Varvara gifts and money to help her. At work, his boss takes pity on him and lends a sum of money. Makar starts thinking about living with Varvara, however, she informs her that she has met a wealthy man whom she is going to marry. The story ends with Makar sending several letters that go unanswered.

**Notes from the Underground, 1864 [9]**: The novel is divided into two parts. In part one, the protagonist (who remains unnamed throughout the story) is talking directly to the reader, explaining his ideas about the meaning of 'suffering' and his internal struggles between reason and will. In part two, the narrator tells a story that took place several years back. It describes a dinner party he goes to, where he and several of his acquaintances are gathered to send off one of their school colleagues. We see how his dignity suffers from the apparent discrepancy between his living conditions and that of his friends. Later on, he goes to a brothel where he meets a young prostitute. He challenges her world view, and she appears to accept the fallacies of her ideas. The next day, she pays him a visit at his house, but, due to his capricious nature, instead of welcoming her, he admonishes her, and she eventually leaves. 

**Crime and Punishment, 1866 [10]**: Rodion Raskolnikov is a young law student who lives in absolute squalor. His conditions are so tough that he can no longer pay his school fees. For a while he has been contemplating the idea of killing an old pawn broker lady and stealing her money. In his mind, he sees the this act not as a heinous crime, since the old lady is not loved by anyone, but as a means to set himself on a path to fulfill his potential, and eventually help humanity. At last he does commit the murder, but is confronted by pangs of guilt that torment him. Eventually, he turns himself to the police and accepts his fate.

**The Brothers Karamazov, 1880 [11]**: This novel tells the story of the Karamazov family. The story starts with an ongoing feud between the father, Fyodor, and the eldest son, Dmitri, over the son's inheritence. The two younger brothers, Ivan (middle), a brilliant intellectual, and Alexei (youngest), who is a novice at an Orthodox monastry, try to reconciliate between them. Soon afterwards, we learn about a love triangle involving Fyodor, Dmitri, and a local beauty, which adds further strain to the already trouble relationship between father and son. While Alexei earnstly tries to resolve the problems within his family, Ivan is more aloof, and does not really try reconcile between his father and his older brother. Alexei and Ivan represent two opposing ideologies, one symblizes Christian love, while the other rejects religion, and is more inclined to rationalism and even nihilism. A fourth brother, although one who is of illegitimate birth, is influcend by Ivan's ideas and kills the father. Dmitri is convicted for the murder, based on their well documented feud. Ivan learns the truth, and is tormented by guilt, and even announces in the courtroom that he is the one responsible for father's murder, but one takes him at his word. The story ends with the two younger brothers devising an escape plan for their older brother. 

### Process   

##### *I- Load the libraries*   

I start the analysis by loading the relevant libraries.

##### *II- Utility Functions*  

First, I define a number of utility functions that I will be using throughout the analysis. The first function ```basic.cleaning()``` performs the following preprocessing steps:  
* Sets all text to lower case
* Handles contractions
* Removes stopwords
* Removes characters names - the names of the characters often occur a lot throughout text, so they dominate any analysis applied to the text, therefore it is better to remove them    
* Removes punctuation  
* Removes numbers  
* Removes symbols  
* Removes special characters  
* Removes extra space  

Next, I define some plotting utilities:  
1- ```my_colors```: A list of colors that will be used throughtout the project   
2- ```theme_dostoevsky```: A function that defines theme components   
3- ```word_chart```: A function that generates a table-like plot. I will use this function to show top words per topic for the LDA analysis

##### *III- Data Loading*  

As mentioned before, I use the ```gutenberg_download()``` function to download the raw texts. Three of the 4 texts are divided into parts, where each part is split into a number of chapters. I use the ```stringi::stri_split_regex``` function to split the text into parts and chapters. I also add some metadata to each text, such as the year the book was published and the book's title name. The data.frame object ```dostoevsky``` combines all the texts together.

The data.frame ```dostoevsky_M``` the text is further split on the sentence level. I use the function ```get_sentences()``` from the syuzhet package to split the text into sentences. A quick word about he syuzhet function:  

Developed by Matthew Jockers [12, 13], the package provides a number utilities to split text and calculate the sentiment value (it uses a lexicon to do so; you can pick one of four available lexicons, syuzhet, bing, afinn, and nrc). The package also provides the ```get_dct_transform()``` function, which calculates a rolling value discretizes the input into a standardized output to make it easy to plot sentiment and compare different plots.

Next, I clean the text using the ```basic.cleaning()``` function. This is followed by POS annotation using cleanNLP's ```cnlp_init_udpipe()``` and ```cnlp_annotate()```. This steps achieves two things:  

1- It tokenizes the sentences into words  
2- It lemmatizes each word, making it possible to dispense with the stem-complete step  

An important thing to note here, some words become stopwords when lemmatized, so it is important to clean the output of the ```cnlp_annotate()``` function. The data.frame object ```dostoevsky_Words``` holds the words and lemmas of the entire corpus.

##### *IV- Word Clouds*  

Here, I generate wordclouds using ```wordcloud2()``` function from the wordcloud2 package. I use the ```dplyr::count()``` function to get the number of occurances per lemma (I use the lemma instead of the original word), and show the top 300 words by count in the wordcloud.

<center>

![alt text](./pictures/wordcloud_overall.png)   
*wordcloud for all 4 novels*  

![alt text](./pictures/wordcloud_poor.png)   
*wordcloud for Poor Folk*  

![alt text](./pictures/wordcloud_notes.png)   
*wordcloud for Notes from the Underground*  

![alt text](./pictures/wordcloud_crime.png)   
*wordcloud for Crime and Punishment*  

![alt text](./pictures/wordcloud_brothers.png)   
*wordcloud for The Brothers Karamazov*  

</center>  

##### *V- Word Frequency*   

The following plot is a flipped bar chart showing the top 10 words per novel. It was created using ```geom_col()``` and ```coord_flip()```.  

<center>

![alt text](./pictures/word_freq.png)   
*word frequency for all 4 novels*  

</center>

##### *VI- Topics per Part (TF-IDF)*   

Next, I use tidytext's ```bind_tf_idf()``` to calculate the TF-IDF value. TF-IDF highlights words that appear frequently in a particular document, but doesn't appear a lot throughout the entire corpus, the assumption here is that this word is relevant to this document. I apply the ```bind_tf_idf()``` on the 'part' level, and not on the entire level. In other words, I'm trying to identify the most salient words per part, and not per the entire novel.

<center>

![alt text](./pictures/topics_tfidf.png)
*TF-IDF per Part per Novel* 

</center>

##### *VII- Topic Modelling (LDA)*  

Another method to identify salient words is LDA. However, it is important to make the distinction here between TF-IDF and LDA. The salient words in LDA are part of a "topic", and not a part of a document like in TF-IDF. Essentially, LDA is an unsupervised modele that takes as input a document-term matrix (where each row is a document, and each column is a word), and a parameter k (number of topics to be identified from the DTM). LDA's output will be the most important words per topic. We can also identify the topic constitution of each document. I used the package topicmodels, specifically the```LDA()``` function, to build the model. The parameters I set were: k (number of topics)=4, method=GIBBS. The below figure shows the most important words per topic according to our LDA model:

<center>  

![alt text](./pictures/lda_topics.png)  
*LDA*  

</center>  

##### *VIII- Source-Theme Relationships*  

Using the LDA model we built in the previous step, I created a chrod-diagram that shows the makeup of each document (novel) in terms of topics. The chord-diagram was created using the circlize package, and the ```chordDiagram()``` function:

<center>  

![alt text](./pictures/chord_diagram.png)  
*Chord Diagram*  

</center>

##### *IX- Word Networks*  

A- Story Connections  

Next, I look at connections between the novels, using a network graph diagram. Here, I've selected the top words per topic per novel, and connected the words that appeared in common:

<center>  

![alt text](./pictures/topic_connections.png)  
*Story Connections*  

</center>  

B- Bigrams  

This next graph plot shows which words appear together. First, I had to create a bigram matrix, which I then used to draw this graph. For both this plot and the previous one I used the packages igraph & ggraph, and the functions ```graph_from_data_frame()``` and ```ggraph()```. Another important factor here is that I speicifed a handful of words to see their networks. I chose to do this to examine if there are any relationships between some of the recurrent themes in Dostoevsky's novels.  

<center>  

![alt text](./pictures/bigrams.png)  
*Bigram Connections*  

</center>  

##### *X- Plot Analysis (Syuzhet)*  

A- Sample  

Here, I plot the story arc using the syuzhet package. The first step is to get the sentiment values for every sentence. This is achieved using the ```get_sentiment()```, which takes as input a text vector and a lexicon (optional, default set to 'syuzhet'). Next, we pass these sentiment values to the ```get_dct_transform()``` function to smoothen them out, otherwise the sentiments will cancel each other as show in the top figure in the below plot. Another important feature of the ```get_dct_transform()``` function is the lps parameter, which determines the number of components to be retained (the lower the value, the smoother the curve).

<center>  

![alt text](./pictures/crime.png)  
*Plot Analysis - Sample*  

</center>

B- Story Arcs  
  
The plots for all 4 novels:  

<center>

![alt text](./pictures/all_plots.png)             
*Plot Analysis - Sample*

</center>


##### *XI- Character Arc*  

To plot these arcs, I extracted all the sentences that had the character's name (including the sentences that mentioned the name in a diminutive form). Then I went through the same steps as before, that is, I retreived the sentiment values, then applied the transformation function ```get_dct_transfrom()``` on them, and finally, plotted the output:  

<center>  

![alt text](./pictures/character_arc.png)  
*Character Arc*  

</center>  


##### *XII- Sentiment Plots*  

To construct these sentiment plot, I used teh 'NRC' lexicon, which provides valence values for eight emotions (not just a positive/negative value). Another thing I had to do here was handling the different lengths of the novels. To do that,  I calculated the rolling mean for each of the emotions usina a rolling window size equal to 10% of the length of the story. Then, I used the ```rescale_x_2()``` function to normalize the length of the story and the rolled mean values to a scale between 0 and 1, in the end all outputs will share the same scale, and can therefore be plotted on the same plot.  

<center>  

![alt text](./pictures/sentiment_arc.png)  
*Sentiment Comparison*  

</center>  


##### *XIII- Word-Sentiment Matrix*  

The last plot was constructed by joining the associated 'NRC' emotions with the most frequent words per novel.

<center>  

![alt text](./pictures/word_sentiment_matrix.png)  
*Word-Sentiment Matrix*  

</center>  

## Conclusion  

The different plots pass the eye test in terms of identifying the main topics of each of the novels. For example, the word cloud and the word frequency bar chart for 'Poor Folk' identify the word "dearest" as one of the top 10 words in the text, which is logical given how the novel is constructed as a series of correspondences between the two relatives, and the vast majority of these letters start with the word as they address each other. Things get more interesting when we introduce TF-IDF, for example, in Crime and Punishment part 1, the words with the highest TF-IDF scores are "mare" and "axe", the first alludes to the nightmare the protagonist has in chapter 5, which is in itself an allusion to the crime he is about to commit; the second ("axe") refers to the weapon of choice. The LDA model provides an even more holistic view of the topics discussed in each of the novels. Take "The Brothers Karamazov", it is mostly associated with topic 3 and topic 1; topic 3 is made up of words like "father", "love", "cry" and "god", all words relevant to the novel, on the other hand topic 1 is made up of words like "begin", "room", "suddenly", "woman", which are also relevant to the story. These connections are illustrated in the chord diagram and the first network graph. 

When it comes to the plot lines, the plots show an evolution over time. In his first published work, the arc of the story is positive for the most part, however this changes in the other three stories, where the arc is mostly on the negative side. It is worth noting here that that these three stories were written after the author's exile. Another important aspect of Dostoevsky's plot shapes is the cascading property, as we can see in the plots of "Crime and Punishment" and "The Brothers Karamazov". The story arc is taking a negative turn, but the emotions fluctuate through the story, reflecting the characters' feelings through different events.

Finally, with regard to the sentiment expression in the novels, what we can see is that "Crime and Punishment" is the most emotive of the four novels, as illustrated in the grid sentiment plots, whereas the lines of the other novels fluctuate between high and low emotive states. 

## References  

[1] Acerbi A, Lampos V, Garnett P, Bentley RA (2013) The Expression of Emotions in 20th Century Books. PLoS ONE 8(3): e59030. doi:10.1371/journal.pone.0059030 https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0059030&type=printable  
[2] Bollen J, Mao H, Zeng X-J (2011) Twitter mood predicts the stock market.Journal of Computational Science 2: 1–8. https://www.sciencedirect.com/science/article/abs/pii/S187775031100007X  
[3] Titlow, John Paul (2017) “This new AI-composed pop song sounds like something from a Spotify playlist”. FactCompany https://www.fastcompany.com/40455600/this-new-ai-composed-pop-song-sounds-like-something-from-a-spotify-playlist   
[4] Brooke Bottoni, Yasmine Moolenaar, Anthony Hevia, Thomas Anchor, Kyle Benko, Rainer Knauf, Klaus Jantke, Avelino Gonzalez, Annie Wu (2020) Character Depth and Sentence Diversification in Automated Narrative Generation   http://www.cs.ucf.edu/~ecl/papers/2005.flairs.gonzalez.pdf  
[5] Abhishek Bhowmick, Udbhav Prasad, Satwik Kottur. Movie Recommendation based on CollaborativeTopic Modeling https://satwikkottur.github.io/reports/F14-ML-Report.pdf  
[6] https://www.gutenberg.org/  
[7] https://ropensci.org/tutorials/gutenbergr_tutorial/   
[8] https://en.wikipedia.org/wiki/Poor_Folk  
[9] https://en.wikipedia.org/wiki/Notes_from_Underground  
[10] https://en.wikipedia.org/wiki/Crime_and_Punishment  
[11] https://en.wikipedia.org/wiki/The_Brothers_Karamazov  
[12] Jockers, M. Revealing Sentiment and Plot Arcs with the Syuzhet Package - Blog post. www.matthewjockers.net/2015/02/02/syuzhet/    
[13] https://github.com/mjockers/syuzhet    
[14] https://youtu.be/1ZFkhmSTqZI  

