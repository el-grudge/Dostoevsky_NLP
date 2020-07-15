get_sentiments(lexicon = c("bing", "afinn", "nrc"))
get_sentiments(lexicon = c("bing", "afinn", "loughran", "nrc"))

Mood Ring
Real-Time Sentiment
The Black Album: 1994 - 1996
This Time It's Personal: 1998
On The Radar: Radar Charts
Sign O' The Times
More Songs
Bigrams Per Decade
Sentiment with Bigrams
Pairwise Comparisons

not_words <- bigrams_separated %>%
bigrams_separated <- prince_bigrams %>%
prince_bigrams <- prince_data %>%


WHY?
        Sentiment analysis descriptive power (acerbi et all)
        Predictive power (stoke prediction through tweet stoking)

WHAT?
        FYODOR DOSTOEVSKY THROUGH TIME
        Literary style
        Plot (Shuyzhet)

HOW?
        Shuyzhet - plot by chapter
        Style - topics, modes, (part of speech)
	will need 2 datasets, one with the chapters intact, another with words only
	find chapters by topic (use example: Tokenized Format Example)

WHEN?

Debbie Liske's Tutorial:
Part 1- Text Mining and Exploratory Analysis

        Load the libraries [DONE]
        Read in the data [DONE]
        Data Conditioning [DONE]
        Basic cleaning [DONE]
        Add a few fields [DONE]
        Descriptive Statistics [DONE]
        Song Stats [DONE]
        Insights [DONE]
        No. 1 Songs! [DONE]
        Text Mining
        Tidy Text Format
        Data Formats and Tokenization
        Word Frequency
        Top Words
        Word Clouds
        Popular Words
        Timeless Words
        Word Length
        Lexical Diversity
        Lexical Density
        TF-IDF

Part 2- Sentiment Analysis and Topic Modeling with NLP

   A- Sentiment Analysis and Topic Modeling with NLP
        Introduction
        Triplets: Three-Part Summary
        Breaking Up Is Hard To Do
        Prerequisites
        Sentiment Analysis Overview
        Prepare Your Questions!
        Prep Work
        Libraries and Functions
        Sushi Data
        Good Clean Fun: prince_tidy
        Descriptive Statistics
        Shipshape: Word Count Per Song
        All Year Round: Song Count Per Year
        Chords: Charted Songs By Decade
        Lexicons and Lyrics
        Explore Sentiment Lexicons
        Match Dot Common
        Don't Take My Word For It
        Word Forms
        More Data Preparation?
        Detailed Analysis
        Create Sentiment Datasets
        In The Mood: Overall Sentiment
        Crank It Up: Chart Level
        Polar Melting: So Blue
        Mood Ring
        Real-Time Sentiment
        The Black Album: 1994 - 1996
        This Time It's Personal: 1998
        On The Radar: Radar Charts
        Sign O' The Times
        More Songs
        Bigrams Per Decade
        Sentiment with Bigrams
        Pairwise Comparisons

    B- Machine Learning and NLP using R: Topic Modeling and Music Classification
	Just For Fun! [DONE]
	Libraries and Functions [DONE]
	Music and Books [DONE]
	Get The Data [DONE]
	Building Models with LDA [DONE]
	Model One: LDA for Three Writers [DONE]
	Examine The Data [DONE]
	Create the Document-Term Matrix [DONE]
	Set Variables [DONE]
	Fit the Model [DONE]
	Identify Themes with Top Words [DONE]
	Classify Documents [DONE]
	Chord Diagram [DONE]
	Top Documents Per Topic [DONE]
	Identify Artists/Authors [DONE]
	Model Two: K-Means for Three Writers [DONE]
	Set Variables and Fit The Model


R Files:
1- libraries.R
2- data_loading.R
3- plot_analysis.R
4- topic_analysis.R


Ideas:
- plot_analysis
- topic analysis (topics per 4 works, topics per book) (circular plot)
- song recommendations based on topics (use kaggle lyrics dataset)
- short book recommendations based on plots (use gutenberg documentation/tutorial) (distance similarity between 2 syuzhets)
- plot-topic graph
