Statistical analysis of emotional valence of perceptual words
=============

-	Bodo winter
-	Repository created July 24, 2017

## Libraries required for this analysis:

-	the following "tidyverse" packages: dplyr, ggplot2, readr, magrittr, tidyr
-	tidytext
-	stringr
-	lme4

## Script files contained in this analysis:

1.	**functions.R**<br>
	Contains plotting function and prediction function for mixed models
2.	**COCA.R**<br>
	Re-analysis of Winter (2016) adj-noun pairs in the Corpus of Contemporary American English
3.	**yelp.R**<br>
	Analysis of Yelp review data
4.	**pang.R**<br>
	Analysis of Pang & Lee (2004) movie review data
5.	**bagli.R**<br>
	Re-analysis of Bagli (2016)'s Shakespeare metaphors

## Data files contained in this analysis:

-	lynott_connell_2009_adj_norms.csv<br>
	Word list of sensory terms; modality norms by Lynott & Connell (2009)
-	warriner_2013_affective_norms.csv<br>
	Valence norms by Warriner et al. (2013)
-	SUBTLEX_US_POS.csv<br>
	Words with their part-of-speech tags, Brysbaert, New & Keuleers (2012)
-	COCA_adj_noun_pairs.csv<br>
	Adjective-noun pairs from COCA, from Winter (2016)
-	yelp_all_sensory_adjectives.csv<br>
	All sensory adjectives from the Yelp challenge dataset
-	pang_lee_2004_objective.txt<br>
	5,000 objective sentences from imdb.com, Pang & Lee (2004) 
-	pang_lee_2004_subjective.txt<br>
	5,000 subjective sentences from rottentomatoes.com, Pang & Lee (2004) 
-	bagli_2016_table.csv<br>
	Table of metaphor counts in five works of Shakespeare from Bagli (2016)

