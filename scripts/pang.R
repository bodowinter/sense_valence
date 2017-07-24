## Bodo Winter
## July 30, 2016
## Analysis of Pang & Lee (2004) objective/subjective review data with Lynott & Connell (2009) norms
## Earlier version presented in my PhD thesis (Winter, 2016)

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Libraries:

library(tidyverse)
library(tidytext)
library(effsize)

## Load in data, Lynott and Connell (2009) adjective norms:

mainPath <- '/Users/winterb/Research/senses_sensory_modalities/review_valence/analysis/data/'
setwd(mainPath)
lyn <- read_csv('lynott_connell_2009_adj_norms.csv')

## Get rid of unnecessary Lynott & Connell columns:

lyn <- lyn %>% select(Word:OlfactoryStrengthMean, ModalityExclusivity)

## Load in data, Pang & Lee (2004) objective and subjective sentences:

obj <- read_lines('pang_lee_2004_objective.txt')
subj <- read_lines('pang_lee_2004_subjective.txt')

## Randomly chosen subjective and objective sentences:

set.seed(42)
sample(obj, 5)
sample(subj, 5)

## Put these into a dataframe:

pang_all <- tibble(IDs = 1:10000, Text = c(obj, subj))

## Tokenize:

pang <- pang_all %>% unnest_tokens(Word, Text)
nrow(pang)	# 215,629 word tokens

## Add objectivity information:

pang <- mutate(pang,
	Objectivity = ifelse(IDs <= 5000, 'objective', 'subjective'))

## Get rid of stop words:

# pang <- pang %>% anti_join(stop_words)
# nrow(pang)	# 97,942

## Get rid of 'alcoholic' because it is only used nominally in the dataset (which is not POS tagged):

pang <- pang %>% filter(Word != 'alcoholic')

## Count words per ID (we need this as a baseline for analysis later):

pcount <- pang %>% group_by(IDs, Objectivity) %>% count()

## Are there overall more words for subjective or objective reviews?

pcount %>% group_by(Objectivity) %>% count()	# almost exactly the same
# this means we don't have to worry about normalizing for word count too much

## Merge with Lynott & Connell (2009) norms:

plyn <- pang %>% inner_join(lyn)		# since they are all approximately th 



##------------------------------------------------------------------
## Inferential stats, continuous perceptual strength values:
##------------------------------------------------------------------

## Get aggregate perceptual strengths for each modality by objectivity:

pagr <- plyn %>% group_by(IDs, Objectivity) %>% 
	summarise(Vis = mean(VisualStrengthMean),
		Aud = mean(AuditoryStrengthMean),
		Hap = mean(HapticStrengthMean),
		Gus = mean(GustatoryStrengthMean),
		Olf = mean(OlfactoryStrengthMean))

## Get objective and subjective subsets:

pagr_obj <- pagr %>% filter(Objectivity == 'objective')
pagr_subj <- pagr %>% filter(Objectivity == 'subjective')

## Perform t-tests:

(vis.t <- t.test(pagr_obj$Vis, pagr_subj$Vis,
	paired = F, var.equal = T))
(aud.t <- t.test(pagr_obj$Aud, pagr_subj$Aud,
	paired = F, var.equal = T))
(hap.t <- t.test(pagr_obj$Hap, pagr_subj$Hap,
	paired = F, var.equal = T))
(gus.t <- t.test(pagr_obj$Gus, pagr_subj$Gus,
	paired = F, var.equal = T))
(olf.t <- t.test(pagr_obj$Olf, pagr_subj$Olf,
	paired = F, var.equal = T))

## Perform multiple comparisons corrections for k = 5 tests using Dunn-Sidak approach:

dunnsidak <- function(P, N) 1 - ((1 - P) ^ N)
dunnsidak(vis.t$p.val, 5)	# sig
dunnsidak(aud.t$p.val, 5)
dunnsidak(hap.t$p.val, 5)
dunnsidak(gus.t$p.val, 5)	# sig
dunnsidak(olf.t$p.val, 5)

## Calculate Cohen's d:

cohen.d(pagr_obj$Vis, pagr_subj$Vis)
cohen.d(pagr_obj$Aud, pagr_subj$Aud)
cohen.d(pagr_obj$Hap, pagr_subj$Hap)
cohen.d(pagr_obj$Gus, pagr_subj$Gus)
cohen.d(pagr_obj$Olf, pagr_subj$Olf)

## Get a list of all the t-test objects in order that I want to display them:

models <- c('gus.t', 'olf.t', 'hap.t', 'aud.t', 'vis.t')

## Get list of names of those modalities to display:

modalities <- c('Taste', 'Smell', 'Touch', 'Sound', 'Sight')

## Create a publication-ready plot of the differences with confidence intervals:

quartz('', 10, 5)
par(mai = c(0.5, 2, 0.5, 0.25))
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n',
	xlim = c(0.5, 5.5), ylim = c(-0.5, 0.5))
## Axes and labels:
abline(h = 0, lty = 2, lwd = 2)
axis(side = 2, at = seq(-0.5, 0.5, 0.25), lwd = 4, lwd.ticks = 4, labels = F)
axis(side = 2, at = seq(-0.5, 0.5, 0.25), tick = F, las = 2, font = 2, cex.axis = 1.25, line = 0.2)
mtext('Perceptual Strength Difference', side = 2, font = 2, line = 7.5, cex = 2)
mtext('(Objective - Subjective)', side = 2, font = 2, line = 5.5, cex = 1.45)
## Add error bars and data points:
for (i in 1:5) {
	this.t <- get(models[i])
	arrows(x0 = i, y0 = this.t$conf.int[1], y1 = this.t$conf.int[2],
		angle = 90, code = 3, length = 0.1, lwd = 2)
	ypos <- diff(this.t$estimate) * -1
	rect(xleft = i - 0.3, xright = i + 0.3,
		ybottom = ypos - 0.035, ytop = ypos + 0.04,
		border = 'black', col = 'white', lwd = 2)
	text(x = i, y = ypos,
		cex = 1.5, font = 2, labels = modalities[i])
	# points(x = i, y = diff(this.t$estimate) * -1, pch = 15, cex = 1.5)
	}
## Add significance stars:
text(x = 1, y = gus.t$conf.int[2] + 0.05, labels = '*', font = 2, cex = 3)
text(x = 5, y = vis.t$conf.int[2] + 0.05, labels = '*', font = 2, cex = 3)

## Do this again with the more exclusive ones:

## Get objective and subjective subsets:

pagr <- plyn %>% group_by(IDs, Objectivity) %>% 
	filter(ModalityExclusivity > median(ModalityExclusivity)) %>%	# new
	summarise(Vis = mean(VisualStrengthMean),
		Aud = mean(AuditoryStrengthMean),
		Hap = mean(HapticStrengthMean),
		Gus = mean(GustatoryStrengthMean),
		Olf = mean(OlfactoryStrengthMean))
pagr_obj <- pagr %>% filter(Objectivity == 'objective')
pagr_subj <- pagr %>% filter(Objectivity == 'subjective')

## Perform t-tests:

(vis.t <- t.test(pagr_obj$Vis, pagr_subj$Vis,
	paired = F, var.equal = T))
(aud.t <- t.test(pagr_obj$Aud, pagr_subj$Aud,
	paired = F, var.equal = T))
(hap.t <- t.test(pagr_obj$Hap, pagr_subj$Hap,
	paired = F, var.equal = T))
(gus.t <- t.test(pagr_obj$Gus, pagr_subj$Gus,
	paired = F, var.equal = T))
(olf.t <- t.test(pagr_obj$Olf, pagr_subj$Olf,
	paired = F, var.equal = T))

## Perform multiple comparisons corrections for k = 5 tests using Dunn-Sidak approach:

dunnsidak <- function(P, N) 1 - ((1 - P) ^ N)
dunnsidak(vis.t$p.val, 5)	# sig
dunnsidak(aud.t$p.val, 5)
dunnsidak(hap.t$p.val, 5)
dunnsidak(gus.t$p.val, 5)	# sig
dunnsidak(olf.t$p.val, 5)

## Calculate Cohen's d:

cohen.d(pagr_obj$Vis, pagr_subj$Vis)
cohen.d(pagr_obj$Aud, pagr_subj$Aud)
cohen.d(pagr_obj$Hap, pagr_subj$Hap)
cohen.d(pagr_obj$Gus, pagr_subj$Gus)
cohen.d(pagr_obj$Olf, pagr_subj$Olf)

## Get a list of all the t-test objects in order that I want to display them:

models <- c('gus.t', 'olf.t', 'hap.t', 'aud.t', 'vis.t')

## Get list of names of those modalities to display:

modalities <- c('Taste', 'Smell', 'Touch', 'Sound', 'Sight')

## Create a publication-ready plot of the differences with confidence intervals:

quartz('', 10, 5)
par(mai = c(0.5, 2, 0.5, 0.25))
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n',
	xlim = c(0.5, 5.5), ylim = c(-0.7, 0.6))
## Axes and labels:
abline(h = 0, lty = 2, lwd = 2)
axis(side = 2, at = seq(-0.5, 0.5, 0.25), lwd = 4, lwd.ticks = 4, labels = F)
axis(side = 2, at = seq(-0.5, 0.5, 0.25), tick = F, las = 2, font = 2, cex.axis = 1.25, line = 0.2)
mtext('Perceptual Strength Difference', side = 2, font = 2, line = 7.5, cex = 2)
mtext('(Objective - Subjective)', side = 2, font = 2, line = 5.5, cex = 1.45)
## Add error bars and data points:
for (i in 1:5) {
	this.t <- get(models[i])
	arrows(x0 = i, y0 = this.t$conf.int[1], y1 = this.t$conf.int[2],
		angle = 90, code = 3, length = 0.1, lwd = 2)
	ypos <- diff(this.t$estimate) * -1
	rect(xleft = i - 0.3, xright = i + 0.3,
		ybottom = ypos - 0.035, ytop = ypos + 0.04,
		border = 'black', col = 'white', lwd = 2)
	text(x = i, y = ypos,
		cex = 1.35, font = 2, labels = modalities[i])
	# points(x = i, y = diff(this.t$estimate) * -1, pch = 15, cex = 1.5)
	}
## Add significance stars:
text(x = 1, y = gus.t$conf.int[2] + 0.05, labels = '*', font = 2, cex = 3)
text(x = 5, y = vis.t$conf.int[2] + 0.05, labels = '*', font = 2, cex = 3)




##------------------------------------------------------------------
## Table and inferential stats on token counts:
##------------------------------------------------------------------

## Get token counts per modality:

lyn_words <- plyn %>%
	group_by(IDs, Objectivity, DominantModality) %>% count()

## How many reviews contain words from Lynott & Connell (2009):

length(unique(lyn_words$IDs))
length(unique(lyn_words$IDs)) / nrow(pang_all)

## Count tokens:

lyn_counts <- lyn_words %>% group_by(Objectivity, DominantModality) %>%
	summarise(n = sum(n))

## Spread this for Chi-square test:

lyn_counts <- lyn_counts %>% spread(Objectivity, n)

## Perform Chi-square tests:

(lyn_chisq <- lyn_counts %>% select(objective:subjective) %>% chisq.test)

## Extract Standardized Pearson's residuals:

lyn_pears <- lyn_chisq$stdres
rownames(lyn_pears) <- lyn_counts$DominantModality

## Look at Pearson's residuals:

lyn_pears		# using 1.96 as cut-off, after Agresti (2002: 81)

## Get more exclusive tokens:

lyn_words <- plyn %>%
	filter(ModalityExclusivity > median(ModalityExclusivity)) %>%
	group_by(IDs, Objectivity, DominantModality) %>% count()

## Count tokens:

lyn_counts <- lyn_words %>% group_by(Objectivity, DominantModality) %>%
	summarise(n = sum(n))

## Spread this for Chi-square test:

lyn_counts <- lyn_counts %>% spread(Objectivity, n)

## Perform Chi-square tests:

(lyn_chisq <- lyn_counts %>% select(objective:subjective) %>% chisq.test)

## Extract Standardized Pearson's residuals:

lyn_pears <- lyn_chisq$stdres
rownames(lyn_pears) <- lyn_counts$DominantModality

## Look at Pearson's residuals:

lyn_pears		# using 1.96 as cut-off, after Agresti (2002: 81)
	# taste & smell don't survive much because they are so highly multinodal



##------------------------------------------------------------------
## Check which words are contributing to the effects:
##------------------------------------------------------------------

## Check unique words for taste & smell, and for visual:

subj.taste <- filter(plyn,	# taste
	DominantModality == 'Gustatory', Objectivity == 'subjective')
subj.taste %>% group_by(Word) %>%
	count() %>%
	arrange(desc(n)) %>%
	print(n = 20)
# check cloying examples:
grep('cloying', pang_all$Text, value = T)
grep('sweet', pang_all$Text, value = T)
subj.smell <- filter(plyn,	# smell
	DominantModality == 'Olfactory', Objectivity == 'subjective')
subj.smell %>% group_by(Word) %>%
	count() %>%
	arrange(desc(n)) %>%
	print(n = 20)
subj.sight <- filter(plyn,	# smell
	DominantModality == 'Visual', Objectivity == 'objective')
subj.sight %>% group_by(Word) %>%
	count() %>%
	arrange(desc(n)) %>%
	print(n = 20)	# many highly multimodal




##------------------------------------------------------------------
## Table and inferential stats on review counts:
##------------------------------------------------------------------

## Transform counts into 1's for summing up by review:

lyn_words.01 <- lyn_words %>% mutate(n = 1)

## Count reviews:

lyn_counts <- lyn_words %>% group_by(Objectivity, DominantModality) %>% count()

## Spread this for Chi-square test:

lyn_counts <- lyn_counts %>% spread(Objectivity, nn)

## Perform Chi-square tests:

(lyn_chisq <- lyn_counts %>% ungroup() %>%
	select(objective:subjective) %>% chisq.test)

## Extract Standardized Pearson's residuals:

lyn_pears <- lyn_chisq$stdres
rownames(lyn_pears) <- lyn_counts$DominantModality

## Look at Pearson's residuals:

lyn_pears		# using 1.96 as cut-off, after Agresti (2002: 81)
	# again not enough taste/smell words



##------------------------------------------------------------------
## Inferential stats, how many sensory adjectives used counts:
##------------------------------------------------------------------

## Get distinct uses, counts per word:

lyn_words <- plyn %>% group_by(Word, Objectivity, DominantModality) %>% count()

## Look at which words are most frequently used per subjective/objective, LC2009:

lyn_words %>% filter(DominantModality == 'Gustatory',
	Objectivity == 'subjective') %>% ungroup %>%
		select(-DominantModality, -Objectivity) %>% arrange(desc(n))
lyn_words %>% filter(DominantModality == 'Gustatory',
	Objectivity == 'objective') %>% ungroup %>%
		select(-DominantModality, -Objectivity) %>% arrange(desc(n))
lyn_words %>% filter(DominantModality == 'Olfactory',
	Objectivity == 'subjective') %>% ungroup %>%
		select(-DominantModality, -Objectivity) %>% arrange(desc(n))
lyn_words %>% filter(DominantModality == 'Olfactory',
	Objectivity == 'objective') %>% ungroup %>%
		select(-DominantModality, -Objectivity) %>% arrange(desc(n))

## Get unique types per modality:

lyn_types <- lyn_words %>% group_by(Objectivity, DominantModality) %>% count()

## Tabulate those counts:

lyn_typecounts <- lyn_types %>% spread(Objectivity, nn)

## Perform Chi-square tests on types:

(lyn_chisq <- lyn_typecounts %>% ungroup() %>%
	select(objective:subjective) %>% chisq.test)

## Look at standardized Pearson residuals:

lyn_stdres <- round(lyn_chisq$stdres, 2)
rownames(lyn_stdres) <- lyn_typecounts$DominantModality
lyn_stdres

## Get unique types in main datasets:

lyn_orig <- lyn %>% group_by(DominantModality) %>% count()

## Merge those unique types (the baseline) into lyn_types:

lyn_typecounts <- lyn_typecounts %>% left_join(lyn_orig)

## Calculate percentages of those mapped:

lyn_typecounts <- lyn_typecounts %>%
	mutate(objective_p = round(objective / n, 2),
		subjective_p = round(subjective / n, 2))

## Make a barplot of this, rearrange dataframe:

lyn_typecounts <- lyn_typecounts[c(2, 4, 3, 1, 5), ]
lyn_typecounts$Modality <- c('Taste', 'Smell', 'Touch',
	'Sound', 'Sight')
lyn_typecounts$XStart <- c(1, 4, 7, 10, 13)

## Make the barplot:

xfac <- 0.15
quartz('', 9, 6)
par(mai = c(0.75, 1.75, 1, 0.15))
emptyplot(xlim = c(0.8, 15.5), ylim = c(0, 0.75))
axis(side = 2, at = seq(0, 0.75, 0.25),
	labels = paste0(seq(0, 0.75, 0.25) * 100, '%'),
	font = 2, lwd.ticks = 2, cex.axis = 1.25, las = 2,
	lwd = 2)
mtext(side = 2, '% Used Adjectives', line = 4.8, font = 2, cex = 2.15)
for (i in 1:5) {
	rect(xleft = lyn_typecounts[i, ]$XStart - xfac,
		xright = lyn_typecounts[i, ]$XStart + 1 - xfac,
		ybottom = 0, ytop = lyn_typecounts[i, ]$objective_p,
		lwd = 2, col = 'white')
	text(x = mean(c(lyn_typecounts[i, ]$XStart - xfac,
			lyn_typecounts[i, ]$XStart + 1 - xfac)),
		y = lyn_typecounts[i, ]$objective_p + 0.025,
		labels = paste0(lyn_typecounts[i, ]$objective_p * 100, '%'),
		font = 2, cex = 1.3)
	text(x = mean(c(lyn_typecounts[i, ]$XStart - xfac,
			lyn_typecounts[i, ]$XStart + 1 - xfac)),
		y = lyn_typecounts[i, ]$objective_p - 0.03,
		labels = lyn_typecounts[i, ]$objective,
		cex = 1.08)
	rect(xleft = lyn_typecounts[i, ]$XStart + 1,
		xright = lyn_typecounts[i, ]$XStart + 2 + xfac,
		ybottom = 0, ytop = lyn_typecounts[i, ]$subjective_p,
		lwd = 2, col = 'darkgrey')
	text(x = mean(c(lyn_typecounts[i, ]$XStart + 1,
			lyn_typecounts[i, ]$XStart + 2 + xfac)),
		y = lyn_typecounts[i, ]$subjective_p + 0.025,
		labels = paste0(lyn_typecounts[i, ]$subjective_p * 100, '%'),
		font = 2, cex = 1.3)
	text(x = mean(c(lyn_typecounts[i, ]$XStart + 1,
			lyn_typecounts[i, ]$XStart + 2 + xfac)),
		y = lyn_typecounts[i, ]$subjective_p - 0.03,
		labels = lyn_typecounts[i, ]$subjective,
		cex = 1.08)
	text(x = mean(c(lyn_typecounts[i, ]$XStart - xfac,
			lyn_typecounts[i, ]$XStart + 2 + xfac)),
		y = -0.04, xpd = NA, font = 2, cex = 1.5,
		labels = lyn_typecounts[i, ]$Modality)
	}
legend("topright", fill = c('white', 'darkgrey'), 
	legend = c('Objective', 'Subjective'), box.lwd = 2)


##------------------------------------------------------------------
## Positive / negative reviews:
##------------------------------------------------------------------

## Join Warriner valence:

war <- read_csv('warriner_2013_affective_norms.csv')

## Create absolute valence:

war <- mutate(war,
	Val_c = Val - mean(Val),
	AbsVal = abs(Val_c))

## Merge into rotten tomatoes dataset:

pang$Val <- war[match(pang$Word, war$Word), ]$Val
pang$AbsVal <- war[match(pang$Word, war$Word), ]$AbsVal

## Get rid of NAs:

nrow(pang)	# 215,624
pwar <- na.omit(pang)
nrow(pwar)	# 67,817

## Get average valence and absolute valence per review:
## Of course, excluding our perceptual words:

pwar <- pwar %>% filter(!(Word %in% lyn$Word))
pwar_agr <- pwar %>% group_by(IDs, Objectivity) %>%
	summarize(Val = mean(Val),
		AbsVal = mean(AbsVal))

## Check absolute valence:

pwar_agr %>% group_by(Objectivity) %>%
	summarize(AbsVal = mean(AbsVal)))
aggregate(AbsVal ~ Objectivity, pwar_agr, sd)
t.test(AbsVal ~ Objectivity, pwar_agr)	# not significant

## Merge counts into there:

plyncounts01 <- plyn %>% group_by(IDs) %>%
	count(DominantModality) %>% spread(DominantModality, n)
plyncounts01 <- mutate(plyncounts01,
	Auditory = ifelse(is.na(Auditory), 0, 1),
	Gustatory = ifelse(is.na(Gustatory), 0, 1),
	Haptic = ifelse(is.na(Haptic), 0, 1),
	Visual = ifelse(is.na(Visual), 0, 1),
	Olfactory = ifelse(is.na(Olfactory), 0, 1))
combined <- left_join(pwar_agr, plyncounts01)

## Get rid of NAs (not a single perceptual word):

combined <- na.omit(combined)

## Correlate with absolute valence:

summary(lm(AbsVal ~ Auditory,
	filter(combined, Objectivity == 'subjective')))
summary(lm(AbsVal ~ Visual,
	filter(combined, Objectivity == 'subjective')))
summary(lm(AbsVal ~ Haptic,
	filter(combined, Objectivity == 'subjective')))
summary(lm(AbsVal ~ Gustatory,
	filter(combined, Objectivity == 'subjective')))
summary(lm(AbsVal ~ Olfactory,
	filter(combined, Objectivity == 'subjective')))

summary(lm(AbsVal ~ Auditory, combined))
summary(lm(AbsVal ~ Visual, combined))
summary(lm(AbsVal ~ Haptic, combined))
summary(lm(AbsVal ~ Gustatory, combined))
summary(lm(AbsVal ~ Olfactory, combined))	# not when combined with objective

## Correlate with raw valence:

summary(lm(Val ~ Auditory,
	filter(combined, Objectivity == 'subjective')))
summary(lm(Val ~ Visual,
	filter(combined, Objectivity == 'subjective')))
summary(lm(Val ~ Haptic,
	filter(combined, Objectivity == 'subjective')))
summary(lm(Val ~ Gustatory,
	filter(combined, Objectivity == 'subjective')))
summary(lm(Val ~ Olfactory,
	filter(combined, Objectivity == 'subjective')))




