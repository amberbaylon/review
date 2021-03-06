# logistic regression homework 

movies <-read.table(text = "A Beautiful Mind,1,76, PG
Gosford Park,0,86, R
In the Bedroom,0,93, R
LOTR: Fellowship,0,91, PG-13
Moulin Rouge,0,76, PG-13
Chicago,1,87, PG-13
Gangs of New York,0,75, R
The Hours,0,81, PG-13
LOTR: Towers,0,96, PG-13
The Pianist,0,96, R
LOTR: Return,1,94, PG-13
Lost in Translation,0,95, R
Master and Commander,0,85, PG-13
Mystic River,0,87, R
Seabiscuit,0,77, PG-13
Million Dollar Baby,1,91, PG-13
The Aviator,0,87, PG-13
Finding Neverland,0,83, PG
Ray,0,81, PG-13
Sideways,0,96, R
Crash,1,75, R
Brokeback Mountain,0,87, R
Capote,0,90, R
Good Night and Good Luck,0,93, PG
Munich,0,78, R
The Departed,1,92, R
Babel,0,69, R
Letters from Iwo Jima,0,91, R
Little Miss Sunshine,0,91, R
The Queen,0,97, PG-13
No Country for Old Men,1,94, R
Atonement,0,83, R
Juno,0,94, PG-13
Michael Clayton,0,90, R
There Will Be Blood,0,91, R
Slumdog Millionaire,1,94, R
Curious Case of Benj Button,0,72, PG-13
Frost/Nixon,0,92, R
Milk,0,94, R
The Reader,0,61, R
The Hurt Locker, 1, 98, R
Avatar, 0, 83, PG-13
The Blind Side, 0, 66, PG-13
District 9, 0, 90, R
An Education, 0, 94, PG-13
Inglourious Basterds, 0, 89, R
Precious, 0, 91, R
A Serious Man, 0, 89, R
Up, 0, 98, PG
Up in the Air, 0, 91, R
The Kings Speech, 1, 95, PG-13
127 Hours, 0, 93, R
Black Swan, 0, 87, R
The Fighter, 0, 90, R
Inception, 0, 86, PG-13
The Kids are All Right, 0, 93, R
The Social Network, 0, 96, PG-13
True Grit, 0, 96, PG-13
Winters Bone, 0, 94, R
The Artist, 1, 97, PG-13
The Descendants, 0, 89, PG
Extremely Loud and Incredibly Close, 0, 46, PG-13
The Help, 0, 75, PG-13
Hugo, 0, 94, PG
Midnight in Paris, 0, 93, PG-13
Moneyball, 0, 94, PG-13
The Tree of Life, 0, 84, PG-13
War Horse, 0, 76, PG-13
Argo, 1, 96, R
Amour, 0, 93, PG-13
Beasts of the Southern Wild, 0, 86, PG-13
Django Unchained, 0, 88, R
Les Miserables, 0, 69, PG-13
Life of Pi, 0, 87, PG
Lincoln, 0, 90, PG-13
Silver Linings Playbook, 0, 92, R
Zero Dark Thirty, 0, 92, R
12 Years a Slave, 1, 96, R
American Hustle, 0, 93, R
Captain Phillips, 0, 93, PG-13
Dallas Buyers Club, 0, 94, R
Gravity, 0, 96, PG-13
Her, 0, 95, R
Nebraska, 0, 91, R
Philomena, 0, 92, PG-13
The Wolf of Wall Street, 0, 77, R
Birdman, 1, 91, R
American Sniper, 0, 72, R
Boyhood, 0, 98, R
The Grand Budapest Hotel, 0, 92, R
The Imitation Game, 0, 90, PG-13
Selma, 0, 99, PG-13
The Theory of Everything, 0, 79, PG-13
Whiplash, 0, 94, R
Spotlight, 1, 96, R
The Big Short, 0, 88, R
Bridge of Spies, 0, 91, PG-13
Brooklyn, 0, 97, PG-13
Mad Max: Fury Road, 0, 97, R
The Martian, 0, 92, PG-13
The Revenant, 0, 81, R
Room, 0, 94, R
Hidden Figures, 0, 93, PG
La La Land, 0, 92, PG-13
Arrival, 0, 94, PG-13
Hacksaw Ridge, 0, 86, R
Fences, 0, 93, PG-13
Lion, 0, 86, PG-13
Manchester by the Sea, 0, 95, R
Moonlight, 1, 98, R
Hell or High Water, 0, 97, R
Shape of Water, 1, 92, R
Call Me by Your Name, 0, 95, R
Darkest Hour, 0, 84, PG-13
Dunkirk, 0, 92, PG-13
Get Out, 0, 98, R
Lady Bird, 0, 99, R
Phantom Thread, 0, 91, R
The Post, 0, 88, PG-13
Three Billboards Outside Ebbing Missouri, 0, 91, R
Green Book, 1, 79, PG-13
Black Panther, 0, 97, PG-13
BlacKkKlansman, 0, 96, R
Bohemian Rhapsody, 0, 61, PG-13
The Favourite, 0, 93, R
Roma, 0, 96, R
A Star is Born, 0, 89, R
Vice, 0, 66, R", header = FALSE, sep = ",")

names(movies) <- c("name", "won", "tomatometer", "rating")
movies
str(movies)
# relevel 
movies$rating <- factor(movies$rating)
movies$rating <- relevel(movies$rating, " R")

str(movies)

# fit the model 
# Model: log(P(Win)/P(not Win)) = beta0 + beta1 tomatometer + beta2 (PG -13) + beta3 (PG)  
# logistic model because the response for win is either a 1 or 0
movies.out <- glm(won ~ tomatometer + rating, data = movies, family = "binomial")
summary(movies.out)

# betahat is the estimate on the coefficent on wins

exp(coef(movies.out)[-1])

# For each additional positive review, we estimate the mean chance of winning an academy award to increase by 
# 5%, holding all else constant. 

# Holding all else constant, PG-13 movies have a 21% decrease in odds of winning the 
# Academy Award over R rated movies (95% CI: 0.25 to 2.28). 


# 95%: CI
exp(confint(movies.out)[-1,])

# Test Ho: MPAA Rating has no effect on winning the Academy Award at α = 0.05
movies.red <- glm(won~ tomatometer, data = movies, family = 'binomial')
anova(movies.red, movies.out, test = "Chisq")

# Conclusion: With a p-value of 0.9 we can conclude that MPAA ratings have no effect on 
# winning an Academy Award.  


predict(movies.out, newdata = data.frame(name = "Black Panther", tomatometer = 97, rating = " PG-13"), 
        type = "response")

logit.hat <- predict(movies.out, newdata = data.frame(name = "Black Panther", tomatometer = 97, rating = " PG-13"), 
        type = "link", se = TRUE)

logit.L <- logit.hat$fit -1.96*logit.hat$se
logit.U <- logit.hat$fit + 1.96*logit.hat$se
phat.L <- exp(logit.L)/(1 + exp(logit.L))
phat.U <- exp(logit.U)/(1 + exp(logit.U))

# ROC
library(ROCR)
movies.pred <- prediction(predict(movies.out, type = "response"), movies$won)
movies.pref <- performance(movies.pred, measure = "tpr", x.measure = "fpr")
plot(movies.pref, xlab = "1 - specificity", ylab = "sensitivity", main = "ROC Curve")
abline(0,1,col = "hot pink")

test.pred <- prediction(predict(movies.out, newdata = movies, type = "response"), movies$won)
test.perf <- performance(test.pred, measure = "tpr", x.measure = "fpr")
plot(test.perf, add = TRUE, col = "green")

# AUC
performance(movies.pred, measure = "auc")
performance(test.pred, measure = "auc")

# tells how good your model is at predicting 

# The AUC for our test prediction is .634.  An AUC of 0.5 means that model tell doesn’t tell us 
# anything.  Since this is greater than 0.5, we know that it’s a better chance than flipping a coin, 
# yet it is definitely not 100% accurate.

