#Risk: n versus m armies - who will win.
library(ggplot2)

1+1 = 2

Rolls <- function(atarm, defarm) { # here atarm is the number of guys -1 - the guys you are actually attacking with
        while (atarm >=2 & defarm >= 1) {
            at <- sort(sample(6, min(3, atarm), replace = TRUE), decreasing = TRUE) #ordered attack dice
            def <- sample(6, min(2, defarm) , replace = TRUE) # defense dice
            
            
            if (at[1]>max(def) & at[2]>min(def)) {defarm <- defarm - min(2, defarm)}
            else if (at[1]<= max(def) & at[2] <= min(def)) 
                {atarm <- atarm - min(2, defarm)} #this is t
            else {defarm <- defarm -1; atarm <- atarm - 1}
        }
        
        while (atarm >=1 & defarm >=1) { #taking care of the case with one attacker
            at <- sample(6,1)
            def <- sample(6, min(2, defarm) , replace = TRUE)
            if (at > max(def)) {defarm <- defarm -1}
                else {atarm <- atarm -1}
        }
    return(c(atarm, defarm))
}

reps <- 100000

sim <- function(att, def, reps = 10000) { #simulates the battle reps times 
outcome <- replicate(reps, Rolls(att,def))
expec <- mean(outcome[1,]) 
winningprob <- 1 - table(outcome[1,])[1]/reps
return(list(expec, winningprob, outcome)) #take hist(outcome[1,]) to see the dist
}

sim(20, 1)[1]


qplot(outcome[1,], binwidth = .5)
table(outcome[1,])
sim(20, 5)[1]

outcome <- replicate(10000, Rolls(40,40))
prob <- table(outcome[1,])/reps
expec <- sum(prob * 0:(length(prob)-1))

prob


#expected value and winning prob on n vs n battLe: blah should have set a seed.
count <- 1:40
ex <- numeric(40)
prob <- numeric(40)
for (i in count){
    ex[i] <- sim(i,i)[[1]]
    prob[i] <- sim(i,i)[[2]]
}

plot(ex)

plot(prob)
abline(h = .5) #attack with more than 10 if you are facing a similar number of armies


#We are varying the defending army size
set.seed(124)
ex3 <- numeric(40)
for (i in count){
    ex3[i] <- sim(20,i)[[1]]
    prob3[i] <- sim(20,i)[[2]]
}

#the graph below shows the number of player you expect to be left with on average if you attack 
#with 20 peeps 
plot(ex3, ylim = c(0,20), xlab = "Defending Army Size", ylab = "Expected Value of Armies You're Left With")
abline(a = 20, b = -1)

# the graph below shows prob you'll win if you attack with 20 people - 
#the big takeaway here is that you stand a good chance against 15 but a 
#terrible chance against 25
plot(prob3, xlab = "Defending Army Size", ylab = "Probability Attacker Wins")
abline(v = 20, h = .5)

sim(20, 1)[3]

#Now we want a confidence interval for say 20 vs 10 based on the distribution:
set.seed(124)
temp <- sim(20, 10)
temp[[1]]
temp[2]

qplot(temp[[3]][1,], binwidth = .5)

dist <- temp[[3]][1,]

hist(dist)
mean(dist)

distance <- abs(dist - mean(dist)) 
length(distance[distance <8])/10000
#we get a probability of .9382 for distance = 8 
#so this means you are in between (3.5 and 19.5) %80 of the time - lots of variance.

    
#So we have figured out if we do 1 on 1 armies - we get the prob you're
#gonna win, the expected value and the distr. BUT what if say you have 20 peeps
#and you are facing four armies 5, 8, 1, 1 In Australia. Should you go for it? And if
#yes which way shuld you go - the 5 or the 8?

#Let's try attacking first the 5 and then the 8:
sim(8, 1)[1]
sim(7, 1)[1]
#Does it really matter? Hard to see since the expected value is not an integer.


temp <- sim(15, 10)
temp[[1]]
temp[2]

qplot(temp[[3]][1,], binwidth = .5)


