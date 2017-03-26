 

library(data.table)
library(vcd)
library(ggplot2)


# load data
train <- read.csv("train.csv", header = TRUE)
train <- data.table(train)
str(train)

train[, Survived := as.factor(Survived)]
train[, Pclass := as.factor(Pclass)]
train[, Name := as.character(Name)]

# check features
table(train$Survived)

table(train$Pclass)

length(unique(train$Name))
head(train$Name)
tail(train$Name)

table(train$Sex)

summary(train$Age) # NAs, min = 0.42 lol
hist(train$Age, breaks = 30)

table(train$SibSp) # 7 with 8, wtf!

table(train$Parch) # 5 w/ 5, 1 w/ 6.. ok

length(unique(train$Ticket)) # there are duplicate tickets? I dont get it

summary(train$Fare)
hist(train$Fare, breaks = 30) # what with these > 500 guys?!
train[Fare > 500, ] # ok there are some of these duplicate tickets and they all
# have this weird price
# one doesnt have a cabin?
# well the other has 3...

# seems more apropriate to make them chr
train[, Cabin := as.character(Cabin)]
head(train$Cabin)
tail(train$Cabin)

table(train$Embarked)

# feature extraction
















# correlations
# first some functions
panel.cor <- function(x, y, digits = 2, cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.5, txt)
}

pairs(
  train[, .(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)],
  lower.panel = panel.cor
) # not so great


mosaic(table(train[, .(Survived, Pclass)]), shade = TRUE) # good
mosaic(table(train[, .(Survived, Sex)]), shade = TRUE) # very good
mosaic(table(train[, .(Survived, Embarked)]), shade = TRUE) # no

ggplot(train, aes(fill = Survived)) + 
  geom_histogram(aes(x = Age), bins = 30) +
  ggthemes::theme_hc() # no
ggplot(train, aes(fill = Survived)) + 
  geom_histogram(aes(x = SibSp), bins = 10) +
  ggthemes::theme_hc() # well.. above 4 survived
ggplot(train, aes(fill = Survived)) + 
  geom_histogram(aes(x = Parch), bins = 10) +
  ggthemes::theme_hc() # no
ggplot(train, aes(fill = Survived)) + 
  geom_histogram(aes(x = Fare), bins = 10) +
  ggthemes::theme_hc() # above 400 also survived

# between features
ggplot(train, aes(fill = Pclass)) + 
  geom_histogram(aes(x = Fare), bins = 10) +
  ggthemes::theme_hc() # thats weird I thought they all payed a lot
ggplot(train, aes(fill = Embarked)) + 
  geom_histogram(aes(x = Fare), bins = 10) +
  ggthemes::theme_hc() # also no...
mosaic(table(train[, .(Pclass, Sex)]), shade = TRUE) # no
mosaic(table(train[, .(Pclass, Sex)]), shade = TRUE) # no









