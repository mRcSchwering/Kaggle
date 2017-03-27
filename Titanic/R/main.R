 

# PKGs --------------------------------------------------------------------


library(data.table) # database-like tables
library(vcd) # actually just for mosaic plot
library(ggplot2) # general plots





# Data --------------------------------------------------------------------


# load data
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)

test <- data.table(test)
train <- data.table(train)


# see variables
str(train)

# ok:
identical(train$PassengerId, seq_along(train$PassengerId))
table(train$Survived)
table(train$Pclass)
table(train$Sex)
table(train$SibSp)
table(train$Parch)

# interesting:
head(train$Name)
summary(train$Age)
head(train$Ticket)
summary(train$Fare)
tail(train$Cabin)
table(train$Embarked)




# Feature Extraction ------------------------------------------------------


# titles
# this is from `mrisdal`
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
table(train$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
train[Title == "Mlle", Title := "Miss"]
train[Title == "Ms", Title := "Miss"]
train[Title == "Mme", Title := "Mrs"]
train[Title %in% rare_title, Title := "Rare Title"]
table(train$Title, train$Sex)

# surnames
train$Surname <- vapply(
  train$Name, 
  function(x) strsplit(x, "[,. ]")[[1]][1],
  character(1)
)
hist(table(train$Surname), breaks = 20)


# tickets


# Cabins
# as many others, identify deck by letter
train$Deck <- vapply(
  train$Cabin, 
  function(x) strsplit(x, "")[[1]][1],
  character(1)
)
train[is.na(Deck), Deck := ""]

# number of Cabins might be interesting
train$nCabins <- vapply(
  train$Cabin, 
  function(x) length(strsplit(x, " ")[[1]]),
  integer(1)
)
table(train$nCabins)


# Families
# by duplicate surnames
counts <- table(train$Surname)
train$SNCount <- vapply(
  train$Surname, 
  function(sn) counts[names(counts) == sn],
  integer(1)
)
hist(train$SNCount, breaks = 20)

# and by Siblings and Parch (+ 1 for person himself)
train$FMembers <- train$SibSp + train$Parch + 1




# Missing Values ----------------------------------------------------------


# Embarked
table(train$Embarked)
train[Embarked == "", ] # both survived
# assumption: didnt embark

# Cabins
table(train$nCabins)
train$hasCabin <- as.integer(train$nCabins > 0)
# maybe this is interesting

# Age
# this is probably important
# maybe  I can regress it with easy model
m <- train[!is.na(Age), .(Age, Pclass, Fare)] # regressing might be hard
pairs(m)
# maybe I can identify children
train$isChild <- as.integer(train$Age < 18)
boxplot(SNCount ~ isChild, data = train) # aha
boxplot(FMembers ~ isChild, data = train) # aha
# this should work
tmp <- 
model <- as.formula(isChild ~ SNCount + FMembers)
fit <- glm(
  isChild ~ SNCount + FMembers, 
  family = binomial(link = 'logit'), 
  data = train[!is.na(isChild)]
)
summary(fit) # its something
anova(fit, test = "Chisq") 
preds <- predict(fit, newdata = train[is.na(isChild)], type = "response")
hist(preds, breaks = 20)
train[is.na(isChild), ] <- preds > 0.5


















pairs(train[, .(DuplicateSurname = SNCount, 
                FamMembers = SibSp + Parch,
                CabinCount = nCabins)])
mosaic(table(train[, .(Survived, DuplicateSurname = SNCount)]), shade = TRUE)
mosaic(table(train[, .(Survived, FamMembers = SibSp + Parch)]), shade = TRUE)
mosaic(table(train[, .(Survived, CabinCount = nCabins)]), shade = TRUE)




















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









