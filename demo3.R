library('ggplot2') 
library('corrplot') 
library("Hmisc") 
library('rpart') 
library('rpart.plot') 
library('randomForest') 
library('MASS')


combat <- read.csv(file = "E:/PROJECT/R/pokemon-challenge/combats.csv", header = TRUE)
pokemon <- read.csv(file = "E:/PROJECT/R/pokemon-challenge/pokemon.csv", header = TRUE)
test <- read.csv(file = "E:/PROJECT/R/pokemon-challenge/tests.csv", header = TRUE)


pokefight <- combat

pokefight$ID_1 <- (combat$First_pokemon)
pokefight$Type1_1 <- pokemon$Type.1[combat$First_pokemon]
pokefight$Type2_1 <- pokemon$Type.2[combat$First_pokemon]
pokefight$HP_1 <- pokemon$HP[combat$First_pokemon]
pokefight$Attack_1 <- pokemon$Attack[combat$First_pokemon]
pokefight$Defense_1 <- pokemon$Defense[combat$First_pokemon]
pokefight$Sp..Atk_1 <- pokemon$Sp..Atk[combat$First_pokemon]
pokefight$Sp..Def_1 <- pokemon$Sp..Def[combat$First_pokemon]
pokefight$Speed_1 <- pokemon$Speed[combat$First_pokemon]
pokefight$Generation_1 <- pokemon$Generation[combat$First_pokemon]
pokefight$Legendary_1 <- pokemon$Legendary[combat$First_pokemon]

pokefight$ID_2 <- (combat$Second_pokemon)
pokefight$Type1_2 <- pokemon$Type.1[combat$Second_pokemon]
pokefight$Type2_2 <- pokemon$Type.2[combat$Second_pokemon]
pokefight$HP_2 <- pokemon$HP[combat$Second_pokemon]
pokefight$Attack_2 <- pokemon$Attack[combat$Second_pokemon]
pokefight$Defense_2 <- pokemon$Defense[combat$Second_pokemon]
pokefight$Sp..Atk_2 <- pokemon$Sp..Atk[combat$Second_pokemon]
pokefight$Sp..Def_2 <- pokemon$Sp..Def[combat$Second_pokemon]
pokefight$Speed_2 <- pokemon$Speed[combat$Second_pokemon]
pokefight$Generation_2 <- pokemon$Generation[combat$Second_pokemon]
pokefight$Legendary_2 <- pokemon$Legendary[combat$Second_pokemon]
pokefight$Result[pokefight$Winner == pokefight$First_pokemon] <- 'Win'
pokefight$Result[pokefight$Winner == pokefight$Second_pokemon] <- 'Loss'

pokefight$Result <- as.factor(pokefight$Result)
pokefight$Generation_1 <- as.factor(pokefight$Generation_1)
pokefight$Generation_2 <- as.factor(pokefight$Generation_2)


ggplot(pokefight, aes(Speed_1, fill = Result)) + geom_density(alpha = 0.5)


pokefight$diff_HP = pokefight$HP_1 - pokefight$HP_2
pokefight$diff_Attack = pokefight$Attack_1 - pokefight$Attack_2
pokefight$diff_Defense = pokefight$Defense_1 - pokefight$Defense_2
pokefight$diff_Sp..Atk = pokefight$Sp..Atk_1 - pokefight$Sp..Atk_2
pokefight$diff_Sp..Def = pokefight$Sp..Def_1 - pokefight$Sp..Def_2
pokefight$diff_Speed = pokefight$Speed_1 - pokefight$Speed_2

# Create binary variable of Result ('Win/Lose' to numeric variable '1/0')
pokefight$binaryResult[pokefight$Result == "Win"] <- 1
pokefight$binaryResult[pokefight$Result == "Loss"] <- 0


numeric_metrics <- pokefight[ ,c('binaryResult', 
                                 'diff_HP', 'diff_Attack', 'diff_Defense', 
                                 'diff_Sp..Atk', 'diff_Sp..Def', 'diff_Speed',
                                 'HP_1', 'Attack_1', 'Defense_1', 'Sp..Atk_1', 'Sp..Def_1', 'Speed_1')]

# Create correlation matrix which consists of numeric features
cor_matrix <- cor(numeric_metrics)


mtxlong <- reshape2::melt(numeric_metrics, id.vars = "binaryResult")

# Generate multiple density chart
ggplot(mtxlong, aes(value, fill = factor(binaryResult))) + #create a canvas 
  facet_wrap(~variable, scales = 'free', strip.position = 'top') + #create sequence of panels
  geom_density(alpha = '0.5') + #create density plot 
  labs(fill = 'binaryResult') #rename the legend


corrplot(cor_matrix, 
         method = "color", 
         addCoef.col = "black", #shows correlation coef
         type = 'upper', #only shows upper plot
         diag = FALSE #hide the diagonal
)


set.seed(117)
random <- sample(1:50000, size = 40000)
train <- pokefight[random, ]
test <- pokefight[-random, ]

# Get the test_label and the test data
test_label <- test[,26] # Column 26 is target
test_data <- test[,-26]
test_data$Result <- '0'

binary_tree_1 <- rpart(Result ~   diff_Speed + Speed_1, data = train, method = 'class' )
result_1 <- predict(binary_tree_1, test_data, type = 'class')
table(result_1, test_label)

ggplot(pokefight, aes(y = Speed_1 , x = diff_Speed, color = Result)) + geom_point(alpha = "0.7") + geom_vline(xintercept = 1)

all_metrics <- pokefight[ ,c('Result',
                             'diff_HP', 'diff_Attack', 'diff_Defense', 
                             'diff_Sp..Atk', 'diff_Sp..Def', 'diff_Speed',
                             'HP_1', 'Attack_1', 'Defense_1', 'Sp..Atk_1',
                             'Sp..Def_1', 'Speed_1', 'Type1_1', 'Type2_1',
                             'Generation_1', 'Legendary_1',
                             'HP_2', 'Attack_2', 'Defense_2', 'Sp..Atk_2',
                             'Sp..Def_2', 'Speed_2', 'Type1_2', 'Type2_2',
                             'Generation_2', 'Legendary_2'
)]


set.seed(117)
random <- sample(1:50000, size = 40000)
train <- all_metrics[random, ]
test <- all_metrics[-random, ]

# Get the test_label and the test data
test_label <- test[,1] # Column 1 is target
test_data <- test[,-1]
test_data$Result <- '0'

rf_tree <- randomForest(Result ~. , data = train, type = 'classification', ntree = 500 )
rf_tree


# Predict the result using Random Forest Model
pred <- predict(rf_tree, test_data)
table(pred, test_label)

oob.err=double(20)
for(mtry in 1:20){
  fit=randomForest(Result~.,data=train,mtry=mtry,ntree=50)
  oob.err[mtry]=fit$err.rate[50]
  cat(mtry," ")
}
oob.err

plot(oob.err, ylab= "Error", xlab= '1:mtry', type = 'l')

rf_tree_final <- randomForest(Result ~. , data = train, type = 'classification', mtry = 20, ntree = 500 )
rf_tree_final

pred <- predict(rf_tree_final, test_data)
table(pred, test_label)

ggplot(pokemon[pokemon$Speed>=quantile(pokemon$Speed,0.95),c("Name","Speed","Attack",'Type.1')], aes(x=Speed, y=Attack, col= Type.1, label = Name)) + geom_text(check_overlap = TRUE)


