#correlation between reactions and likes is 0.97 which is very strong, so we can predict number of likes using number of reactions



imported_data = read.csv('/home/AI_Assignment/nytimes_facebook_statuses.csv')
outlier_reactions = boxplot(imported_data$num_reactions)$out
outlier_likes = boxplot(imported_data$num_likes)$out

x = cor(imported_data$num_reactions, imported_data$num_likes) #find correlation between reactions and likes, == 0.9736489
print(x)
plot(imported_data$num_reactions, imported_data$num_likes)

for(i in outlier_reactions){
  imported_data$num_reactions[imported_data$num_reactions==i] <- NA
}
for(j in outlier_likes){
  imported_data$num_likes[imported_data$num_likes==j] <- NA
}
imported_data = na.omit(imported_data) #omit outliers
hist(imported_data$num_reactions)
plot(density(imported_data$num_likes))
hist(imported_data$num_likes)
set.seed(7) #randomness
row <- sample(1:nrow(imported_data), 0.7*nrow(imported_data)) #70% data used to train the model and remaining data to test values
train = imported_data[row,]
test = imported_data[-row,]

#using linear model
LinearModel <- lm(num_likes~num_reactions,data=train) #num_likes Y-axix (Dependant), num_reactions X-axis (independant), See number of reactions and predict number of shares
prediction <- predict(LinearModel,test)
summary(LinearModel)
summary(prediction)
scatter.smooth(prediction)

#rm(list=ls()) clears variable list
