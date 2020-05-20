###############
#LOAD LIBRARIES
###############
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(readxl)) install.packages("readxl")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(grid)) install.packages("grid")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(PRROC)) install.packages("PRROC")
if(!require(kableExtra)) install.packages("kableExtra") #make the tables in rmarkdown
if(!require(float)) install.packages("float") #for table formating in rmarkdown
##########################
#DATA IMPORT & FIRST LOOK
##########################

house_cra_votes <- read_excel("20200512_0813_voteview_download.xls", sheet = 1) 
h88 <- read_csv("H088_members.csv")

head(house_cra_votes,5)
head(data.frame(h88, 5))

str(data.frame(house_cra_votes))
str(data.frame(h88))

##################
#DATA PREPARATION
##################

#Join datasets
cra_data <- inner_join(h88, house_cra_votes)
cra_data <- as.data.frame(cra_data)


#remove not-needed variables & values, rename the voting column
cra_data <- cra_data %>%
  mutate(party = if_else(party_code == "100", "democrat", "republican")) %>%
  filter(str_detect(chamber, "President", negate = T)) %>%
  rename(vote = V1) %>%
  select(nominate_dim1, nominate_dim2, nokken_poole_dim1, nokken_poole_dim2, party,
         vote)

#replace the voting values
not_voted <- c(2,3,5,7)
cra_data <- cra_data %>%
  mutate(vote = replace(vote, vote == 1, "yea")) %>%
  mutate(vote = replace(vote, vote == 6, "nea")) %>%
  filter(!vote %in% not_voted) %>%
  mutate(vote = as.factor(vote))

###################
#DATA VISUALIZATION
###################

#party distribution
cra_data %>%
  ggplot(aes(party, fill = party)) +
  geom_bar(stat="count", alpha = 0.8, show.legend = F) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  scale_fill_manual(values = c(republican = "#FF0000", democrat = "#0009FF")) +
  labs(title = "Party Distribution", x = "Party", y ="count") +
  theme_minimal() 


#votes
table(cra_data$vote)

#vote distribution
cra_data %>%
  ggplot(aes(vote, fill = party)) +
  geom_bar(stat = "count", alpha = 0.8, position = "dodge") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position = position_dodge(width = 0.85)) +
  scale_fill_manual(values = c(republican = "#FF0000", democrat = "#0009FF")) +
  labs(title = "Vote Distribution", x = "Votes", y ="Count") +
  theme_minimal()

#votes
table(cra_data$vote)

#ideology for politicians career 88th Congress. 
cra_data %>%
  ggplot(aes(nominate_dim1, nominate_dim2, color = party)) +
  geom_point(alpha = 0.4, size = 2) +
  scale_color_manual(values = c(republican = "#FF0000", democrat = "#0009FF"))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlim(-1,1) +
  ylim(-1,1) +
  ggtitle("Politician Ideological Vote by Party (Nominate Estimates)") +
  labs(x = "nominate_dim1 \n Liberal - Conservative", 
       y = "nominate_dim2 \n Liberal - Conservative") +
  theme_bw()


#ideology for 88th congress
cra_data %>%
  ggplot(aes(nokken_poole_dim1, nokken_poole_dim2, color = party)) +
  geom_point(alpha = 0.4, size = 2) +
  scale_color_manual(values = c(republican = "#FF0000", democrat = "#0009FF"))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlim(-1,1) +
  ylim(-1,1) +
  ggtitle("88th Congress Ideological Vote by Party (Nokken-Poole Estimates)") +
  labs(x = "nokken_poole_dim1 \n Liberal - Conservative", 
       y = "nokken_poole_dim2 \n Liberal - Conservative") +
  theme_bw()

#divide into test and training set
set.seed(1997, sample.kind = "Rounding")

test_index <- createDataPartition(cra_data$vote, times = 1, p= 0.20, list = F)

train_cra <- cra_data[-test_index,]

test_cra <- cra_data[test_index,]


#######
#MODELS 
#######

#naive bayes
set.seed(13, sample.kind = "Rounding")
ctrl <- trainControl(method="cv", number = 10, p = 0.9, savePredictions = T, classProbs = T)

nb <- train(vote ~ nominate_dim1 + nominate_dim2 + nokken_poole_dim1 + nokken_poole_dim2,
                data = train_cra, method = "nb", trControl = ctrl)

pred_nb <- predict(nb, test_cra, type = "prob")

cm_nb <- confusionMatrix(predict(nb, test_cra), test_cra$vote, positive = "yea")

results <- tibble(model = "Naive Bayes",
                      accuracy = cm_nb$overall["Accuracy"],
                      recall = cm_nb$byClass["Sensitivity"],
                      specificity = cm_nb$byClass["Specificity"],
                      precision = cm_nb$byClass["Pos Pred Value"],
                      f1_score = F_meas(predict(nb, test_cra), test_cra$vote),
                      AUC_PR = pr.curve(scores.class0 = pred_nb$yea[test_cra$vote == "yea"], 
                                        scores.class1 = pred_nb$nea[test_cra$vote == "nea"],
                                        curve = T)$auc.integral)

#glm
set.seed(73, sample.kind = "Rounding")

ctrl <- trainControl(method="cv", number = 10, p = 0.9, savePredictions = T, classProbs = T)

glm <- train(vote ~ nominate_dim1 + nominate_dim2 + nokken_poole_dim1 + nokken_poole_dim2,
                 data = train_cra, method = "glm", trControl = ctrl)

pred_glm <- predict(glm, test_cra, type = "prob")

cm_glm <- confusionMatrix(predict(glm, test_cra), test_cra$vote, positive = "yea")

results <- results %>%
  add_row(model = "GLM",
          accuracy = cm_glm$overall["Accuracy"],
          specificity = cm_glm$byClass["Specificity"],
          recall = cm_glm$byClass["Sensitivity"],
          precision = cm_glm$byClass["Pos Pred Value"], 
          f1_score = F_meas(predict(glm, test_cra), test_cra$vote),
          AUC_PR = pr.curve(scores.class0 = pred_glm$yea[test_cra$vote == "yea"], 
                            scores.class1 = pred_glm$nea[test_cra$vote == "nea"],
                            curve = T)$auc.integral)

#knn
set.seed(23, sample.kind = "Rounding")

ctrl <- trainControl(method="cv", number = 10, p = 0.9, savePredictions = T, classProbs = T)

knn <- train(vote ~ nominate_dim1 + nominate_dim2 + nokken_poole_dim1 + nokken_poole_dim2,
             data = train_cra, method = "knn", 
             tuneGrid = data.frame(k = seq(1,50)), trControl = ctrl)
knn$bestTune

pred_knn <- predict(knn, test_cra, type = "prob")

cm_knn <- confusionMatrix(predict(knn, test_cra),test_cra$vote, positive = "yea")

results <- results %>%
  add_row(model = "KNN",
          accuracy = cm_knn$overall["Accuracy"],
          specificity = cm_knn$byClass["Specificity"],
          recall = cm_knn$byClass["Sensitivity"],
          precision = cm_knn$byClass["Pos Pred Value"], 
          f1_score = F_meas(predict(knn, test_cra), test_cra$vote),
          AUC_PR = pr.curve(scores.class0 = pred_knn$yea[test_cra$vote == "yea"], 
                            scores.class1 = pred_knn$nea[test_cra$vote == "nea"],
                            curve = T)$auc.integral)


#classification tree
set.seed(53, sample.kind = "Rounding")

ctrl <- trainControl(method="cv", number = 10, p = 0.9, savePredictions = T, classProbs = T)

tree <- train(vote ~ nominate_dim1 + nominate_dim2 + nokken_poole_dim1 + nokken_poole_dim2, 
                  data = train_cra, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                  trControl = ctrl)

tree$bestTune

rpart.plot(tree$finalModel)


pred_tree <- predict(tree, test_cra, type = "prob")

cm_tree <- confusionMatrix(predict(tree, test_cra), test_cra$vote, positive = "yea")

results <- results %>%
  add_row(model = "Class. Tree",
          accuracy = cm_tree$overall["Accuracy"],
          specificity = cm_tree$byClass["Specificity"],
          recall = cm_tree$byClass["Sensitivity"],
          precision = cm_tree$byClass["Pos Pred Value"], 
          f1_score = F_meas(predict(tree, test_cra), test_cra$vote),
          AUC_PR = pr.curve(scores.class0 = pred_tree$yea[test_cra$vote == "yea"], 
                            scores.class1 = pred_tree$nea[test_cra$vote == "nea"],
                            curve = T)$auc.integral)

#Random Forest
set.seed(42, sample.kind = "Rounding")

ctrl <- trainControl(method="cv", number = 10, p = 0.9, savePredictions = T, classProbs = T)

rf <- train(vote ~ nominate_dim1 + nominate_dim2 + nokken_poole_dim1 + nokken_poole_dim2,
                importance = T, data = train_cra, method = "rf", tuneGrid = data.frame(mtry = seq(1,2)), 
                ntree = 100,
                trControl = ctrl)
pred_rf <- predict(rf, test_cra, type = "prob")

cm_rf <- confusionMatrix(predict(rf, test_cra), test_cra$vote, positive = "yea")

results <- results %>%
  add_row(model = "Random Forest",
          accuracy = cm_rf$overall["Accuracy"],
          specificity = cm_rf$byClass["Specificity"],
          recall = cm_rf$byClass["Sensitivity"],
          precision = cm_rf$byClass["Pos Pred Value"], 
          f1_score = F_meas(predict(rf, test_cra), test_cra$vote),
          AUC_PR = pr.curve(scores.class0 = pred_rf$yea[test_cra$vote == "yea"], 
                            scores.class1 = pred_rf$nea[test_cra$vote == "nea"],
                            curve = T)$auc.integral)


#SVM
set.seed(117, sample.kind = "Rounding")

ctrl <- trainControl(method="cv", number = 10, p = 0.9, classProbs = T, savePredictions = T, classProbs = T)

svm <- train(vote ~ nominate_dim1 + nominate_dim2 + nokken_poole_dim1 + nokken_poole_dim2,
                 data = train_cra, method = "svmLinear", trControl = ctrl, 
                 tuneGrid = data.frame(C = seq(1, 5, len = 25)))

svm$bestTune

pred_svm <- predict(svm, test_cra, type = "prob")

cm_svm <- confusionMatrix(predict(svm, test_cra), test_cra$vote, positive = "yea")

results <- results %>%
  add_row(model = "SVM",
          accuracy = cm_svm$overall["Accuracy"],
          specificity = cm_svm$byClass["Specificity"],
          recall = cm_svm$byClass["Sensitivity"],
          precision = cm_svm$byClass["Pos Pred Value"], 
          f1_score = F_meas(predict(svm, test_cra), test_cra$vote),
          AUC_PR = pr.curve(scores.class0 = pred_svm$yea[test_cra$vote == "yea"], 
                            scores.class1 = pred_svm$nea[test_cra$vote == "nea"],
                            curve = T)$auc.integral)



########################
#Check & Compare Results
########################

#ACCURACY
results %>%
  arrange(desc(accuracy)) %>%
  select(model, accuracy)

#F1 score
results %>%
  arrange(desc(f1_score))

########################
#THRESHOLD EXPLANATION
########################

#glm
cutoff <- seq(0,1, 0.1)
set.seed(73, sample.kind = "Rounding")
ctrl_thresh <- trainControl(method="cv", number = 10, p = 0.9, savePredictions = T, classProbs = T)

glm_thres <- train(vote ~ nominate_dim1 + nominate_dim2 + nokken_poole_dim1 + nokken_poole_dim2,
             data = train_cra, method = "glm", trControl = ctrl_thresh)

acc_thresh_glm <- thresholder(glm_thres, cutoff, final = T, statistics = "Accuracy")

f1_thresh_glm <- thresholder(glm_thres, cutoff, final = T, statistics = "F1")

#svm
set.seed(117, sample.kind = "Rounding")

ctrl_thresh <- trainControl(method="cv", number = 10, p = 0.9, classProbs = T, savePredictions = T)

svm_thresh <- train(vote ~ nominate_dim1 + nominate_dim2 + nokken_poole_dim1 + nokken_poole_dim2,
             data = train_cra, method = "svmLinear", trControl = ctrl_thresh, 
             tuneGrid = data.frame(C = seq(1, 5, len = 25)))

acc_thresh_svm <- thresholder(svm_thresh, cutoff, final = T, statistics = "Accuracy")

f1_thresh_svm <- thresholder(svm_thresh, cutoff, final = T, statistics = "F1")


########
#AUC_PR
########
results %>%
  arrange(desc(AUC_PR))


#AUC-R PLOT
plot(pr.curve(scores.class0 = pred_tree$yea[test_cra$vote == "yea"],
              scores.class1 = pred_tree$nea[test_cra$vote == "nea"], curve = T), 
     color = "#7CCA89", auc.main = F, main = "PR-Plot") 
plot(pr.curve(scores.class0 = pred_svm$yea[test_cra$vote == "yea"],
              scores.class1 = pred_svm$nea[test_cra$vote == "nea"], curve = T), 
     color = "#AAD178", add = T, auc.main = F)
plot(pr.curve(scores.class0 = pred_glm$yea[test_cra$vote == "yea"],
              scores.class1 = pred_glm$nea[test_cra$vote == "nea"], curve = T), 
     color = "#D7D868", add = T, auc.main = F)
plot(pr.curve(scores.class0 = pred_rf$yea[test_cra$vote == "yea"],
              scores.class1 = pred_rf$nea[test_cra$vote == "nea"], curve = T), 
     color = "#E8B45E", add = T, auc.main = F)
plot(pr.curve(scores.class0 = pred_knn$yea[test_cra$vote == "yea"],
              scores.class1 = pred_knn$nea[test_cra$vote == "nea"], curve = T), 
     color = "#DC645A", add = T, auc.main = F)
plot(pr.curve(scores.class0 = pred_nb$yea[test_cra$vote == "yea"],
              scores.class1 = pred_nb$nea[test_cra$vote == "nea"], curve = T), 
     color = "#D01556", add = T, auc.main = F)

legend_tree <- sprintf("Class. Tree (AUC: %.2f)", pr.curve(scores.class0 = pred_tree$yea[test_cra$vote == "yea"], 
                                                               scores.class1 = pred_tree$nea[test_cra$vote == "nea"],
                                                               curve = T)$auc.integral) 
legend_svm <- sprintf("SVM (AUC: %.2f)", pr.curve(scores.class0 = pred_svm$yea[test_cra$vote == "yea"], 
                                                      scores.class1 = pred_svm$nea[test_cra$vote == "nea"],
                                                      curve = T)$auc.integral) 
legend_glm <- sprintf("GLM (AUC: %.2f)", pr.curve(scores.class0 = pred_glm$yea[test_cra$vote == "yea"], 
                                                      scores.class1 = pred_glm$nea[test_cra$vote == "nea"],
                                                      curve = T)$auc.integral) 
legend_rf <- sprintf("Random Forest (AUC: %.2f)", pr.curve(scores.class0 = pred_rf$yea[test_cra$vote == "yea"], 
                                                               scores.class1 = pred_rf$nea[test_cra$vote == "nea"],
                                                               curve = T)$auc.integral)
legend_knn <- sprintf("KNN (AUC: %.2f)", pr.curve(scores.class0 = pred_knn$yea[test_cra$vote == "yea"], 
                                                      scores.class1 = pred_knn$nea[test_cra$vote == "nea"],
                                                      curve = T)$auc.integral)
legend_nb <- sprintf("NB (AUC: %.2f)", pr.curve(scores.class0 = pred_nb$yea[test_cra$vote == "yea"], 
                                                    scores.class1 = pred_nb$nea[test_cra$vote == "nea"],
                                                    curve = T)$auc.integral) 

legend("bottomright", col=c("#7CCA89", "#AAD178", "#D7D868", "#E8B45E", "#DC645A", "#D01556"), lwd = 3,
       legend=c(legend_tree, legend_svm, legend_glm, legend_rf, legend_knn, legend_nb))

##########################
#Variable Importance plots
##########################

#Nominate
mod_list <- list(nb, glm, knn, tree, rf, svm)

glm$modelInfo$label <- "GLM"
tree$modelInfo$label <- "Class. Tree"
svm$modelInfo$label <- "SVM"

var_imp_plots <- lapply(mod_list, function(i){
  imp_nom <- varImp(i)
  plot_imp_nom <- ggplot(imp_nom, aes(x = nom_imp[,1])) +
    geom_bar(stat = "identity", fill = "orange") +
    ggtitle(i$modelInfo$label) +
    theme_minimal()
})

grid.arrange(grobs=var_imp_plots, 
             top = textGrob('Variable Importance' ,gp=gpar(fontsize=20)))
