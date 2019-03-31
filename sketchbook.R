

tree_nas <- rpart(Risk ~., credits)

rpart.plot(tree_nas)

tree_wnas <- rpart(Risk ~., credits_wna)
rpart.plot(tree_wnas)

task_nas <- makeClassifTask(id = "with_nas",
                            data = credits,
                            target = "Risk",
                            positive = "bad")

task_wnas <- makeClassifTask(id = "without_nas",
                             data = credits_wna,
                             target = "Risk",
                             positive = "bad")

classif_rpart = makeLearner('classif.rpart', predict.type = 'prob', par.vals = list(cp = 0.00001))

rdesc <- makeResampleDesc('CV', predict = 'both', stratify = TRUE, iters = 10)

res_nas <- resample(learner = classif_rpart,
                    task = task_nas,
                    resampling = rdesc,
                    measures = list(tpr, tnr, auc, acc))

res_wnas <- resample(learner = classif_rpart,
                     task = task_wnas,
                     resampling = rdesc,
                     measures = list(tpr, tnr, auc, acc))
