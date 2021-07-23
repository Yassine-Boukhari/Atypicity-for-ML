library(ROCR)
fr <- data.frame(score = c(0.61, 0.36, 0.43, 0.14, 0.38, 0.24, 0.97, 0.89, 0.78, 0.86, 0.71, 0.36),
                 label = c(1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0))
pred <- prediction(fr$score, fr$label)
perf <- performance(pred, "tpr", "fpr")
plot(perf)