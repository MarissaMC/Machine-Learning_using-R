# confusion matrix

contrasts(wine$quality)
table(y_hat,testing_y)














#                        Prediction
#                  Buy(+)    Not Buy(-)   Total
#       Buy          a          b         a+b
#True
#       Not Buy      c          d         c+d
#
#total               a+c        b+d       a+b+c+d
#
# Misclassification error=(b+c)/(a+b+c+d)
#
# Correctly classified=(a+d)/(a+b+c+d)
#
# Sensitivity=a/(a+b)
#
# Specificity=d/(c+d)
#
#
#
# True positive=a/(a+b)
#
# False negative=b/(a+b)
#
# True negative=d/(c+d)
#
# False positive=c/(c+d)