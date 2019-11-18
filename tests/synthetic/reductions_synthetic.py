import functools
import numpy as np
import pandas
import fairlearn.moments as moments
import fairlearn.classred as red

from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score
from importlib import reload

def reduce_and_predict(df1, df2, epsilon):
   # concatenate the data and clean it
  df = pandas.concat([df1, df2])
  ntrain = len(df1)
  ntest = len(df2)

  label_names = ['Y']
  protected_attribute_names = ['A']
  train_data = df.head(ntrain)
  test_data = df.tail(ntest)

  dataX = train_data.drop(columns = ['Y', 'A'])
  dataA = train_data['A']
  dataY = train_data['Y']

  X_test = test_data.drop(columns = ['Y', 'A'])
  learner = LogisticRegression()


  #moments = reload(moments)
  #red = reload(red)
  results_tuple = red.expgrad(dataX, dataA , dataY, learner, eps = epsilon)
  best_class = results_tuple.best_classifier

  Y_hat = best_class(X_test)
  Y_hat = Y_hat.to_frame()
  Y_hat = np.float64(Y_hat.as_matrix().ravel())

  return Y_hat
