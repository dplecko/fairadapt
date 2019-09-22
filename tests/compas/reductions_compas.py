import functools
import numpy as np
import pandas
import fairlearn.moments as moments
import fairlearn.classred as red

from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score

def reduce_and_predict(df1, df2, epsilon):
  # concatenate the data and clean it
  df = pandas.concat([df1, df2])
  ntrain = len(df1)
  ntest = len(df2)
  '''
  df = pandas.read_csv("compas.csv")
  ntrain = 5000
  ntest = 2214
  epsilon = 0.01
  '''
  df = pandas.get_dummies(df, prefix = ['sex', 'race', 'c_charge_degree'], drop_first = True)
  df = df.rename(columns = {'race_Non-White':'race', 'sex_Male':'sex', 'c_charge_degree_M':'charge_degree'})
  df = df.astype('int64')
  # set up the BinaryLabelDataset
  label_names = ['two_year_recid']
  protected_attribute_names = ['race']
  train_data = df.head(ntrain)
  test_data = df.tail(ntest)


  dataX = train_data.drop(columns = ['two_year_recid', 'race'])
  dataA = train_data['race']
  dataY = train_data['two_year_recid']
  X_test = test_data.drop(columns = ['two_year_recid', 'race'])
  learner = LogisticRegression()


  results_tuple = red.expgrad(dataX, dataA , dataY, learner, eps = epsilon)
  best_class = results_tuple.best_classifier

  Y_hat = best_class(X_test) >= 0.5
  Y_hat = Y_hat.to_frame()
  Y_hat = np.int64(Y_hat.as_matrix().ravel())

  return Y_hat
