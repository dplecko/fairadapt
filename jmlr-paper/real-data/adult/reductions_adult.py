import functools
import numpy as np
import pandas
import fairlearn.moments as moments
import fairlearn.classred as red

from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score
from importlib import reload

import warnings
warnings.filterwarnings("ignore", category=FutureWarning)

def reduce_and_predict(df1, df2, epsilon):
   # concatenate the data and clean it
  df = pandas.concat([df1, df2])
  ntrain = len(df1)
  ntest = len(df2)
  '''
  df = pandas.read_csv("UCIAdult.csv")
  ntrain = 18000
  ntest = 7839
  df.drop(columns = ['Unnamed: 0'])
  '''
  df = pandas.get_dummies(df, prefix = ['income', 'sex', 'native_country', 'marital_status',\
    'workclass', 'occupation'], drop_first = True)
  df = df.rename(columns = {'income_>50K':'income', 'sex_Female':'sex', 'native_country_United-States':'native_country',\
    'marital_status_Not-Married':'marital_status'})
  df = df.astype('int64')
  label_names = ['income']
  protected_attribute_names = ['sex']
  train_data = df.head(ntrain)
  test_data = df.tail(ntest)

  dataX = train_data.drop(columns = ['income', 'sex'])
  dataA = train_data['sex']
  dataY = train_data['income']

  X_test = test_data.drop(columns = ['income', 'sex'])
  learner = LogisticRegression()


  #moments = reload(moments)
  #red = reload(red)
  results_tuple = red.expgrad(dataX, dataA , dataY, learner, eps = epsilon)
  best_class = results_tuple.best_classifier

  Y_hat = best_class(X_test) >= 0.5
  Y_hat = Y_hat.to_frame()
  Y_hat = np.int64(Y_hat.as_matrix().ravel())

  return Y_hat
