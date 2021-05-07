import pandas
from aif360.datasets import BinaryLabelDataset
from aif360.datasets import AdultDataset, GermanDataset, CompasDataset
from aif360.metrics import BinaryLabelDatasetMetric
from aif360.metrics import ClassificationMetric
from aif360.algorithms.preprocessing.reweighing import Reweighing

from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score

import warnings
warnings.filterwarnings("ignore", category=FutureWarning)

def reweigh_and_predict(df1, df2):
  # concatenate the data and clean it
  df = pandas.concat([df1, df2])
  ntrain = len(df1)
  ntest = len(df2)

  #df = pandas.read_csv("UCIAdult.csv")
  df = pandas.get_dummies(df, prefix = ['income', 'sex', 'native_country', 'marital_status',\
                            'workclass', 'occupation'], drop_first = True)
  df = df.rename(columns = {'income_>50K':'income', 'sex_Female':'sex', 'native_country_United-States':'native_country',\
  'marital_status_Not-Married':'marital_status'})
  #df = df.drop(columns = ['Unnamed: 0'])
  # set up the BinaryLabelDataset
  label_names = ['income']
  protected_attribute_names = ['sex']
  train_data = df.head(ntrain)
  test_data = df.tail(ntest)


  train_data = BinaryLabelDataset(df = train_data, label_names = label_names,
                                   protected_attribute_names = protected_attribute_names)
  test_data = BinaryLabelDataset(df = test_data, label_names = label_names,
                                   protected_attribute_names = protected_attribute_names)

  privileged_groups = [{'sex': 1}]
  unprivileged_groups = [{'sex':0}]
  RW = Reweighing(unprivileged_groups=unprivileged_groups,
                 privileged_groups=privileged_groups)
  RW.fit(train_data)
  dataset_transf_train = RW.transform(train_data)

  scale_transf = StandardScaler()
  X_train = scale_transf.fit_transform(dataset_transf_train.features)
  y_train = dataset_transf_train.labels.ravel()

  lmod = LogisticRegression()
  lmod.fit(X_train, y_train,
        sample_weight=dataset_transf_train.instance_weights)
  y_train_pred = lmod.predict(X_train)

  dataset_transf_test_pred = test_data
  X_test = scale_transf.fit_transform(dataset_transf_test_pred.features)
  y_test = dataset_transf_test_pred.labels
  dataset_transf_test_pred.scores = lmod.predict(X_test)
  Y_hat = dataset_transf_test_pred.scores

  return Y_hat
