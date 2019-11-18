import pandas
from aif360.datasets import BinaryLabelDataset
from aif360.datasets import AdultDataset, GermanDataset, CompasDataset
from aif360.metrics import BinaryLabelDatasetMetric
from aif360.metrics import ClassificationMetric
from aif360.algorithms.preprocessing.reweighing import Reweighing

from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score


def reweigh_and_predict(df1, df2):
  label_names = ['Y']
  protected_attribute_names = ['A']

  df = pandas.concat([df1, df2])
  ntrain = len(df1)
  ntest = len(df2)

  train_data = df.head(ntrain)
  test_data = df.tail(ntest)


  train_data = BinaryLabelDataset(df = train_data, label_names = label_names,
                                   protected_attribute_names = protected_attribute_names)
  test_data = BinaryLabelDataset(df = test_data, label_names = label_names,
                                   protected_attribute_names = protected_attribute_names)

  privileged_groups = [{'A': 0}]
  unprivileged_groups = [{'A': 1}]
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
  dataset_transf_test_pred.scores = lmod.predict_proba(X_test)[:,1:2].ravel()
  Y_hat = dataset_transf_test_pred.scores

  return Y_hat
