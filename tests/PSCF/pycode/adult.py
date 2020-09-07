from __future__ import print_function
import argparse
import torch
import pdb
import os
import pandas as pd
import numpy as np
import time
import torch.utils.data
from torch import nn, optim
from torch.nn import functional as F
from torchvision import datasets, transforms
from torchvision.utils import save_image
from torch.utils.data import Dataset, DataLoader

os.chdir("/Users/pleckod/fairness/fairadapt")

parser = argparse.ArgumentParser(description='VAE MNIST Example')
parser.add_argument('--batch-size', type=int, default=128, metavar='N',
                    help='input batch size for training (default: 128)')
parser.add_argument('--epochs', type=int, default=30, metavar='N',
                    help='number of epochs to train (default: 30)')
parser.add_argument('--no-cuda', action='store_true', default=False,
                    help='enables CUDA training')
parser.add_argument('--seed', type=int, default=1, metavar='S',
                    help='random seed (default: 1)')
parser.add_argument('--log-interval', type=int, default=10, metavar='N',
                    help='how many batches to wait before logging training status')
args, unknown = parser.parse_known_args()
args.cuda = not args.no_cuda and torch.cuda.is_available()

torch.manual_seed(args.seed)

device = torch.device("cuda" if args.cuda else "cpu")

folder = "tests/PSCF/pycode/"

exec(open(folder + "adult_helpers.py").read())
exec(open(folder + "adult_arch.py").read())

train_dataset = UCIAdultDataset(csv_file='tests/PSCF/data/UCIAdult_train.csv',
                                    root_dir='tests/PSCF/data/', transform=None)


test_dataset = UCIAdultDataset(csv_file='tests/PSCF/data/UCIAdult_test.csv',
                                    root_dir='tests/PSCF/data/', transform=None)

trainloader = DataLoader(train_dataset, batch_size=128,
                        shuffle=True, num_workers=0)

testloader = DataLoader(test_dataset, batch_size=1,
                        shuffle=False, num_workers=0)


cnet = ClassNet().to(device)
optimizer = optim.Adam(cnet.parameters(), lr=1e-2)
criterion = nn.CrossEntropyLoss()

start_time = time.time()
for epoch in range(30):  # loop over the dataset multiple times

    running_loss = 0.0
    for i, data in enumerate(trainloader, 0):
        # get the inputs; data is a list of [inputs, labels]
        _, inputs, labels = torch.split(data, (1, 23, 1), 1)
        inputs = inputs.float()
        labels.float()
        labz = torch.squeeze(labels)

        #pdb.set_trace()

        # zero the parameter gradients
        optimizer.zero_grad()

        # forward + backward + optimize
        outputs = cnet(inputs)
        loss = criterion(outputs, labz)
        loss.backward()
        optimizer.step()

        # print statistics
        running_loss += loss.item()
        if i % 10 == 9:    # print every 2000 mini-batches
            print('[%d, %5d] loss: %.3f' %
                  (epoch + 1, i + 1, running_loss))
            running_loss = 0.0

print("--- %s seconds ---" % (time.time() - start_time))
print('Finished Training')


correct = 0
total = 0
with torch.no_grad():
    for data in testloader:
        inputs = torch.narrow(data, 1, 1, 23)
        inputs = inputs.float()

        labels = torch.narrow(data, 1, 24, 1)
        labels.float()
        labz = torch.squeeze(labels)

        outputs = cnet(inputs)

        _, predicted = torch.max(outputs.data, 1)
        total += labels.size(0)
        correct += (predicted == labz).sum().item()

100 * correct / total

def trainM(epoch):
    modelM.train()
    train_loss = 0
    train_ngll = 0
    train_kl = 0
    train_mmd = 0
    for batch_idx, data in enumerate(trainloader, 0):
        data = data.to(device)

        optimizerM.zero_grad()

        _, inputs, _ = torch.split(data, (1, 23, 1), 1)
        inputs = inputs.float()

        recon_batch, target, mu, logvar, z, a = modelM(inputs)
        #pdb.set_trace()
        l_ngll, kl, mmd = loss_functionM(recon_batch, target, mu, logvar, mu, a, beta)

        loss = l_ngll + kl + mmd

        loss.backward()

        train_loss += loss.item()

        train_ngll += l_ngll
        train_kl += kl
        train_mmd += mmd

        optimizerM.step()
        # if batch_idx % args.log_interval == 0:
        #     print('Train Epoch: {} [{}/{} ({:.0f}%)]\tLoss: {:.6f}'.format(
        #         epoch, batch_idx * len(data), len(trainloader.dataset),
        #         100. * batch_idx / len(trainloader),
        #         loss.item() / len(data)))
    print('Likelihood {:.6f}, KL {:.6f}, MMD {:.6f}'.format(
                train_ngll/(batch_idx+1), train_kl/(batch_idx+1), train_mmd/(batch_idx+1)))
    print('====> Epoch: {} Average loss: {:.4f}'.format(
          epoch, train_loss / len(trainloader.dataset)))

def trainL(epoch):
    modelL.train()
    train_loss = 0
    train_ngll1 = 0
    train_ngll2 = 0
    train_kl = 0
    train_mmd = 0
    for batch_idx, data in enumerate(trainloader):
        data = data.to(device)

        optimizerL.zero_grad()

        _, inputs, _ = torch.split(data, (1, 23, 1), 1)
        inputs = inputs.float()

        L_mu, target, mu, logvar, z, attr = modelL(inputs)


        l_ngll1, l_ngll2, kl, mmd = loss_functionL(L_mu.squeeze(), target, mu, logvar, mu, attr, beta)



        loss = l_ngll1 + l_ngll2 + kl + mmd


        loss.backward()
        train_loss += loss.item()

        train_ngll1 += l_ngll1
        train_ngll2 += l_ngll2
        train_kl += kl
        train_mmd += mmd

        optimizerL.step()
        # if batch_idx % args.log_interval == 0:
        #     print('Train Epoch: {} [{}/{} ({:.0f}%)]\tLoss: {:.6f}'.format(
        #         epoch, batch_idx * len(data), len(trainloader.dataset),
        #         100. * batch_idx / len(trainloader),
        #         loss.item() / len(data)))
    print('Likelihood ({:.6f}, {:.6f}), KL {:.6f}, MMD {:.6f}'.format(
          train_ngll1/(batch_idx+1), train_ngll2/(batch_idx+1), train_kl/(batch_idx+1), train_mmd/(batch_idx+1)))
    print('====> Epoch: {} Average loss: {:.4f}'.format(
          epoch, train_loss / len(trainloader.dataset)))
    print('Mean variance in this epoch is {:.6f}'.format(train_ngll2/len(trainloader.dataset)))

    return (train_ngll2/len(trainloader.dataset)).pow(1/2)



beta_seq = np.array([0, 10, 100, 1000])
predict_both = True

for k in range(len(beta_seq)):
    beta = beta_seq[k]

    start_time_train = time.time()


    modelM = VAE_M().to(device)
    optimizerM = optim.Adam(modelM.parameters(), lr=1e-2)

    if __name__ == "__main__":
        for epoch in range(1, args.epochs + 1):
            beta = beta_seq[k]
            if epoch <= -1: beta = 0
            trainM(epoch)

    modelL = VAE_L().to(device)
    optimizerL = optim.Adam(modelL.parameters(), lr=1e-2)

    if __name__ == "__main__":
        for epoch in range(1, args.epochs + 1):
            beta = beta_seq[k]
            if epoch <= -1: beta = 0
            sd_L = trainL(epoch)

    ### sampling and predicting
    test_predictions = torch.Tensor()
    with torch.no_grad():
        for data in testloader:
            _, inputs, labels = torch.split(data, (1, 23, 1), 1)
            inputs = inputs.float()
            labels.float()
            labz = torch.squeeze(labels)


            if inputs[0, 0] == 1 and (not predict_both): # if Male simply predict
                outputs = cnet(inputs)
                pred = F.softmax(outputs)[:, 1]
                test_predictions = torch.cat((test_predictions, pred))
            else: # if Female do counterfactual inference
                inputs[0, 0] = 1 # swap the gender from Female to Male (if necessary)

                mu, logvar = modelM.encode(inputs) # get the posterior q(M | V*)

            # sample from the posterior
                parents_M = inputs[:, :3]
                inputs_expand = inputs
                post_M = torch.normal(mu, logvar).view(1, -1)
                for i in range(499):
                    inputs_expand = torch.cat((inputs_expand, inputs), 0)
                    parents_M = torch.cat((parents_M, inputs[:, :3]))
                    post_M = torch.cat((post_M, torch.normal(mu, logvar).view(1, -1)), dim = 0)

            # decode sample + posterior samples & sample from CF distribution
                M_CF = torch.bernoulli(F.softmax(modelM.decode(torch.cat((parents_M, post_M), dim = 1)))[:, 1])

                mu_L, logvar_L = modelL.encode(inputs) # get the posterior q(L | V*)

            # sample from the posterior
                post_L = torch.normal(mu_L, logvar_L).view(1, -1)
                for i in range(499):
                    post_L = torch.cat((post_L, torch.normal(mu_L, logvar_L).view(1, -1)), dim = 0)

                parents_L = torch.cat((parents_M, M_CF.view(-1, 1)), 1)

            # decode sample + posterior samples
                mean_L = modelL.decode(torch.cat((parents_L, post_L), dim = 1))
            #print(torch.squeeze(mean_L))
            # sample from CF distribution
                L_CF = torch.randn(500)
                L_CF = torch.squeeze(mean_L) # + L_CF*sd_L

            # merge all the CF data together

                inputs_expand[:, 3] = M_CF
                inputs_expand[:, 4] = L_CF

            # make a prediction using cnet
                pred = F.softmax(cnet(inputs_expand))[:, 1].mean()
                test_predictions = torch.cat((test_predictions, pred.view(-1)))

    ### put predictions in numpy
    print("--- %s seconds ---" % (time.time() - start_time_train))

    pred_path = os.path.join("tests", "PSCF", "pred", "adult_pred" + str(beta) + ".csv")
    np.savetxt(pred_path, test_predictions.numpy(), delimiter=",")
