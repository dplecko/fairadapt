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

py_folder = os.path.join(os.getcwd(), "tests", "PSCF", "pycode")

parser = argparse.ArgumentParser(description='VAE MNIST Example')
parser.add_argument('--batch-size', type=int, default=128, metavar='N',
                    help='input batch size for training (default: 128)')
parser.add_argument('--epochs', type=int, default=20, metavar='N',
                    help='number of epochs to train (default: 20)')
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

exec(open(os.path.join(py_folder, "compas_helpers.py")).read())
exec(open(os.path.join(py_folder, "compas_arch.py")).read())

train_dataset = CompasDataset(
    csv_file=os.path.join(py_folder, "..", "data", "compas_train.csv"),
    root_dir=os.path.join(py_folder, "..", "PSCF", "data"), transform=None
)

test_dataset = CompasDataset(
    csv_file=os.path.join(py_folder, "..", "data", "compas_test.csv"),
    root_dir=os.path.join(py_folder, "..", "data"), transform=None
)

trainloader = DataLoader(train_dataset, batch_size=128,
                        shuffle=True, num_workers=0)

testloader = DataLoader(test_dataset, batch_size=1,
                        shuffle=False, num_workers=0)


cnet = ClassNet().to(device)
optimizer = optim.Adam(cnet.parameters(), lr=1e-2)
criterion = nn.CrossEntropyLoss()

start_time = time.time()
for epoch in range(20):  # loop over the dataset multiple times

    running_loss = 0.0
    for i, data in enumerate(trainloader, 0):
        # get the inputs; data is a list of [inputs, labels]
        _, inputs, labels = torch.split(data, (1, 6, 1), 1)
        inputs = inputs.float()
        labels.float()
        labz = torch.squeeze(labels)

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
        _, inputs, labels = torch.split(data, (1, 6, 1), 1)
        inputs = inputs.float()
        labels.float()
        labz = torch.squeeze(labels)

        outputs = cnet(inputs)

        _, predicted = torch.max(outputs.data, 1)
        total += labels.size(0)
        correct += (predicted == labz).sum().item()

100 * correct / total

def trainJ(epoch):
    modelJ.train()
    train_loss = 0
    train_ngll = 0
    train_kl = 0
    train_mmd = 0
    tot_sum = 0
    for batch_idx, data in enumerate(trainloader, 0):
        data = data.to(device)

        optimizerJ.zero_grad()

        _, inputs, _ = torch.split(data, (1, 6, 1), 1)
        inputs = inputs.float()


        recon_batch, target, mu, logvar, z, a = modelJ(inputs)
        tot_sum += target.sum()
        #pdb.set_trace()
        l_ngll, kl, mmd = loss_functionJP(recon_batch.squeeze(), target, mu, logvar, mu, a, beta)

        loss = l_ngll + kl + mmd

        loss.backward()

        train_loss += loss.item()

        train_ngll += l_ngll
        train_kl += kl
        train_mmd += mmd

        optimizerJ.step()
        # if batch_idx % args.log_interval == 0:
        #     print('Train Epoch: {} [{}/{} ({:.0f}%)]\tLoss: {:.6f}'.format(
        #         epoch, batch_idx * len(data), len(trainloader.dataset),
        #         100. * batch_idx / len(trainloader),
        #         loss.item() / len(data)))
    print('Likelihood {:.6f}, KL {:.6f}, MMD {:.6f}'.format(
                train_ngll/(batch_idx+1), train_kl/(batch_idx+1), train_mmd/(batch_idx+1)))
    print('====> Epoch: {} Average loss: {:.4f}'.format(
          epoch, train_loss / len(trainloader.dataset)))

def trainP(epoch):
    modelP.train()
    train_loss = 0
    train_ngll = 0
    train_kl = 0
    train_mmd = 0
    tot_sum = 0
    for batch_idx, data in enumerate(trainloader, 0):
        data = data.to(device)

        optimizerP.zero_grad()

        _, inputs, _ = torch.split(data, (1, 6, 1), 1)
        inputs = inputs.float()

        recon_batch, target, mu, logvar, z, a = modelP(inputs)
        l_ngll, kl, mmd = loss_functionJP(recon_batch.squeeze(), target, mu, logvar, mu, a, beta)

        tot_sum += target.sum()

        if batch_idx == 1 and False:
            print(target)
            print(target.mean())
            print(recon_batch.squeeze().mean())

        loss = l_ngll + kl + mmd

        loss.backward()

        train_loss += loss.item()

        train_ngll += l_ngll
        train_kl += kl
        train_mmd += mmd

        optimizerP.step()
        # if batch_idx % args.log_interval == 0:
        #     print('Train Epoch: {} [{}/{} ({:.0f}%)]\tLoss: {:.6f}'.format(
        #         epoch, batch_idx * len(data), len(trainloader.dataset),
        #         100. * batch_idx / len(trainloader),
        #         loss.item() / len(data)))
    print('Likelihood {:.6f}, KL {:.6f}, MMD {:.6f}'.format(
                train_ngll/(batch_idx+1), train_kl/(batch_idx+1), train_mmd/(batch_idx+1)))
    print('====> Epoch: {} Average loss: {:.4f}'.format(
          epoch, train_loss / len(trainloader.dataset)))

beta_seq = np.array([0, 10, 100, 1000])
predict_both = True

for k in range(len(beta_seq)):
    beta = beta_seq[k]

    start_time_train = time.time()

    modelJ = VAE_J().to(device)
    optimizerJ = optim.Adam(modelJ.parameters(), lr=1e-2)

    if __name__ == "__main__":
        for epoch in range(1, args.epochs + 1):
            beta = beta_seq[k]
            if epoch <= -1: beta = 0
            trainJ(epoch)

    modelP = VAE_P().to(device)
    optimizerP = optim.Adam(modelP.parameters(), lr=1e-2)

    if __name__ == "__main__":
        for epoch in range(1, args.epochs + 1):
            beta = beta_seq[k]
            if epoch <= -1: beta = 0
            trainP(epoch)

    ### sampling and predicting
    test_predictions = torch.Tensor()
    with torch.no_grad():
        for data in testloader:
            _, inputs, labels = torch.split(data, (1, 6, 1), 1)
            inputs = inputs.float()
            labels.float()
            labz = torch.squeeze(labels)

            if inputs[0, 0] == 1 and (not predict_both): # if Male simply predict
                outputs = cnet(inputs)
                pred = F.softmax(outputs)[:, 1]
                test_predictions = torch.cat((test_predictions, pred))
            else: # if Female do counterfactual inference
                inputs[0, 0] = 1 # swap the gender from Female to Male (if necessary)

                mu, logvar = modelJ.encode(inputs) # get the posterior q(M | V*)

            # sample from the posterior
                parents_J = inputs[:, :3]
                inputs_expand = inputs
                post_J = torch.normal(mu, logvar).view(1, -1)
                for i in range(499):
                    inputs_expand = torch.cat((inputs_expand, inputs), 0)
                    parents_J = torch.cat((parents_J, inputs[:, :3]))
                    post_J = torch.cat((post_J, torch.normal(mu, logvar).view(1, -1)), dim = 0)

            # decode sample + posterior samples & sample from CF distribution

                #J_CF = modelJ.decode(torch.cat((parents_J, post_J), dim = 1))
                #prob_J = modelJ.decode(torch.cat((parents_J, post_J), dim = 1))
                #prob_J = torch.exp(prob_J) / (1+torch.exp(prob_J))
                #geom_J = torch.distributions.geometric.Geometric(prob_J)
                J_CF = modelJ.decode(torch.cat((parents_J, post_J), dim = 1)).squeeze()#geom_J.sample()
                J_CF = torch.round(J_CF)
                J_CF[J_CF < 0] = 0

                #print(J_CF.mean())
                mu_P, logvar_P = modelP.encode(inputs) # get the posterior q(L | V*)

            # sample from the posterior
                post_P = torch.normal(mu_P, logvar_P).view(1, -1)
                #pdb.set_trace()
                for i in range(499):
                    post_P = torch.cat((post_P, torch.normal(mu_P, logvar_P).view(1, -1)), dim = 0)

                parents_P = torch.cat((parents_J, J_CF.view(-1, 1)), 1)

            # decode sample + posterior samples
                #prob_P = modelP.decode(torch.cat((parents_P, post_P), dim = 1))
                #prob_P = torch.exp(prob_P) / (1+torch.exp(prob_P))

            # sample from CF distribution
                #geom_P = torch.distributions.geometric.Geometric(prob_P)
                P_CF = modelP.decode(torch.cat((parents_P, post_P), dim = 1)).squeeze()#geom_P.sample()#
                P_CF = torch.round(P_CF)
                P_CF[P_CF < 0] = 0
            # merge all the CF data together

                inputs_expand[:, 3] = J_CF.squeeze()
                inputs_expand[:, 4] = P_CF.squeeze()

            # make a prediction using cnet
                pred = F.softmax(cnet(inputs_expand))[:, 1].mean()
                test_predictions = torch.cat((test_predictions, pred.view(-1)))

    ### put predictions in numpy
    print("--- %s seconds ---" % (time.time() - start_time_train))

    pred_path = os.path.join(py_folder, "..", "pred", "compas_pred" + str(beta) + ".csv")
    np.savetxt(pred_path, test_predictions.numpy(), delimiter=",")
