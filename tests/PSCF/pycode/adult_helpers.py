class UCIAdultDataset(Dataset):
    """UCI Adult."""

    def __init__(self, csv_file, root_dir, transform=None):
        """
        Args:
            csv_file (string): Path to the csv file with annotations.
            root_dir (string): Directory with all the images.
            transform (callable, optional): Optional transform to be applied
                on a sample.
        """
        self.adult_frame = pd.read_csv(csv_file)
        self.root_dir = root_dir
        self.transform = transform

    def __len__(self):
        return len(self.adult_frame)

    def __getitem__(self, idx):
        if torch.is_tensor(idx):
            idx = idx.tolist()

        adult = self.adult_frame.iloc[idx,:]
        sample = torch.from_numpy(adult.values)

        if self.transform:
            sample = self.transform(sample)

        return sample

def gaussian_kernel(a, b):
    dim1_1, dim1_2 = a.shape[0], b.shape[0]
    depth = a.shape[1]
    a = a.view(dim1_1, 1, depth)
    b = b.view(1, dim1_2, depth)
    a_core = a.expand(dim1_1, dim1_2, depth)
    b_core = b.expand(dim1_1, dim1_2, depth)
    numerator = (a_core - b_core).pow(2).mean(2)/depth
    return torch.exp(-numerator)

def mmd_penalty(a, b):
    return gaussian_kernel(a, a).mean() + gaussian_kernel(b, b).mean() - 2*gaussian_kernel(a, b).mean()

CELloss = nn.CrossEntropyLoss(reduction='sum')

def loss_functionM(recon, target, mu, logvar, z, a, beta):
    BCE = CELloss(recon, target.long())

    # see Appendix B from VAE paper:
    # Kingma and Welling. Auto-Encoding Variational Bayes. ICLR, 2014
    # https://arxiv.org/abs/1312.6114
    # 0.5 * sum(1 + log(sigma^2) - mu^2 - sigma^2)
    KLD = -0.5 * torch.sum(1 + logvar - mu.pow(2) - logvar.exp())

    MMD = beta * mmd_penalty(z[a == 0, :], z[a == 1, :])

    return BCE, KLD, MMD

def loss_functionL(L_mu, target, mu, logvar, z, a, beta):

    NGLL1 = 0#torch.sum(torch.log(L_sigma*2.5))
    #NGLL2 = 1 / 2 * torch.sum(((target - L_mu) / L_sigma).pow(2))
    NGLL2 = torch.sum((target - L_mu).pow(2))
    # see Appendix B from VAE paper:
    # Kingma and Welling. Auto-Encoding Variational Bayes. ICLR, 2014
    # https://arxiv.org/abs/1312.6114
    # 0.5 * sum(1 + log(sigma^2) - mu^2 - sigma^2)
    KLD = (-0.5 * torch.sum(1 + logvar - mu.pow(2) - logvar.exp()))

    MMD = beta * mmd_penalty(z[a == 0, :], z[a == 1, :])

    return NGLL1, NGLL2, KLD, MMD

def loss_functionLS(L_mu, L_sigma, target, mu, logvar, z, a, beta):

    NGLL1 = torch.sum(torch.log(L_sigma*2.5))
    NGLL2 = 1 / 2 * torch.sum(((target - L_mu) / L_sigma).pow(2))
    #NGLL2 = torch.sum((target - L_mu).pow(2))
    # see Appendix B from VAE paper:
    # Kingma and Welling. Auto-Encoding Variational Bayes. ICLR, 2014
    # https://arxiv.org/abs/1312.6114
    # 0.5 * sum(1 + log(sigma^2) - mu^2 - sigma^2)
    KLD = (-0.5 * torch.sum(1 + logvar - mu.pow(2) - logvar.exp()))

    MMD = beta * mmd_penalty(z[a == 0, :], z[a == 1, :])

    return NGLL1, NGLL2, KLD, MMD
