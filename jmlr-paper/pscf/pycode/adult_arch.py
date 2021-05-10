class ClassNet(nn.Module):
    def __init__(self):
        super(ClassNet, self).__init__()

        self.fc1 = nn.Linear(23, 100)
        self.fc2 = nn.Linear(100, 2)


    def forward(self, x):
        x = torch.tanh(self.fc1(x))
        return self.fc2(x)

class VAE_M(nn.Module):
    def __init__(self):
        super(VAE_M, self).__init__()

        self.el1 = nn.Linear(23, 20)
        self.el2 = nn.Linear(20, 20)
        self.el31 = nn.Linear(20, 10)
        self.el32 = nn.Linear(20, 10)

        self.dl1 = nn.Linear(13, 100)
        self.dl2 = nn.Linear(100, 2)

    def encode(self, x):
        h = torch.tanh(self.el1(x))
        h = torch.tanh(self.el2(h))

        return self.el31(h), self.el32(h)

    def reparameterize(self, mu, logvar):
        std = torch.exp(0.5*logvar)
        eps = torch.randn_like(std)
        return mu + eps*std

    def decode(self, dec_dat):
        dc = torch.tanh(self.dl1(dec_dat))
        return self.dl2(dc)

    def forward(self, x):
        mu, logvar = self.encode(x)
        z = self.reparameterize(mu, logvar)
        target = x[:, 3]
        attr = x[:, 0]
        decode_dat = torch.cat((x[:, :3], z), 1)
        return self.decode(decode_dat), target, mu, logvar, z, attr

class VAE_L(nn.Module):
    def __init__(self):
        super(VAE_L, self).__init__()

        self.el1 = nn.Linear(23, 20)
        self.el2 = nn.Linear(20, 20)
        self.el31 = nn.Linear(20, 10)
        self.el32 = nn.Linear(20, 10)

        self.dl1 = nn.Linear(14, 100)
        self.dl21 = nn.Linear(100, 1)

    def encode(self, x):
        h = torch.tanh(self.el1(x))
        h = torch.tanh(self.el2(h))

        return self.el31(h), self.el32(h)

    def reparameterize(self, mu, logvar):
        std = torch.exp(0.5*logvar)
        eps = torch.randn_like(std)
        return mu + eps*std

    def decode(self, dec_dat):
        dc = torch.tanh(self.dl1(dec_dat))
        return self.dl21(dc)

    def forward(self, x):
        mu, logvar = self.encode(x)
        z = self.reparameterize(mu, logvar)
        target = x[:, 4]
        attr = x[:, 0]
        decode_dat = torch.cat((x[:, :4], z), 1)
        L_mu = self.decode(decode_dat)
        return L_mu, target, mu, logvar, z, attr

class VAE_LS(nn.Module):
    def __init__(self):
        super(VAE_LS, self).__init__()

        self.el1 = nn.Linear(23, 20)
        self.el2 = nn.Linear(20, 20)
        self.el31 = nn.Linear(20, 10)
        self.el32 = nn.Linear(20, 10)

        self.dl1 = nn.Linear(14, 100)
        self.dl21 = nn.Linear(100, 1)
        self.dl22 = nn.Linear(100, 1)

    def encode(self, x):
        h = torch.tanh(self.el1(x))
        h = torch.tanh(self.el2(h))

        return self.el31(h), self.el32(h)

    def reparameterize(self, mu, logvar):
        std = torch.exp(0.5*logvar)
        eps = torch.randn_like(std)
        return mu + eps*std

    def decode(self, dec_dat):
        dc = torch.tanh(self.dl1(dec_dat))
        return self.dl21(dc), torch.exp(0.5*self.dl22(dc))

    def forward(self, x):
        mu, logvar = self.encode(x)
        z = self.reparameterize(mu, logvar)
        target = x[:, 4]
        attr = x[:, 0]
        decode_dat = torch.cat((x[:, :4], z), 1)
        L_mu, L_sigma = self.decode(decode_dat)
        return L_mu, L_sigma, target, mu, logvar, z, attr
