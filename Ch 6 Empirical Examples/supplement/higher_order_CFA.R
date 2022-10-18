library(MIIVsem)
library(lavaan)

## collect data
colom.n <- 198
lower.corr <- '
1
0.21	1
0.24	0.11	1
0.34	0.14	0.07	1
0.42	0.12	0.09	0.40	1
0.41	0.19	0.16	0.50  0.49	1
0.19	0.19	0.17	0.18	0.20  0.26	1
0.19	0.22	0.09	0.13	0.04	0.22	0.43	1
0.32	0.17	0.04	0.06	0.16	0.29	0.21	0.39	1
0.30  0.14	0.26	0.20  0.13	0.26	0.28	0.20  0.25	1
0.35	0.23	0.30  0.19	0.06	0.21	0.20  0.32	0.35	0.39	1
0.32	0.29	0.12	0.10  0.17	0.24	0.18	0.42	0.33	0.30  0.49	1
'
mean <- '26.3  30.1  8.9  39.6  37.8  38.3  35  35.2  22.9  15.7  22.4  23.9'
std <- '10.3  9.6  8.6  10.9  9.6  10.5  6.9  6.7  6.5  3.4  10.6  4.6'
labels <- c('z1', 'z2', 'z3', 'z5', 'z6', 'z4',
            'z8', 'z7', 'z9', 'z11', 'z10', 'z12')

colom.corr <- getCov(lower.corr, names=labels)
colom.mean <- as.numeric(strsplit(mean, '  ')[[1]]); names(colom.mean) <- labels
colom.std <- as.numeric(strsplit(std, '  ')[[1]]); names(colom.std) <- labels

# sigma_i * sigma_j * rho_ij = Cov(X_i, X_j)
colom.cov <- (colom.std %*% t(colom.std)) * colom.corr
dimnames(colom.cov) <- list(labels,labels)

## Specify the model
colom.mod <- '
L1 =~ L2 + L3 + L4 + L5
# measurements
L2 =~ z1 + z2 + z3
L3 =~ z4 + z5 + z6
L4 =~ z7 + z8 + z9
L5 =~ z10 + z11 + z12
'

## ML estimates
colom.fit <- sem(colom.mod,
    sample.cov = colom.cov, sample.mean = colom.mean,
    sample.nobs = colom.n)
fitmeasures(colom.fit)
summary(colom.fit, standardized = TRUE)


## 2SLS estimates
colom.miiv <- '
z4 ~ z9 + z10
z7 ~ z6 + z10
z10 ~ z6 + z9
z2 ~ z3 + z6
z3 ~ z2 + z6
z5 ~ z6 + z1
z6 ~ z5 + z9
z8 ~ z9 + z12
z9 ~ z8 + z12
z11 ~ z12 + z3
z12 ~ z11 + z9
'

miive(model = colom.mod, instruments = colom.miiv,
      sample.cov = colom.cov, sample.mean = colom.mean,
      sample.nobs = colom.n)

