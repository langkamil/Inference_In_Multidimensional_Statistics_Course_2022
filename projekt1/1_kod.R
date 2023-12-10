# Autor: Kamil Langowski
# Nr indeksu: 176553
# Kierunek: Matematyka
# Specjalizacja: Analityk Danych 
# Poziom ksztalcenia: Studia II stopnia
# Semestr studiow: 2 



# Czesc I -----------------------------------------------------------------

# -------------------------------------------------------------------------

# niezbedne pakiety
library(pacman)
pacman::p_load(ggplot2, rgl, car)


# Zadanie 1 ---------------------------------------------------------------

# odczyt pliku
breastcancer_data <- read.csv('danePCA.csv', header = T, sep = ';')

# 5 pierwszych obserwacji
head(breastcancer_data)
# 5 ostatnich obserwacji
tail(breastcancer_data)
# wymiar zbioru
dim(breastcancer_data)
# typy zmiennych
str(breastcancer_data)
# podstawowe statystyki
summary(breastcancer_data)

# wybor zmiennych
X <- breastcancer_data[, 3:32]



# Zadanie 2 ---------------------------------------------------------------

X <- as.matrix(X, header = T)

# wektor wartosci srednich
col_means <- function(data) t(apply(data, 2, sum)/nrow(data))
X_mean <- col_means(X)

# macierz kowariancji
cov_matrix <- function(data){
  data_means = col_means(data)
  diff_matrix = data - matrix(data = 1, nrow = nrow(data), ncol = 1) %*% data_means
  return((nrow(data)-1)^(-1) * t(data) %*% diff_matrix)
}
X_cov <- cov_matrix(X)

# odchylenie standardowe
standard_deviation <- function(data){
  data_means = col_means(data)
  diff_matrix = data - matrix(data = 1, nrow = nrow(data), ncol = 1) %*% data_means
  return(t(sqrt((apply(diff_matrix^2, 2, sum))/(nrow(diff_matrix)-1))))
}
  

# standaryzacja 
standardization <- function(data){
  data_means = col_means(data)
  data_means_matrix = matrix(data = 1, nrow = nrow(data), ncol = 1) %*% data_means
  diff_matrix = data - data_means_matrix
  data_sd = standard_deviation(diff_matrix)
  return(sweep(diff_matrix, 2, data_sd, FUN = '/'))
}

Y <- standardization(X)
scale(X)
Y_means <- col_means(Y)
Y_cov <- cov_matrix(Y)
# Zadanie 3 ---------------------------------------------------------------

# zagadnienie wlasne 
eval <- eigen(Y_cov)$values
evec <- eigen(Y_cov)$vectors

# Zadanie 4 ---------------------------------------------------------------

# wariancja to wartosci wlasne
PC_variance <- eval

# proporcja wariancji
PC_prop_variance <- PC_variance/sum(eval)

# skumulowana proporcja 
PC_cumulative_proportion <- cumsum(eval)/sum(eval)

# Zadanie 5 ---------------------------------------------------------------

#wariancja
qplot(1:30, eval) +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', size = 0.2) +
  geom_line() +
  xlab('skladowe glowne') +
  ylab('wariancja') +
  ylim(0, 14)

# wyjasniana wariancja 
qplot(1:30, PC_prop_variance) +
  geom_line() +
  xlab('skladowe golwne') +
  ylab('wyjasniana wariancja') +
  ylim(0, 0.5)

# Zadanie 6 ---------------------------------------------------------------

# korelacje

# r dla PC1
r_Y_Z1 <- c()
r_Y_Z2 <- c()
for (i in 1:30) {
  r_Y_Z1[i] = evec[i, 1] * sqrt(eval[1] / Y_cov[i,i])
  r_Y_Z2[i] = evec[i, 2] * sqrt(eval[2] / Y_cov[i,i])
}

# wspolrzedne
x_c <- 0
y_c <- 0
r <- 1

# wykres
qplot(r_Y_Z1, r_Y_Z2, col = 'red', show.legend = F) +
  xlab('PC1') +
  ylab('PC2') +
  ylim(-1, 1) +
  xlim(-1.5, 1.5) +
  geom_text(aes(label = c(1:30)), col = 'black') +
  annotate('path',
           x = x_c + r * cos(seq(0, 2*pi, length.out = 100)),
           y = y_c + r * sin(seq(0, 2*pi, length.out = 100)))


# Zadanie 7 ---------------------------------------------------------------

qplot(c(1:30), PC_cumulative_proportion)+
  geom_line() +
  xlab('skladowe glowne') +
  ylab('wyjasniana wariancja') +
  ylim(0.4,1.1) +
  geom_line(aes(x = 6, col = 'red'), linetype = 'dashed', show.legend = FALSE) +
  geom_line(aes(y = PC_cumulative_proportion[6], col = 'red'), linetype = 'dashed', show.legend = FALSE)


# Zadanie 8 ---------------------------------------------------------------

PC1_proj <- c()
PC2_proj <- c()
PC3_proj <- c()
for (i in 1:nrow(Y)) {
  PC1_proj[i] = sum(evec[, 1] * Y[i,])
  PC2_proj[i] = sum(evec[, 2] * Y[i,])
  PC3_proj[i] = sum(evec[, 3] * Y[i,])
}

# projekcja jednowymiarowa
stripchart(PC1_proj, col = 'red')

# projekcja dwuwymiarowa
plot(PC1_proj, PC2_proj, 
     col = c('red', 'green'), 
     ylim = c(-8,13), 
     xlim = c(-17,5))

# projekcja trojwymiarowa
plot3d(PC1_proj, PC2_proj, PC3_proj, 
       col = c('red', 'green', 'blue'), 
       ylim = c(-8,13),
       xlim = c(-17,5),
       zlim=c(-10,10))

# Zadanie 9 ---------------------------------------------------------------

# dane z typem nowotworu
diagnosis <- breastcancer_data[,2]
diagnosis_data <- data.frame(PC1_proj, PC2_proj, diagnosis)

# zlosliwe
M <- subset(diagnosis_data[,1:ncol(diagnosis_data)], diagnosis == "M")

# lagodne
B <- subset(diagnosis_data[,1:ncol(diagnosis_data)], diagnosis == "B")


# parametry elpis 
center_m <- c(1/nrow(M) * sum(M[,1]), 1/nrow(M) * sum(M[, 2]))
shape_m <- cov(M[,1:2])

center_b <- c(1/nrow(B) * sum(B[,1]), 1/nrow(B) * sum(B[,2]))
shape_b <- cov(B[,1:2])

plot(PC1_proj,
     PC2_proj,
     pch = 20,
     col = as.factor(diagnosis_data$diagnosis))
ellipse(center = center_m,
        shape = shape_m,
        radius = sqrt(qchisq(.95, df = 2)),
        col = 'red',
        fill = T)
ellipse(center = center_b,
        shape = shape_b,
        radius = sqrt(qchisq(.95, df = 2)),
        col = 'black',
        fill = T)
legend('topright',
       legend = c('zlosliwy', 'lagodny'),
       pch = c(20, 20),
       col = c('red', 'black'),
       cex = 1)


# Czesc II ----------------------------------------------------------------


# -------------------------------------------------------------------------

# pakiety
pacman::p_load(factoextra)

# Zadanie 1 ---------------------------------------------------------------

# tak samo jak w cz. I

# odczyt pliku
breastcancer_data <- read.csv('danePCA.csv', header = T, sep = ';')

# 5 pierwszych obserwacji
head(breastcancer_data)
# 5 ostatnich obserwacji
tail(breastcancer_data)
# wymiar zbioru
dim(breastcancer_data)
# typy zmiennych
str(breastcancer_data)
# podstawowe statystyki
summary(breastcancer_data)

# wybor zmiennych
X <- breastcancer_data[, 3:32]


# Zadanie 2 ---------------------------------------------------------------

# wektor srednich 
X_means_2 <- colMeans(X)

# macierz kowariancji 
X_cov_2 <- cov(X)

# wystandaryzowana i scentrowana macierz Y
Y_2 <- scale(X)

#wektor srednich dla Y
Y_means_2 <- colMeans(Y_2)

#macierz kowariancji 
Y_cov_2 <- cov(Y_2)

PCA <- prcomp(Y_2, center = TRUE, scale. = TRUE)

# Zadanie 3 ---------------------------------------------------------------

evec_1 <- eigen(Y_cov_2)$vectors
eval_2 <- eigen(Y_cov_2)$values

# Zadanie 4 ---------------------------------------------------------------

PCA_eigen = get_eigenvalue(PCA)

# Zadanie 5 ---------------------------------------------------------------

fviz_eig(PCA, choice = 'eigenvalue', geom = 'line', ncp = 30)
fviz_eig(PCA, choice = 'variance', geom = 'line', ncp = 30)

# Zadanie 6 ---------------------------------------------------------------

fviz_pca_var(PCA, geom = 'point', labelsize = 3)

# Zadanie 7 ---------------------------------------------------------------

#wykres skumulowanej proporcji

qplot(c(1:30), summary(PCA)$importance[3,])+
  geom_line() +
  xlab('skladowe glowne') +
  ylab('wyjasniana wariancja') +
  ylim(0.4,1.1) +
  geom_line(aes(x = 6, col = 'red'), linetype = 'dashed', show.legend = FALSE) +
  geom_line(aes(y = summary(PCA)$importance[3,][6], col = 'red'), linetype = 'dashed', show.legend = FALSE)

# Zadanie 8 ---------------------------------------------------------------

# dane dla PC2 musza byc przemnozone przez -1 celem uzyskania identycznych wynikow dla cz. I

# jednowymiarowa
stripchart(PCA$x[,1],
           col = 'red')

# dwuwymiarowa
plot(PCA$x[,1], -PCA$x[,2],
     col = c('red', 'green'),
     ylim = c(-8,13),
     xlim = c(-17,5))

# trojwymiarowa
plot3d(PCA$x[,1], -PCA$x[,2], PCA$x[,3],
       col = c('red', 'blue', 'green'),
       ylim = c(-8,13),
       xlim = c(-17,5),
       zlim=c(-10,10))


# Zadanie 9 ---------------------------------------------------------------

fviz_pca_ind(PCA,
             label = 'none', 
             habillage = breastcancer_data$diagnosis,
             addEllipses=TRUE, 
             ellipse.level = 0.95)
PCA_2 <- PCA
PCA_2$x[,2] <- -PCA_2$x[,2]

fviz_pca_ind(PCA_2,
             label = 'none', 
             habillage = breastcancer_data$diagnosis,
             addEllipses=TRUE, 
             ellipse.level = 0.95)

