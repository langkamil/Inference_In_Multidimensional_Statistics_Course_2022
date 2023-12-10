# Autor: Kamil Langowski
# Nr indeksu: 176553
# Kierunek: Matematyka
# Specjalizacja: Analityk Danych 
# Poziom ksztalcenia: Studia II stopnia
# Semestr studiow: 2



# Zadanie 1 ---------------------------------------------------------------

# 1.1 
# wczytanie danych
data1 <- read.csv("C:\\Users\\Kamil\\Documents\\Matematyka st. II\\Matematyka sem. II st. II\\Wnioskowanie w Wielowymiarowej Statystyce\\projekt\\projekt_2\\dane_zad1_edited.csv",
                   header = T,
                   sep = ';')

# wymiar danych
dim(data1)
str(data1)

# wykres
plot(data1, pch = 20, cex = 1)

# 1.2

# petla for k = 1:10   
# zapisujemy wartosc calkowitej zmiennosci wewnatrzgrupowej
total_within_clusters_sum_squares <- c()

# wykonujemy algorytm k-means dla k = 1:10
for(k in 1:10) {
  k_means_1_10 = kmeans(x = data1, centers = k)
  total_within_clusters_sum_squares[k] = k_means_1_10$tot.withinss
}

# wykres osypiska 
library(ggplot2)
qplot(1:10, total_within_clusters_sum_squares) +
  geom_line() +
  geom_text(aes(label = round(total_within_clusters_sum_squares, 1))) +
  xlab('liczba klastrów') +
  ylab('wartość całkowitej zmienności wewnątrzgrupowej') +
  scale_x_continuous(breaks = 1:10) 

# to samo za pomoca wbudowanej funkcji
library(ClusterR)
Optimal_Clusters_KMeans(data = data1, max_clusters = 10, criterion = 'WCSSE')

# grupowanie dla liczby klastrow k = 2
k_means_2 <- kmeans(x = data1, centers = 2)

data1_k2 <- data1
data1_k2$grupy <- as.factor(k_means_2$cluster)

ggplot(data1_k2,
       aes(x = V1, y = V2, color = grupy)) +
  xlab('V1') +
  ylab('V2') +
  geom_point(aes(x = k_means_2$centers[1,1], y = k_means_2$centers[1,2]),
             colour = 'red', size = 5) + 
  geom_point(aes(x = k_means_2$centers[2,1], y = k_means_2$centers[2,2]),
             colour = 'blue', size = 5) +
  geom_point() 
  
# grupowanie dla liczby klastrow k = 5
k_means_5 <- kmeans(x = data1, centers = 5)

data1_k5 <- data1
data1_k5$grupy <- as.factor(k_means_5$cluster)

ggplot(data1_k5,
       aes(x = V1, y = V2, color = grupy)) +
  xlab('V1') +
  ylab('V2') +
  geom_point(aes(x = k_means_5$centers[1,1], y = k_means_5$centers[1,2]),
             colour = 'red', size = 5) + 
  geom_point(aes(x = k_means_5$centers[2,1], y = k_means_5$centers[2,2]),
             colour = 'green', size = 5) +
  geom_point(aes(x = k_means_5$centers[3,1], y = k_means_5$centers[3,2]),
             colour = 'aquamarine3', size = 5) + 
  geom_point(aes(x = k_means_5$centers[4,1], y = k_means_5$centers[4,2]),
             colour = 'deepskyblue', size = 5) +
  geom_point(aes(x = k_means_5$centers[5,1], y = k_means_5$centers[5,2]),
             colour = 'purple', size = 5) +  
  geom_point() 

# 1.3 
# zmienne biegunowe 
data1_transformed <- data.frame(r = sqrt(data1$V1^2 + data1$V2^2),
                                theta = atan(data1$V2/data1$V1))
plot(data1_transformed, pch = 20, cex = 1)

# optymalna liczba klastrow k
Optimal_Clusters_KMeans(data = data1_transformed, max_clusters = 10, criterion = 'WCSSE')
# k = 2

# algortm k-means dla k = 2
k_means_2_transformed <- kmeans(x = data1_transformed, centers = 2)

data1_transformed$grupy <- as.factor(k_means_2_transformed$cluster)

# wykres 
ggplot(data1,
       aes(x = V1, y = V2, color = data1_transformed$grupy)) +
  xlab('V1') +
  ylab('V2') +
  geom_point(aes(x = k_means_2_transformed$centers[1,1], y = k_means_2_transformed$centers[1,2]),
             colour = 'red', size = 5) + 
  geom_point(aes(x = k_means_2_transformed$centers[2,1], y = k_means_2_transformed$centers[2,2]),
             colour = 'blue', size = 5) +
  geom_point() 

# 1.4

# metoda pojedynczego wiazania
single_link <- hclust(dist(data1), method = 'single')
plot(single_link,
     sub = "",
     xlab = "",
     ylab = "Wysokość")

# wektor etykiet 
tree_single <- cutree(tree = single_link, k = 1:10)
# k=2
plot(data1, col = tree_single[,2], pch = 20, cex = 1)

# metoda pelnego wiazania
complete_link <- hclust(dist(data1), method = 'complete')
plot(complete_link,
     sub = "",
     xlab = "",
     ylab = "Wysokość")

# wektor etykiet 
tree_complete <- cutree(tree = complete_link, k = 1:10)
# k=2
plot(data1, col = tree_complete[,2], pch = 20, cex = 1)



# Zadanie 2 ---------------------------------------------------------------

# 2.1
library(DAAG)
data2 <- ais[,1:11]
dim(data2)
str(data2)
sex <- ais[,12]

# 2.2
# dane nieskalowane
data2_pca_no_scale <- prcomp(data2, center = F, scale. = F)

# wykres 2d
library(factoextra)

# PC1 i PC2
fviz_pca_ind(data2_pca_no_scale,
             axes = c(1, 2),
             geom = "point",
             title = "",
             xlab = "PC1",
             ylab = "PC2",
             col.ind = sex, 
             addEllipses = T)

# PC1 i PC3
fviz_pca_ind(data2_pca_no_scale,
             axes = c(1, 3),
             geom = "point",
             title = "",
             xlab = "PC1",
             ylab = "PC3",
             col.ind = sex, 
             addEllipses = T)

# PC2 i PC3
fviz_pca_ind(data2_pca_no_scale,
             axes = c(2, 3),
             geom = "point",
             title = "",
             xlab = "PC1",
             ylab = "PC3",
             col.ind = sex, 
             addEllipses = T)

# wykres 3d 
# kobiety 1 mezczyzni 2
levels(sex) <- c(1,2)
data2_pca_no_scale_sex <- cbind(data2_pca_no_scale$x[,1:3], sex)

library(rgl)
plot3d(x = data2_pca_no_scale_sex[,1],
       y = data2_pca_no_scale_sex[,2],
       z = data2_pca_no_scale_sex[,3],
       col = sex,
       xlab = 'PC1',
       ylab = 'PC2', 
       zlab = 'PC3'
       )

# 2.3 
# algorytm k-means na nieskalowanych danych
data2_no_scale_k_means <- kmeans(data2_pca_no_scale_sex[,1:2], 2)

data2_no_scale_k2 <- data.frame(data2_pca_no_scale_sex[,1:2])
data2_no_scale_k2$grupy <- as.factor(data2_no_scale_k_means$cluster)

# wykres
plot(x = data2_no_scale_k2$PC1,
     y = data2_no_scale_k2$PC2,
     pch = 20,
     col = as.numeric(as.factor(sex)),
     main ="",
     xlab = 'PC1',
     ylab = 'PC2')

legend("topright",
       legend = c("1","2"),
       pch = c(20,20),
       col = c('red','black'),
       cex = 0.7)


# 2.4
# PCA na skalowanych danych

data2_pca_scale <- prcomp(data2, center = T, scale. = T)

# PC1 i PC2
fviz_pca_ind(data2_pca_scale,
             axes = c(1, 2),
             geom = "point",
             title = "",
             xlab = "PC1",
             ylab = "PC2",
             col.ind = sex, 
             addEllipses = T)

# PC1 i PC3
fviz_pca_ind(data2_pca_scale,
             axes = c(1, 3),
             geom = "point",
             title = "",
             xlab = "PC1",
             ylab = "PC3",
             col.ind = sex, 
             addEllipses = T)

# PC2 i PC3
fviz_pca_ind(data2_pca_scale,
             axes = c(2, 3),
             geom = "point",
             title = "",
             xlab = "PC1",
             ylab = "PC3",
             col.ind = sex, 
             addEllipses = T)

# wykres 3d 
# kobiety 1 mezczyzni 2
levels(sex) <- c(1,2)
data2_pca_scale_sex <- cbind(data2_pca_scale$x[,1:3], sex)

plot3d(x = data2_pca_scale_sex[,1],
       y = data2_pca_scale_sex[,2],
       z = data2_pca_scale_sex[,3],
       col = sex,
       xlab = 'PC1',
       ylab = 'PC2', 
       zlab = 'PC3')

# 2.5
# kmeans na danych po pca (ze skalowaniem)

data2_scale_k_means <- kmeans(data2_pca_scale$x[,1:2],2)


data2_scale_k2 <- data.frame(data2_pca_scale_sex[,1:2])
data2_scale_k2$grupy <- as.factor(data2_scale_k_means$cluster)

plot(x = data2_scale_k2$PC1,
     y = data2_scale_k2$PC2,
     pch = data2_scale_k_means$cluster,
     col = as.numeric(as.factor(sex)),
     main ="",
     xlab = 'PC1',
     ylab = 'PC2')

legend("topright",
       legend = c("1","2"),
       pch = c(20,20),
       col = c('red','black'),
       cex = 0.7)
# Zadanie 3 ---------------------------------------------------------------

library(imager)
library(OpenImageR)
fpath <- system.file("extdata/parrots.png",package="imager")
papugi <- readImage(fpath)

# wyswietlenie obrazka
imageShow(papugi)

# zapisanie do png
writeImage(papugi,"papugi.png")

# 3.1
# zapis do ramki danych
papugi[,,1]

imgRGB <- data.frame(x = rep(1:dim(papugi)[2], each = dim(papugi)[1]),
                      y = rep(dim(papugi)[1]:1, times = dim(papugi)[2]),
                      R = as.vector(papugi[,,1]),
                      G = as.vector(papugi[,,2]),
                      B = as.vector(papugi[,,3]))
head(imgRGB)
tail(imgRGB)

# 3.2
# klasteryzacja dla k = {2, 16, 64}
papugi_k_means_2 <- kmeans(imgRGB[,3:5], 2)
papugi_k_means_16 <- kmeans(imgRGB[,3:5], 16)
papugi_k_means_64 <- kmeans(imgRGB[,3:5], 64)

# 3.3
# klastry
img_kod_2 <- cbind(imgRGB[,1:2], nr_klastra = papugi_k_means_2$cluster)
img_kod_16 <- cbind(imgRGB[,1:2], nr_klastra = papugi_k_means_16$cluster)
img_kod_64 <- cbind(imgRGB[,1:2], nr_klastra = papugi_k_means_64$cluster)

head(img_kod_2)
head(img_kod_16)
head(img_kod_64)

# kodowanie
centra_2 <- as.data.frame(cbind(nr_klastra = 1:2, papugi_k_means_2$centers))
centra_16 <- as.data.frame(cbind(nr_klastra = 1:16, papugi_k_means_16$centers))
centra_64 <- as.data.frame(cbind(nr_klastra = 1:64, papugi_k_means_64$centers))

head(centra_2)
head(centra_16)
head(centra_64)


# 3.4
# dekodowanie 
library(dplyr)

# laczenie
img_klastry_2 <- full_join(img_kod_2, centra_2)
img_klastry_16 <- full_join(img_kod_16, centra_16)
img_klastry_64 <- full_join(img_kod_64, centra_64)

# k=2
R2 <- matrix(img_klastry_2[,4],
            ncol = max(img_klastry_2[,1]),
            nrow = max(img_klastry_2[,2]),
            byrow = F)
G2 <- matrix(img_klastry_2[,5],
            ncol = max(img_klastry_2[,1]),
            nrow = max(img_klastry_2[,2]),
            byrow = F)
B2 <- matrix(img_klastry_2[,6],
            ncol = max(img_klastry_2[,1]),
            nrow = max(img_klastry_2[,2]),
            byrow = F)
img_final_2 <- array(c(R2, G2, B2), dim = c(max(img_klastry_2[,2]), max(img_klastry_2[,1]), 3))

# k = 16
R16 <- matrix(img_klastry_16[,4],
             ncol = max(img_klastry_16[,1]),
             nrow = max(img_klastry_16[,2]),
             byrow = F)
G16 <- matrix(img_klastry_16[,5],
             ncol = max(img_klastry_16[,1]),
             nrow = max(img_klastry_16[,2]),
             byrow = F)
B16 <- matrix(img_klastry_16[,6],
             ncol = max(img_klastry_16[,1]),
             nrow = max(img_klastry_16[,2]),
             byrow = F)
img_final_16 <- array(c(R16, G16, B16), dim = c(max(img_klastry_16[,2]), max(img_klastry_16[,1]), 3))

# k = 64
R64 <- matrix(img_klastry_64[,4],
             ncol = max(img_klastry_64[,1]),
             nrow = max(img_klastry_64[,2]),
             byrow = F)
G64 <- matrix(img_klastry_64[,5],
             ncol = max(img_klastry_64[,1]),
             nrow = max(img_klastry_64[,2]),
             byrow = F)
B64 <- matrix(img_klastry_64[,6],
             ncol = max(img_klastry_64[,1]),
             nrow = max(img_klastry_64[,2]),
             byrow = F)
img_final_64 <- array(c(R64, G64, B64), dim = c(max(img_klastry_64[,2]), max(img_klastry_64[,1]), 3))

dim(img_final_2)
dim(img_final_16)
dim(img_final_64)

# wyswietlenie 
imageShow(img_final_2)
imageShow(img_final_16)
imageShow(img_final_64)

# zapisanie 
writeImage(img_final_2, "papugi_2.png")
writeImage(img_final_16, "papugi_16.png")
writeImage(img_final_64, "papugi_64.png")

