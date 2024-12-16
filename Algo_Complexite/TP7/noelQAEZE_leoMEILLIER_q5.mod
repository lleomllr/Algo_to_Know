param n;  # Nombre de plats
param jours;   # Jours disponibles pour la production

# Paramètres pour chaque produit
param Demande{1..n};
param PrixVente{1..n};
param CoutProd{1..n};

param QuotaProd{1..n};

param CoutActivProd{1..n}; 

param ProdMinProduit{1..n}; 

var c{i in 1..n} >= 0, <= Demande[i]; #Quantité de plats cuisinés 
var y{i in 1..n}, binary; #Si ligne de prod activée pour le plat i alors 1, sinon 0

maximize Gain : 
    sum{i in 1..n} ((PrixVente[i] - CoutProd[i]) * c[i]) - sum{i in 1..n} (CoutActivProd[i] * y[i]);

s.t. JourProduction:
    sum{i in 1..n} (c[i] / QuotaProd[i]) <= jours;  # Temps total de production disponible

s.t. Activation{i in 1..n}:
    c[i] <= Demande[i] * y[i];  # Si une ligne n'est pas activée, aucune production pour ce plat

s.t. MinimumActivation{i in 1..n}:
    c[i] >= ProdMinProduit[i] * y[i]; 

solve; 

display c, y, Gain; 

#Appel data avec le fichier data.dat
 


