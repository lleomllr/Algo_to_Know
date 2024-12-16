param n;  # Nombre de plats
param jours;   # Jours disponibles pour la production

# Paramètres pour chaque plat
param Demande{1..n};
param PrixVente{1..n};
param CoutProd{1..n};

param QuotaProd{1..n};

param CoutActivProd{1..n}; 

var c{i in 1..n} >= 0, <= Demande[i]; #Quantité de plats cuisinés 
var y{i in 1..n}, binary; #Si ligne de prod activée pour le plat i alors 1, sinon 0

maximize Gain : 
    sum{i in 1..n} ((PrixVente[i] - CoutProd[i]) * c[i]) - sum{i in 1..n} (CoutActivProd[i] * y[i]);

s.t. JourProduction:
    sum{i in 1..n} (c[i] / QuotaProd[i]) <= jours;  # Temps total de production disponible

s.t. Activation{i in 1..n}:
    c[i] <= Demande[i] * y[i];  # Si une ligne n'est pas activée, aucune production pour ce plat

s.t. MinimumActivation{i in 1..n}:
    c[i] >= 0; 

solve; 

display c, y, Gain; 

data; 

param n := 3;  # Nombre de plats
param jours := 30;   # Jours disponibles pour la production

# Paramètres pour chaque plats
param Demande :=
    1 4321
    2 3948
    3 5463;

param PrixVente :=
    1 111
    2 112
    3 113;

param CoutProd :=
    1 75
    2 65
    3 55;

param QuotaProd :=
    1 500
    2 450
    3 350; 


param CoutActivProd :=
    1 1234
    2 1345
    3 1456; 

end; 


