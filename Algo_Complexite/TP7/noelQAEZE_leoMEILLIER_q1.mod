param n;  # Nombre de plats
param jours;   # Jours disponibles pour la production

# Paramètres pour chaque plat
param Demande{1..n};
param PrixVente{1..n};
param CoutProd{1..n};


var c{i in 1..n} >= 0, <= Demande[i]; #Quantité de plats cuisinés 

#Sans prendre en compte le quota donc les jours de production
maximize Gain : 
    sum{i in 1..n} ((PrixVente[i] - CoutProd[i]) * c[i]); 

solve; 

display c, Gain; 

#Initialisation des données
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

end; 
