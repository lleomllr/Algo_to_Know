#Exemple de prise en main de glpsol et mathprog

#paramètres dont les valeurs sont dans le fichier .dat
param nbVariables;
param coefObj{1..nbVariables}; #attention à respecter cette dimension dans le .dat 
param valeursLimites{1..3}; #on aurait pu déclarer un nbContraintes
param coefVariables{1..3, 1..2};

#déclaration des variables utiles
var x{1..nbVariables};

#objectif à maximiser (utilisation d'une somme itérée)
maximize z: sum{i in 1..nbVariables} coefObj[i] * x[i];

#contraintes (on pourrait là aussi utiliser une double somme itérée)
subject to
	c1: coefVariables[1,1] *  x[1] + coefVariables[1,2] * x[2] <= valeursLimites[1];
	c2: coefVariables[2,1] *  x[1] + coefVariables[2,2] * x[2] <= valeursLimites[2];
	c3: coefVariables[3,1] *  x[1] + coefVariables[3,2] * x[2] <= valeursLimites[3];

#résolution
solve;

#affichage éventuel
display x[1], x[2], z;

data; 

param nbVariables := 2;

param coefObj :=
1 3
2 5;

param valeursLimites :=
1 4
2 6
3 18;

param coefVariables : 1 2 :=
1 1 0
2 0 1
3 3 2;

end;


