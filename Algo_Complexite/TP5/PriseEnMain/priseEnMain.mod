#Exmple de prise en main de glpsol et mathprog

#déclaration des variables utiles
var x1;
var x2;

#objectif à maximiser
maximize z: 3 * x1 + 5 * x2;

#contraintes
subject to
	c1: x1 <= 4;
	c2: x2 <= 6;
	c3: 3*x1 + 2* x2 <= 18;

#résolution
solve;

#affichage éventuel
display x1, x2, z;

end;
