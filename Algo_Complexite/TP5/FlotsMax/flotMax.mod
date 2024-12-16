param nbSommets; 

param capArcs{0..5, 0..5}; 

var f{0..nbSommets, 0..nbSommets}; 

maximize z: sum{i in 0..nbSommets} f[i,5]; 

subject to 
    c1 {i in 0..nbSommets, j in 0..nbSommets}: 0 <= f[i,j] <= capArcs[i,j]; 
    c2 {i in 1..nbSommets-1}: sum{j in 0..nbSommets} f[i,j] == sum{j in 0..nbSommets} f[j,i];

solve;
display f, z; 

end; 

