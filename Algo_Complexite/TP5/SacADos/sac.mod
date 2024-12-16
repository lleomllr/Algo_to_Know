param n; 
param capMax; 

param pObj{1..n}; 
param val{1..n}; 

var x{1..n} binary;

maximize z: sum{i in 1..n} val[i] * x[i];

subject to Capacity: 
    sum{i in 1..n} pObj[i] * x[i] <= capMax;

solve;
display x, z; 

