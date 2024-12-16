var qA; 
var qB; 

maximize z: 4 * qA + 5 * qB; 

subject to 
    fraise : 2 * qA + qB <= 800; 
    lait : qA + 2 * qB <= 700; 
    sucre : qB <= 300; 

solve; 

display qA, qB, z; 

end;
