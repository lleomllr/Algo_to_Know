#PGCD : Given two integers a, b, we look for the largest integer p such that a and b 
#can be expressed as integer multiples of p. This is the largest common divisor.

def pgcd(a, b):
    return a if b == 0 else pgcd(b, a%b)

#test
print(pgcd(45, 27))