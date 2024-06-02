#binomial coefficients : (n / k) = n! / (k! (n-k))

def bezout(a, b):
    if b == 0:
        return (1, 0)
    else:
        x, y = bezout(b, a%b)
        return (y, x - (a // b) * y)

def inv(a, p):
    return bezout(a, p)[0] % p

#binomial
def binomial(n, k):
    prod = 1
    for i in range(k):
        prod = (prod * (n-i)) // (i+1)
    return prod

#binomial coefficients modulo a prime number p
def binomial_modulo(n, k, p):
    prod = 1
    for i in range(k):
        prod = (prod * (n-i) * inv(i+1, p)) % p
    return prod
