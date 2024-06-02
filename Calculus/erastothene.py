#Eratosthenes sieve: find all the primes below n
#we start with a list of all the integers less than n, 
#initially crossing out 0 and 1. Then for each p = 2,3,4,..., n-1, 
#if p is not crossed out then it is prime.

def eratosthene(n):
    p = [True] * n
    rep = [2]
    #iterates over odd numbers from 3 up to n-1
    for i in range(3, n, 2):
        if p[i]:
            rep.append(i)
            #marks all multiples of i as non-prime by setting them to False in the list p. 
            #The loop starts at 2 * i (the first multiple of i greater than i) and ends before n, incrementing by i.
            for j in range(2 * i, n, i):
                p[j] = False
    return rep