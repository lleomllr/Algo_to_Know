#Coin Change : we want to obtain an amount R with coins or notes worth x0, ..., xn-1 centimes respectively. 
#The problem is to determine whether there is a positive linear combination 
#of x0, ..., xn-1 that is worth R.

def coin_change(x, R):
    b = [False] * (R + 1)
    #The sum 0 can always be formed with 0 pieces
    b[0] = True
    #each part value xi in the x list is browsed.
    for xi in x:
        #For each coin xi, we run through all the possible sums of xi up to R.
        for s in range(xi, R + 1):
            b[s]|= b[s - xi]
    return b[R]

#test
def test():
    x1 = [1, 2, 5]
    R1 = 11
    print(coin_change(x1, R1))
    
    x2 = [1, 2, 5, 10, 20, 50, 100]
    R2 = 203
    print(coin_change(x2, R2))
    
    x3 = [2, 5]
    R3 = 3
    print(coin_change(x3, R3))
    
test()