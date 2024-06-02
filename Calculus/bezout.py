#Bezout's identity : Let a and b be two integers with greatest common divisor d. 
#Then there are integers x and y such that ax + by = d.
#More generally, integers in the form ax + by are exactly multiples of d.
#If a = qb + r, ax + by = d corresponds to (qb + r)x + by = d or bx‘ + ry’ = d for :
# |x' = qx + y 
# |y' = x
#are equivalent to :
# |x = y’
# |y = x‘ - qy’

def bezout(a, b):
    if b == 0:
        return (1, 0)
    else:
        x, y = bezout(b, a%b)
        return (y, x - (a // b) * y)
    
#test 
print(bezout(4, 9))