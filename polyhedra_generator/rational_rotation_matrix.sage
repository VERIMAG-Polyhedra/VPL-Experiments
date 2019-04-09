# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# Given a dimension N
# creates a twisted hypercubes with rational coordinates
# by applying a sequences of rotations to some dimensions
# ==============================================================================

# 1. COUPLES OF RATIONAL COSINUS, RATIONAL SINUS
#
# Rational sinus and cosinus correspond to Pythagorean triples
#
# 1.1 A Pythagorean triple (a,b,c) satisfies the relation a^2 + b^2 = c^2
# Euler formula gives the way to generate such triples
# choose m prime and n pair

def euler(m,n):
    if (n>m):
        a = n*n - m*m
    else:
        a = m*m - n*n
    b = 2 * m * n
    c = m*m + n*n
    return (a,b,c, a*a + b*b == c*c)

def pythagorean_triples(a_prime_number):
    m = a_prime_number
    triples = []
    for i in range(a_prime_number):
        n = 2 * (i+1)
        if gcd(m,n)==1:
            triples += [ euler(m,n) ]
    return triples

def list_sum(list_of_list):
    L = []
    for l in list_of_list:
        L += l
    return L

# 1.2 couples of rational sinus and cosinus
#
# Forall t,  cos^2(t) + sin^2(t) = 1
# Then, given a Pythagorean triple (a,b,c)
# a^2 + b^2 = c^2 is equivalent to (a/c)^2 + (b/c)^2 = 1
# Therefore, (a/c) and (b/c) are respectively the rational cos and sin of a certain angle.

def create_rational_cosinus_sinus(Integer):
    p = next_prime(1)
    cs = []
    while len(cs) < Integer :
        p = next_prime(p)
        triples = pythagorean_triples(p)
        cs += [ (Rational(a/c) , Rational(b/c)) for (a,b,c,_) in triples ]
    return cs

# 2. ROTATION MATRIX
# https://en.wikipedia.org/wiki/Rotation_matrix
# https://fr.wikipedia.org/wiki/Matrice_de_rotation#En_dimension_trois
#
# A rotation matix M must satsifies M * M^t = Id
#     vi       vj
# [ cos(t)  -sin(t) ]
# [ sin(t)   cos(t) ]

def matrix_identity(Dimension):
    M = matrix(QQ,Dimension,Dimension)
    for i in range(Dimension):
        M[i,i] = 1
    return M

def matrix_rotation(Dimension,i,j,(cos,sin)):
    M = matrix_identity(Dimension)
    M[i,i] = cos
    M[j,j] = cos
    M[i,j] = -sin
    M[j,i] = sin
    return M
