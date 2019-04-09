
# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
#  Detecting redundancy and computing farkas coefficients
#
# ==============================================================================

# REDUNDANT CONSTRAINTS

def syntactically_redundant(constraint_as_vector1,constraint_as_vector2):
    v1 = constraint_as_vector1
    v2 = constraint_as_vector2
    if (len(v1) != len(v2)): return False
    else:
        already_ratio = False
        for j in range(len(v1)-1):
            i = j+1 # the constants v1[0] and v2[0] have a special treatment
            c1 = v1[i]
            c2 = v2[i]
            if ((c1 == 0 and c2 != 0) or (c1 != 0 and c2 == 0)):
                return False
            elif (c1 != 0 and c2 != 0):
                r = float(c1) / float(c2)
                if (already_ratio):
                    if (abs(ratio - r) > 0.00000001): return False
                else:
                    already_ratio = True
                    ratio = r
	return (v2[0] * ratio > v1[0])

def syntactically_redundant_wrt(constraint, constraintS):
    for c in constraintS:
        if (syntactically_redundant(constraint,c)):
            return True
    return False


# farkas = list of nonnegative coefficients
def farkas_combination_of(farkas, inequalities):
    F = farkas
    C = inequalities
    dimension = len(C[0])
    v = vector([0] * dimension)
    for i in range( min(len(F),len(C)) ):
        v += F[i] * vector(C[i])
    coefficients = list(v)
    #g = gcd(coefficients)
    #if g!=1: coefficients = [ c/g for c in coefficients ]
    return coefficients

def generate_farkas_combination_of(inequalities, coeff_max=100):
    farkas = []
    for i in range(len(inequalities)):
        farkas += [ random.randint(0,coeff_max) ]
    s = sum(farkas)
    print "sum=",s
    print "Farkas=", farkas
    farkas = [ Rational(f)/Rational(s) for f in farkas ]# Hence, sum farkas = 1
    print "Farkas=", farkas
    return farkas_combination_of(farkas,inequalities)
