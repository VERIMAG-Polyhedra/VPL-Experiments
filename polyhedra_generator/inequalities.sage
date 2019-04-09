# Michael PERIN, Verimag / Universite Grenoble-Alpes, septembre 2017
#
# Representation of inequalities as list of coefficients
# =========================================

# The inequality       1 x1 -2 x2 +3 x3 >= 4
# is equivalent to -4 +1 x1 -2 x2 +3 x3 >= 0
# It is represented by the list of coefficients
# [cst , c1 , c2 , c3] = [-4 , 1 , -2 , 3]
# The head coefficient is that of the constraints

# The inequality      1 x1 -2 x2 +3 x3 <= 6
# is equivalent to   -1 x1 +2 x2 -3 x3 >= -6
# is equivalent to 6 -1 x1 +2 x2 -3 x3 >= 0
# is encoded as   [6 , -1 , 2 , -3]

# The inequality     x1 + x2 >= 0
# is encoded as   [0, 1, 1, 0]

Vars = list(var('x1,x2,x3'))
Poly = Polyhedron(ieqs = [ [-4,1,-2,3] , [6,-1,2,-3] , [0,1,1,0] ])

Poly.plot().show(viewer='threejs')

def print_inequalities(polyhedron, var_list):
    inequalities = polyhedron.inequalities_list()
    cst_vars = [1] + var_list
    for c in inequalities:
        print vector(c) * vector(cst_vars) >= 0

print_inequalities(Poly,Vars)
