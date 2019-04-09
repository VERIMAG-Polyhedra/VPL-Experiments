# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# Given a dimension D
# GENERATION OF FOOT BALLS
#   number of constrains ~ D * number of generators
# some vertices are the intersection of more that D faces
# ==============================================================================

load("generate_rotated.sage")
load("football.sage")

dimension_min = 3
dimension_max = 5
number_of_rotations = 5

n = (1 + dimension_max - dimension_min) * number_of_rotations
print "\nGENERATION OF %i FOOT BALLS" % n

for d in range(1 + dimension_max - dimension_min):
    dimension = dimension_min + d
    polyhedra = footballs(dimension, 2) # the degree 2 is the maximum
    generate_rotated_in_folder_from_polyhedra("benchmark/", "football", polyhedra, number_of_rotations)

# A MONSTER : 2^13 contraints, 2^12 generators
# F3_3 = football_of_degree(3,3)
# generate_rotated_in_folder_from_polyhedra("benchmark/", "football", [ F3_3 ], 1)
