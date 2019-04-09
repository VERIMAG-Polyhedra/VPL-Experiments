# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017

#
# GENERATION OF CROSS POLYTOPES:
# - linear number of generators
# - exponential number of constraints
# ==============================================================================

load("generate_rotated.sage")
load("cross_polytope.sage")

dimension_min = 3
dimension_max = 8
number_of_rotations = 10

n = (1 + dimension_max - dimension_min) * number_of_rotations
print "\nGENERATION OF %i CROSS POLYTOPES (exponential number of constraints)" % n

for d in range(1 + dimension_max - dimension_min):
    dimension = dimension_min + d
    polyhedra = [ cross_polytope(dimension) ]
    generate_rotated_in_folder_from_polyhedra("benchmark/", "cross_polytope", polyhedra, number_of_rotations)
