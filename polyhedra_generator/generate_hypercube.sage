# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# GENERATION OF ROTATED HYPERCUBES
# - linear number of constraints
# - exponential number of generators
# ==============================================================================

load("generate_rotated.sage")
load("hypercube.sage")

dimension_min = 3
dimension_max = 8
number_of_rotations = 10

n = (1 + dimension_max - dimension_min) * number_of_rotations
print "\nGENERATION OF %i ROTATED HYPERCUBES (exponential number of vertices)" % n

for d in range(1 + dimension_max - dimension_min):
    dimension = dimension_min + d
    polyhedra = [ hypercube(dimension) ]
    generate_rotated_in_folder_from_polyhedra("benchmark/", "hypercube", polyhedra, number_of_rotations)
