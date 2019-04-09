# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# Create a sphere approximation
# =============================================================================

import random
import sys

load("polyhedron.sage")

load("generate_random.sage")

def progress():
	sys.stdout.write('.')
	sys.stdout.flush()

load("polyhedron.sage")
load("hypercube.sage")
load("redundancy.sage")


def generate_one_more_constraint(polyhedron, number_of_nonzero, radius, coeff_max=100):
	dimension = polyhedron.ambient_dim()
	inequalities = polyhedron.inequalities_list()
	if len(inequalities) >= 2:
		new_constraint = generate_farkas_combination_of(inequalities)
		print "\nFarkas Constraint =", new_constraint
		new_constraint[0] = radius /2 # The constant is shifted to get an irredundant constraint
		print "\nFarkas Constraint =", new_constraint
	else:
		while True:
			direction = generate_list_random_coefficients(dimension, number_of_nonzero, (-coeff_max,coeff_max))
			new_constraint = [radius] + direction
			if not( syntactically_redundant_wrt(new_constraint, inequalities) ): break
	return new_constraint

def generate_constraints(number_of_inequalities, dimension, nb_nonzero, radius):
	constraints = []
	trivial_constraint = [1] + ([0] * dimension)
	# polyhedron = Polyhedron(ieqs = [ trivial_constraint ])
	# The whole space polyhedron of dimension 0:  Polyhedron(ieqs = [])
	polyhedron = hypercube(dimension, bound = 2* radius)
	for i in range(number_of_inequalities):
		progress()
		new_constraint = generate_one_more_constraint(polyhedron, nb_nonzero, radius)
		polyhedron = polyhedron.intersection( Polyhedron(ieqs = [ new_constraint ]) )
		constraints += [ new_constraint ]
	sys.stdout.write('\n')
	return polyhedron


# number of irredundant constraints, number of variables, number of redundancies, number of nonzero coefficients per constraint, sphere rayon
def silexoid(nb_cstrs, dim, nb_nzero, rayon):
	P = generate_constraints(nb_cstrs, dim, nb_nzero, rayon)
	return P
