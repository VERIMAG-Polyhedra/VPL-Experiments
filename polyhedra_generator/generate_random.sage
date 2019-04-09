# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, July 2017
#
# Generates vectors of random integer coefficients
# =============================================================================

import random

def generate_list_random_indices(dimension, number_of_indices):
	indices = []
	for n in range(min(number_of_indices,dimension)):
		i = random.randint(0,dimension-1)
		while i in indices:
			i = random.randint(0,dimension-1)
		indices.append(i)
	return indices

# interval = couple of integer = (lower,upper)
def generate_list_random_coefficients(dimension, number_of_nonzero, (lower,upper)):
	indices = generate_list_random_indices(dimension, number_of_nonzero)
	coefficientS = []
	for i in range(dimension):
		if (i in indices):
			r = random.randint(lower,upper)
			while r == 0:
				r = random.randint(lower,upper)
			coefficientS.append(r)
		else:
			coefficientS.append(0)
	return coefficientS




def vector_to_cstr(vec):
	 return [vec[len(vec)-1]] + [-1 * x for x in vec[0:len(vec)-1]]

#Adding constraints one by one
def get_cstrs(n_cstrs, dim, nb_nzero, rayon):
	cstrs = []
	if nb_nzero == 1:
		max_cstrs = 2*dim
	elif nb_nzero == 0:
		max_cstrs = 0
	else:
		max_cstrs = 2000

	P = Polyhedron(ieqs = [[0] * (dim+1)])

	for i in range(n_cstrs):
		sys.stdout.write('.')
		sys.stdout.flush()
		if i + 1 >= max_cstrs:
			break

		cstr = get_vector(dim, nb_nzero, rayon)

		#check redundancy
		cstr2 = vector_to_cstr (cstr)
		P = P.intersection(Polyhedron(ieqs = [cstr2]))
		while syntactic_redundant2(cstr,cstrs) or number_of_constraints(P) != i+1 :
			cstr = get_vector(dim, nb_nzero, rayon)
			cstr2 = vector_to_cstr (cstr)
			P = P.intersection(Polyhedron(ieqs = [cstr2]))

		cstrs.append(cstr)
	sys.stdout.write('\n')
	return (cstrs,P)

# number of irredundant constraints, number of variables, number of redundancies, number of nonzero coefficients per constraint, sphere rayon
def potatoid(nb_cstrs, dim, nb_nzero, rayon):

	(cstrs,P) = get_cstrs(nb_cstrs, dim, nb_nzero, rayon)

	return P
