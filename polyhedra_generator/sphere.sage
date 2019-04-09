# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# Create a sphere approximation
# =============================================================================

import random
import sys

load("polyhedron.sage")

def get_vector(dim, nb_nzero, rayon):
	indicies = []
	for n in range(nb_nzero):
		i = random.randint(0,dim-1)
		while i in indicies:
			i = random.randint(0,dim-1)
		indicies.append(i)
	v = []
	for i in range(dim):
		if(i in indicies):
			r = random.randint(-100,100)
			while r == 0:
				r = random.randint(-100,100)
			v.append(r)
		else:
			v.append(0)

	n = int(vector(v).norm())
	#cste part:
	v.append(rayon * n)
	return v

def syntactic_redundant(v1,v2):
	if (len(v1) != len(v2)):
		return False

	already_coeff = False
	for i in range (len(v1)):
		x = v1[i]
		y = v2[i]
		if ((x == 0 and y != 0) or (x != 0 and y == 0)):
			return False
		if (x != 0 and y != 0):
			c2 = float(x) / float(y)
			if (already_coeff):
				if(abs(c - c2) > 0.00000001):
					return False
			else:
				already_coeff = True
				c = c2
	return True

def syntactic_redundant2(v1,cstrs):
	for c in cstrs:
		if (syntactic_redundant(v1,c)):
			return True
	return False

def number_of_generators(P):
	return len(P.Vrepresentation())

def number_of_constraints(P):
	return len(P.Hrepresentation())

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

def fast_get_cstrs(n_cstrs, dim, nb_nzero, rayon):
	sys.stdout.write("Generating a polyhedron of %i constraints" % n_cstrs)
	sys.stdout.flush()

	if nb_nzero == 1:
		max_cstrs = 2*dim
	elif nb_nzero == 0:
		max_cstrs = 0
	else:
		max_cstrs = 10^8

	cstrs = [get_vector(dim, nb_nzero, rayon) for i in range(n_cstrs)]
	cstrs2 = [vector_to_cstr (cstr) for cstr in cstrs]
	P = Polyhedron(ieqs = cstrs2)
	if number_of_constraints(P) != n_cstrs:
		print('Error with fast generation, switching to slow generation\n')
		return get_cstrs(n_cstrs, dim, nb_nzero, rayon)

	sys.stdout.write('\nFast generation succeded\n')
	return (cstrs,P)

# number of irredundant constraints, number of variables, number of redundancies, number of nonzero coefficients per constraint, sphere rayon
def sphere(nb_cstrs, dim, nb_nzero, rayon):

	(cstrs,P) = fast_get_cstrs(nb_cstrs, dim, nb_nzero, rayon)

	return P
