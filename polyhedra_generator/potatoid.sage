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

	v.append(rayon)
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
	last = vec[-1:]
	tail = vec[:-1]
	return last + [-1 * x for x in tail]

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


load("generate_random.sage")

def progress():
	sys.stdout.write('.')
	sys.stdout.flush()

load("polyhedron.sage")
load("redundancy.sage")

def generate_one_more_constraint(polyhedron, number_of_nonzero, radius):
	dimension = polyhedron.ambient_dim()
	nbc = number_of_constraints(polyhedron)
	inequalities = polyhedron.inequalities_list()
	direction = generate_list_random_coefficients(dimension, number_of_nonzero, (-100,100))
	norme = vector(direction).norm()
	delta = 10
	rmin = ceil( (100 - delta)/100 * 100 * radius/norme )
	rmax = ceil( (100 + delta)/100 * 100 * radius/norme )
	print "\nchoose constant in [%i,%i]" % (rmin , rmax)
	p = polyhedron
	for constant in IntegerRange(rmin,rmax):
		new_constraint = [constant] + direction
		if not( syntactically_redundant_wrt(new_constraint, inequalities) ):
			p = polyhedron.intersection(Polyhedron(ieqs = [ new_constraint ]))
			if number_of_constraints(p) > nbc:
				break
	if number_of_constraints(p) == nbc+1:
		return [ new_constraint ]
	else:
		return []


def generate_constraints(n_cstrs, dimension, nb_nonzero, radius):
	cstrs = []
	trivial_constraint = [1] + ([0] * dimension)
	polyhedron = Polyhedron(ieqs = [ trivial_constraint ])
	# The whole space polyhedron of dimension 0:  Polyhedron(ieqs = [])
	for i in range(n_cstrs):
		progress()
		opt_cstr = generate_one_more_constraint(polyhedron, nb_nonzero, radius)
		if opt_cstr != []:
			polyhedron = polyhedron.intersection( Polyhedron(ieqs = opt_cstr) )
			cstrs += opt_cstr
	sys.stdout.write('\n')
	return (cstrs,polyhedron)


# number of irredundant constraints, number of variables, number of redundancies, number of nonzero coefficients per constraint, sphere rayon
def potatoid(nb_cstrs, dim, nb_nzero, rayon):

	(cstrs,P) = generate_constraints(nb_cstrs, dim, nb_nzero, rayon)

	return P
