# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# A complete polyhedron descriptor provides the following fields
# ==============================================================================
#  typ : string = the type of pplyhedron (hypercube,  patatoid, sphereoid, polytope, crossed)
#  pid : int = its identifier
#  nbi : int = its number of inequalities
#  nbg : int = its number of vertices
#  dim : int = its dimenstion = its number of variables
#  den :  %  = its density =  100 * the average (the number of 0 / the number of coefficient of each constraint)
#  bit : int = number of bits used by the coefficients of the matrix of constraints
#  red :  %  = its redundancy rate = 100 * the number of redundant constraint / the number of constraints
#  ieq : its list of inequations
#  gen : its list of generators

# KEYS
TYP = 'typ'
PID = 'pid'
NBI = 'nbi'
NBG = 'nbg'
DIM = 'dim'
DEN = 'den'
BIT = 'bit'
RED = 'red'
IEQ = 'ieq'
GEN = 'gen'

# The magnitude of a given integer i is the integer m such that
#   2^{m-1} <= i <= 2^m

def magnitude(input_integer):
	integer = ceil(float(log(input_integer,2)))
	return integer

def bigint_to_string(input_integer):
	if input_integer<=32:
		return str(input_integer)
	else:
		return "2^" + str(magnitude(input_integer))

# Computing the number of generators

def compute_gen(polyhedron_descriptor):
	d = polyhedron_descriptor
	if GEN in d.keys():
		return d[GEN]
	if IEQ in d.keys():
  		p = Polyedron(ieqs = d[IEQ])
		vertices = p.vertices_list()
		return vertices

def compute_nbg(polyhedron_descriptor):
	d = polyhedron_descriptor
	if NBG in d.keys():
		return d[NBG]
	else:
		generators = compute_gen(d)
		return len(generators)

# Computing IEQ = inequalities of a polyhedron

def compute_ieq(polyhedron_descriptor):
	d = polyhedron_descriptor
	if IEQ in d.keys():
		return d[IEQ]
	if GEN in d.keys():
		p = Polyhedron(vertices = d[GEN])
		return p.inequalities_list()

def compute_nbi(polyhedron_descriptor):
	d = polyhedron_descriptor
	if NBI in d.keys():
		return d[NBI]
	else:
		inequalities = compute_ieq(d)
		return len(inequalities)

# Computing density = number of zero / number of coefficients

def matrix_density(nb_rows,nb_cols,matrix):
	z = 0
	for r in range(nb_rows):
		for c in range(nb_cols):
			if matrix[r][c]==0:
				z += 1
	return (100.0 * z)/(nb_rows * nb_cols)

def compute_average_density(polyhedron_descriptor):
	d = polyhedron_descriptor
	matrix = compute_ieq(d)
	nb_rows = d[NBI]
	nb_cols = d[DIM]
	return matrix_density(nb_rows,nb_cols,matrix)

# total number of bits in the matrix

def matrix_nbits(nb_rows,nb_cols,matrix):
	n = 0
	for r in range(nb_rows):
		for c in range(nb_cols):
			rat = matrix[r][c]
			n += rat.numer().nbits() + rat.denom().nbits()
	return n

def compute_number_of_bits(polyhedron_descriptor):
	d = polyhedron_descriptor
	matrix = compute_ieq(d)
	nb_rows = d[NBI]
	nb_cols = d[DIM]
	return matrix_nbits(nb_rows,nb_cols,matrix)

# CREATE DESCRIPTION

def create_polyhedron_descriptor(polyhedron):
	d = {}
	d[DIM] = polyhedron.ambient_dim()
	d[GEN] = polyhedron.vertices_list()
	d[IEQ] = polyhedron.inequalities_list()
	d[RED] = 0
	return d


# COMPLETE DESCRIPTION

import time

def complete_polyhedron_descriptor(polyhedron_descriptor):
	d = polyhedron_descriptor
	if not(TYP in d.keys()):
		d[TYP] = "P"
	if not(PID in d.keys()):
		d[PID] = ceil(1000 * time.time())
	if not(RED in d.keys()):
		d[RED] = 0
	if not(IEQ in d.keys()):
		d[IEQ] = compute_ieq(d)
	if not(NBI in d.keys()):
		d[NBI] = compute_nbi(d)
	if not(DEN in d.keys()):
		d[DEN] = compute_average_density(d)
	if not(BIT in d.keys()):
		d[BIT] = compute_number_of_bits(d)
	if not(GEN in d.keys()):
		d[GEN] = compute_gen(d)
	if not(NBG in d.keys()):
		d[NBG] = compute_nbg(d)
	return d

# BUILD A COMPLETE DESCRIPTION

def build_polyhedron_descriptor(type_of_polyhedron, Polyhedron):
	d = create_polyhedron_descriptor(Polyhedron)
	d = complete_polyhedron_descriptor(d)
	d[TYP] = type_of_polyhedron
	return d

def build_polyhedra_descriptors(type_of_polyhedra, Polyhedra):
    return [ build_polyhedron_descriptor(type_of_polyhedron,p) for p in Polyhedra ]

# FILE and POLYHEDRON NAMING

def make_file_name(polyhedron_descriptor):
	d = polyhedron_descriptor
	string = 'P_%i_%i_%i'  % ( d[DIM] , d[NBI] , d[RED] )
	return string

def make_polyhedron_header(polyhedron_descriptor):
	d = polyhedron_descriptor
	return '%s: %i Var, %s Cstr, %i Red, %i%% Zero, %i bits, %s Gen' % (d[TYP] , d[DIM], bigint_to_string(d[NBI]) , d[RED] , d[DEN] , d[BIT] , bigint_to_string(d[NBG]) )

def make_polyhedron_name(polyhedron_descriptor):
	sage_id = 'P_%i' % ( polyhedron_descriptor[PID])
	return sage_id

# FOLDER CREATION

import os

def ensure_directory(file_path):
    directory = os.path.dirname(file_path)
    if not os.path.exists(directory):
        os.makedirs(directory)

# VPL OUTPUT IN FILES

# A inequalities is a list of coefficients [a0,a1,a2,...,an]
# It stands for a1 x1 + a2 x2 + ... + an xn + a0 >=0

def ineq_to_string(coefficients):
	s = ' '.join([str(c) for c in coefficients[1:]])
	return '%s <= %s' % (s, str(coefficients[0]))

def ieqs_to_string(polyhedron_descriptor):
	inequalities = polyhedron_descriptor[IEQ]
	s = '\n'.join([ineq_to_string(ineq) for ineq in inequalities])
	return s

def vpl_output_polyhedron_descriptor_in_file(filename, polyhedron_descriptor):
	filename_sage = filename + ".vpl"
	print "\noutput in: %s" % filename_sage
	f = open(filename_sage, 'a')
	f.write('\n\n' + '# === ' + make_polyhedron_header(polyhedron_descriptor) )
	f.write('\n\n' + make_polyhedron_name(polyhedron_descriptor) )
	f.write('\n' + ieqs_to_string(polyhedron_descriptor) )
	f.close()

def vpl_output_polyhedron_descriptor_in_folder(folder, polyhedron_descriptor):
	filename = folder + make_file_name(polyhedron_descriptor)
	ensure_directory(filename)
	vpl_output_polyhedron_descriptor_in_file(filename, polyhedron_descriptor)

# SAGE OUTPUT IN FILES

#def UNUSED_output_header(file_descriptor, nb_polyhedra, polyhedron_descriptor):
# 	f = file_descriptor
#	d = polyhedron_descriptor
#	header = 'polyhedra = %i\nconstraints = %i\nredundancy = %i\nvariables = %i\nzeros = %i' % (nb_polyhedra, d[NBI] , d[RED] , d[DIM] , d[DEN])
#	f.write(header)

def sage_output_polyhedron_descriptor_in_file(filename, polyhedron_descriptor):
	filename_sage = filename + ".sage"
	print "\noutput in: %s" % filename_sage
	f = open(filename_sage, 'a')
	f.write('\n\n# === ' + make_polyhedron_header(polyhedron_descriptor) )
	f.write('\n\n'       + make_polyhedron_name(polyhedron_descriptor) )
	f.write( " = Polyhedron(ieqs = " + str(polyhedron_descriptor[IEQ]) + " )" )
	f.close()

def sage_output_polyhedron_descriptor_in_folder(folder, polyhedron_descriptor):
	filename = folder + make_file_name(polyhedron_descriptor)
	ensure_directory(filename)
	sage_output_polyhedron_descriptor_in_file(filename, polyhedron_descriptor)

# XML OUTPUT IN FILES

def output_polyhedron_descriptor_in_folder(folder, polyhedron_descriptor):
	vpl_output_polyhedron_descriptor_in_folder(folder, polyhedron_descriptor)
	sage_output_polyhedron_descriptor_in_folder(folder, polyhedron_descriptor)

def output_polyhedron_in(directory, name, polyhedron):
	subdirectory = name + "/"
	folder = directory + subdirectory
	descriptor = build_polyhedron_descriptor(name, polyhedron)
	output_polyhedron_descriptor_in_folder(folder, descriptor)
