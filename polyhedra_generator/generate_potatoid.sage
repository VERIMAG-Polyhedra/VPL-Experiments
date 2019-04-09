# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# ==============================================================================

load("polyhedra_descriptor.py")
load("potatoid.sage")

nb_polys = 10
dimensions = range(2,5)
n_cstrs = range(8,10)
densities = [0.5]
rayon = 20

n = nb_polys * len(dimensions) * len(n_cstrs) * len(densities)
print "\nGENERATION OF %i POTATOIDS" % n

for dimension in dimensions:
	for n_cstr in n_cstrs:
		for density in densities:
			n_zero = dimension - int(density * dimension)

			if n_zero > 0 and n_zero < dimension - 1 :
				for i in range(nb_polys):
					n_nzero = dimension - n_zero
					P = potatoid(n_cstr, dimension, n_nzero, rayon)

					output_polyhedron_in("benchmark/", "potatoid", P)
