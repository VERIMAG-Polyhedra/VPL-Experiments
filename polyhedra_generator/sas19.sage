# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# ==============================================================================

load("polyhedra_descriptor.py")
load("sphere.sage")

nb_polys = 1
dimensions = [10]
n_cstrs = [25]
densities = [0.5]
rayon = 100

n = nb_polys * len(dimensions) * len(n_cstrs) * len(densities)
print "\nGENERATION OF %i SPHERES" % n

for dimension in dimensions:
	for n_cstr in n_cstrs:
		for density in densities:
			n_zero = dimension - int(density * dimension)

			if n_zero > 0 and n_zero < dimension - 1 :
				for i in range(nb_polys):
					n_nzero = dimension - n_zero
					P = sphere(n_cstr, dimension, n_nzero, rayon)

					output_polyhedron_in("benchmark/", "sphere", P)

n_cstrs = [50]
dimensions = [5]
rayon = 10

n = nb_polys * len(dimensions) * len(n_cstrs) * len(densities)
print "\nGENERATION OF %i SPHERES" % n

for dimension in dimensions:
	for n_cstr in n_cstrs:
		for density in densities:
			n_zero = dimension - int(density * dimension)

			if n_zero > 0 and n_zero < dimension - 1 :
				for i in range(nb_polys):
					n_nzero = dimension - n_zero
					P = sphere(n_cstr, dimension, n_nzero, rayon)

					output_polyhedron_in("benchmark/", "sphere", P)
