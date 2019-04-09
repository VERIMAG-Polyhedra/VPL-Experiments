# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# GENERATION OF CROSS POLYTOPES:
# - linear number of generators
# - exponential number of constraints
# ==============================================================================

load("cross_polytope.sage")
load("generate_rotated.sage")

dimension = 3
number_of_rotations = 5

polyhedra = generate_rotations_of(cross_polytope(dimension), number_of_rotations)

# RENDERING
G = Graphics()
G += polyhedron_plot_with_vertices(polyhedra[0],'blue')
G += polyhedron_plot_with_vertices(polyhedra[1],'yellow')
G += polyhedron_plot_with_vertices(polyhedra[2],'green')
G += polyhedron_plot_with_vertices(polyhedra[3],'orange')
G += polyhedron_plot_with_vertices(polyhedra[4],'purple')
G.show(viewer='threejs')
