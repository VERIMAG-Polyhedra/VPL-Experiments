# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# Plot farkasoids
# =============================================================================

load("farkasoid.sage")

Dimension = 3
Non_zero_coefficients = 3

# farkasoid 1

Constraints = 50
Rayon = 10

P1 = farkasoid(Constraints, Dimension, Non_zero_coefficients, Rayon)
print "\nnumber_of_inequalities =", number_of_inequalities(P1)
print P1.inequalities_list()

# farkasoid 2

Constraints = 100
Rayon = 15

P2 = farkasoid(Constraints, Dimension, Non_zero_coefficients, Rayon)
print "\nnumber_of_inequalities =", number_of_inequalities(P2)
print P2.inequalities_list()

# farkasoid 3

Constraints = 200
Rayon = 20

P3 = farkasoid(Constraints, Dimension, Non_zero_coefficients, Rayon)
print "\nnumber_of_inequalities =", number_of_inequalities(P3)
print P3.inequalities_list()

# RENDERING

G = polyhedron_plot_with_wireframe(P1)
G.show(frame = False, viewer='threejs')

G = polyhedron_plot_with_wireframe(P2)
G.show(frame = False, viewer='threejs')

G = polyhedron_plot_with_wireframe(P3)
G.show(frame = False, viewer='threejs')
