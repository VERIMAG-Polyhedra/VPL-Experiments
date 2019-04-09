# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# Plot potatoids
# =============================================================================

load("potatoid.sage")

Dimension = 3
Non_zero_coefficients = 3

# POTATOID 1

Constraints = 50
Rayon = 10

P1 = potatoid(Constraints, Dimension, Non_zero_coefficients, Rayon)

# POTATOID 2

Constraints = 100
Rayon = 15

P2 = potatoid(Constraints, Dimension, Non_zero_coefficients, Rayon)

# POTATOID 3

Constraints = 200
Rayon = 20

P3 = potatoid(Constraints, Dimension, Non_zero_coefficients, Rayon)

# RENDERING

G = polyhedron_plot_with_wireframe(P1)
G.show(frame = False, viewer='threejs')

G = polyhedron_plot_with_wireframe(P2)
G.show(frame = False, viewer='threejs')

G = polyhedron_plot_with_wireframe(P3)
G.show(frame = False, viewer='threejs')
