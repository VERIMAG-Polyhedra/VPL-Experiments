# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# Plot sphere approximations
# =============================================================================

load("sphere.sage")

# dimension
Dimension = 3
Constraints = 20
Non_zero_coefficients = 3
Rayon = 10

S = sphere(Constraints, Dimension, Non_zero_coefficients, Rayon)

Constraints = 80
Rayon = 15

S2 = sphere(Constraints, Dimension, Non_zero_coefficients, Rayon)

Constraints = 500
Rayon = 15

S3 = sphere(Constraints, Dimension, Non_zero_coefficients, Rayon)

# RENDERING
G = polyhedron_plot_with_wireframe(S)
G.show(frame = False, viewer='threejs')

G = polyhedron_plot_with_wireframe(S2)
G.show(frame = False, viewer='threejs')

G = polyhedron_plot_with_wireframe(S3)
G.show(frame = False, viewer='threejs')
