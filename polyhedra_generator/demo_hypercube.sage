# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
#  GENERATION OF ROTATED HYPERCUBES
#
# ==============================================================================

load("polyhedron.sage")

Dimension = 3

# 1. The standard hypercube

load("hypercube.sage")

V1 = polyhedron_vertices_as_vectors(hypercube_vertices(Dimension))
print "\nVertices(H1)=", V1

# 2. Rotated hypercubes

load("rational_rotation_matrix.sage")

# Rational Rotation Matrix
CS = create_rational_cosinus_sinus(10)
R2 = matrix_rotation(Dimension,1,2,CS[2])
R3 = matrix_rotation(Dimension,0,1,CS[3])
R4 = matrix_rotation(Dimension,0,2,CS[4])

V2 = [ (R2 * v) for v in V1 ]
print "\nVertices(H2)=", V2

V3 = [ (R3 * v) for v in V2 ]
print "\nVertices(H3)=", V3

V4 = [ (R4 * v) for v in V3 ]
print "\nVertices(H4)=", V4

# 3. Generalization: combination of several rotations

def rotated_hypercube(dimension, number_of_rotations):
    cs = create_rational_cosinus_sinus(number_of_rotations+1) # couples of rational (cos,sin)
    vectors = polyhedron_vertices_as_vectors(hypercube_vertices(dimension))
    for n in range(number_of_rotations):
        i = mod(n  , dimension) # coordinate to rotate are (0,1) ; (1,2) ; ... ; (D-1,0)
        j = mod(n+1, dimension) # coordinate to rotate
        mr = matrix_rotation(dimension,i,j,cs[n+1])
        rotated_vectors = [ mr * v for v in vectors ]
    return Polyhedron(vertices = rotated_vectors)

H5 = rotated_hypercube(Dimension, 5)
print "\nVertices(H5)=", polyhedron_vertices_of(H5)

# 4. Rendering

G = Graphics()

G += polyhedron_plot_from_vertices(V1,'red')
G += polyhedron_plot_from_vertices(V2,'blue')
G += polyhedron_plot_from_vertices(V3,'green')
G += polyhedron_plot_from_vertices(V4,'yellow')
G += polyhedron_plot_with_vertices(H5,'purple')

G.show(viewer='threejs')
