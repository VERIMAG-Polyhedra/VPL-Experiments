# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# Creates rotated polytopes with rational coordinates
# by applying a sequences of rotations to SOME RANDOMLY CHOSEN dimensions
# ==============================================================================

load("polyhedron.sage")
load("rational_rotation_matrix.sage")

# ROTATED POLYTOPE

def random_couple(dimension):
    i = ZZ.random_element(0,dimension,'uniform')
    j = i
    while (i==j):
        j = ZZ.random_element(0,dimension-1,'uniform')
    return (i,j)

def rotate_vertices(dimension, vectors, cos_sin):
    (i,j) = random_couple(dimension)
    mr = matrix_rotation(dimension, i, j, cos_sin)
    rotated_vectors = [ (mr * v) for v in vectors ]
    return rotated_vectors

def rotate_polyhedron(polyhedron, cos_sin):
    dimension = polyhedron.ambient_dim()
    vectors = polyhedron_vertices_as_vectors(polyhedron.vertices_list())
    rotated_vectors = rotate_vertices(dimension, vectors, cos_sin)
    return Polyhedron(vertices = rotated_vectors)
