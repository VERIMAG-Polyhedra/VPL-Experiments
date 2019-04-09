# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# The "combine" functions generate n^2 vertices by combining any two vertices
# of a set of n vertices
#
# ==============================================================================

# Enumerates all the couples of E x F
def product(E,F):
    L = []
    for e in E:
        for f in F:
            L += [(e,f)]
    return L

# Given a radius and a vector direction, the vector is normalized to correspond to a point on the sphere
def on_sphere(radius,vector):
    n = vector.norm()
    if (n!= 0):
        return radius * (vector / vector.norm())
    else:
        return vector

# Convert a floating-point vector into a rational vector
def rational_vector(vector):
    return map(Rational, map(float, list(vector)))

# Generate n^2 vertices by combining v1,v2 from all v1,v2 in vertices

def combine_vectors(vectors):
    return [ v1 + v2 for (v1,v2) in product(vectors,vectors) ]

def combine_vertices_on_sphere(radius,vertices):
    vectors = polyhedron_vertices_as_vectors(vertices)
    vectors = combine_vectors(vectors)
    return [ vector(rational_vector(on_sphere(radius,v))) for v in vectors ]

def combine_vertices(vertices):
    radius = 1
    return combine_vertices_on_sphere(radius,vertices)
