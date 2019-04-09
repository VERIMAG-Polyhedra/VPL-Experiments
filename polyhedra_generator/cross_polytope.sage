# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# Create an so-called cross potytope
# - exponential number of constraints
# - linear number of generators
# see [F.Benoy, A.King, F.Mesnard - Computing Convex Hull with a Linear Solver]
# =============================================================================

# create a list of two vectors (0,...,0) fo size N with 1 and -1 at the d^th coordinate
def make_vertices(dimension,d):
    v = [0] * dimension
    v[d] = 1
    return [ vector(v), -vector(v) ]

# create the vertices of a cross-polytope fro a given dimension
def cross_polytope_vertices(dimension):
    vectors = []
    for d in range(dimension):
        vectors += make_vertices(dimension,d)
    return vectors

def cross_polytope(dimension):
    vectors = cross_polytope_vertices(dimension)
    return Polyhedron(vertices = vectors)

# APPLICATION: see demo_cross_polytope.sage.py
