# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# Given a dimension D
# GENERATION OF FOOT BALLS
#   number of constrains ~ D * number of generators
# some vertices are the intersection of more that D faces
# ==============================================================================

load("polyhedron.sage")
load("combine_vertices.sage")

def next_combine(polyhedron):
    vertices = polyhedron_vertices_of(polyhedron)
    vectors = combine_vertices(vertices)
    return Polyhedron(vertices = vectors)

load("cross_polytope.sage")

def football_of_degree(dimension, degree): #degree in {0,1,2}
    polyhedron = cross_polytope(dimension)
    for i in range(degree):
        polyhedron = next_combine(polyhedron)
    return polyhedron

def footballs(dimension, number_of_polyhedra):
    polyhedra = []
    polyhedron = cross_polytope(dimension)
    # vertices = polyhedron_vertices_of(polyhedron)
    for i in range(number_of_polyhedra):
        polyhedron = next_combine(polyhedron)
        polyhedra += [ polyhedron ]
    return polyhedra

# APPLICATION: see demo_football.sage.py
