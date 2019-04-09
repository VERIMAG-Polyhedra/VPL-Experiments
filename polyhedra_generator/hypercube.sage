# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# GENERATION OF (ROTATED) HYPERCUBE
#   -1 <= xi <= 1   for  i=1...Dimension
# =======================================================

def hypercube(dimension, bound=1):
    h = polytopes.hypercube(dimension)
    inequalities = []
    for ineq in h.inequalities_list():
        ineq[0] = bound * ineq[0]
        inequalities += [ ineq ]
    return Polyhedron(ieqs= inequalities)

def hypercube_vertices(dimension):
    p = hypercube(dimension)
    return p.vertices_list()

# APPLICATION: see demo_hypercube.sage.py
