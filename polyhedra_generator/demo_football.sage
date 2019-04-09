# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# Given a dimension D
# GENERATES FOOT BALL POLYTOPES
#   number of constrains ~ D * number of generators
# some vertices are the intersection of more that D faces.
# ==============================================================================

load("football.sage")

# dimension
Dimension = 3

Footballs = footballs(Dimension, 2)# 2 is the maximum

# RENDERING
G = polyhedron_plot_with_vertices(Footballs[0],'purple')
G.show(viewer='threejs')

G = polyhedron_plot_with_vertices(Footballs[1],'yellow')
G.show(viewer='threejs')
