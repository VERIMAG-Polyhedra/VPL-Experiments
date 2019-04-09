# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# Plot silexoids
# =============================================================================

load("polyhedron.sage")
load ("rational_rotation_matrix.sage")

def rotate_vertex(vector, cos_sin):
    dimension = 2
    mr = matrix_rotation(dimension, 0, 1, cos_sin)
    return (mr * vector)

def vertices_of_polyhedron_approximating_a_circle(number_of_vertices,radius):
    n = number_of_vertices
    angle = 2*pi/n
    rcos = float(cos(angle))
    rsin = float(sin(angle))
    fv = vector([0,radius])
    vectors = [ fv ]
    for i in range(n):
        fv = rotate_vertex(fv, (rcos,rsin))
        #rv = [ Rational(c) for c in fv ]
        vectors += [ fv ]
    return [ list(v) for v in vectors ]

def polyhedron_approximating_a_circle(number_of_vertices,radius):
    Vertices = vertices_of_polyhedron_approximating_a_circle(number_of_vertices,radius)
    return Polyhedron(vertices = Vertices)

def add_coefficient_at(i, coefficient, list_of_coefficients):
    l = list_of_coefficients
    return (l[:i] + [coefficient] + l[i:])

# Axis = 0 for x, 1 for y, 2 for z
def vertices_of_polyhedron_approximating_a_cylinder(number_of_facets, radius, length, axis=0):
    Vertices = vertices_of_polyhedron_approximating_a_circle(number_of_facets,radius)
    coef = length/2
    return [ add_coefficient_at(axis,-coef,v) for v in Vertices ] + [ add_coefficient_at(axis,coef,v) for v in Vertices ]

def polyhedron_approximating_a_cylinder(number_of_facets,radius,length, axis=0):
    Vertices = vertices_of_polyhedron_approximating_a_cylinder(number_of_facets, radius, length, axis)
    return Polyhedron(vertices = Vertices)

def fm_killer(number_of_facets):
    radius = 2
    length = 3 * radius
    cylinder = polyhedron_approximating_a_cylinder(2 * number_of_facets,radius,length,axis=0)
    #  x<=z  =~=  0<= -x -y + z =~= [0,-1,0,1]
    # -x<=z  =~=  0<=  x -y + z =~= [0,1,0,1]
    return cylinder.intersection(Polyhedron(ieqs = [ [0,-1,0,1] , [0,1,0,1] ]))

def fm_monster(number_of_facets):
    radius = 2
    length = 3 * radius
    cylinderX = polyhedron_approximating_a_cylinder(2 * number_of_facets,radius,length,axis=0)
    cylinderY = polyhedron_approximating_a_cylinder(2 * number_of_facets,radius,length,axis=1)
    cylinderZ = polyhedron_approximating_a_cylinder(2 * number_of_facets,radius,length,axis=2)
    return cylinderX.intersection(cylinderY).intersection(cylinderZ)
