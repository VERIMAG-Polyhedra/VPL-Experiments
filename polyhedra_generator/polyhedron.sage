
# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
#  Basic functions on polyhedra
#
# ==============================================================================

#def basic_make_from_vertices(vectors):
#    return Polyhedron(vertices = vectors)

#def basic_make_from_inequalities(vectors):
#    return Polyhedron(ineq = vectors)

def polyhedron_vertices_of(polyhedron):
    return polyhedron.vertices_list()

def polyhedron_vertices_as_vectors(vertices):
    return [ vector(v) for v in vertices ]

# HOW MANY ?

def number_of_dimension(polyhedron):
    return polyhedron.ambient_dim()

def number_of_variables(polyhedron):
	return polyhedron.ambient_dim()

def number_of_generators(polyhedron):
	return len(polyhedron.Vrepresentation())

def number_of_vertices(polyhedron):
	return len(polyhedron.vertices_list())

def number_of_constraints(polyhedron):
	return len(polyhedron.Hrepresentation())

def number_of_inequalities(polyhedron):
	return len(polyhedron.inequalities_list())

# projection
# Axis = 0 for 'x', 1 for 'y', ...

def remove_coordinate_of(i, list_of_coefficients):
    del(list_of_coefficients[i])
    return list_of_coefficients

def set_coordinate_to(i, coef, list_of_coefficients):
    list_of_coefficients[i]=coef
    return list_of_coefficients

def min_of_coordinate(i, list_of_vertices):
    return min([ v[i] for v in list_of_vertices ])

def shadow_along_axis(polyhedron, i):
    dimension = number_of_dimension(polyhedron)
    direction = [0] * dimension
    direction[i]=1
    minkowsky = polyhedron + Polyhedron(lines=[ direction ])
    m = min_of_coordinate(i, polyhedron.vertices_list())
    x_i_equal_m = [m] + direction
    return minkowsky.intersection(Polyhedron(eqns = [x_i_equal_m]))


# Eliminate variable (which is a string) from polyhedron P
# variables (list of string variables) must contain variable
def variable_elimination(P, variable, variables):
	ring = get_ring(variables)
	line = []
	for v in variables:
		if v == variable:
			line.append(1)
		else:
			line.append(0)
	Pline = Polyhedron(lines = [line])
	Res = P + Pline
	# +1 because of the constant
	i = variables.index(variable) + 1
	eqs = []
	for eq in Res.equations_list():
		eq.pop(i)
		eqs.append(eq)
	ineqs = []
	for ineq in Res.inequalities_list():
		ineq.pop(i)
		ineqs.append(ineq)
	return Polyhedron(ieqs = ineqs, eqns = eqs)

# RENDERING

def polyhedron_plot_with_vertices(polyhedron, the_color):
    g = Graphics()
    vertices = polyhedron_vertices_of(polyhedron)
    for p in vertices:
        g += point(p, color = the_color, size=15)
    g += polyhedron.plot(color = 'yellow')
    return g

def polyhedron_plot_from_vertices(vertices, the_color):
    polyhedron = Polyhedron(vertices = vertices)
    return polyhedron_plot_with_vertices(polyhedron, the_color)

def polyhedron_plot_with_wireframe(P, face_color = 'yellow', wire_color = 'blue'):
	G = Graphics()
	dim = number_of_variables(P)
	if dim == 1:
		G = P.projection().render_line_1d()
	elif dim == 2:
		G = P.projection().render_fill_2d(color = face_color)
		G += P.projection().render_outline_2d(thickness = 2,color = wire_color)
	elif dim == 3:
		G = P.projection().render_solid_3d(opacity = 0.2, color = face_color)
		G += P.projection().render_wireframe_3d(thickness = 2, color = wire_color)
	else:
		print('Polyhedron of dimension %i is not plotable' % (dim))
	return G
