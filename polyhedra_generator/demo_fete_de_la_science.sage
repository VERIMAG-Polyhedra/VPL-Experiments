# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# Plot raytracing
# This version draws every facet that is shown irredundant by their orthogonal ray.
# =============================================================================

load("sphere.sage")

random.seed(3)

# dimension
Dimension = 3
Constraints = 500
Non_zero_coefficients = 3
Rayon = 10

# Number of colored faces
nb_colored_faces = 300

S = sphere(Constraints, Dimension, Non_zero_coefficients, Rayon)

#ipoint = vector((random.randint(-2,2), random.randint(-2,2),random.randint(-2,2)))
ipoint = vector((0,0,0))

# Raytracing
def normal(C):
	return vector(C[1:])

def inters_t(C, d, x):
	a = normal(C)
	den = a.dot_product(d)
	if den == 0:
		return None
	r = (-1*C[0] -  a.dot_product(x)) / a.dot_product(d)
	if r > 0:
		return r
	return None

def intersection(C,ipoint):
	n = -1*normal(C)
	t = inters_t(C, n, ipoint)
	return ipoint + t * n

def get_colored_faces(ineqs, ipoint, n_colored_faces):
	colored_faces = []
	i = 0
	ids = range(0,len(ineqs))
	while i < n_colored_faces and len(ids) >= 1 :
		id_ids = random.randint(0,len(ids)-1)
		id = ids[id_ids]
		face = ineqs[id]
		d = -1*normal(face)
		t_s = [inters_t(C, d, ipoint) for C in ineqs]
		if t_s[id] == min([t for t in t_s if t >= 0]):
			colored_faces.append(face)
			ids.pop(id_ids)
			i += 1
		else:
			ids.pop(id_ids)
	return colored_faces

def get_irredundant_faces(ineqs, ipoint):
	colored_faces = []
	for i in range(0,len(ineqs)):
		face = ineqs[i]
		d = -1*normal(face)
		t_s = [inters_t(C, d, ipoint) for C in ineqs]
		id = numpy.argmin([t for t in t_s if t >= 0])
		face_to_add = ineqs[id]
		if not(face_to_add in colored_faces):
			colored_faces.append(face_to_add)
	return colored_faces

# RENDERING
G = S.projection().render_wireframe_3d(thickness = 2)

G += point(ipoint, size = 10, color = 'black')

ineqs = S.inequalities_list()

colored_faces = get_colored_faces(ineqs, ipoint, nb_colored_faces)
print('faces chosen to be colored: ', colored_faces)

# Render filled faces:
for face in colored_faces:
	P = S.intersection(Polyhedron(eqns = [face]))
	G += P.projection().render_solid_3d(alpha = 0.3, color = 'green')

# Render rays
for face in colored_faces:
	n = normal(face)
	w = ipoint - 20*n.normalized()
	G += line([ipoint,w], color = 'red', thickness = 1)

# Render intersection points
for face in colored_faces:
	inter = intersection(vector(face), ipoint)
	G += point(inter, color = 'black', size = 10)

G.show(frame = False)#, viewer='threejs')
