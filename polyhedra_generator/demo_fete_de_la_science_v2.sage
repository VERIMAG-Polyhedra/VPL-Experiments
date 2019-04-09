# Alexandre MARÉCHAL, Verimag / Université Grenoble-Alpes, July 2017
#
# Plot raytracing
# This version draws every facet that is shown irredundant by raytracing (but not necessarily by their orthogonal ray).
# =============================================================================
load("sphere.sage")

random.seed(3)

# dimension
Dimension = 3
Constraints = 150
Non_zero_coefficients = 3
Rayon = 10


#ipoint = vector((random.randint(-6,6), random.randint(-6,6),random.randint(-6,6)))
ipoint = vector((0,0,0))

S = sphere(Constraints, Dimension, Non_zero_coefficients, Rayon)

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

def intersection(C, d, ipoint):
	t = inters_t(C, d, ipoint)
	return ipoint + t * d

def get_irredundant_faces(ineqs, ipoint):
	colored_faces = []
	face_association = []
	for face in ineqs:
		d = -1*normal(face)
		t_s = [inters_t(C, d, ipoint) for C in ineqs]
		mini = min([t for t in t_s if t >= 0])
		for i in range(0,len(ineqs)):
			if inters_t(ineqs[i], d, ipoint) == mini:
				id = i
				break
		face_to_add = ineqs[id]
		face_association.append((face, face_to_add))
		#if not(face_to_add in colored_faces):
		colored_faces.append(face_to_add)
	return (colored_faces,face_association)

# RENDERING
G = S.projection().render_wireframe_3d(thickness = 2)

G += point(ipoint, size = 10, color = 'black')

ineqs = S.inequalities_list()

(colored_faces,face_association) = get_irredundant_faces(ineqs, ipoint)
print('faces chosen to be colored: ', colored_faces)

# Render filled faces:
for face in colored_faces:
	P = S.intersection(Polyhedron(eqns = [face]))
	G += P.projection().render_solid_3d(alpha = 0.3, color = 'green')

# Render rays
for face in ineqs:
	n = normal(face)
	w = ipoint - 15*n.normalized()
	G += line([ipoint,w], color = 'red', thickness = 1)

# Render intersection points
for (face, first_face_hit) in face_association:
	inter = intersection(first_face_hit, -1*normal(face), ipoint)
	G += point(inter, color = 'black', size = 10)

G.show(frame = False)#, viewer='threejs')
