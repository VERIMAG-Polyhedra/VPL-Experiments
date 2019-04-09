# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, mars 2017
#
#  Generation of cones with N-facets
# ==============================================================================

N = 10

I = range(0,N)
A = [ k * (2*pi/N) for k in I ]

print(A)

XY = [ (cos(a),sin(a)) for a in A ]

print(XY)

def rational_approx(exact_expression):
    d = 10 * N
    n = floor(d * float(exact_expression))
    return Rational((n,d))

XY = [ (rational_approx(x), rational_approx(y)) for (x,y) in XY ]

print(XY)

Zmax = 1
Rays = [ (x,y,1) for (x,y) in XY ]

print(Rays)

Flower_cone = Cone( Rays ) ;

P_cone  = Polyhedron(rays = Rays)

P_plane = Polyhedron(ieqs= [[Zmax,0,0,-1]])  # equiv.  -z + Zmax >=0   ie.  z <= -Zmax

P_sliced_cone = P_cone.intersection(P_plane)

print (P_sliced_cone.Hrepresentation())

G = Graphics()
G += P_plane.plot()
G += P_sliced_cone.plot()
#G += Flower_cone.plot()
G.show()
