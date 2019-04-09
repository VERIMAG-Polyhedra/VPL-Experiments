# Michael PÉRIN, Verimag / Université Grenoble-Alpes, July 2017
#
# Polyehdra made from cylinders
# =============================================================================

load("polyhedron.sage")
load("cylinder.sage")

# CIRCLE

Circle = polyhedron_approximating_a_circle(30,2)
G2D = Circle.plot()
#G2D.show(frame = False)

# CYLINDER
Cyl = polyhedron_approximating_a_cylinder(30,2,6)
G = polyhedron_plot_with_wireframe(Cyl)
G = polyhedron_plot_with_vertices(Cyl,'blue')
G.show(frame = False, viewer='threejs')

# unbounded cylinder
Icyl = Cyl.inequalities_list()
Ucyl = Polyhedron(ieqs= Icyl[1:])
G = polyhedron_plot_with_vertices(Ucyl,'blue')
G.show(frame = False, viewer='threejs')

# Example of doubling THE NUMBER OF FACES BY projection

G = Graphics()
FMK = fm_killer(10)
G += polyhedron_plot_with_wireframe(FMK)
SHD = shadow_along_axis(FMK,2)
G += polyhedron_plot_with_wireframe(SHD, face_color='gray', wire_color='red')
G.show(frame = False, viewer='threejs')


# JAPANESE PAPER LAMP

# The construction

G = Graphics()
CX = polyhedron_approximating_a_cylinder(10,2,6, axis=0)
G += polyhedron_plot_with_wireframe(CX, face_color='green')
CY = polyhedron_approximating_a_cylinder(10,2,16, axis=1)
G += polyhedron_plot_with_wireframe(CY, face_color='yellow')
#G.show(frame = False, viewer='threejs')

# The result

FMM = fm_monster(25)
G = polyhedron_plot_with_wireframe(FMM)
SHD = shadow_along_axis(FMM,2)
G += SHD.plot()
G += polyhedron_plot_with_wireframe(SHD, face_color='gray', wire_color='red')
#G.show(frame = False, viewer='threejs')
