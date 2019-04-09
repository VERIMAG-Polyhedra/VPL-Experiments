# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, juin 2017
#
# Generation of rotated polyhedra
# ==============================================================================

load("polyhedra_descriptor.py")
load("rotate_polytope.sage")

def generate_rotations_of(polyhedron, number_of_rotations):
    cs = create_rational_cosinus_sinus(number_of_rotations+1) # couples of rational (cos,sin)
    polyhedra = [ polyhedron ]
    for i in range(number_of_rotations-1):
        polyhedron = rotate_polyhedron(polyhedron, cs[i+1] )
        polyhedra += [ polyhedron ]
    return polyhedra

def generate_rotated_in_folder_from(directory, name, initial_polyhedron, number_of_rotations):
    polyhedron = initial_polyhedron
    subdirectory = name + "/"
    folder = directory + subdirectory
    cs = create_rational_cosinus_sinus(number_of_rotations+1) # couples of rational (cos,sin)
    for i in range(number_of_rotations-1):
        polyhedron = rotate_polyhedron(polyhedron, cs[i+1] )
        descriptor = build_polyhedron_descriptor(name, polyhedron)
        output_polyhedron_descriptor_in_folder(folder, descriptor)

def generate_rotated_in_folder_from_polyhedra(directory, name, polyhedra, number_of_rotations):
    for polyhedron in polyhedra:
        generate_rotated_in_folder_from(directory, name, polyhedron, number_of_rotations)
