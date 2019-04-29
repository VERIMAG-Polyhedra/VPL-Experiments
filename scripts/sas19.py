from lib.library import *
from lib.data import *
from lib.utils import *
from lib.make_curve import *
import os

# Config
dims = range(4,8)
cstrs = [14]#range(8,15)
target_dims = range(2,4)
n_assume_back_cstrs = range(2,10)

libs_file = get_libs_file()
bench_folder = get_bench_folder()

# Exec ProjIncl
wanted_libs = ["VPL_PROJINCL"]
xml_file = 'sas19.xml'
data_file = get_data_file(xml_file)

# Reading data from files
data = Data(data_file)
# Loading libraries
libs = {name:value for (name,value) in import_libs(libs_file).items() if name in wanted_libs}

def make_vars(dim):
    return ', '.join(['x%i' % i for i in range(dim)])

def make_hypercube(dim, size):
    def cstr(i):
        return 'x%i >= -%i && x%i <= %i' % (i, size, i , size)
    return ' && '.join([cstr(i) for i in range(dim)])

def make_parameters(n_cstr, n_dim, target_dim, n_assume_back_cstr, polyhedron_name, other_polyhedron_name) -> Parameters:
    params = Parameters()
    params.add(make_vars(n_dim))
    poly_name = polyhedron_name.split('_')[1]
    params.add("P_%i_%i_0.%s" % (n_dim, n_cstr, poly_name))
    params.add(make_hypercube(target_dim, 10))
    other_poly_name = other_polyhedron_name.split('_')[1]
    params.add("P_%i_%i_0.%s" % (n_dim, n_assume_back_cstr, other_poly_name))
    return params

# Execution
for n_cstr in cstrs:
    for n_dim in [n_dim for n_dim in dims if n_dim < n_cstr]:
        for target_dim in [t_dim for t_dim in target_dims if t_dim < n_dim]:
            for n_assume_back_cstr in n_assume_back_cstrs:
                print("TEST", n_cstr, n_dim, target_dim, n_assume_back_cstr)
                polyhedra = get_polyhedra(n_cstr, n_dim)
                polyhedra2 = get_polyhedra(n_assume_back_cstr, n_dim)
                for polyhedron_name in polyhedra:
                    other_polyhedron_name = [name for name in polyhedra2][0]
                    params = make_parameters(n_cstr, n_dim, target_dim, n_assume_back_cstr, polyhedron_name, other_polyhedron_name)
                    data.make_source_file(params)

                    for (_,lib) in libs.items():
                        if data.instance_exist(params, lib):
                            print('Instance already ran for library %s and parameters %s' % (lib.name,
                                params))
                        else:
                            lib.run()
                            ins = data.get_instance(params)
                            ins.add_exp(lib)

            # Exporting data to file
            data.export(xml_file)
