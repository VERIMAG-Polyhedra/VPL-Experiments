from lib.library import *
from lib.data import *
from lib.utils import *
from lib.make_curve import *
import os

# Config
dims = range(4,8)
cstrs = range(8,20)
target_dims = range(2,4)
n_assume_back_cstrs = range(2,6)

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

wanted_libs = ["Newpolka", "VPL"]
xml_file = 'sas19_noab.xml'

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

def make_assume_back_replacement(other_polyhedron, n_assume_back: int):
    def make_cstr(cstr):
        l = cstr.split('<=')
        vars = [x for x in l[0].split(' ') if x is not '']
        lin = ' + '.join(['%s*x%i' % (s,i) for (i,s) in enumerate(vars) if s is not '0'])
        return '%s <= %s' % (lin, l[1])

    s = 'abs_value P%i = guard(P%i, %s);\nabs_value P%i = projincl(P%i, P2);\n'
    param = [s % (4, 1, make_cstr(other_polyhedron[0]), 5, 4)]
    for i in range(1,min(n_assume_back, len(other_polyhedron))):
        cstr = other_polyhedron[i]
        j = 4+2*i
        k = 2+2*i
        param.append(s % (j, k, make_cstr(cstr), j+1, j))
    return '\n'.join(param)

def make_parameters(n_cstr: int, n_dim: int, target_dim: int, n_assume_back: int,
    polyhedron_name: str, other_polyhedron: str) -> Parameters:
    params = Parameters()
    params.add(make_vars(n_dim))
    poly_name = polyhedron_name.split('_')[1]
    params.add("P_%i_%i_0.%s" % (n_dim, n_cstr, poly_name))
    params.add(make_hypercube(target_dim, 10))
    params.add(make_assume_back_replacement(other_polyhedron, n_assume_back))
    return params

# Execution
for n_cstr in cstrs:
    for n_dim in [n_dim for n_dim in dims if n_dim < n_cstr]:
        for target_dim in [t_dim for t_dim in target_dims if t_dim < n_dim]:
            for n_assume_back_cstr in n_assume_back_cstrs:
                print("TEST", n_cstr, n_dim, target_dim, n_assume_back_cstr)
                polyhedra = get_polyhedra(n_cstr, n_dim)
                for polyhedron_name in polyhedra:
                    other_polyhedron_name = [name for name in polyhedra if name is not polyhedron_name][0]
                    params = make_parameters(n_cstr, n_dim, target_dim, n_assume_back_cstr, polyhedron_name, polyhedra[other_polyhedron_name])
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
