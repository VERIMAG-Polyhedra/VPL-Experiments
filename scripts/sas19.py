from lib.library import *
from lib.data import *
from lib.utils import *
from lib.make_curve import *
import os

data_file = get_data_file('sas19.xml')
libs_file = get_libs_file()

data = Data(data_file)
libs = import_libs(libs_file)

def make_parameters() -> Parameters:
    params = Parameters()
    params.add("P_7_10_0.1554809760632")
    params.add("VAR1 >= -1 && VAR1 <= 1 && VAR2 >= -1 && VAR2 <= 1")

    return params

params = make_parameters()
data.make_source_file(params)

for (_,lib) in libs.items():
    if data.instance_exist(params, lib):
        print('Instance already ran for library %s and parameters %s' % (lib.name,
            params))
    else:
        lib.run()
        ins = data.get_instance(params)
        ins.add_exp(lib)
data.export('sas19.xml')

a = AbscissaChoice(Abscissa.CSTR, 0)
a.set(Abscissa.DIM, 0, 7)
ord = Ordinate.TOTAL
g = Graph()

for (_,lib) in libs.items():
    c = Curve(lib, data, a, ord, color = random_color())
    print(lib.name, c.points)
    g.add_curve(c)

g.show()
