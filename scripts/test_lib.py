from lib.library import *

libs = import_libs('test.xml')
vpl = libs["VPL"]

print(libs["VPL"].execution_string())

from lib.data import *

data = Data('data_test.xml')

print(data)

i = data.instances[0]
print(i.has_lib(vpl))
