import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import math
from lib.utils import *
from lib.data import *
from enum import Enum
from typing import Union

class Ordinate(Enum):
    TOTAL = 1
    PROJ = 2
    JOIN = 3
    ASSUME = 4
    PROJINCL = 5
    ASSUMEBACK = 6
    ASSIGN = 7
    MIN = 8
    WIDEN = 9
    NEW_REGIONS = 10
    DELETED_REGIONS = 11
    UPDATED_REGIONS = 12
    TOTAL_REGIONS = 13

    def label(self) -> str:
        if self == Ordinate.TOTAL:
            return 'Total'
        elif self == Ordinate.PROJ:
            return 'projection'
        elif self == Ordinate.JOIN:
            return 'Join'
        elif self == Ordinate.ASSUME:
            return 'Assume'
        elif self == Ordinate.PROJINCL:
            return 'Proj_inclusion'
        elif self == Ordinate.ASSUMEBACK:
            return 'Assume_back'
        elif self == Ordinate.ASSIGN:
            return 'Assignment'
        elif self == Ordinate.MIN:
            return 'Minimization'
        elif self == Ordinate.WIDEN:
            return 'Widening'
        elif self == Ordinate.NEW_REGIONS:
            return 'New regions'
        elif self == Ordinate.DELETED_REGIONS:
            return 'Deleted regions'
        elif self == Ordinate.UPDATED_REGIONS:
            return 'Updated regions'
        elif self == Ordinate.TOTAL_REGIONS:
            return 'Total number of regions'

    def tag(self) -> str:
        if self == Ordinate.TOTAL:
            return 'total'
        elif self == Ordinate.PROJ:
            return 'project'
        elif self == Ordinate.JOIN:
            return 'join'
        elif self == Ordinate.ASSUME:
            return 'assume'
        elif self == Ordinate.PROJINCL:
            return 'proj_incl'
        elif self == Ordinate.ASSUMEBACK:
            return 'assume_back'
        elif self == Ordinate.ASSIGN:
            return 'assign'
        elif self == Ordinate.MIN:
            return 'min'
        elif self == Ordinate.WIDEN:
            return 'widen'
        elif self == Ordinate.NEW_REGIONS:
            return 'new_regions'
        elif self == Ordinate.DELETED_REGIONS:
            return 'deleted_regions'
        elif self == Ordinate.UPDATED_REGIONS:
            return 'updated_regions'
        elif self == Ordinate.TOTAL_REGIONS:
            return 'total_regions'

class ParamChoice:

    # i_abs: abscissa parameter
    def __init__(self, i_abs: int, get_value):
        self.get_values = dict()
        self.i_abs = i_abs
        self.get_abs_value = get_value

    def set(self, i_param: int, get_value, value: Union[int, float]) -> None:
        if i_param not in self.get_values:
            self.get_values[i_param] = []
        self.get_values[i_param].append((get_value, value))

    def satisfy(self, ins: Instance) -> bool:
        for i in range(len(ins.parameters.values)):
            param = ins.parameters.values[i]
            if i in self.get_values:
                for (get_value, value) in self.get_values[i]:
                    if get_value(param) != value:
                        return False
        return True

    def abs_value(self, ins: Instance):
        param = ins.parameters.values[self.i_abs]
        return self.get_abs_value(param)

    def copy_from(self, p2: 'ParamChoice') -> None:
        self.get_values = dict(p2.get_values)

def to_seconds(time: int) -> float:
    '''Translates a time from ns to s'''
    return float(time)/float(pow(10,9))

class Curve:

    def __init__(self, lib: Library, data: Data, param_choice: ParamChoice, ord: Ordinate, color = 'black', to_seconds = False, label = None):
        self.points = []
        self.color = color
        self.lib = lib
        if label is None:
            self.label = ord.label()
        else:
            self.label = label
        self.gather(data, param_choice, ord)
        if to_seconds:
            self.to_seconds()
        self.sort()
        self.stack()

    def gather(self, data: Data, param_choice: ParamChoice, ord: Ordinate) -> None:
        for instance in data.instances:
            if param_choice.satisfy(instance):
                x = param_choice.abs_value(instance)
                timings = instance.libs[self.lib.name]
                y = int(timings.timings[ord.tag()])
                self.points.append((x, y))

    def sort(self) -> None:
    	self.points = sorted(self.points)

    def to_seconds(self) -> None:
        self.points = [(x, to_seconds(y)) for (x,y) in self.points]

    @staticmethod
    def make_point(x,ys):
        n = len(ys)
        mean = sum(ys) / n
        sd = math.sqrt((1/n) * sum([(y - mean)**2 for y in ys]))
        return (x, mean, sd)

    def stack(self) -> None:
        '''Stacks all points that share the same abscissa
        Requires a sorted list of points'''
        if self.points != []:
            points = []
            x_prec = self.points[0][0]
            ys = [self.points[0][1]]
            for (x,y) in self.points[1:]:
                if x == x_prec:
                    ys.append(y)
                else:
                    points.append(Curve.make_point(x_prec,ys))
                    x_prec = x
                    ys = [y]
            #last point :
            points.append(Curve.make_point(x_prec,ys))
            self.points = points

class Graph:

    def __init__(self, xlabel = 'No label',
        ylabel = 'Execution Time (in seconds)'):
        plt.xlabel(xlabel)
        plt.ylabel(ylabel)
        #plt.yscale('log')

    def add_curve(self, curve: Curve, label = None, linestyle = '-') -> None:
        xs = [p[0] for p in curve.points]
        ys = [p[1] for p in curve.points]
        es = [p[2] for p in curve.points]
        if label is None:
            label = curve.lib.name
        plt.errorbar(xs, ys, es, label = label, color = curve.color, linestyle = linestyle)

    def show(self) -> None:
        plt.legend(loc = 'best')
        plt.show()

import numpy
class Histogram:

    def __init__(self, xlabel = 'no label',
        ylabel = 'no label'):
        plt.xlabel(xlabel)
        plt.ylabel(ylabel)
        self.curves = []
        self.hatches = []

    def add_curve(self, curve: Curve, hatch = None) -> None:
        self.curves.append(curve)
        self.hatches.append(hatch)

    def show(self):
        xs = numpy.array([p[0] for p in self.curves[0].points])
        ys = [numpy.array([int(p[1]) for p in curve.points]) for curve in self.curves]
        for i in range(len(ys)):
            b = []
            if i + 1 < len(ys):
                b = ys[i+1]
                for j in range(i+2,len(ys)):
                    b = b + ys[j]
            if b == []:
                plt.bar(xs, ys[i], color = self.curves[i].color,
                label = self.curves[i].label,
                hatch = self.hatches[i],
                linewidth = 1,
                edgecolor = 'black')
            else:
                plt.bar(xs, ys[i], color = self.curves[i].color,
                bottom = b,
                label = self.curves[i].label,
                hatch = self.hatches[i],
                linewidth = 1,
                edgecolor = 'black')

        plt.legend(loc="best")
        plt.show()

import random
random.seed(1)
def random_color():
	r = lambda: random.random()
	return (r(),r(),r())
