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

class Curve:

    def __init__(self, lib: Library, data: Data, param_choice: ParamChoice, ord: Ordinate, color = 'black'):
        self.points = []
        self.color = color
        self.lib = lib
        self.gather(data, param_choice, ord)
        self.sort()
        self.stack()

    def gather(self, data: Data, param_choice: ParamChoice, ord: Ordinate) -> None:
        for instance in data.instances:
            if param_choice.satisfy(instance):
                x = param_choice.abs_value(instance)
                timings = instance.libs[self.lib.name]
                y = Curve.to_second(int(timings.timings[ord.tag()]))
                self.points.append((x, y))

    @staticmethod
    def to_second(time: int) -> float:
        '''Translates a time from ns to s'''
        return float(time)/float(pow(10,9))

    def sort(self) -> None:
    	self.points = sorted(self.points)

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

    def __init__(self, label: str):
        plt.xlabel(label)
        plt.ylabel('Time in ms (logscale)')
        plt.yscale('log')

    def add_curve(self, curve: Curve) -> None:
        xs = [p[0] for p in curve.points]
        ys = [p[1] for p in curve.points]
        es = [p[2] for p in curve.points]
        plt.errorbar(xs, ys, es, color = curve.color, label = curve.lib.name)

    def show(self) -> None:
        plt.legend(loc = 'best')
        plt.show()

import random
random.seed(1)
def random_color():
	r = lambda: random.random()
	return (r(),r(),r())
