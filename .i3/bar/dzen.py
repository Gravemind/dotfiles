#!/usr/bin/python3

import math

def draw_r(w, h):
    return '^r(' + str(w) + 'x' + str(h) + ')'

class Bar:
    width = 0
    height = 0
    w_seg = 0
    w_sep = 0
    vmin = 0
    vmax = 100
    _wsum = 0
    _nb_seg = 0
    _out = ''

    def __init__(self,
                 width = 80, height = 20,
                 vmin = 0, vmax = 100,
                 w_segment = 0, w_separator = 0):
        self.width = width
        self.height = height
        self.vmin = vmin
        self.vmax = vmax
        self.w_seg = w_segment
        self.w_sep = w_separator
        if self.w_seg > 0:
            self._nb_seg = self.width // (self.w_seg + self.w_sep)

    def push(self, value, color):
        self._out += '^fg(' + color + ')'
        width = ((value - self.vmin) * self.width) // self.vmax
        if self.w_seg == 0:
            self._out += draw_r(width, self.height)
            self._wsum += width
        else:
            nb_seg = width // (self.w_seg + self.w_sep)
            for i in range(nb_seg):
                self._out += draw_r(self.w_seg, self.height)
                self._out += '^p(' + str(self.w_sep) + ';)'
            self._wsum += nb_seg

    def draw(self, bg_color):
        self._out += '^fg(' + bg_color + ')'
        if self.w_seg == 0 and self._wsum < self.width:
            self._out += draw_r(self.width - self._wsum, self.height)
        elif self.w_seg > 0 and self._nb_seg > self._wsum:
            for i in range(self._nb_seg - self._wsum):
                self._out += draw_r(self.w_seg, self.height)
                self._out += '^p(' + str(self.w_sep) + ';)'
        return self._out


class DZen:
    dico = {}
    out = ''

    def __init__(self, dico = {}):
        self.dico = dico

    def reset(self):
        self.out = ''

    def a(self, *out):
        for o in out:
            self.out += o
        return self

    def fg(self, v = ''):
        return self.elem('fg', v)

    def bg(self, v = ''):
        return self.elem('bg', v)

    def i(self, v = ''):
        return self.elem('i', v)

    def p(self, x = 0, y = 0):
        o = ''
        if x != 0:
            o += '{:+}'.format(x)
        o += ';'
        if y != 0:
            o += '{:+}'.format(y)
        return self.a('^p(', o, ')')

    def pa(self, v = ''):
        return self.elem('pa', v)

    def elem(self, tag, value = ''):
        if value == '':
            return self.a('^', tag, '()')
        if value in self.dico:
            return self.a('^', tag, '(', self.dico[value], ')')
        return self.a('^', tag, '(', value, ')')
