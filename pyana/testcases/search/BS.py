## {{{ http://code.activestate.com/recipes/81188/ (r1)
def binary_search(seq, t):
    min = 0; max = len(seq) - 1
    while 1:
        if max < min:
            return -1
        m = (min + max) / 2
        if seq[m] < t:
            min = m + 1
        elif seq[m] > t:
            max = m - 1
        else:
            return m
## end of http://code.activestate.com/recipes/81188/ }}}


