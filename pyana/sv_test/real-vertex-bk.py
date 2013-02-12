# this is not intended to run by python.
# read_int() will treated as external libs
# read_verts is the same.

＃import random

def read_array(a, i , lo, hi):
    if (i>= len(a)):
        return
    else:
        num = read_int()
        ＃random.randrange(0,10)
        a[i] = num
        i = i + 1
        read_array(a, i, lo, hi)

# this is also uninterpreted
def read_verts():    
    # the array returned will be uninterpreted.
    return [1,2,3,4]


# recursive structure
# here we want to prove that the code will not
# have out-of bound error.

def emit_mesh(vrt, mesh, i):
    if (i>= len(mesh)):
        return
    else:
        elem_ind = mesh[i]
        elem = vrt[elem_ind]
        print elem 
        i = i+1
        emit_mesh(vrt, mesh, i)

# tiny harness code        
def test():
    vertex = read_verts()
    mesh = read_array(vertex,0, 0, len(vertex))
    emit_mesh(vertex, mesh, 0)

    
        
#read_array([1,2],0,0,0
