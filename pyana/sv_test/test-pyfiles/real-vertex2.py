def read_array(a, i , lo, hi):
    if (i>= len(a)):
        return
    else:
        num =   read_int()
        if (lo<= num and num<hi):
            a[i] = num
            i = i + 1
            read_array(a, i, lo, hi)
        else:
            return
        
# this is also uninterpreted
def read_verts():    
    # the array returned will
    # be uninterpreted.
    return [1,2,3,4]

def emit_mesh(vrt, mesh, i):
    if (i>= len(mesh)):
        return
    else:
        elem_ind = mesh[i]
        elem = vrt[elem_ind]
        #print(elem) 
        i = i+1
        emit_mesh(vrt, mesh, i)

# tiny harness code        
def test():
    mesh = [1,1,1,1]
    vertex = read_verts()
    read_array(mesh,0, 0, len(vertex))
    emit_mesh(vertex, mesh, 0)
    #print(vertex)
    #print(mesh)
    
test()

