

#lst = [10,20,30,40]

# some indexes in the mesh
#mash = [0,5,1,4]

def emit_mesh(verts, mesh, index):
    if (index >= len(verts)):
        return
    else:
        print(verts[mesh[index]])
        emit_mesh(verts, mesh, index+1)

def perform_tast():
    lst = [10,20,30,40]
    mesh = [0,5,2,1]
    emit_mesh(lst, mesh, 0)

perform_tast()


    
