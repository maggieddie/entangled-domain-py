
lst = [10,20,30,40]



def read_arry(i, mash):
    if (i < len(lst)):
        cur_index = lst[mash[i]]
        print(cur_index)
        i = i+1
        read_arry(i, mash)
    else:
        return

def test():
    tmash = [0,2,1,3]
    read_arry(0, tmash)


test()



    
