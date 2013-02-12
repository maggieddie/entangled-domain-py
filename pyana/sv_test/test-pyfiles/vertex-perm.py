
lst = [10,20,30,40]

mash = [0,2,1,3]

def read_arry(i):
    if (i < len(lst)):
        cur_index = lst[mash[i]]
        print(cur_index)
        i = i+1
        read_arry(i)
    else:
        return

read_arry(0)

    
