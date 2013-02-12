lst = [1,2,3]
#direct access
#lst[3]

# in a procedure
def simple_arr_set(x):

    if (x < len(lst)):
        lst[x]=100
        return 100
    else:
        return -1
    
simple_arr_set(10)
print(lst)



