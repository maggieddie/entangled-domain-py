def loop(index, lst):
    total_len = len(lst)
    if(index < total_len):
        print(lst[index])
        index = 1 + index
        loop(index, lst)
    else:
        return

#def simple_loop(lst):
#    loop(0, lst)
        
glst = [1,2,3,4]
#simple_loop(lst)
loop(0,glst)




                
