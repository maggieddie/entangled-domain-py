def rest(lstr):
    res = []
    
    total_len = len(lstr)
    
    if(total_len == 0):
        return lstr
    elif(total_len == 1):
        return []
    else:
        i = 1
        while (i < total_len): 
            res.append(lstr[i])
            i = i+1

        return res



def client(lstc, mask_list):
    def g(j, y):
        lstc[j] = (y & mask_list[j])
        
    if(len(lstc) == len(mask_list)):
        iter(0, mask_list, g)
        return lstc
     
    else:
        return []

def iter(index, lsti, func):
    if(len(lsti) == 0):
        return []
    
    else: 
        func(index, lsti[0])
        iter (index+1, rest(lsti), func)
                        
        


lst = [1,2,3,4]
msl = [0,0,0,0]
lst2 = [10,20,30,40]
msl2 = [1,2,3,4]

result1 = client(lst, msl)
result2 = client(lst2, msl2)
print(result1)
print(result2)


