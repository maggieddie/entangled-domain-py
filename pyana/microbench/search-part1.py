OR = "OR"
CASE_SENSITIVE = "Sensitive"
SCRIPT_PAT = ['<', 's', 'c', 'r', 'i', 'p','t', '>']

# little helper
def lst_to_str(lst):
    #    print(lst)
    l = len(lst)
    i = 0
    res_str =""
    while(i<l):
        res_str = res_str + lst[i]
        i = i+1
    return res_str

# small script searcher
def issubstring(s, p):
   # print(s)
    i = 0
    tmpl = len(p)
    while i <= len(s) - tmpl:
        j = i
        for c in p:
            if c != s[j]:
                break
            else:
                j = j+1
                if j - i == tmpl:
                    return True
        i = i+1
    return False


##

class Request():
    def __init__(self, data):

        self.data = data

    # mimic the santimizer wrapper to some standard lib    
    def santinize_proc(self):
#        print(self.data)
        res = issubstring(self.data, SCRIPT_PAT)
 #       print(res)
        if res == True:
            self.data = -1 

    def getData(self):
        return self.data

