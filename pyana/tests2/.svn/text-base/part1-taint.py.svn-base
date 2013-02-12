OR = "OR"
CASE_SENSITIVE = "Sensitive"
SCRIPT_PAT = ['<', 's', 'c', 'r', 'i', 'p','t', '>']

# eliminates the file open read and string find operations.

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
	   i = 0
	   while i <= len(s) - len(p):
	       j = i
	       for c in p:
	           if c != s[j]:
	               break
	           else:
	               j = j+1
	               if j - i == len(p):
	                   return True
	       i = i+1
	   return False

# simple tiny part to simulate santinization process
def santinize_proc(lst):
    return issubstring(lst, SCRIPT_PAT)