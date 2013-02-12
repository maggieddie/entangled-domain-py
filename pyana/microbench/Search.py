# The OO version of the simulated search cgi program backend.


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

class Response():
    def __init__(self, req):
        self.result = []
        self.body = ""
        self.req = req
        
    def gen_body(self):
        len_res = len(self.result)
        tmp = "<div id= \"search_results\">\n<ol>)\n"
        if len_res == 0:
            self.body = tmp + "<h3>Your search did not return any results.</h3>" + lst_to_str(self.req.getData())
        indx = 0
        while indx < len_res:
            self.body = self.body + "<li <a href=\"test/" + self.result[indx][1] + "?search=true&term=" + lst_to_str(self.req.getData()) +"\">\n" + self.result[indx][0] + "</a>\n"
            indx = indx + 1
        self.body = self.body + "</ol>\n</div>\n"    
            

class Search():
    def __init__(self, input):
        
        self.resp = Response(Request(input))
        
    def doSearch(self):
        # resp.result = search(self.req.getData())
        self.resp.req.santinize_proc()
        if self.resp.req.getData() == -1:
            print("no scripting allowed")
            return
        
        l = len(self.resp.req.getData())
        #mock
        if l > 3:
            self.resp.result = []
        else:
            self.resp.result = [("Colors", 'colors.html'),
                                ("Numbers", 'numbers.html'),
                                ("Numbers and Colors", "numbersandcolors.html"),
                                ("Numbers and Colors With Tag", "numbersandcolorswithtag.html")]
        self.resp.gen_body()
        
    def render(self):
        print(self.resp.body)
    
#########

def test_init_input():
    simulated_cgi_entry1 = {"terms": ["red", "blue","three"], "boolean" : OR,
                            "case": CASE_SENSITIVE,
                            "files": ['colors.html', 'numbers.html', 'numbersandcolors.html', 'numbersandcolorswithtag.html']}
   
    return simulated_cgi_entry1["terms"]

def mock_mal_strlst ():
    return ['e', '<', 's', 'c', 'r','i', 'p', 't', '>']
def gen_malicious_input():
    simulated_cgi_entry2 = {"terms": mock_mal_strlst()}
    return simulated_cgi_entry2["terms"]

srch1 = Search(test_init_input())
srch1.doSearch()
srch1.render()
srch2 = Search(gen_malicious_input())
srch2.doSearch()
srch2.render()

