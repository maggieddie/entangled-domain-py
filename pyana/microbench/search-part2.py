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
