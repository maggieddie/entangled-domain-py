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

