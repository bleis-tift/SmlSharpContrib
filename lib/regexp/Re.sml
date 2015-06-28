structure Re =
struct
    structure RA = RegexpAST(Char)
    structure RL = RegexpLexer(structure C = Char
                               structure S = String)
    structure RP = RegexpParser(structure C = Char
                                structure S = String
                                structure AST = RA
                                structure Lexer = RL)
    structure RI = RegexpInter(structure C = Char
                               structure S = String
                               structure AST = RA)
    structure RE = Regexp(structure S = String
                          structure Matcher = RI)
    exception Lex = RL.Lex
    exception Parse = RP.Parse
    val re = RP.re                 
    open RE
end
