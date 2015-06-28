structure RegexpParserTest =
struct
    structure Lexer = RegexpLexer(structure C = Char
                                  structure S = String)
    structure RP = RegexpParser(structure C = Char
                                structure S = String
                                structure AST = RegexpAST(Char)
                                structure Lexer = Lexer)
open SMLUnit
open RP
open Assert
exception Lex = Lexer.Lex

fun assertWork str =
  (re str; assert "function work" true)
  handle a => fail "function didn't work"

fun assertLexError str =
  (re str; fail "Exception `Re.Lex` wasn't raised")
  handle a => assertEqualExceptionName Lex a

fun assertParseError str =
  (re str; fail "Exception `Re.Parse` wasn't raised")
  handle a => assertEqualExceptionName Parse a

(* re *)
fun re_simplest_test () =
  assertWork "a"
fun re_paren_test () =
  (assertWork "(a)";
   assertWork "(a)(b)";
   assertWork "((a)b)")
fun re_charset_test () =
  (assertWork "[a]";
   assertWork "[ab]";
   assertWork "[a]b";
   assertWork "a[b]";
   assertWork "([a])";
   assertWork "[(]";
   assertWork "[)]";
   assertWork "[{]";
   assertWork "[}]";
   assertWork "[[]";
   assertWork "[]]";
   assertWork "[a-c]";
   assertWork "[a-a]";
   assertWork "[a-]";
   assertWork "[-a]";
   assertWork "[\\a]";
   assertWork "[\\\\a]";
   assertWork "[|]";
   assertWork "[a^]";
   assertWork "[-^]")
fun re_charset_complement_test () =
  (assertWork "[^a]";
   assertWork "[^ab]";
   assertWork "[^a-c]";
   assertWork "[^a-a]";
   assertWork "[^!]";
   assertWork "[^|]";
   assertWork "[^{]";
   assertWork "[^}]";
   assertWork "[^(]";
   assertWork "[^)]";
   assertWork "[^[]";
   assertWork "[^]]";
   assertWork "[^\\a]";
   assertWork "[^\\\\a]";
   assertWork "[^-]";
   assertWork "[^-a]";
   assertWork "[^,]")
fun re_backslash_test () =
  assertWork "a\\b"
fun re_backslash_metachars_test () =
  (assertWork "\\.";
   assertWork "\\*";
   assertWork "\\+";
   assertWork "\\?")
fun re_backslash_leftparen_test () =
  (assertWork "a\\(b";
   assertWork "a\\(()b";
   assertWork "a()\\(b";
   assertWork "a(\\()b")
fun re_backslash_rightparen_test () =
  (assertWork "a\\)b";
   assertWork "a\\)()b";
   assertWork "a()\\)b";
   assertWork "a(\\))b")
fun re_linestart_test () =
  (assertWork "^";
   assertWork "^a";
   assertWork "a^";
   assertWork "^[^a]";
   assertWork "\\^";
   assertWork "a^";
   assertWork "(^)")
fun re_lineend_test () =
  (assertWork "$";
   assertWork "\\$";
   assertWork "$a";
   assertWork "^$";
   assertWork "[$]";
   assertWork "($)")
fun re_curly_brace_test () =
  (assertWork "a{0,1}";
   assertWork "a{0,0}";
   assertWork "a{0,10}";
   assertWork "a{10,11}";
   assertWork "a{,1}";
   assertWork "a{0,10}";
   assertWork "a{0,}";
   assertWork "a{10,}";
   assertWork "(a){10,}";
   assertWork "(a|b){10,}";
   assertWork "[a-b]{10,}")
fun re_bar_test () =
  (assertWork "a|b";
   assertWork "ab|ac";
   assertWork "a|b|c";
   assertWork "a|b|c|d";
   assertWork "a|b*";
   assertWork "a*|b";
   assertWork "a*|b*";
   assertWork "a?|b";
   assertWork "a|b?";
   assertWork "a[a-c]|b";
   assertWork "|";
   assertWork "a|";
   assertWork "|b";
   assertWork "a(|)b";
   assertWork "(a|)b";
   assertWork "a(|b)")
fun re_lex_error_backslash_test () =
  assertLexError "\\"
fun re_parse_error_star_no_leading_char_test () =
  assertParseError "*"
fun re_parse_error_plus_no_leading_char_test () =
  assertParseError "+"
fun re_parse_error_question_no_leading_char_test () =
  assertParseError "?"
fun re_parse_error_paren () =
  (assertParseError "(";
   assertParseError ")(";
   assertParseError "(\\)";
   assertParseError "([)]")
fun re_parse_error_char_set () =
  (assertParseError "[";
   assertParseError "[\\]";
   assertParseError "][";
   assertParseError "[z-a]"
  )
fun re_parse_error_curly_brace () =
  (assertParseError "{0,1}";
   assertParseError "a{2,1}";
   assertParseError "a{-1,1}";
   assertParseError "a{a,1}";
   assertParseError "a{0,a}";
   assertParseError "a{a,b}";
   assertParseError "a{0-1}";
   assertParseError "a{,a}";
   assertParseError "a{a,}";
   assertParseError "a{,}"
  )

fun suite _ =Test.labelTests [
      ("re: simple regexp", re_simplest_test),
      ("re: contianing parens", re_paren_test),
      ("re: charset", re_charset_test),
      ("re: charset complement", re_charset_complement_test),
      ("re: line start", re_linestart_test),
      ("re: line end", re_lineend_test),
      ("re: number specified matchng", re_curly_brace_test),
      ("re: simple quote", re_backslash_test),
      ("re: quoting meta chars", re_backslash_metachars_test),
      ("re: quoting left paren", re_backslash_leftparen_test),
      ("re: bar", re_bar_test),
      ("re: lex error against last #\"\\\"", re_lex_error_backslash_test),
      ("re: parse error against #\"*\" without any leading chars", re_parse_error_star_no_leading_char_test),
      ("re: parse error against #\"+\" without any leading chars", re_parse_error_plus_no_leading_char_test),
      ("re: parse error against #\"?\" without any leading chars", re_parse_error_question_no_leading_char_test),
      ("re: parse error against unmatching parens", re_parse_error_paren),
      ("re: parse error against charset", re_parse_error_char_set),
      ("re: parse error against curly braces", re_parse_error_curly_brace)
  ]
end
