structure ReTest =
struct
open SMLUnit
open Re
open Assert

fun assertWork str =
  (re str; assert "function work" true)
  handle a => fail "function didn't work"

fun assertLexError str =
  (re str; fail "Exception `Re.Lex` wasn't raised")
  handle a => assertEqualExceptionName Lex a

fun assertParseError str =
  (re str; fail "Exception `Re.Parse` wasn't raised")
  handle a => assertEqualExceptionName Parse a

fun assertMatch (s, e, gs) actual =
  (assertSome actual;
   assertEqual3Tuple (assertEqualInt, assertEqualInt, assertEqualArray(assertEqual2Tuple(assertEqualInt, assertEqualInt))) (s, e, Array.fromList gs) (Option.valOf actual))
fun assertNotMatch actual =
  assertNone actual

fun assertMatchString expected actual =
  (assertSome actual;
   assertEqual2Tuple (assertEqualString, assertEqualArray assertEqualString) expected (Option.valOf actual))
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
(* match *)
fun match_simplest_test () =
  (assertMatch (0, 1,  []) (match(re "a", "a", 0));
   assertMatch (1, 3,  []) (match(re "ab", "aab", 0));
   assertNotMatch          (match(re "a", "b", 0));
   assertNotMatch          (match(re "abcd", ".abce", 0));
   assertMatch (1, 2,  []) (match(re "a", "ba", 0)))
fun match_start_from_i_test () =
  (assertMatch (0, 1,  []) (match(re "a", "ab", 0));
   assertNotMatch          (match(re "a", "ab", 1));
   assertMatch (6, 10, []) (match(re "abcd", "aababcabcdabcaba", 0));
   assertMatch (6, 10, []) (match(re "abcd", "aababcabcdabcaba", 5));
   assertMatch (6, 10, []) (match(re "abcd", "aababcabcdabcaba", 6));
   assertNotMatch                         (match(re "abcd", "aababcabcdabcaba", 7)))
fun match_any_test () =
  (assertMatch (0, 1, []) (match(re ".", "a", 0));
   assertMatch (0, 1, []) (match(re ".", "b", 0));
   assertMatch (0, 1, []) (match(re ".", ".", 0));
   assertMatch (0, 1, []) (match(re ".", "*", 0));
   assertMatch (0, 1, []) (match(re ".", "+", 0));
   assertMatch (0, 1, []) (match(re ".", "?", 0));
   assertMatch (0, 1, []) (match(re ".", "\\", 0));
   assertMatch (0, 1, []) (match(re ".", "|", 0));
   assertMatch (0, 1, []) (match(re ".", "(", 0));
   assertMatch (0, 1, []) (match(re ".", ")", 0)))

fun match_star_test () =
  (assertMatch (0, 0, []) (match(re "a*", "", 0));
   assertMatch (0, 1, []) (match(re "a*", "a", 0));
   assertMatch (0, 2, []) (match(re "a*", "aa", 0));
   assertMatch (0, 0, []) (match(re "a*", "b", 0));
   assertMatch (0, 0, []) (match(re "a*", "ba", 0)))

fun match_plus_test () =
  (assertNotMatch     (match(re "a+", "", 0));
   assertMatch (0, 1, []) (match(re "a+", "a", 0));
   assertMatch (0, 2, []) (match(re "a+", "aa", 0));
   assertNotMatch     (match(re "a+", "b", 0));
   assertMatch (1, 2, []) (match(re "a+", "ba", 0)))
fun match_option_test () =
  (assertMatch (0, 1, []) (match(re "a?", "a", 0));
   assertMatch (0, 0, []) (match(re "a?", "", 0));
   assertMatch (0, 0, []) (match(re "a?", "ba", 0)))
fun match_linestart_test () =
  (assertMatch (0, 0, []) (match(re "^", "", 0));
   assertMatch (0, 0, []) (match(re "^", "a", 0));
   assertNotMatch         (match(re "a^", "a", 0));
   assertMatch (0, 2, []) (match(re "a^", "a^b", 0));
   assertMatch (2, 3, []) (match(re "^a", "b\na", 0)))
fun match_lineend_test () =
  (assertMatch (0, 0, []) (match(re "$", "", 0));
   assertMatch (1, 1, []) (match(re "$", "a", 0));
   assertNotMatch         (match(re "$a", "a", 0));
   assertMatch (1, 3, []) (match(re "$a", "b$a", 0));
   assertMatch (0, 1, []) (match(re "a$", "a\nb", 0)))
fun match_charset_test () =
  (assertMatch (0, 1, []) (match(re "[a]", "a", 0));
   assertNotMatch         (match(re "[a]", "b", 0));
   assertMatch (0, 1, []) (match(re "[ab]", "a", 0));
   assertMatch (0, 1, []) (match(re "[ab]", "b", 0));
   assertMatch (0, 6, []) (match(re "[ab]+", "abbaab", 0));
   assertMatch (0, 3, []) (match(re "[a-c]+", "abc", 0));
   assertNotMatch         (match(re "[a-c]+", "-", 0));
   assertMatch (0, 1, []) (match(re "[a-]", "-", 0));
   assertMatch (0, 1, []) (match(re "[-a]", "-", 0));
   assertMatch (3, 4, []) (match(re "[^abc]", "abcd", 0));
   assertMatch (3, 4, []) (match(re "[^a-c]", "abcd", 0));
   assertNotMatch         (match(re "[^a-c]", "abc", 0));
   assertMatch (1, 2, []) (match(re "[^a-]", "-b", 0));
   assertMatch (1, 2, []) (match(re "[^-a]", "-b", 0))
  )
fun match_curlybraces_test () =
  ((assertMatch (0, 2, []) (match(re "a{0,2}", "aaa", 0)));
   (assertMatch (0, 2, []) (match(re "a{0,2}", "aaaa", 0)));
   (assertMatch (0, 1, []) (match(re "a{0,2}", "a", 0)));
   (assertMatch (0, 1, []) (match(re "a{1,2}", "a", 0)));
   (assertNotMatch         (match(re "a{2,2}", "a", 0)));
   (assertMatch (0, 1, []) (match(re "a{1,}", "a", 0)));
   (assertMatch (0, 2, []) (match(re "a{1,}", "aa", 0)));
   (assertMatch (0, 5, []) (match(re "a{1,}", "aaaaa", 0)));
   (assertMatch (0, 0, []) (match(re "a{,2}", "", 0)));
   (assertMatch (0, 1, []) (match(re "a{,2}", "a", 0)));
   (assertMatch (0, 2, []) (match(re "a{,2}", "aa", 0)));
   (assertMatch (0, 2, []) (match(re "a{,2}", "aaa", 0)));
   (assertMatch (0, 2, []) (match(re "ab{1,2}", "ab", 0)));
   (assertMatch (0, 2, []) (match(re "ab{1,2}", "abab", 0)));
   (assertMatch (0, 1, []) (match(re "[ab]{,2}", "a", 0)))
  )
fun match_bar_test () =
  ((assertMatch (0, 1, []) (match(re "a|b", "a", 0)));
   (assertMatch (0, 1, []) (match(re "a|b", "b", 0)));
   (assertMatch (0, 2, []) (match(re "ab|cd", "ab", 0)));
   (assertMatch (0, 2, []) (match(re "ab|cd", "cd", 0)));
   (assertMatch (0, 2, []) (match(re "ab|cd|ef", "ab", 0)));
   (assertMatch (0, 2, []) (match(re "ab|cd|ef", "cd", 0)));
   (assertMatch (0, 2, []) (match(re "ab|cd|ef", "ef", 0)));
   (assertMatch (0, 2, []) (match(re "a.|cd|ef", "ab", 0)));
   (assertMatch (0, 0, []) (match(re "a*|cd|ef", "b", 0)));
   (assertMatch (0, 3, []) (match(re "ab|cd{1,2}|ef", "cdd", 0)))
  )
(* matchString *)
fun matchString_simple_test () =
  assertMatchString ("a", Array.fromList []) (matchString(re "a", "a", 0))
(* matchstrings *)
fun matchStrings_simple_test () =
  assertEqualStringList ["a", "a"] (matchStrings(re "a", "aa", 0))
(* doesMatch *)
fun doesMatch_simple_test () =
  assertTrue (doesMatch(re "a", "a", 0))
(* split *)
fun split_simpl_test () =
  assertEqualStringList ["a", "b", "c"] (split(re " ", "a b c", 0))
(* replace *)
fun replace_simple_test () =
  assertEqualString "b" (replace(re"a", "a", 0, "b"))
(* replaceAll *)
fun replaceAll_simple_test () =
  assertEqualString "b" (replace(re"a", "a", 0, "b"))
      

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
      ("re: parse error against curly braces", re_parse_error_curly_brace),
      ("match: simple string", match_simplest_test),
      ("match: match start from non-zero", match_start_from_i_test),
      ("match: /./", match_any_test),
      ("match: /a*/", match_star_test),
      ("match: /a+/", match_plus_test),
      ("match: /a?/", match_option_test),
      ("match: /^/", match_linestart_test),
      ("match: /$/", match_lineend_test),
      ("match: /[]/", match_charset_test),
      ("match: /{,}/", match_curlybraces_test),
      ("match: /|/", match_bar_test),
      
      ("matchString: simple string", matchString_simple_test),

      ("matchStrings: simple string", matchStrings_simple_test),

      ("doesMatch: simple string", doesMatch_simple_test),

      ("split: simple string", split_simpl_test),

      ("replace: simple string" , replace_simple_test),

      ("replaceAll: simple string" , replaceAll_simple_test)
  ]
end
