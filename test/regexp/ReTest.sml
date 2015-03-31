structure ReTest =
struct
open SMLUnit
open Re
open Assert

fun assertWork f =
  (f (); assert "function work" true)
  handle a => fail "function didn't work"

fun assertLexError f =
  (f (); fail "Exception `Re.Lex` wasn't raised")
  handle a => assertEqualExceptionName Lex a

fun assertParseError f =
  (f (); fail "Exception `Re.Parse` wasn't raised")
  handle a => assertEqualExceptionName Parse a

fun assertMatch expected actual =
  (assertSome actual;
   assertEqual2Tuple (assertEqualInt, assertEqualInt) expected (Option.valOf actual))
fun assertNotMatch actual =
  assertNone actual

(* re *)

fun re_simplest_test () =
  assertWork (fn _ => re "a")
fun re_paren_test () =
  (assertWork (fn _ => re "(a)");
   assertWork (fn _ => re "(a)(b)");
   assertWork (fn _ => re "((a)b)"))

fun re_backslash_test () =
  assertWork (fn _ => re "a\\b")
fun re_backslash_metachars_test () =
  (assertWork (fn _ => re "\\.");
   assertWork (fn _ => re "\\*");
   assertWork (fn _ => re "\\+");
   assertWork (fn _ => re "\\?"))
fun re_backslash_leftparen_test () =
  (assertWork (fn _ => re "a\\(b");
   assertWork (fn _ => re "a\\(()b");
   assertWork (fn _ => re "a()\\(b");
   assertWork (fn _ => re "a(\\()b"))
fun re_backslash_rightparen_test () =
  (assertWork (fn _ => re "a\\)b");
   assertWork (fn _ => re "a\\)()b");
   assertWork (fn _ => re "a()\\)b");
   assertWork (fn _ => re "a(\\))b"))

fun re_lex_error_backslash_test () =
  assertLexError (fn _ => re "\\")
fun re_parse_error_star_no_leading_char_test () =
  assertParseError (fn _ => re "*")
fun re_parse_error_plus_no_leading_char_test () =
  assertParseError (fn _ => re "+")
fun re_parse_error_question_no_leading_char_test () =
  assertParseError (fn _ => re "?")
fun re_parse_error_bar_test () =
  (assertParseError (fn _ =>re "|");
   assertParseError (fn _ =>re "a|");
   assertParseError (fn _ =>re "|b");
   assertParseError (fn _ =>re "a(|)b");
   assertParseError (fn _ =>re "(a|)b");
   assertParseError (fn _ =>re "a(|b)"))

(* match *)
fun match_simplest_test () =
  (assertMatch (0, 1) (match(re "a", "a", 0));
   assertNotMatch     (match(re "a", "b", 0));
   assertMatch (1, 2) (match(re "a", "ba", 0)))
fun match_start_from_i_test () =
  (assertMatch (0, 1) (match(re "a", "ab", 0));
   assertNotMatch     (match(re "a", "ab", 1)))
fun match_any_test () =
  (assertMatch (0, 1) (match(re ".", "a", 0));
   assertMatch (0, 1) (match(re ".", "b", 0));
   assertMatch (0, 1) (match(re ".", ".", 0));
   assertMatch (0, 1) (match(re ".", "*", 0));
   assertMatch (0, 1) (match(re ".", "+", 0));
   assertMatch (0, 1) (match(re ".", "?", 0));
   assertMatch (0, 1) (match(re ".", "\\", 0));
   assertMatch (0, 1) (match(re ".", "|", 0));
   assertMatch (0, 1) (match(re ".", "(", 0));
   assertMatch (0, 1) (match(re ".", ")", 0)))

fun match_star_test () =
  (assertMatch (0, 0) (match(re "a*", "", 0));
   assertMatch (0, 1) (match(re "a*", "a", 0));
   assertMatch (0, 2) (match(re "a*", "aa", 0));
   assertMatch (0, 0) (match(re "a*", "b", 0));
   assertMatch (0, 0) (match(re "a*", "ba", 0)))

fun match_plus_test () =
  (assertNotMatch     (match(re "a+", "", 0));
   assertMatch (0, 1) (match(re "a+", "a", 0));
   assertMatch (0, 2) (match(re "a+", "aa", 0));
   assertNotMatch     (match(re "a+", "b", 0));
   assertMatch (1, 2) (match(re "a+", "ba", 0)))

(* matchString *)
(* matchstrings *)
(* doesMatch *)
(* split *)
(* replace *)
fun replace_simplest_test () =
  assertEqual
(* replaceAll *)
      

fun suite _ =Test.labelTests [
      ("re: simple regexp", re_simplest_test),
      ("re: contianing parens", re_paren_test),
      ("re: simple quote", re_backslash_test),
      ("re: quoting meta chars", re_backslash_metachars_test),
      ("re: quoting left paren", re_backslash_leftparen_test),
      ("re: lex error against last #\"\\\"", re_lex_error_backslash_test),
      ("re: parse error against #\"*\" without any leading chars", re_parse_error_star_no_leading_char_test),
      ("re: parse error against #\"+\" without any leading chars", re_parse_error_plus_no_leading_char_test),
      ("re: parse error against #\"?\" without any leading chars", re_parse_error_question_no_leading_char_test),
      ("re: parse error against illeagal #\"|\"", re_parse_error_bar_test),
      ("match: simple string", match_simplest_test),
      ("match: match start from non-zero", match_start_from_i_test),
      ("match: /./", match_any_test),
      ("match: /a*/", match_star_test),
      ("match: /a+/", match_plus_test)
  ]
end