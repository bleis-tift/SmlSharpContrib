structure RegexpTest =
struct
open SMLUnit
open Re
open Assert

fun assertMatch (s, e, gs) actual =
  (assertSome actual;
   assertEqual3Tuple (assertEqualInt, assertEqualInt, assertEqualArray(assertEqual2Tuple(assertEqualInt, assertEqualInt))) (s, e, Array.fromList gs) (Option.valOf actual))
fun assertNotMatch actual =
  assertNone actual

fun assertMatchString expected actual =
  (assertSome actual;
   assertEqual2Tuple (assertEqualString, assertEqualArray assertEqualString) expected (Option.valOf actual))

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
fun match_group_test () =
  ((assertMatch (0, 1, [(0, 1)]) (match(re"(a)", "a", 0)));
   (assertMatch (0, 2, [(0, 1)]) (match(re"(a)b", "ab", 0)));
   (assertMatch (0, 2, [(0, 2)]) (match(re"(ab)", "ababab", 0)));
   (assertMatch (0, 2, [(0, 2)]) (match(re"(ab)?", "ababab", 0)));
   (assertMatch (0, 6, [(0, 6)]) (match(re"(ab)*", "ababab", 0)));
   (assertMatch (0, 6, [(0, 6)]) (match(re"(ab)+", "ababab", 0)));
   (assertNotMatch               (match(re"(ab)+", "a", 0)));
   (assertMatch (0, 2, [(0, 1), (1, 2)]) (match(re"(a)(b)", "ab", 0)));
   (assertMatch (0, 3, [(0, 1)]) (match(re"(a|b)cd", "acd", 0)));
   (assertMatch (0, 3, [(0, 1)]) (match(re"(a|b)cd", "bcd", 0)));
   (assertMatch (0, 3, [(0, 1)]) (match(re"(^a|b)cd", "acd", 0)));
   (assertMatch (0, 3, [(0, 1)]) (match(re"(^a|b)cd", "bcd", 0)));
   (assertNotMatch               (match(re"(^a|b)cd", "bacd", 0)));
   (assertMatch (1, 4, [(1, 2)]) (match(re"(^a|b)cd", "abcd", 0)));
   (assertMatch (0, 3, [(2, 3)]) (match(re"ab(c|d$)", "abc", 0)));
   (assertMatch (0, 3, [(2, 3)]) (match(re"ab(c|d$)", "abd", 0)));
   (assertMatch (0, 3, [(2, 3)]) (match(re"ab(c|d$)", "abca", 0)));
   (assertNotMatch               (match(re"ab(c|d$)", "abda", 0)));
   (assertMatch (0, 4, [(0, 4), (1, 4), (2, 4), (3, 4)]) (match(re"(a(b(c(d))))", "abcd", 0)))
  )
(* matchString *)
fun matchString_simple_test () =
  (assertMatchString ("a", Array.fromList []) (matchString(re "a", "a", 0));
   assertMatchString ("abbb", Array.fromList ["bbb"]) (matchString(re "a(b*)", "abbb", 0));
   assertMatchString ("a", Array.fromList [""]) (matchString(re "a(b*)", "a", 0))
  )
(* matchstrings *)
fun matchStrings_simple_test () =
  (assertEqualStringList ["a", "a"] (matchStrings(re "a", "aa", 0));
   assertEqualStringList ["a", "a", "ab"] (matchStrings(re "ab?", "aaab", 0))
  )
(* doesMatch *)
fun doesMatch_simple_test () =
  (assertTrue (doesMatch(re "a", "a", 0));
   assertFalse (doesMatch(re "a", "b", 0))
  )
(* split *)
fun split_simpl_test () =
  (assertEqualStringList ["a", "b", "c"] (split(re " ", "a b c", 0));
   assertEqualStringList ["a", "", "b", "c"] (split(re " ", "a  b c", 0));
   assertEqualStringList ["a", "b", "c"] (split(re " +", "a  b c", 0))
  )
(* replace *)
fun replace_simple_test () =
  (assertEqualString "b" (replace(re"a", "a", 0, "b"));
   assertEqualString "bb" (replace(re"a+", "aaab", 0, "b"));
   assertEqualString "bbaa" (replace(re"a+", "aaabaa", 0, "b"))
  )
(* replaceAll *)
fun replaceAll_simple_test () =
  (assertEqualString "b" (replaceAll(re"a", "a", 0, "b"));
   assertEqualString "bb" (replaceAll(re"a+", "aaab", 0, "b"));
   assertEqualString "bbb" (replaceAll(re"a+", "aaabaa", 0, "b"))
  )
      

fun suite _ =Test.labelTests [
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
      ("match: /()/", match_group_test),
      
      ("matchString: simple string", matchString_simple_test),

      ("matchStrings: simple string", matchStrings_simple_test),

      ("doesMatch: simple string", doesMatch_simple_test),

      ("split: simple string", split_simpl_test),

      ("replace: simple string" , replace_simple_test),

      ("replaceAll: simple string" , replaceAll_simple_test)
  ]
end
