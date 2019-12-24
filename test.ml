(** Our approach to testing invloved testing as much of our back-end functions
    as we could, which mostly involved testing all the functions in State. 
    However, since part of our front-end functionality (i.e. printing lines of 
    text from a file and erasing the terminal screen where appropriate using 
    ANSITerminal) involved a lot of IO (this kind of stuff happened 
    mostly in the Main module), there were a few things that we manually checked 
    to ensure they were working properly. For example, things like "hoff"--the 
    horizontal offset used to see how much to pan the screen to the 
    right--depends directly on the size of the terminal in which 
    NanOCaml is being run in. As such, writing test cases to make sure the 
    hoff is what it should be would require writing code similar to what we 
    already have in state.ml in test.ml. Besides hoff, we also tested to see 
    if voff (vertical offset), line numbers, and all erasing was being done 
    properly via manual testing. 

    We also didn't KeyPress in OUnit, which was the module that translated
    user inputs on the keyboard into commands to which NanOCaml could react to. 
    This is because by manually testing main ourselves, we were implicitly 
    testing KeyPress. Moreover, the whole point of KeyPress was to properly 
    translate input from the keyboard, so it was required that we do this 
    manually.

    That aside, we used a glass-box testing approach. It was important to 
    make sure that all the edge cases we could think of were accounted for 
    and that we did not redundantly write test cases. By having OUnit cases
    for state, we implicitly tested TextFile.

    We feel that our approach to testing has demonstrated the correctness of 
    our system. Our manual testing involved spending multiple hours testing 
    as many unique user inputs as we could think of. We actually compared the 
    way our text editor behaved to things like Microsoft Word and even VSCode 
    itself. We specifically made sure to test ugly and unexpected user input 
    as well. For example, when the user is prompted to enter the line number 
    where they would like to jump to or copy/cut, we have written to code 
    to handle these erroneus cases. From our glass-box testing in OUnit, 
    we were able to demonstrate that all the functions in the State module 
    match the expected output. This included testing updates to content 
    after adding a character, adding a next line, deleting a line, etc.
*)


open OUnit2
open TextFile
open State
open TernarySearchTree

let width,height = ANSITerminal.size()

let determine_vertical_pos x = if x +1 < height then x +1 else height

(*Begin replication of keyboard events *)
let file = TextFile.init_file "dummy.ml"
let initial_state = State.init_state file
let try_backspace = State.handle_backspace (State.copy initial_state) 
let add_lettr = State.add_char (State.copy try_backspace) "T" 
let add_snd_lettr = State.add_char (State.copy add_lettr) "E"
let add_nxtline = State.add_nextline (State.copy add_snd_lettr)
let backspace = State.handle_backspace (State.copy add_nxtline)
let add_a_tab = State.add_tab (State.copy backspace)
let undo_last = State.handle_undo (State.copy add_a_tab)
let do_redo = State.handle_redo (State.copy undo_last)
(**Doesn't change anything, end of line*)
let move_to_right = State.move_cursor_right (State.copy do_redo) 
let move_to_left = State.move_cursor_left (State.copy move_to_right)
let nextline_between = State.add_nextline (State.copy move_to_left)
let add_lettr_newline = State.add_char (State.copy nextline_between) "S"
let move_up = State.move_cursor_up (State.copy add_lettr_newline)
let move_right = State.move_cursor_right (State.copy move_up)
let move_right_again = State.move_cursor_right (State.copy move_right)
(** Cursor pos should be end of next line*)
let move_down = State.move_cursor_down (State.copy move_right_again) 
let move_bol = State.move_bol (State.copy move_down)
let move_eol = State.move_eol (State.copy move_bol)
let move_top = State.move_top (State.copy move_eol)
let move_btm = State.move_btm (State.copy move_top)
let type_a_char = State.add_char (State.copy move_btm) "T"
let try_redo_fail = State.handle_redo (State.copy type_a_char)
let find_and_replace = State.find_and_replace (State.copy try_redo_fail) "T" 
    "TEST"
let find_and_replace_previous = State.find_and_replace 
    (State.copy find_and_replace) "TEST" "3110"
let find_and_replace_nonexistent = State.find_and_replace 
    (State.copy find_and_replace_previous) "SAFIJAFNKJASNFJKASBFJKAB" "TEST"
let find_and_replace_number = State.find_and_replace 
    (State.copy find_and_replace_nonexistent) "3110" "This is a test"
let find_and_replace_phrase_case_insensitive = State.find_and_replace 
    (State.copy find_and_replace_number) "ThIS iS" "TEST"
let cut_line_one = State.cut 
    (State.copy find_and_replace_phrase_case_insensitive) (1,1)
let paste_line_one = State.paste (State.copy cut_line_one) 1
let copy_line_one = State.handle_copy (State.copy paste_line_one) (1,1)
let paste_line_two = State.paste (State.copy copy_line_one) 2
let cut_line_one_two = State.cut (State.copy paste_line_two) (1,2)
let paste_line_one_two = State.paste (State.copy cut_line_one_two) 1
let copy_line_one_to_three = State.handle_copy 
    (State.copy paste_line_one_two) (1,3)
let paste_line_one_to_three = State.paste (State.copy copy_line_one_to_three) 1

let alice = TextFile.init_file "alice.txt"
let alice_text = State.init_state alice 
let jump_out_of_bounds = State.jump (State.copy alice_text) 9030343904903
let jump_to_bottom = State.jump (State.copy jump_out_of_bounds) 30
let jump_to_middle = State.jump (State.copy jump_to_bottom) (height/2)
let jump_to_top = State.jump (State.copy jump_to_middle) 1
let jump_to_middle_text = State.jump (State.copy jump_to_top) 588
let create_portal_enter = State.toggle_portal (State.copy jump_to_middle_text)
let jump_to_lower_line = State.jump (State.copy create_portal_enter) 4000
let create_portal_exit = State.toggle_portal (State.copy jump_to_lower_line)
let try_portal_jump_down = State.portal_jump_down 
    (State.copy create_portal_exit)
let try_portal_jump_up = State.portal_jump_up (State.copy try_portal_jump_down)


let check_content
    (name : string)
    (input : State.t)
    (expected_output : string array) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (State.current_text_lines input) 
        ~printer:(fun x-> Array.fold_left (^) "" x))

let check_cursor_pos
    (name : string)
    (input : State.t)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (State.get_cursor_pos input) 
        ~printer:(fun (x,y) -> (string_of_int x) ^ (string_of_int y)))

let check_voff 
    (name: string)
    (input : State.t)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (State.get_voff input) 
        ~printer:(fun x -> (string_of_int x)))

let check_length
    (name: string)
    (input : State.t)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (State.get_length input) 
        ~printer:(fun x -> (string_of_int x)))

(**SETUP TO TEST Ternary Search Tree *)
let tree = TernarySearchTree.empty
let tree' = TernarySearchTree.insert "shiva" tree
let tree'' = TernarySearchTree.insert "sam" tree'
let tree''' = TernarySearchTree.insert "apple" tree''

let make_search_test 
    (name : string)
    (string : string)
    (input : TernarySearchTree.t)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (TernarySearchTree.search string input))

let tests = [

  check_content "Test #1 initalize" initial_state [|""|];
  check_content "Test #2 backspace at start" try_backspace [|""|];
  check_content "Test #3 add letter" add_lettr [|"T"|];
  check_content "Test #4 add letter" add_snd_lettr [|"TE"|];
  check_content "Test #5 press enter" add_nxtline [|"TE"; ""|];
  check_content "Test #6 press backspace " backspace [|"TE"|];
  check_content "Test #7 press tab" add_a_tab [|"TE     "|];
  check_content "Test #8 undo last" undo_last [|"TE"|];
  check_content "Test #9 do redo" do_redo [|"TE     "|];
  check_cursor_pos "Test #10 move right" move_to_right (14,2);
  check_cursor_pos "Test #11 move left" move_to_left (13,2);
  check_content "Test #12 next line in middle of line" 
    nextline_between [|"TE    "; " "|];
  check_content "Test #13 next line in middle of line" 
    add_lettr_newline [|"TE    "; "S "|];
  check_cursor_pos "Test #14 move up" move_up (8,2);
  check_cursor_pos "Test #15 move down" move_down (9,3);
  check_cursor_pos "Test #16 move BOL" move_bol (7,3);
  check_cursor_pos "Test #17 move EOL" move_eol (9,3);
  check_cursor_pos "Test #18 move to top" move_top (7,2);
  check_cursor_pos "Test #19 move to bottom" move_btm (7,3);
  check_content "Test #20 add char to test redo" 
    type_a_char [|"TE    "; "TS "|];
  check_content "Test #21 redo with no effect" 
    try_redo_fail [|"TE    "; "TS "|];
  check_content "Test #22 find and replace" find_and_replace 
    [|"TESTE    "; "TESTS "|];
  check_content "Test #23 find and replace previous word" 
    find_and_replace_previous [|"3110E    "; "3110S "|];
  check_content "Test #24 find and replace nonexistent word" 
    find_and_replace_nonexistent [|"3110E    "; "3110S "|];
  check_content "Test #25 find and replace number" 
    find_and_replace_number [|"This is a testE    "; "This is a testS "|];
  check_content "Test #26 find and replace phrase case insensitive" 
    find_and_replace_phrase_case_insensitive 
    [|"TEST a testE    "; "TEST a testS "|];
  check_content "Test #27 cut a single line" cut_line_one [|"TEST a testS "|];
  check_content "Test #28 paste a single line" paste_line_one 
    [|"TEST a testE    "; "TEST a testS "|];
  check_content "Test #29 copy a single line" copy_line_one 
    [|"TEST a testE    "; "TEST a testS "|];
  check_content "Test #30 paste a single line" paste_line_two 
    [|"TEST a testE    "; "TEST a testE    "; "TEST a testS ";|];
  check_content "Test #31 cut multiple lines" cut_line_one_two 
    [|"TEST a testS "|];
  check_content "Test #32 paste a multiple lines" paste_line_one_two 
    [|"TEST a testE    "; "TEST a testE    "; "TEST a testS ";|];
  check_content "Test #33 copy a multiple lines" copy_line_one_to_three 
    [|"TEST a testE    "; "TEST a testE    "; "TEST a testS ";|];
  check_content "Test #34 paste a multiple lines" paste_line_one_to_three
    [|"TEST a testE    "; "TEST a testE    "; "TEST a testS "; 
      "TEST a testE    "; "TEST a testE    "; "TEST a testS ";|];
  check_length "Test #35 text length" paste_line_one_to_three 6;

  (**BEGIN TESTING on alice.txt *)
  check_cursor_pos "Test #1 jump out of bounds" jump_out_of_bounds (7, 2);
  check_cursor_pos "Test #2 jump to bottom" jump_to_bottom 
    (7, (determine_vertical_pos 30));
  check_cursor_pos "Test #3 jump to middle" jump_to_middle (7, (1 + height/2));
  check_cursor_pos "Test #4 jump to top" jump_to_top (7, 2);
  check_cursor_pos "Test #5 jump portal down" try_portal_jump_down 
    (7, (determine_vertical_pos 4000));
  check_cursor_pos "Test #6 jump portal up" try_portal_jump_up 
    (7, (determine_vertical_pos 588));
  check_voff "Test #7 voff after jumping to top" jump_to_top 0;
  check_voff "Test #8 voff after jumping to middle" jump_to_middle_text 
    (588 - (height-1));
  check_length "Test #9 text length" jump_to_middle_text 3735;

  (** BEGIN TESTING FOR Ternary Search Tree *)
  make_search_test "Test #1 for search" "Shiva" tree' true;
  make_search_test "Test #2 for search" "Shiv" tree' false;
  make_search_test "Test #3 for search" "Shiva" tree'' true;
  make_search_test "Test #4 for search" "Sam" tree'' true;
  make_search_test "Test #5 for search" "Apple" tree''' true;
  make_search_test "Test #6 for search" "Shiva" tree''' true;
]

let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite