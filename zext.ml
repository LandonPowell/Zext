#use "topfind"
#require "curses"

open Curses
open String

let cursorx = ref 0;;
let cursory = ref 0;;
(* 
    This function should allow 
    users to change their base 
    number system eventually. 
*)
let parseInt int =
    string_of_int int
;;

let filename =
    if (Array.length Sys.argv) == 1 then
        "untitled.text"
    else
        Sys.argv.(1)
;;

let openFile = 
    if (Array.length Sys.argv) == 1 then
        ref [|""|]
    else

    let f = open_in Sys.argv.(1) in 
    let b = ref [||] in
    try while true do
        b := Array.append !b [|input_line f|]
    done; assert false
    with End_of_file ->
        b
;;

let rec renderScreen lines yoffset whitespace =
    let height, width = get_size() in
    for x = yoffset to min ((Array.length lines) - 1) (height + yoffset - 1) do
        move (x - yoffset) 0;
        attr_on (A.color_pair 1);
        addstr (parseInt x);
        addstr (make (whitespace - length(parseInt x)) ' ');

        attr_off (A.color_pair 1);
        addstr lines.(x);
    done;

    move (!cursory - yoffset) (!cursorx + whitespace);
;;

let rec keyEvent event = 
    (* Testing Code :: Used for when I need to find a listener's int.
    print_int event;
    print_endline "";
    *)

    (* The magical invisible complexity that is arrow keys. *)
    if event = Key.up && !cursory > 0 then (
        cursory := !cursory - 1;
        cursorx := min !cursorx (length !openFile.(!cursory));
    )
    else if event = Key.down && !cursory < (Array.length !openFile) - 1 then (
        cursory := !cursory + 1;
        cursorx := min !cursorx (length !openFile.(!cursory));
    )
    else if event = Key.left then (
        if !cursorx <= 0 then (
            if !cursory > 0 then (
                cursory := !cursory - 1;
                cursorx := length !openFile.(!cursory)
            )
        )
        else
            cursorx := !cursorx - 1;
    )
    else if event = Key.right then (
        if !cursorx >= (length !openFile.(!cursory)) then (
            if !cursory + 1 < Array.length !openFile then (
                cursorx := 0;
                cursory := !cursory + 1
            )
        )
        else
            cursorx := !cursorx + 1
    )

    (* Ctrl-S *)
    else if event = Key.save || event = 19 then (
        let f = open_out filename in
        for x = 0 to (Array.length !openFile) - 1 do
            Printf.fprintf f "%s\n" !openFile.(x);
        done;
        close_out f;
    )

    (* Backspace *)
    else if event = Key.backspace then (
        if !cursorx = 0 && !cursory > 0 then (
            cursorx := length !openFile.(!cursory - 1);
            openFile := Array.concat [
                (Array.sub !openFile 0 (!cursory - 1));
                [| concat "" [!openFile.(!cursory - 1); !openFile.(!cursory)] |];
                (Array.sub !openFile (!cursory + 1) ((Array.length !openFile) - !cursory - 1));
            ];
            cursory := !cursory - 1;
        )
        else if !cursorx > 0 then (
            let line = !openFile.(!cursory) in
            !openFile.(!cursory) <- concat
                ""
                [sub line 0 (!cursorx - 1); sub line !cursorx ((length line) - !cursorx)];
            cursorx := !cursorx - 1
        )
    )

    (* Enter *)
    else if event = Key.enter || event = 10 then (
        let line = !openFile.(!cursory) in
        openFile := Array.concat [
            (Array.sub !openFile 0 !cursory);
            [| sub line 0 !cursorx; sub line !cursorx ((length line) - !cursorx) |];
            (Array.sub !openFile (!cursory + 1) ((Array.length !openFile) - (!cursory + 1)));
        ];
        cursorx := 0;
        cursory := !cursory + 1
    )

    (* Tab *)
    else if event = 9 then (
        let line = !openFile.(!cursory) in
        !openFile.(!cursory) <- concat
            (make 4 ' ')
            [sub line 0 !cursorx; sub line !cursorx ((length line) - !cursorx)];
        cursorx := !cursorx + 4
    )

    (* Regular character entry which isn't caught by earlier conditionals. *)
    else try (
        let line = !openFile.(!cursory) in
        !openFile.(!cursory) <- concat
            (Char.escaped (char_of_int event))
            [sub line 0 !cursorx; sub line !cursorx ((length line) - !cursorx)];
        cursorx := !cursorx + 1
    ) with e -> (print_int event; print_endline "")

and mainLoop screen = 
    renderScreen !openFile (
        let y, x = get_size() in
        if !cursory < (y / 2) then
            0
        else
            !cursory - (y / 2)
    ) (length (parseInt(Array.length !openFile)));

    let event = getch() in 
    if event != 27 && event != 3 then (
        erase();
        keyEvent event;
        mainLoop screen;
    )
;;

(* 
    This is the 'main' which initializes curses.
*)
let () =
    let screen = initscr() in (
        raw();
        start_color();
        keypad screen true;
        use_default_colors();
        init_pair 1 7 0;
        mainLoop screen
    );
    endwin();
;;
