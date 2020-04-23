# NanOCaml

To begin, I must give credit to Shiva Sudanagunta and Elliot Van Huijgevoort. This project was a team effort and none of this would have been possible without them.

This is a functional text editor based in the terminal, similar to nano. Instructions for its use are in install.txt.

NOTE: The dictionary we use from spellcheck comes from:
 https://github.com/dwyl/english-words

BEFORE using NanOCaml, if you are using a Mac, please make sure that in your 
terminal preferences, the option key has been set to be the meta key. To do 
this, got to Terminal -> Preferences, move to the Keyboard tab,
then check the box saying "Use Option as Meta Key".

To build NanOCaml, simply run "make run". You will have to wait about 4 seconds 
for NanOCaml to load up since we need to build the massive 300,000 word dictionary 
we use for spellchecking. After this is done, you will be prompted to enter a 
directory name from the /users directory for Macs, and the home directory for 
Linux. If the directory doesn't exist you will be prompted to enter a valid 
directory name. 

Once a valid directory name is provided, the directory navigator will display 
where you can search through to find a file (navigate using the arrow keys 
and "enter".), or type the name of the file you are looking for (ctrl + n to 
type the name). If the file doesn't exist in the current directory, it will be 
created in the current directory, otherwise you should 
see the text in the file load up. 

There are no third party libraries currently being used. 

Navigating Directory:
Arrow keys -> move up and down to select files
Return --> Enter selected directory or select file for editing
Ctrl+N --> Search for directory or file in current directory, and enter that 
    directory/edit that file
Select ".." directory at end of every menu to move back a directory

Current key bindings to use in NanOCaml main editor window:

The notation for shortcuts is as follows: Control-key sequences are notated with
a caret (^) symbol and can be entered by using the Control (Ctrl) key.  
Escape-key sequences are notated with the Meta (M-) symbol and can be entered 
using either the Alt or Meta key depending on your keyboard setup. Once again, 
to assure all capabilities of NanoCaml are possible, please assign the 
Alt/Option key as the Meta key in your terminal preferences
before using the editor.

WE HAVE INCLUDED "dummy.txt" and "alice.txt" (courtesy of Project Gutenberg)
for your convenience in trying out NanOCaml. Please note, right now, we have 
limited the readable files to .txts, .mls, and .mlis. It is possible to read 
other text files (.py, .java, etc.) but we have yet to handle situations
such as trying to open .jpgs and .bytes among other possible file types.

KEY BINDINGS:
Arrow keys --> move cursor up, down, left, and right

Letters/numbers/space bar --> Input all letter characters on the keyboard

Return --> Creates new line, and moves to beginning of next line

Delete --> "Backspace"/"Delete"

Tab --> Tab spacing

^X --> Exit out of NanOCaml

^U --> Undo previous action (characters or whitespace)

^R --> Redo the undone action (characters or whitespace)

^F --> Find and replace 

^N --> Exit current file, search directory for another file or create new file

^P --> Save file, but allow continued editing

^H --> Display help page

M-X --> Cut

M-C --> Copy 

M-V --> Paste

M-A --> Move cursor to beginning of current line

M-D --> Move cursor to end of current line

M-W --> Move cursor to beginning of current file

M-S --> Move cursor to end of current file

M-J --> Jump to specific line of file

M-T --> Toggle spellcheck

M-P --> Toggle Portal at current line of file

M-Shift+(<) --> Jump up to above portal

M-Shift+(>) --> Jump down to below portal
