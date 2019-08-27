// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(WAITKEY)
@KBD
D=M
@WAITKEY
D;JEQ

@8192
D=A
@words
M=D

@word
M=0

(PAINTBLACK)
@SCREEN
D=A
@word
D=D+M
A=D

M=-1

@word
M=M+1
@words
D=M
@word
D=D-M

@WAITZERO
D;JEQ

@PAINTBLACK
0;JMP

(WAITZERO)
@KBD
D=M
@WAITZERO
D;JNE

@word
M=0

(PAINTWHITE)
@SCREEN
D=A
@word
D=D+M
A=D

M=0

@word
M=M+1
@words
D=M
@word
D=D-M

@WAITKEY
D;JEQ

@PAINTWHITE
0;JMP
