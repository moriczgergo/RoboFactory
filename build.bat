@ECHO OFF
rgbasm -omain.obj main.asm
xlink -mmain.map -nmain.sym LINK
rgbfix -p 0 -v robofactory.gbc
bgb robofactory.gbc